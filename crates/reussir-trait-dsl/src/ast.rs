//! The `builtin_traits!` grammar and its `syn` parser.
//!
//! Every token in the DSL is a real Rust keyword or punctuation, so `syn`'s
//! keyword tokens carry the parse:
//!
//! ```text
//! input      ::= item*
//! item       ::= trait-item | impl-item
//! trait-item ::= attr* 'trait' ident (':' ident ('+' ident)*)? (';' | '{' member* '}')
//! member     ::= assoc-type | method
//! assoc-type ::= 'type' ident '=' tyref ';'
//! method     ::= 'fn' ident '(' (ident ':' tyref) (',' ident ':' tyref)* ')'
//!                ('->' tyref)? ';'
//! tyref      ::= 'Self' | 'Self' '::' ident | ident
//! impl-item  ::= 'impl' ident 'for' ident ('|' ident)* ';'
//! attr       ::= '#[sealed]' | '#[structural]' | doc comment
//! ```

use syn::Token;
use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream};

pub(crate) struct Input {
    pub items: Vec<Item>,
}

pub(crate) enum Item {
    Trait(TraitDecl),
    Impl(ImplDecl),
}

/// `type Name = Default;` in a trait declaration.
pub(crate) struct AssocTy {
    pub name: syn::Ident,
    pub default: TypeRef,
}

pub(crate) struct TraitDecl {
    /// Forwarded doc comments (rendered onto the generated `Builtins` field).
    pub docs: Vec<syn::Attribute>,
    pub sealed: bool,
    pub structural: bool,
    pub name: syn::Ident,
    pub supertraits: Vec<syn::Ident>,
    /// Associated types with their defaults. The impls of the table bind
    /// nothing, so every one takes its default.
    pub assoc_tys: Vec<AssocTy>,
    pub methods: Vec<MethodSig>,
}

pub(crate) struct MethodSig {
    pub name: syn::Ident,
    pub params: Vec<(syn::Ident, TypeRef)>,
    pub ret: Option<TypeRef>,
}

pub(crate) enum TypeRef {
    SelfTy,
    /// `Self::Name`: an associated type of the trait being declared.
    Assoc(syn::Ident),
    Scalar(syn::Ident),
}

pub(crate) struct ImplDecl {
    pub trait_name: syn::Ident,
    pub self_tys: Vec<syn::Ident>,
}

impl Parse for Input {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut items = Vec::new();
        while !input.is_empty() {
            items.push(input.parse()?);
        }
        Ok(Input { items })
    }
}

impl Parse for Item {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![impl]) {
            return Ok(Item::Impl(input.parse()?));
        }
        Ok(Item::Trait(input.parse()?))
    }
}

impl Parse for TraitDecl {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attrs = input.call(syn::Attribute::parse_outer)?;
        let mut docs = Vec::new();
        let mut sealed = false;
        let mut structural = false;
        for attr in attrs {
            if attr.path().is_ident("doc") {
                docs.push(attr);
            } else if attr.path().is_ident("sealed") {
                sealed = true;
            } else if attr.path().is_ident("structural") {
                structural = true;
            } else {
                return Err(syn::Error::new_spanned(
                    &attr,
                    "unknown attribute; `builtin_traits!` accepts `#[sealed]`, \
                     `#[structural]`, and doc comments",
                ));
            }
        }
        input.parse::<Token![trait]>()?;
        let name: syn::Ident = input.parse()?;
        let mut supertraits = Vec::new();
        if input.peek(Token![:]) {
            input.parse::<Token![:]>()?;
            supertraits.push(input.parse()?);
            while input.peek(Token![+]) {
                input.parse::<Token![+]>()?;
                supertraits.push(input.parse()?);
            }
        }
        let mut assoc_tys = Vec::new();
        let mut methods = Vec::new();
        if input.peek(syn::token::Brace) {
            let body;
            syn::braced!(body in input);
            while !body.is_empty() {
                if body.peek(Token![type]) {
                    assoc_tys.push(body.parse()?);
                } else {
                    methods.push(body.parse()?);
                }
            }
        } else {
            input.parse::<Token![;]>()?;
        }
        Ok(TraitDecl {
            docs,
            sealed,
            structural,
            name,
            supertraits,
            assoc_tys,
            methods,
        })
    }
}

impl Parse for MethodSig {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![fn]>()?;
        let name: syn::Ident = input.parse()?;
        let args;
        syn::parenthesized!(args in input);
        let mut params = Vec::new();
        while !args.is_empty() {
            // `self` is a keyword to Rust but an ordinary parameter name in
            // the DSL's signature grammar.
            let pname: syn::Ident = args.call(syn::Ident::parse_any)?;
            args.parse::<Token![:]>()?;
            let ty: TypeRef = args.parse()?;
            params.push((pname, ty));
            if args.peek(Token![,]) {
                args.parse::<Token![,]>()?;
            }
        }
        let ret = if input.peek(Token![->]) {
            input.parse::<Token![->]>()?;
            Some(input.parse()?)
        } else {
            None
        };
        input.parse::<Token![;]>()?;
        Ok(MethodSig { name, params, ret })
    }
}

impl Parse for AssocTy {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![type]>()?;
        let name = input.parse()?;
        input.parse::<Token![=]>()?;
        let default = input.parse()?;
        input.parse::<Token![;]>()?;
        Ok(AssocTy { name, default })
    }
}

impl Parse for TypeRef {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.parse::<Option<Token![Self]>>()?.is_none() {
            return Ok(TypeRef::Scalar(input.parse()?));
        }
        if input.parse::<Option<Token![::]>>()?.is_some() {
            return Ok(TypeRef::Assoc(input.parse()?));
        }
        Ok(TypeRef::SelfTy)
    }
}

impl Parse for ImplDecl {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![impl]>()?;
        let trait_name: syn::Ident = input.parse()?;
        input.parse::<Token![for]>()?;
        let mut self_tys = vec![input.parse()?];
        while input.peek(Token![|]) {
            input.parse::<Token![|]>()?;
            self_tys.push(input.parse()?);
        }
        input.parse::<Token![;]>()?;
        Ok(ImplDecl {
            trait_name,
            self_tys,
        })
    }
}
