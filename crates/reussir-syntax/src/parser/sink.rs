//! Translates the flat event stream into a lossless `cstree` green tree,
//! re-attaching trivia (whitespace and comments) between the meaningful
//! tokens.
//!
//! Nodes are built with [`GreenNode::new`], not with
//! [`GreenNodeBuilder::finish_node`]. The builder's node cache hands out an
//! earlier node for any new node of at most three children that has the same
//! kind, text length and 32-bit hash of its children; it never compares the
//! children themselves. Two different subtrees can meet that condition, and
//! the later one then became a copy of the earlier one. In large files a
//! literal was replaced by a variable of an unrelated function (an "unknown
//! variable" error), or by a variable in scope, with no diagnostic. The
//! builder is still used to intern token text and create the tokens: its
//! token cache compares whole tokens (kind, text key and length), so sharing
//! tokens is sound.

use cstree::Syntax;
use cstree::build::GreenNodeBuilder;
use cstree::green::{GreenNode, GreenToken};
use cstree::interning::Interner;
use cstree::util::NodeOrToken;

use super::Event;
use crate::kind::SyntaxKind;
use crate::lexer::Token;

pub(crate) struct Sink<'s> {
    /// All tokens, trivia included.
    tokens: &'s [Token],
    /// The green token of each entry of `tokens`, at the same index.
    green_tokens: Vec<GreenToken>,
    cursor: usize,
    events: Vec<Event>,
    /// The open nodes, innermost last: each node's kind and the index in
    /// `children` of its first child.
    parents: Vec<(SyntaxKind, usize)>,
    /// The children of the open nodes, in order; a node's children are the
    /// tail of this list that starts at its index in `parents`.
    children: Vec<NodeOrToken<GreenNode, GreenToken>>,
}

impl<'s> Sink<'s> {
    pub(crate) fn new<I>(
        source: &'s str,
        tokens: &'s [Token],
        events: Vec<Event>,
        interner: &mut I,
    ) -> Self
    where
        I: Interner<cstree::interning::TokenKey>,
    {
        Sink {
            tokens,
            green_tokens: green_tokens(source, tokens, interner),
            cursor: 0,
            events,
            parents: Vec::new(),
            children: Vec::new(),
        }
    }

    pub(crate) fn finish(mut self) -> GreenNode {
        for idx in 0..self.events.len() {
            match std::mem::replace(&mut self.events[idx], Event::Token) {
                Event::Start { kind: None, .. } => {}
                Event::Start {
                    kind: Some(kind),
                    forward_parent,
                } => {
                    // Resolve the forward-parent chain: parents created by
                    // `precede` appear later in the stream but must open
                    // before this node.
                    let mut kinds = vec![kind];
                    let mut fp = forward_parent;
                    while let Some(parent_idx) = fp {
                        let parent_idx = parent_idx as usize;
                        match std::mem::replace(&mut self.events[parent_idx], Event::Token) {
                            Event::Start {
                                kind,
                                forward_parent,
                            } => {
                                if let Some(kind) = kind {
                                    kinds.push(kind);
                                }
                                fp = forward_parent;
                            }
                            _ => unreachable!("forward parent must be a start event"),
                        }
                        // Mark the parent event as consumed (tombstone).
                        self.events[parent_idx] = Event::Start {
                            kind: None,
                            forward_parent: None,
                        };
                    }
                    for kind in kinds.into_iter().rev() {
                        // Outermost first: each `precede` parent wraps
                        // everything started after it.
                        self.attach_trivia();
                        self.parents.push((kind, self.children.len()));
                    }
                }
                Event::Finish => {
                    if self.parents.len() == 1 {
                        // The root is about to close: pull in any trailing
                        // trivia so the tree stays lossless.
                        self.attach_trivia();
                    }
                    let (kind, first_child) = self.parents.pop().unwrap();
                    let node = GreenNode::new(kind.into_raw(), self.children.drain(first_child..));
                    self.children.push(NodeOrToken::Node(node));
                }
                Event::Token => {
                    self.attach_trivia();
                    self.token();
                }
            }
        }
        assert!(self.parents.is_empty() && self.children.len() == 1);
        match self.children.pop() {
            Some(NodeOrToken::Node(root)) => root,
            _ => unreachable!("the root must be a node"),
        }
    }

    /// Trivia (and lexer error tokens, which the parser never consumes)
    /// attaches to the innermost open node; before a node opens this is the
    /// enclosing node, so leading whitespace does not become part of the
    /// new node. Nothing can be attached before the root is open.
    fn attach_trivia(&mut self) {
        if self.parents.is_empty() {
            return;
        }
        while let Some(token) = self.tokens.get(self.cursor) {
            if !token.kind.is_trivia() && token.kind != SyntaxKind::ErrorToken {
                break;
            }
            self.token();
        }
    }

    fn token(&mut self) {
        let token = self.green_tokens[self.cursor].clone();
        self.children.push(NodeOrToken::Token(token));
        self.cursor += 1;
    }
}

/// Create the green token of every lexed token, in order, interning its text
/// into `interner`. The builder only offers tokens as children of a node, so
/// they are collected under a scratch node that is then dropped.
fn green_tokens<I>(source: &str, tokens: &[Token], interner: &mut I) -> Vec<GreenToken>
where
    I: Interner<cstree::interning::TokenKey>,
{
    let mut builder: GreenNodeBuilder<'_, '_, SyntaxKind, I> =
        GreenNodeBuilder::with_interner(interner);
    builder.start_node(SyntaxKind::SourceFile);
    for token in tokens {
        builder.token(token.kind, token.text(source));
    }
    builder.finish_node();
    let (scratch, _cache) = builder.finish();
    scratch
        .children()
        .map(|child| {
            child
                .into_token()
                .expect("the scratch node holds only tokens")
                .clone()
        })
        .collect()
}
