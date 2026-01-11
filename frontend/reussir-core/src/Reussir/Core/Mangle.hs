{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Reussir.Core.Mangle
Description : Name mangling scheme for Reussir.

= Overview

This module implements the name mangling scheme for Reussir, borrowing heavily
from Rust's v0 symbol format. Mangled names are used to generate unique,
linker-safe symbols for functions, types, and other entities.

= Complete BNF Grammar

@
mangled-name    → "_R" symbol

symbol          → path
                | type

path            → crate-root
                | nested-path

crate-root      → "C" identifier

nested-path     → "N" namespace path identifier

namespace       → "v"                                    -- value namespace (functions, variables)

identifier      → decimal-number [disambiguator] body
                | "u" decimal-number [disambiguator] body  -- unicode (punycode)

disambiguator   → "_"                                    -- only if body starts with digit or '_'

body            → {ASCII-char}                           -- for ASCII identifiers
                | {punycode-char}                        -- for unicode (with '-' replaced by '_')

decimal-number  → {digit}+

generic-args    → "I" path {generic-arg}+ "E"

generic-arg     → type
                | capability

type            → basic-type
                | "I" path {type}+ "E"                   -- generic type application (TypeRecord)
                | "F" "K" identifier {type}* "E" type    -- closure type
                | "z"                                    -- bottom type (never)

basic-type      → "b"                                    -- bool
                | "e"                                    -- str
                | "u"                                    -- unit
                | integral-type
                | float-type

integral-type   → "a"                                    -- i8
                | "s"                                    -- i16
                | "l"                                    -- i32
                | "x"                                    -- i64
                | "h"                                    -- u8
                | "t"                                    -- u16
                | "m"                                    -- u32
                | "y"                                    -- u64

float-type      → "f"                                    -- f32
                | "d"                                    -- f64
                | "C" "3" "f16"                          -- f16
                | "C" "4" "f128"                         -- f128
                | "C" "4" "bf16"                         -- bfloat16
                | "C" "2" "f8"                           -- float8

capability      → "C" decimal-number capability-name

capability-name → "Flex" | "Rigid" | "Shared" | "Value"
                | "Unspecified" | "Field" | "Regional"

base-62-number  → {base-62-digit}* "_"

base-62-digit   → digit                                  -- 0-9 → values 0-9
                | lower                                  -- a-z → values 10-35
                | upper                                  -- A-Z → values 36-61
@

= Encoding Details

== Base-62 Numbers

Used for encoding indices. The encoding is:

  * Value 0 encodes as just @_@
  * Value n > 0 encodes as @base62(n-1)@ followed by @_@

Examples:

  * 0   → @_@
  * 1   → @0_@
  * 11  → @a_@
  * 62  → @Z_@
  * 63  → @10_@
  * 1000 → @g7_@

== Identifiers

  * ASCII identifiers: @length + body@ (with @_@ separator if body starts with digit or @_@)
  * Unicode identifiers: @u + length + punycode'(body)@ where @-@ is replaced with @_@

Examples:

  * @example@ → @7example@
  * @_foo@ → @4__foo@ (note the disambiguator)
  * @日本語@ → @u9_xn8h1c@

== Paths

Paths are mangled recursively:

  * Single identifier: @C@ + identifier
  * Nested path: @Nv@ + prefix + identifier

Examples:

  * @foo@ → @C3foo@
  * @foo::bar@ → @NvC3foo3bar@
  * @a::b::c@ → @NvNvC1a1b1c@

== Generic Types

Generic type applications use the @I...E@ wrapper:

  * @Foo\<T\>@ → @IC3Foo⟨mangled T⟩E@
  * @Foo\<T, U\>@ → @IC3Foo⟨mangled T⟩⟨mangled U⟩E@

== Special Types

  * @Rc\<T, Cap\>@ → @IC2Rc⟨mangled T⟩⟨mangled Cap⟩E@
  * @Ref\<T, Cap\>@ → @IC3Ref⟨mangled T⟩⟨mangled Cap⟩E@
  * Closures: @FK⟨"ReussirClosure" identifier⟩⟨arg types⟩E⟨return type⟩@
-}
module Reussir.Core.Mangle where

import Data.Char (isAscii)
import Data.Char qualified as C
import Data.Text qualified as T
import Data.Text.Builder.Linear as TB
import Data.Text.Encoding qualified as TE
import Data.Text.Punycode qualified as Punycode
import Reussir.Core.Types.Type (FloatingPointType (..), IntegralType (..), Type (..))
import Reussir.Parser.Types.Capability (Capability (..))
import Reussir.Parser.Types.Lexer

-- | Typeclass for types that can be mangled into a symbol representation.
class Manglable a where
    -- | Convert a value to its mangled representation as a Builder.
    mangle :: a -> TB.Builder

{- |
A base-62 number, used for encoding generic indices.

    0-9 maps to 0-9
    a-z maps to 10 to 35
    A-Z maps to 36 to 61

    base-62-number → { digit | lower | upper } _

Example:
    0	    _
    1	    0_
    11	    a_
    62	    Z_
    63	    10_
    1000	g7_
-}
newtype B62Num = B62Num Int

-- | Convert a digit value (0-61) to its base-62 character
digitToChar :: Int -> Char
digitToChar d
    | d < 10 = toEnum (fromEnum '0' + d)
    | d < 36 = toEnum (fromEnum 'a' + d - 10)
    | otherwise = toEnum (fromEnum 'A' + d - 36)

-- | Encode a non-negative integer to base-62 representation (without trailing _)
encodeB62 :: Int -> TB.Builder
encodeB62 n
    | n < 62 = TB.fromChar (digitToChar n)
    | otherwise = encodeB62 (n `div` 62) <> TB.fromChar (digitToChar (n `mod` 62))

instance Manglable B62Num where
    mangle (B62Num n)
        | n == 0 = TB.fromChar '_'
        | otherwise = encodeB62 (n - 1) <> TB.fromChar '_'

{- | For path mangling, we directly use the nested path mangling
scheme from v0 symbol format.
1. single root path: C + baseName
2. nested path: `Nv` + prefix + baseName

Example:
    1. C7example -> example
    2. NvC7mycrate7example -> mycrate::example
    3. NvNvC1a7example7exampla -> a::example::exampla
-}
instance Manglable Path where
    mangle (Path baseName segments) = go (reverse segments) baseName
      where
        go [] name =
            -- Root path: C + baseName
            TB.fromChar 'C' <> mangle name
        go (s : ss) name =
            -- Nested path: Nv + prefix + baseName
            TB.fromChar 'N' <> TB.fromChar 'v' <> go ss s <> mangle name

{- | For ascii identifiers, the mangle is simply length+identifier.
| For unicode identifiers, the mangle is `u` + length + punycode'(identifier). where punycode' is the punycode encoding where `-` are replaced with `_`.
| If the identifier (or punycode) starts with a digit or an underscore, add `_` between the length and the body.
-}
instance Manglable Identifier where
    mangle (Identifier name)
        | T.all isAscii name =
            -- ASCII: length + [_] + identifier
            let sep = if startsWithDigitOrUnderscore name then TB.fromChar '_' else mempty
             in TB.fromDec (T.length name) <> sep <> TB.fromText name
        | otherwise =
            -- Unicode: u + length + [_] + punycode'(identifier)
            let encoded = TE.decodeASCII $ Punycode.encode name
                -- Replace '-' with '_' in the punycode output
                punycode' = T.map (\c -> if c == '-' then '_' else c) encoded
                sep = if startsWithDigitOrUnderscore punycode' then TB.fromChar '_' else mempty
             in TB.fromChar 'u'
                    <> TB.fromDec (T.length punycode')
                    <> sep
                    <> TB.fromText punycode'

-- | Check if a Text starts with a digit
startsWithDigitOrUnderscore :: T.Text -> Bool
startsWithDigitOrUnderscore t = case T.uncons t of
    Just (c, _) -> C.isDigit c || c == '_'
    Nothing -> False

{- | Floating-point type mangling.

BNF:
@
float-type      → "f"                  -- f32 (IEEEFloat 32)
                | "d"                  -- f64 (IEEEFloat 64)
                | "C" "3" "f16"        -- f16 (IEEEFloat 16)
                | "C" "4" "f128"       -- f128 (IEEEFloat 128)
                | "C" "4" "bf16"       -- bfloat16
                | "C" "2" "f8"         -- float8
@
-}
instance Manglable FloatingPointType where
    mangle Float8 = "C2f8" -- float8 → C2f8
    mangle BFloat16 = "C4bf16" -- bfloat16 → C4bf16
    mangle (IEEEFloat 16) = "C3f16" -- f16 → C3f16
    mangle (IEEEFloat 32) = "f" -- f32 → f
    mangle (IEEEFloat 64) = "d" -- f64 → d
    mangle (IEEEFloat 128) = "C4f128" -- f128 → C4f128
    mangle (IEEEFloat _) = error "Unsupported floating point type"

{- | Integral type mangling.

BNF:
@
integral-type   → "a"        -- i8 (Signed 8)
                | "s"        -- i16 (Signed 16)
                | "l"        -- i32 (Signed 32)
                | "x"        -- i64 (Signed 64)
                | "h"        -- u8 (Unsigned 8)
                | "t"        -- u16 (Unsigned 16)
                | "m"        -- u32 (Unsigned 32)
                | "y"        -- u64 (Unsigned 64)
@
-}
instance Manglable IntegralType where
    mangle (Signed 8) = "a" -- i8 → a
    mangle (Signed 16) = "s" -- i16 → s
    mangle (Signed 32) = "l" -- i32 → l
    mangle (Signed 64) = "x" -- i64 → x
    mangle (Signed _) = error "Unsupported signed integer type"
    mangle (Unsigned 8) = "h" -- u8 → h
    mangle (Unsigned 16) = "t" -- u16 → t
    mangle (Unsigned 32) = "m" -- u32 → m
    mangle (Unsigned 64) = "y" -- u64 → y
    mangle (Unsigned _) = error "Unsupported unsigned integer type"

{- | Capability mangling.

Capabilities are mangled as crate-root paths with their names.

BNF:
@
capability      → "C" decimal-number capability-name
capability-name → "Flex" | "Rigid" | "Shared" | "Value"
                | "Unspecified" | "Field" | "Regional"
@

Examples:
  * Flex → C4Flex
  * Unspecified → C11Unspecified
-}
instance Manglable Capability where
    mangle Flex = "C4Flex" -- Flex → C4Flex
    mangle Rigid = "C5Rigid" -- Rigid → C5Rigid
    mangle Shared = "C6Shared" -- Shared → C6Shared
    mangle Value = "C5Value" -- Value → C5Value
    mangle Unspecified = "C11Unspecified" -- Unspecified → C11Unspecified
    mangle Field = "C5Field" -- Field → C5Field
    mangle Regional = "C7Regional" -- Regional → C7Regional

{- | Mangle a path with type/capability arguments.

BNF:
@
generic-args    → "I" path {generic-arg}+ "E"
generic-arg     → type | capability
@

If the argument list is empty, the path is mangled without the I...E wrapper.

Examples:
  * @Foo@ (no args) → @C3Foo@
  * @Foo<i32>@ → @IC3FoolE@
  * @Foo<i32, bool>@ → @IC3FoolbE@
-}
manglePathWithArgs :: Path -> [Either Type Capability] -> TB.Builder
manglePathWithArgs path [] = mangle path
manglePathWithArgs path args =
    TB.fromChar 'I' <> mangle path <> foldMap (either mangle mangle) args <> TB.fromChar 'E'

{- | Type mangling.

BNF:
@
type            → basic-type
                | "I" path {type}+ "E"                   -- TypeRecord with type args
                | "F" "K" identifier {type}* "E" type    -- TypeClosure
                | "z"                                    -- TypeBottom

basic-type      → "b"                                    -- TypeBool
                | "e"                                    -- TypeStr
                | "u"                                    -- TypeUnit
                | integral-type                          -- TypeIntegral
                | float-type                             -- TypeFP
@

Examples:
  * @bool@ → @b@
  * @i32@ → @l@
  * @Foo<i32>@ → @IC3FoolE@
  * @(i32) -> bool@ → @FK14ReussirClosurelEb@
  * @Rc<i32, Shared>@ → @IC2RclC6SharedE@
-}
instance Manglable Type where
    -- Delegate to integral type mangling
    mangle (TypeIntegral it) = mangle it
    -- Delegate to floating-point type mangling
    mangle (TypeFP fpt) = mangle fpt
    -- Basic types: bool → b, str → e, unit → u, bottom → z
    mangle TypeBool = "b"
    mangle TypeStr = "e"
    mangle TypeUnit = "u"
    mangle TypeBottom = "z"
    -- Closure: F + K + "ReussirClosure" identifier + arg types + E + return type
    mangle (TypeClosure args ret) =
        "FK" <> mangle (Identifier "ReussirClosure") <> foldMap mangle args <> "E" <> mangle ret
    -- Rc<T, Cap>: treated as generic type Rc with type and capability args
    mangle (TypeRc t cap) = manglePathWithArgs (Path "Rc" []) [Left t, Right cap]
    -- Ref<T, Cap>: treated as generic type Ref with type and capability args
    mangle (TypeRef t cap) = manglePathWithArgs (Path "Ref" []) [Left t, Right cap]
    -- Generic types should be resolved before mangling
    mangle (TypeGeneric _) = error "Unsupported generic type in ABI mangle"
    -- Holes should be resolved before mangling
    mangle (TypeHole _) = error "Unsupported hole type in ABI mangle"
    -- TypeRecord: mangle path with type arguments
    mangle (TypeRecord path tyArgs) = manglePathWithArgs path (map Left tyArgs)

{- | Generate a complete ABI-mangled name.

Prepends "_R" prefix to the mangled symbol.

BNF:
@
mangled-name    → "_R" symbol
@

Examples:
  * @foo@ (path) → @_RC3foo@
  * @foo::bar@ → @_RNvC3foo3bar@
  * @i32@ (type) → @_Rl@
  * @Foo<i32>@ → @_RIC3FoolE@
-}
mangleABIName :: (Manglable a) => a -> T.Text
mangleABIName x = TB.runBuilder . ("_R" <>) . mangle $ x
