{-# LANGUAGE OverloadedStrings #-}
module CodeGen where

import Prelude hiding (take, drop, head, tail)
import Data.Text hiding (reverse, zip, filter)
import Data.Monoid
import System.Environment
import Language.Haskell.Exts as HS hiding (prettyPrint)

data Swift = Swift { _name :: Text, _derive :: [Text]
                   , _tyargs :: [Text]
                   , _cons :: [(Text, [(Either Text Text, Text)])] }

prettyPrint :: Swift -> Text
prettyPrint sw@(Swift n cs ts ct) =
  -- struct Foo<A> : Bar, Baz {
      st' <~> n <> tyargs <> cs' <~> "{"
  
  -- let x: T
  -- let y: Box<Foo<A>>
  -- case Boop(x: T)
  <|> (case ct of
        [(con, xs)] -> mkCon xs
        (_:_) -> intercalate "\n" (fmap (\(con, xs) -> "  case" <~> con <> "(" <> intercalate ", " (fmap mcon xs) <> ")") ct))
  
  -- init functions and .create
  -- TODO
  
  -- deriving function requirements
  <|> intercalate "\n" (filter (/= "") (fmap (runDriverInside sw) cs))
  
  -- insert quasi quoted swift
  -- TODO
  
  -- }
  <|> "}"
  
  -- external function requirements (i.e. extensions)
  <|> intercalate "\n" (filter (/= "") (fmap (runDriverOutside sw) cs))
  
  where
    st' = case ct of
      [_] -> "struct"
      _ -> "enum"
    cs' = case cs of
      (x:xs) -> " :" <~> intercalate ", " (x:xs)
      [] -> ""
    tyargs = case ts of
      -- TODO: should this supported class constraints? No. but maybe?
      (x:xs) -> "<" <> intercalate ", " (x:xs) <> ">"
      [] -> ""
    -- single con
    mkCon xs = intercalate "\n" (fmap (\(n, t) -> "  let" <~> argname n <> t) xs)
      where
        argname (Left v) = v <> ": "
        argname (Right v) = v <> ": "
    
    -- multi con
    mcon (Left n, t) = t
    mcon (Right n, t) = n <> ": " <> t

-- all the magic
mkData :: DataOrNew -> Context -> Name -> [TyVarBind] -> [QualConDecl] -> [Deriving] -> Swift
mkData _ (x:xs) _ _ _ _         = error "constraints on data types not supported"
mkData _ _ (Symbol _) _ _ _     = error "symbols as names of data types not supported"
mkData _dn _ (Ident n) bs qs dr = Swift n' dr' bs' qs'
  where
    n' = pack n
    bs' = fmap mktyarg bs
    qs' = fmap mkcon qs
    dr' = fmap (dequal . fst) dr -- discard type arguments, multi param type classes not supported
    
    mktyarg (KindedVar _ _)          = error "kind annotations not supported"
    mktyarg (UnkindedVar (Ident n))  = toTitle' n
    mktyarg (UnkindedVar (Symbol _)) = error "symbols not supported in type arguments"
    
    -- TODO: un-support forall bindings
    mkcon (QualConDecl _ tvs ctx (ConDecl n ts)) = (unn n, fmap (\(k, t) ->
              (Left ("a" <> pack (show k)), typ (unb t))) (zip [1..] ts))
    mkcon (QualConDecl _ tvs ctx (RecDecl n ts)) = (unn n, fmap (\(ns, t) ->
              (Right (nameRec ns), typ (unb t))) ts)
    mkcon (QualConDecl _ tvs ctx (InfixConDecl _ _ _)) = error "infix con decls not supported"
    
    -- drop 1 _ from the start of records
    nameRec [(Ident ('_':n))] = pack n
    nameRec [(Ident n)] = pack n
    nameRec [(Symbol n)] = error "symbolic record names not supported"
    nameRec _ = error "multiple names on single record not supported" -- could be, does anyone use this?
    
    unb (UnBangedTy t) = t
    unb _ = error "bangs on types not supported"

typ :: Type -> Text
typ (TyTuple _ ts)                             = "(" <> intercalate ", " (fmap typ ts) <> ")"
typ (TyApp (TyCon (UnQual (Ident "Maybe"))) y) = typ y <> "?"
typ (TyApp x y)                                = let
  (h:xs)                                       = reverse (y:foldup x)
  in (typ h) <> "<" <> intercalate ", " (fmap typ xs) <> ">"
typ (TyVar (Ident n))                          = toTitle' n
typ (TyVar (Symbol _))                         = error "symbolic tyvars not supported"
typ (TyCon q)                                  = dequal q
typ (TyParen t)                                = typ t
typ (TyFun x y)                                = error $ "tyfun not supported: " <> (show (x, y))
-- not in language
typ (TyForall _ _ _)                           = error "foralls not supported"
typ (TyList _)                                 = error "list syntax not supported"
typ (TyInfix _ _ _)                            = error "infix types not supported"
typ (TyKind _ _)                               = error "type kinds not supported"
typ (TyPromoted _)                             = error "promoted types not supported"

foldup (TyApp l r) = (r:foldup l)
foldup x = [x]

-- deriving drivers

runDriverInside :: Swift -> Text -> Text
runDriverInside (Swift n _ _ cs) "Printable" =
      "  var description: String {"
  <|> "    get {"
  <|> (case cs of
    [(con, [])] ->
        "      return \"" <> n <> "()\""
    [(con, xs)] -> -- 1 con, n fields, struct
        "      return" <~> showCase True con xs
    
    _ -> -- enum, n cons
        "      switch self {"
        <|> intercalate "\n" (fmap (mkEqCases False showCase) cs)
        <|> "    }")
  <|> "    }"
  <|> "  }"
  where
    descStr (n, _) = "\\(" <> n <> ")"
    showCase _ con xs = "\"" <> con <> "(" <> intercalate ", " (fmap (mkQuote . get . fst) xs) <> ")\""
    mkQuote x = "\\(" <> x <> ")"
runDriverInside sw "Equatable" = ""
-- runDriverInside sw "Comparable" = ""
-- runDriverInside sw "Hashable" = ""
-- swiftz
runDriverInside (Swift n d ts [cs]) "JSON" = "" -- error "JSON not implemented"
  --     "  static func fromJSON(x: JSValue) -> T? {"
  -- <|> "    var v$N: $T?"
  -- <|> "    switch x {"
  -- <|> "      case let .JSObject(d):"
  -- <|> "        v$N = d[\"$N\"] >>= $JST"
  -- <|> "        if ($NS) {"
  -- <|> "          return $T($N: v$N!, ...)"
  -- <|> "        } else {"
  -- <|> "          return nil"
  -- <|> "        }"
  -- <|> "      default: return nil"
  -- <|> "    }"
  -- <|> "  }"
  -- -- toJson
  -- <|> "  func toJSON() -> JSValue {"
  -- <|> "  }"

-- We could do this one day
runDriverInside _ "JSON" = error "json can only be derived on a struct, not an enum"
runDriverInside sw x = error ("no known deriver for " <> unpack x)

runDriverOutside :: Swift -> Text -> Text
runDriverOutside sw "Printable" = ""
runDriverOutside (Swift n _ ty cons) "Equatable" =
  "func==" <> tyParams <> "(lhs: " <> thetype <> ", rhs: " <> thetype <> ") -> Bool {"
  
  <|>
  (case cons of
    [(con, [])] -> "  return true" -- 1 con, 0 fields
    [(con, xs)] -> -- 1 con, n fields, struct
        "  return" <~> crossProdIf True con xs
    
    _ -> -- enum, n cons
        "  switch (lhs, rhs) {"
        <|> intercalate "\n" (fmap (mkEqCases True crossProdIf) cons)
        <|> "  }")
  <|>
  "}"
  where
    tyParams = case ty of
      [] -> ""
      (_:_) -> "<" <> intercalate ", " (fmap (\x -> x <> ": Equatable") ty) <> ">"
    thetype = case ty of
      [] -> n
      (_:_) -> n <> "<" <> intercalate ", " ty <> ">"
    
    crossProdIf _ _ [] = "true"
    crossProdIf useob _ xs = intercalate " && " (fmap (\(n, _) -> if useob
                            then "lhs." <> get n <> " == " <> "rhs." <> get n
                            else "l" <> get n <> " == " <> "r" <> get n) xs)

-- runDriverOutside sw "Comparable" = ""
-- runDriverOutside sw "Hashable" = ""
-- swiftz
runDriverOutside sw "JSON" = ""
runDriverOutside sw x = error ("no known deriver for " <> unpack x)

mkEqCases mkTwo retFun (con, []) =   "    case (." <> con <> sndArg <> "):"
                    <|> "      return" <~> retFun False con []
                    where
                      sndArg = if mkTwo then ", ." <> con else "" 
mkEqCases mkTwo retFun (con, xs) =   "    case let (."
                                <> con <> "(" <> fields lPrefix xs <> ")"
                                <> sndArg <> "):"
                    <|> "      return" <~> retFun False con xs
                    where
                      lPrefix = if mkTwo then "l" else ""
                      sndArg = if mkTwo then ", ." <> con <> "(" <> fields "r" xs <> ")" else ""
fields idf xs = intercalate ", " (fmap (\(n, _) -> idf <> get n) xs)


-- meh
transform :: Module -> Text
transform (Module _ _ (x:xs) _ _ _ _) = error "'module pragma' is unsupported"
transform (Module _ _ _ _ _ (x:xs) _) = error "'import' is unsupported"
transform (Module _ _ _ _ _ [] xs)    = intercalate "\n\n" $ fmap (prettyPrint . produce) xs

produce :: Decl -> Swift
produce (TypeDecl _ n bs t)              = mkType n bs t
produce (TypeFamDecl _ _ _ _)            = error "type families not supported"
produce (DataDecl _ dn ct n bs qs dr)    = mkData dn ct n bs qs dr
produce (GDataDecl _ dn ct n bs k gd dr) = mkGadt dn ct n bs k gd dr
produce (DataFamDecl _ _ _ _ _)          = error "data families not supported"
produce (TypeInsDecl _ _ _)              = error "data instances not supported"
produce (GDataInsDecl _ _ _ _ _ _)       = error "data instances (gadt) not supported"
produce (ClassDecl _ ct n bs fd cd)      = error "type classes not supported"
produce (InstDecl _ ct n ts ds)          = error "type class instances not supported"
produce (DerivDecl _ _ _ _)              = error "standalone deriving not supported"
produce (InfixDecl _ _ _ _)              = error "infix decls not supported"
produce (DefaultDecl _ _)                = error "default decl not supported"
produce (SpliceDecl _ _)                 = error "TH splice not supported"
produce (TypeSig _ _ _)                  = error "type signatures not supported"
produce (FunBind _)                      = error "no fun allowed"
produce (PatBind _ _ _ _ _)              = error "pattern binding not supported"
produce (ForImp _ _ _ _ _ _)             = error "foreign imports not supported"
produce (ForExp _ _ _ _ _)               = error "foreign exports not supported"
produce (RulePragmaDecl _ _)             = error "rule pragmas not supported"
produce (DeprPragmaDecl _ _)             = error "deprecated pragma not supported"
produce (WarnPragmaDecl _ _)             = error "warning pragma not supported"
produce (InlineSig _ _ _ _)              = error "inline pragma not supported"
produce (InlineConlikeSig _ _ _)         = error "inline conlike pragma not supported"
produce (SpecSig _ _ _ _)                = error "specialise pragma not supported"
produce (SpecInlineSig _ _ _ _ _)        = error "specialise inline pragma not supported"
produce (InstSig _ _ _ _)                = error "specialise instance pragma not supported"
produce (AnnPragma _ _)                  = error "ann pragma not supported"

mkType :: Name -> [TyVarBind] -> Type -> Swift
mkType n bs t = error "type aliases not supported"

mkGadt :: DataOrNew -> Context -> Name -> [TyVarBind] -> (Maybe Kind) -> [GadtDecl] -> [Deriving] -> Swift
mkGadt = error "gadts not supported"

-- halp
(<~>) :: Text -> Text -> Text
x <~> y = x <> " " <> y

(<|>) :: Text -> Text -> Text
x <|> y = x <> "\n" <> y

dequal :: QName -> Text
dequal (UnQual (Ident s)) = pack s
dequal _ = error "names must be unqualified"

unn (Ident s) = pack s
unn (Symbol _) = error "symbolic names not supported"

toTitle' n = toTitle (take 1 (pack n)) <> drop 1 (pack n)

get (Left v) = v
get (Right v) = v
