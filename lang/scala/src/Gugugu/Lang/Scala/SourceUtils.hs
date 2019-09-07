{-|
Utilities to generate Scala sources.

Note, it is a simplified version and not intended for general purpose use.

Vide: https://www.scala-lang.org/files/archive/spec/2.13/13-syntax-summary.html
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module Gugugu.Lang.Scala.SourceUtils
  ( CompilationUnit(..)
  , TopStat(..)

  , ClassDef(..)
  , ObjectDef(..)
  , TraitDef(..)
  , TemplateStat(..)

  , FunDcl(..)
  , PatDef(..)
  , FunDef(..)

  , ClassParam
  , Param(..)
  , ParamType

  , Pattern(..)
  , Expr(..)
  , BlockStat(..)

  , Type(..)

  , Modifier(..)
  , StableId(..)
  , QualId(..)
  , Id
  , VarId
  ) where

import           Control.Monad
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text          (Text)
import qualified Data.Text          as T

import           Gugugu.Utilities


{-|
@
CompilationUnit   ::=  {‘package’ QualId semi} TopStatSeq
TopStatSeq        ::=  TopStat {semi TopStat}
@
 -}
data CompilationUnit
  = CompilationUnit
    { cuPackage    :: QualId
    , cuPkgImports :: [(QualId, [Id])]
    , cuTopStats   :: [TopStat]
    }
  deriving Show

{-|
@
TopStat           ::=  {Annotation [nl]} {Modifier} TmplDef
                    |  Import
                    |  Packaging
                    |  PackageObject
                    |
TmplDef           ::=  [‘case’] ‘class’ ClassDef
                    |  [‘case’] ‘object’ ObjectDef
                    |  ‘trait’ TraitDef
@
 -}
data TopStat
  = TSC ClassDef
  | TSO ObjectDef
  | TST TraitDef
  deriving Show


{-|
@
ClassDef          ::=  id [TypeParamClause] {ConstrAnnotation} [AccessModifier]
                       ClassParamClauses ClassTemplateOpt
ClassParamClauses ::=  {ClassParamClause}
                       [[nl] ‘(’ ‘implicit’ ClassParams ‘)’]
ClassParamClause  ::=  [nl] ‘(’ [ClassParams] ‘)’
ClassParams       ::=  ClassParam {‘,’ ClassParam}
ClassTemplateOpt  ::=  ‘extends’ ClassTemplate | [[‘extends’] TemplateBody]
ClassTemplate     ::=  [EarlyDefs] ClassParents [TemplateBody]
@
 -}
data ClassDef
  = ClassDef
    { cdModifiers :: [Modifier]
    , cdName      :: Id
    , cdParams    :: [ClassParam]
    }
  deriving Show

{-|
@
ObjectDef         ::=  id ClassTemplateOpt
ClassTemplateOpt  ::=  ‘extends’ ClassTemplate | [[‘extends’] TemplateBody]
ClassTemplate     ::=  [EarlyDefs] ClassParents [TemplateBody]
ClassParents      ::=  Constr {‘with’ AnnotType}
Constr            ::=  AnnotType {ArgumentExprs}
TemplateBody      ::=  [nl] ‘{’ [SelfType] TemplateStat {semi TemplateStat} ‘}’
@
 -}
data ObjectDef
  = ObjectDef
    { odModifiers :: [Modifier]
    , odName      :: Id
    , odBody      :: [TemplateStat]
    }
  deriving Show

{-|
@
TraitDef          ::=  id [TypeParamClause] TraitTemplateOpt
TraitTemplateOpt  ::=  ‘extends’ TraitTemplate | [[‘extends’] TemplateBody]
@
 -}
data TraitDef
  = TraitDef
    { tdName    :: Id
    , tdTParams :: [Text]
    , tdBody    :: [TemplateStat]
    }
  deriving Show

{-|
@
TemplateStat      ::=  Import
                    |  {Annotation [nl]} {Modifier} Def
                    |  {Annotation [nl]} {Modifier} Dcl
                    |  Expr
                    |
Def               ::=  PatVarDef
                    |  ‘def’ FunDef
                    |  ‘type’ {nl} TypeDef
                    |  TmplDef
PatVarDef         ::=  ‘val’ PatDef
                    |  ‘var’ VarDef
Dcl               ::=  ‘val’ ValDcl
                    |  ‘var’ VarDcl
                    |  ‘def’ FunDcl
                    |  ‘type’ {nl} TypeDcl
@
 -}
data TemplateStat
  = TMSD FunDcl
  | TMSV PatDef
  | TMSF FunDef
  deriving Show


{-|
@
FunDcl            ::=  FunSig [‘:’ Type]
@
 -}
data FunDcl
  = FunDcl
    { fdModifiers :: [Modifier]
    , fdName      :: Id
    , fdTParams   :: [Id]
    , fdParams    :: [Param]
    , fdRType     :: Type
    }
  deriving Show

{-|
@
PatVarDef         ::=  ‘val’ PatDef
                    |  ‘var’ VarDef
PatDef            ::=  Pattern2 {‘,’ Pattern2} [‘:’ Type] ‘=’ Expr
@
 -}
data PatDef
  = PatDef
    { pdModifiers :: [Modifier]
    , pdPattern   :: Pattern
    , pdType      :: Maybe Type
    , pdDef       :: Expr
    }
  deriving Show

{-|
@
FunDef            ::=  FunSig [‘:’ Type] ‘=’ Expr
                    |  FunSig [nl] ‘{’ Block ‘}’
                    |  ‘this’ ParamClause ParamClauses
                        (‘=’ ConstrExpr | [nl] ConstrBlock)
FunSig            ::=  id [FunTypeParamClause] ParamClauses
FunTypeParamClause::=  ‘[’ TypeParam {‘,’ TypeParam} ‘]’
TypeParam         ::=  (id | ‘_’) [TypeParamClause] [‘>:’ Type] [‘<:’ Type]
                        {‘<%’ Type} {‘:’ Type}
ParamClauses      ::=  {ParamClause} [[nl] ‘(’ ‘implicit’ Params ‘)’]
ParamClause       ::=  [nl] ‘(’ [Params] ‘)’
Params            ::=  Param {‘,’ Param}
@
 -}
data FunDef
  = FunDef
    { fdSig :: FunDcl
    , fdDef :: Expr
    }
  deriving Show


{-|
@
ClassParam        ::=  {Annotation} {Modifier} [(‘val’ | ‘var’)]
                       id ‘:’ ParamType [‘=’ Expr]
Param             ::=  {Annotation} id [‘:’ ParamType] [‘=’ Expr]
@
 -}
type ClassParam = Param

{-|
@
Param             ::=  {Annotation} id [‘:’ ParamType] [‘=’ Expr]
@
 -}
data Param
  = Param
    { pName :: Id
    , pType :: ParamType
    }
  deriving Show

{-|
@
ParamType         ::=  Type
                    |  ‘=>’ Type
                    |  Type ‘*’
@
 -}
type ParamType = Type


{-|
@
Pattern           ::=  Pattern1 { ‘|’ Pattern1 }
Pattern1          ::=  boundvarid ‘:’ TypePat
                    |  ‘_’ ‘:’ TypePat
                    |  Pattern2
Pattern2          ::=  id [‘@’ Pattern3]
                    |  Pattern3
Pattern3          ::=  SimplePattern
                    |  SimplePattern { id [nl] SimplePattern }
SimplePattern     ::=  ‘_’
                    |  varid
                    |  Literal
                    |  StableId
                    |  StableId ‘(’ [Patterns] ‘)’
                    |  StableId ‘(’ [Patterns ‘,’] [id ‘@’] ‘_’ ‘*’ ‘)’
                    |  ‘(’ [Patterns] ‘)’
                    |  XmlPattern
Patterns          ::=  Pattern [‘,’ Patterns]
                    |  ‘_’ ‘*’
@
 -}
data Pattern
  = PSimple VarId
  | PTuple (NonEmpty VarId)
  deriving Show

{-|
@
Expr              ::=  (Bindings | [‘implicit’] id | ‘_’) ‘=>’ Expr
                    |  Expr1
Expr1             ::=  ‘if’ ‘(’ Expr ‘)’ {nl} Expr [[semi] ‘else’ Expr]
                    |  ‘while’ ‘(’ Expr ‘)’ {nl} Expr
                    |  ‘try’ Expr [‘catch’ Expr] [‘finally’ Expr]
                    |  ‘do’ Expr [semi] ‘while’ ‘(’ Expr ‘)’
                    |  ‘for’ (‘(’ Enumerators ‘)’ | ‘{’ Enumerators ‘}’) {nl} [‘yield’] Expr
                    |  ‘throw’ Expr
                    |  ‘return’ [Expr]
                    |  [SimpleExpr ‘.’] id ‘=’ Expr
                    |  SimpleExpr1 ArgumentExprs ‘=’ Expr
                    |  PostfixExpr
                    |  PostfixExpr Ascription
                    |  PostfixExpr ‘match’ ‘{’ CaseClauses ‘}’
PostfixExpr       ::=  InfixExpr [id [nl]]
InfixExpr         ::=  PrefixExpr
                    |  InfixExpr id [nl] InfixExpr
PrefixExpr        ::=  [‘-’ | ‘+’ | ‘~’ | ‘!’] SimpleExpr
SimpleExpr        ::=  ‘new’ (ClassTemplate | TemplateBody)
                    |  BlockExpr
                    |  SimpleExpr1 [‘_’]
SimpleExpr1       ::=  Literal
                    |  Path
                    |  ‘_’
                    |  ‘(’ [Exprs] ‘)’
                    |  SimpleExpr ‘.’ id
                    |  SimpleExpr TypeArgs
                    |  SimpleExpr1 ArgumentExprs
                    |  XmlExpr
BlockExpr         ::=  ‘{’ CaseClauses ‘}’
                    |  ‘{’ Block ‘}’
Block             ::=  BlockStat {semi BlockStat} [ResultExpr]
@
 -}
data Expr
  = ESimple StableId
  | EMember Expr Id
  | ETuple (NonEmpty Expr)
  | EAnon (NonEmpty Id) Expr
  | EBlock [BlockStat]
  | ECall Expr [Expr]
  | ETCall Expr (NonEmpty Type)
  | ENew Type (Maybe [TemplateStat])
  | EIf Expr Expr (Maybe Expr)
  | EReturn Expr
  | EBinary Expr Text Expr
  | EMatch Expr [(Pattern, Expr)]
  deriving Show

{-|
@
BlockStat         ::=  Import
                    |  {Annotation} [‘implicit’ | ‘lazy’] Def
                    |  {Annotation} {LocalModifier} TmplDef
                    |  Expr1
                    |
@
 -}
data BlockStat
  = BSE Expr
  | BSP PatDef
  deriving Show


{-|
@
Type              ::=  FunctionArgTypes ‘=>’ Type
                    |  InfixType [ExistentialClause]
FunctionArgTypes  ::= InfixType
                    | ‘(’ [ ParamType {‘,’ ParamType } ] ‘)’
ExistentialClause ::=  ‘forSome’ ‘{’ ExistentialDcl {semi ExistentialDcl} ‘}’
ExistentialDcl    ::=  ‘type’ TypeDcl
                    |  ‘val’ ValDcl
InfixType         ::=  CompoundType {id [nl] CompoundType}
CompoundType      ::=  AnnotType {‘with’ AnnotType} [Refinement]
                    |  Refinement
AnnotType         ::=  SimpleType {Annotation}
SimpleType        ::=  SimpleType TypeArgs
                    |  SimpleType ‘#’ id
                    |  StableId
                    |  Path ‘.’ ‘type’
                    |  ‘(’ Types ‘)’
TypeArgs          ::=  ‘[’ Types ‘]’
Types             ::=  Type {‘,’ Type}
Refinement        ::=  [nl] ‘{’ RefineStat {semi RefineStat} ‘}’
RefineStat        ::=  Dcl
                    |  ‘type’ TypeDef
                    |
@
 -}
data Type
  = TParamed StableId [Type]
  | TTuple (NonEmpty Type)
  | TFun [Type] Type
  deriving Show


{-|
@
Modifier          ::=  LocalModifier
                    |  AccessModifier
                    |  ‘override’
LocalModifier     ::=  ‘abstract’
                    |  ‘final’
                    |  ‘sealed’
                    |  ‘implicit’
                    |  ‘lazy’
AccessModifier    ::=  (‘private’ | ‘protected’) [AccessQualifier]
AccessQualifier   ::=  ‘[’ (id | ‘this’) ‘]’
@

I know that @case@ is not a modifier but convenient
 -}
data Modifier
  = MCase
  | MImplicit
  | MOverride
  deriving Show

{-|
@
StableId          ::=  id
                    |  Path ‘.’ id
                    |  [id ‘.’] ‘super’ [ClassQualifier] ‘.’ id
Path              ::=  StableId
                    |  [id ‘.’] ‘this’
@
 -}
newtype StableId
  = StableId
    { unStableId :: NonEmpty Id
    }
  deriving Show

{-|
@
QualId            ::=  id {‘.’ id}
@
 -}
newtype QualId
  = QualId
    { unQualId :: NonEmpty Id
    }
  deriving Show

{-|
@
id               ::=  plainid
                   |  ‘`’ { charNoBackQuoteOrNewline | UnicodeEscape | charEscapeSeq } ‘`’
@
 -}
type Id = Text

{-|
@
varid            ::=  lower idrest
@
 -}
type VarId = Text


-- Utilities

writeModifiers :: (Foldable t, Monad m) => t Modifier -> SrcCompT m ()
writeModifiers = traverse_ $ \m -> do
  writeSrcComp m
  writeText " "

writeTParams :: (Foldable t, Monad m) => t Type -> SrcCompT m ()
writeTParams ts = do
  writeText "["
  forWithComma_ ts writeSrcComp
  writeText "]"


-- Instances

instance SrcComp CompilationUnit where
  writeSrcComp CompilationUnit{..} = do
    withNewLine $ do
      writeText "package "
      writeSrcComp cuPackage
    unless (null cuPkgImports) $ writeText "\n"
    for_ cuPkgImports $ \(pkgId, members) -> withNewLine $ do
      writeText "import "
      writeSrcComp pkgId
      case members of
        []     -> pure ()
        [name] -> do
          writeText "."
          writeText name
        _      -> do
          writeText "{"
          forWithComma_ members writeText
          writeText "}"
    for_ cuTopStats $ \ts -> do
      writeText "\n"
      writeSrcComp ts

instance SrcComp TopStat where
  writeSrcComp v = withNewLine $ case v of
    TSC c -> writeSrcComp c
    TSO c -> writeSrcComp c
    TST c -> writeSrcComp c


instance SrcComp ClassDef where
  writeSrcComp ClassDef{..} = do
    writeModifiers cdModifiers
    writeText "class "
    writeText cdName
    case cdParams of
      []      -> writeText "()"
      p' : ps -> do
        writeText "\n"
        indentBy 2 $ do
          withNewLine $ do
            writeText "( "
            writeSrcComp p'
          for_ ps $ \p -> withNewLine $ do
            writeText ", "
            writeSrcComp p
          doIndent
          writeText ")"

instance SrcComp ObjectDef where
  writeSrcComp ObjectDef{..} = do
    writeModifiers odModifiers
    writeText "object "
    writeText odName
    unless (null odBody) $ do
      writeText " {\n\n"
      indentBy 2 $ for_ odBody $ \ts -> do
        writeSrcComp ts
        writeText "\n"
      doIndent
      writeText "}"

instance SrcComp TraitDef where
  writeSrcComp TraitDef{..} = do
    writeText "trait "
    writeText tdName
    unless (null tdTParams) $ do
      writeText "["
      forWithComma_ tdTParams writeText
      writeText "]"
    unless (null tdBody) $ do
      writeText " {\n"
      indentBy 2 $ traverse_ writeSrcComp tdBody
      doIndent
      writeText "}"

instance SrcComp TemplateStat where
  writeSrcComp v = withNewLine $ case v of
    TMSV c -> writeSrcComp c
    TMSF c -> writeSrcComp c
    TMSD c -> writeSrcComp c


instance SrcComp FunDcl where
  writeSrcComp FunDcl{..} = do
    for_ fdModifiers $ \m -> do
      writeSrcComp m
      writeText " "
    writeText "def "
    writeText fdName
    unless (null fdTParams) $ do
      writeText "["
      forWithComma_ fdTParams writeText
      writeText "]"
    unless (null fdParams) $ do
      writeText "("
      forWithComma_ fdParams writeSrcComp
      writeText ")"
    writeText ": "
    writeSrcComp fdRType

instance SrcComp PatDef where
  writeSrcComp PatDef{..} = do
    for_ pdModifiers $ \m -> do
      writeSrcComp m
      writeText " "
    writeText "val "
    writeSrcComp pdPattern
    for_ pdType $ \t -> do
      writeText ": "
      writeSrcComp t
    writeText " = "
    writeSrcComp pdDef

instance SrcComp FunDef where
  writeSrcComp FunDef{..} = do
    writeSrcComp fdSig
    writeText " = "
    writeSrcComp fdDef


instance SrcComp Param where
  writeSrcComp Param{..} = do
    writeText pName
    writeText ": "
    writeSrcComp pType


instance SrcComp Pattern where
  writeSrcComp v = case v of
    PSimple t -> writeText t
    PTuple ts -> do
      writeText "("
      forWithComma_ ts writeText
      writeText ")"

instance SrcComp Expr where
  writeSrcComp v = case v of
    ESimple n        -> writeSrcComp n
    EMember e name   -> do
      writeSrcComp e
      writeText "."
      writeText name
    ETuple es        -> do
      writeText "("
      forWithComma_ es writeSrcComp
      writeText ")"
    EAnon ps e       -> do
      case ps of
        p :| [] -> writeText p
        _       -> do
          writeText "("
          forWithComma_ ps writeText
          writeText ")"
      writeText " => "
      writeSrcComp e
    EBlock bss       -> do
      writeText "{\n"
      indentBy 2 $ traverse_ writeSrcComp bss
      doIndent
      writeText "}"
    ECall e args     -> do
      writeSrcComp e
      writeText "("
      forWithComma_ args writeSrcComp
      writeText ")"
    ETCall e ts      -> do
      writeSrcComp e
      writeTParams ts
    ENew t body      -> do
      writeText "new "
      writeSrcComp t
      for_ body $ \tss -> case tss of
        [] -> writeText " {}"
        _  -> do
          writeText " {\n"
          indentBy 2 $ traverse_ writeSrcComp tss
          doIndent
          writeText "}"
    EIf cond e1 e2   -> do
      writeText "if ("
      writeSrcComp cond
      writeText ") "
      writeSrcComp e1
      for_ e2 $ \e -> do
        writeText " else "
        writeSrcComp e
    EReturn e        -> do
      writeText "return "
      writeSrcComp e
    EBinary e1 op e2 -> do
      writeSrcComp e1
      writeText " "
      writeText op
      writeText " "
      writeSrcComp e2
    EMatch e cases   -> do
      writeSrcComp e
      writeText " match {\n"
      indentBy 2 $ for_ cases $ \(pat, expr) -> do
        doIndent
        writeText "case "
        writeSrcComp pat
        writeText " => "
        writeSrcComp expr
        writeText "\n"
      doIndent
      writeText "}"

instance SrcComp BlockStat where
  writeSrcComp v = withNewLine $ case v of
    BSE c -> writeSrcComp c
    BSP c -> writeSrcComp c


instance SrcComp Type where
  writeSrcComp v = case v of
    TParamed t ts -> do
      writeSrcComp t
      unless (null ts) $ writeTParams ts
    TTuple ts     -> do
      writeText "("
      forWithComma_ ts writeSrcComp
      writeText ")"
    TFun ps t2    -> do
      case ps of
        [p] -> writeSrcComp p
        _   -> do
          writeText "("
          forWithComma_ ps writeSrcComp
          writeText ")"
      writeText " => "
      writeSrcComp t2


instance SrcComp Modifier where
  writeSrcComp v = case v of
    MCase     -> writeText "case"
    MImplicit -> writeText "implicit"
    MOverride -> writeText "override"

instance SrcComp StableId where
  writeSrcComp v = writeText $ T.intercalate "." $ toList $ unStableId v

instance SrcComp QualId where
  writeSrcComp v = writeText $ T.intercalate "." $ toList $ unQualId v
