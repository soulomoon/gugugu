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

  , ClassParam
  , Param(..)
  , ParamType

  , Type(..)

  , Modifier(..)
  , StableId(..)
  , QualId(..)
  , Id
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
    { cuPackage  :: QualId
    , cuTopStats :: [TopStat]
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
    for_ cuTopStats $ \ts -> do
      writeText "\n"
      writeSrcComp ts

instance SrcComp TopStat where
  writeSrcComp v = withNewLine $ case v of
    TSC c -> writeSrcComp c


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


instance SrcComp Param where
  writeSrcComp Param{..} = do
    writeText pName
    writeText ": "
    writeSrcComp pType


instance SrcComp Type where
  writeSrcComp v = case v of
    TParamed t ts -> do
      writeSrcComp t
      unless (null ts) $ writeTParams ts


instance SrcComp Modifier where
  writeSrcComp v = case v of
    MCase -> writeText "case"

instance SrcComp StableId where
  writeSrcComp v = writeText $ T.intercalate "." $ toList $ unStableId v

instance SrcComp QualId where
  writeSrcComp v = writeText $ T.intercalate "." $ toList $ unQualId v
