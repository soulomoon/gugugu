{-|
Utilities to generate Rust sources.

Note, it is a simplified version and not intended for general purpose use.

Vide:
https://doc.rust-lang.org/reference/introduction.html
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module Gugugu.Lang.Rust.SourceUtils
  ( Identifier

  , Crate(..)

  , Item(..)
  , VisItem(..)
  , RsModule(..)
  , TypeAlias(..)
  , Struct(..)
  , StructField(..)
  , Enumeration(..)

  , OuterAttribute(..)
  , Attr(..)

  , Type(..)
  ) where

import           Control.Monad
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text          (Text)

import           Gugugu.Utilities


-- | Rust identifier
type Identifier = Text


{-|
https://doc.rust-lang.org/reference/crates-and-source-files.html

@
Crate:
  UTF8BOM?
  SHEBANG?
  InnerAttribute\*
  Item\*
@
 -}
newtype Crate
  = Crate
    { cItems :: [Item]
    }
  deriving Show


{-|
https://doc.rust-lang.org/reference/items.html

@
Item:
  OuterAttribute\*
    VisItem
  | MacroItem

MacroItem:
    MacroInvocationSemi
  | MacroRulesDefinition
@
 -}
data Item
  = Item [OuterAttribute] VisItem
  deriving Show

{-|
https://doc.rust-lang.org/reference/items.html

@
VisItem:
  Visibility?
  (
      Module
    | ExternCrate
    | UseDeclaration
    | Function
    | TypeAlias
    | Struct
    | Enumeration
    | Union
    | ConstantItem
    | StaticItem
    | Trait
    | Implementation
    | ExternBlock
  )
@
 -}
data VisItem
  = IModule RsModule
  | ITypeAlias TypeAlias
  | IStruct Struct
  | IEnumeration Enumeration
  deriving Show

{-|
https://doc.rust-lang.org/reference/items/modules.html

@
Module:
    mod IDENTIFIER ;
  | mod IDENTIFIER {
      InnerAttribute\*
      Item\*
    }
@
 -}
newtype RsModule
  = RsModule
    { mName :: Identifier
    }
  deriving Show

{-|
https://doc.rust-lang.org/reference/items/type-aliases.html

@
TypeAlias:
  type IDENTIFIER Generics? WhereClause? = Type ;
@
 -}
data TypeAlias
  = TypeAlias
    { taName :: Identifier
    , taType :: Type
    }
  deriving Show

{-|
https://doc.rust-lang.org/reference/items/structs.html

@
Struct:
    StructStruct
  | TupleStruct

StructStruct:
  struct IDENTIFIER  Generics? WhereClause? ( { StructFields? } | ; )

StructFields:
  StructField (, StructField)\* ,?

StructField:
  OuterAttribute\*
  Visibility?
  IDENTIFIER : Type

TupleStruct:
  struct IDENTIFIER  Generics? ( TupleFields? ) WhereClause? ;

TupleFields:
  TupleField (, TupleField)\* ,?

TupleField:
  OuterAttribute\*
  Visibility?
  Type
@
 -}
data Struct
  = Struct
    { sName   :: Identifier
    , sFields :: [StructField]
    }
  deriving Show

{-|
https://doc.rust-lang.org/reference/items/structs.html

@
StructFields:
  StructField (, StructField)\* ,?

StructField:
  OuterAttribute\*
  Visibility?
  IDENTIFIER : Type
@
 -}
data StructField
  = StructField
    { sfName :: Identifier
    , sfType :: Type
    }
  deriving Show

{-|
https://doc.rust-lang.org/reference/items/enumerations.html

@
Enumeration:
  enum IDENTIFIER  Generics? WhereClause? { EnumItems? }

EnumItems:
  EnumItem ( , EnumItem )\* ,?

EnumItem:
  OuterAttribute\*
  IDENTIFIER ( EnumItemTuple | EnumItemStruct | EnumItemDiscriminant )?

EnumItemTuple:
  ( TupleFields? )

EnumItemStruct:
  { StructFields? }

EnumItemDiscriminant:
  = Expression
@
 -}
data Enumeration
  = Enumeration
    { eName  :: Identifier
    , eItems :: [Identifier]
    }
  deriving Show


{-|
https://doc.rust-lang.org/reference/attributes.html

@
InnerAttribute:
  # ! [ Attr ]

OuterAttribute:
  # [ Attr ]
@
 -}
newtype OuterAttribute
  = OuterAttribute Attr
  deriving Show

{-|
https://doc.rust-lang.org/reference/attributes.html

@
Attr:
  SimplePath AttrInput?

AttrInput:
    DelimTokenTree
  | = LiteralExpression without suffix
@
 -}
data Attr
  -- | https://doc.rust-lang.org/reference/attributes/derive.html
  = ADerive [Type]
  deriving Show


{-|
https://doc.rust-lang.org/reference/types.html#type-expressions

@
Type:
    TypeNoBounds
  | ImplTraitType
  | TraitObjectType

TypeNoBounds:
    ParenthesizedType
  | ImplTraitTypeOneBound
  | TraitObjectTypeOneBound
  | TypePath
  | TupleType
  | NeverType
  | RawPointerType
  | ReferenceType
  | ArrayType
  | SliceType
  | InferredType
  | QualifiedPathInType
  | BareFunctionType
  | MacroInvocation
@
 -}
data Type
  = TSimple Identifier
  {-|
https://doc.rust-lang.org/reference/paths.html#paths-in-types

@
TypePath:
  ::? TypePathSegment (:: TypePathSegment)*

TypePathSegment:
  PathIdentSegment ::? (GenericArgs | TypePathFn)?

TypePathFn:
( TypePathFnInputs? ) (-> Type)?

TypePathFnInputs:
Type (, Type)* ,?
@
   -}
  | TPath (NonEmpty Identifier)
  {-|
https://doc.rust-lang.org/reference/paths.html#paths-in-types

@
TypePath:
  ::? TypePathSegment (:: TypePathSegment)*

TypePathSegment:
  PathIdentSegment ::? (GenericArgs | TypePathFn)?

TypePathFn:
( TypePathFnInputs? ) (-> Type)?

TypePathFnInputs:
Type (, Type)* ,?
@
   -}
  | TParam Type [Type] [(Identifier, Type)]
  deriving Show


-- Utilities

writePubItem :: (Monad m, SrcComp a) => a -> SrcCompT m ()
writePubItem a = withNewLine $ do
  writeText "pub "
  writeSrcComp a


-- Instances

instance SrcComp Crate where
  writeSrcComp Crate{..} = traverse_ writeSrcComp cItems


instance SrcComp Item where
  writeSrcComp v = case v of
    Item attrs i -> do
      traverse_ (withNewLine . writeSrcComp) attrs
      writeSrcComp i

instance SrcComp VisItem where
  writeSrcComp v = case v of
    IModule i      -> writePubItem i
    ITypeAlias i   -> writePubItem i
    IStruct i      -> writePubItem i
    IEnumeration i -> writePubItem i

instance SrcComp RsModule where
  writeSrcComp RsModule{..} = do
    writeText "mod "
    writeText mName
    writeText ";"

instance SrcComp TypeAlias where
  writeSrcComp TypeAlias{..} = do
    writeText "type "
    writeText taName
    writeText " = "
    writeSrcComp taType
    writeText ";"

instance SrcComp Struct where
  writeSrcComp Struct{..} = do
    writeText "struct "
    writeText sName
    if null sFields
      then writeText ";"
      else do
        writeText " {\n"
        indentBy 2 $ traverse_ writePubItem sFields
        doIndent
        writeText "}"

instance SrcComp StructField where
  writeSrcComp StructField{..} = do
    writeText sfName
    writeText ": "
    writeSrcComp sfType
    writeText ","

instance SrcComp Enumeration where
  writeSrcComp Enumeration{..} = do
    writeText "enum "
    writeText eName
    writeText " {"
    if null eItems
      then writeText " }"
      else do
        writeText "\n"
        indentBy 2 $ for_ eItems $ \item -> withNewLine $ do
          writeText item
          writeText ","
        doIndent
        writeText "}"


instance SrcComp OuterAttribute where
  writeSrcComp v = case v of
    OuterAttribute attr -> do
      writeText "#["
      writeSrcComp attr
      writeText "]"

instance SrcComp Attr where
  writeSrcComp v = case v of
    ADerive ts -> do
      writeText "derive("
      forWithComma_ ts writeSrcComp
      writeText ")"


instance SrcComp Type where
  writeSrcComp v = case v of
    TSimple t       -> writeText t
    TPath ps        -> forWith_ "::" ps writeText
    TParam t ps nps -> do
      writeSrcComp t
      writeText "<"
      forWithComma_ ps writeSrcComp
      when (not (null ps || null nps)) $ writeText ", "
      forWithComma_ nps $ \(n, t') -> do
        writeText n
        writeText " = "
        writeSrcComp t'
      writeText ">"
