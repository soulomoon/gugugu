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
  , Function(..)
  , FunctionParam(..)
  , TypeAlias(..)
  , Struct(..)
  , StructField(..)
  , Enumeration(..)
  , ConstantItem(..)
  , Trait(..)
  , TraitItem(..)
  , TraitType(..)
  , TraitImpl(..)
  , Generics
  , WhereClause

  , OuterAttribute(..)
  , Attr(..)

  , Statement(..)
  , Expression(..)

  , Pattern(..)

  , Type(..)
  , TypeParamBounds
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
  | IFunction Function
  | ITypeAlias TypeAlias
  | IStruct Struct
  | IEnumeration Enumeration
  | IConstantItem ConstantItem
  | ITrait Trait
  | ITraitImpl TraitImpl
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
https://doc.rust-lang.org/reference/items/functions.html

@
Function:
  FunctionQualifiers fn IDENTIFIER Generics?
    ( FunctionParameters? )
    FunctionReturnType? WhereClause?
    BlockExpression

FunctionQualifiers:
   AsyncConstQualifiers? unsafe? (extern Abi?)?

AsyncConstQualifiers:
   async | const

Abi:
   STRING_LITERAL | RAW_STRING_LITERAL

FunctionParameters:
   FunctionParam (, FunctionParam)* ,?

FunctionParam:
   OuterAttribute* Pattern : Type

FunctionReturnType:
   -> Type
@

https://doc.rust-lang.org/reference/items/traits.html

@
TraitFunc:
  TraitFunctionDecl ( ; | BlockExpression )

TraitMethod:
  TraitMethodDecl ( ; | BlockExpression )

TraitFunctionDecl:
  FunctionQualifiers fn IDENTIFIER Generics?
    ( TraitFunctionParameters? )
    FunctionReturnType? WhereClause?

TraitMethodDecl:
  FunctionQualifiers fn IDENTIFIER Generics?
    ( SelfParam (, TraitFunctionParam)* ,? )
    FunctionReturnType? WhereClause?

TraitFunctionParameters:
  TraitFunctionParam (, TraitFunctionParam)* ,?

TraitFunctionParam†:
  OuterAttribute* ( Pattern : )? Type
@
 -}
data Function
  = Function
    { fName    :: Identifier
    , fTParams :: Generics
    , fParams  :: [FunctionParam]
    , fRType   :: Type
    , fWhere   :: WhereClause
    , fBody    :: Maybe Expression
    }
  deriving Show

{-|
https://doc.rust-lang.org/reference/items/functions.html

@
FunctionParam:
   OuterAttribute* Pattern : Type
@

https://doc.rust-lang.org/reference/items/traits.html

@
TraitFunctionParam†:
  OuterAttribute* ( Pattern : )? Type
@
 -}
data FunctionParam
  = FunctionParam
    { fpPattern :: Pattern
    , fpType    :: Type
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
https://doc.rust-lang.org/reference/items/constant-items.html

@
ConstantItem:
  const ( IDENTIFIER | _ ) : Type = Expression ;
@
 -}
data ConstantItem
  = ConstantItem
    { ciName :: Identifier
    , ciType :: Type
    , ciBody :: Expression
    }
  deriving Show

{-|
https://doc.rust-lang.org/reference/items/traits.html

@
Trait:
  unsafe? trait IDENTIFIER  Generics? ( : TypeParamBounds? )? WhereClause? {
    TraitItem*
  }

TraitItem:
  OuterAttribute* (
      TraitFunc
    | TraitMethod
    | TraitConst
    | TraitType
    | MacroInvocationSemi
  )

TraitFunc:
  TraitFunctionDecl ( ; | BlockExpression )

TraitMethod:
  TraitMethodDecl ( ; | BlockExpression )

TraitFunctionDecl:
  FunctionQualifiers fn IDENTIFIER Generics?
    ( TraitFunctionParameters? )
    FunctionReturnType? WhereClause?

TraitMethodDecl:
  FunctionQualifiers fn IDENTIFIER Generics?
    ( SelfParam (, TraitFunctionParam)* ,? )
    FunctionReturnType? WhereClause?

TraitFunctionParameters:
  TraitFunctionParam (, TraitFunctionParam)* ,?

TraitFunctionParam†:
  OuterAttribute* ( Pattern : )? Type

TraitConst:
  const IDENTIFIER : Type ( = Expression )? ;

TraitType:
  type IDENTIFIER ( : TypeParamBounds? )? ;
@
 -}
data Trait
  = Trait
    { tName    :: Identifier
    , tTParams :: Generics
    , tItems   :: [TraitItem]
    }
  deriving Show

{-|
https://doc.rust-lang.org/reference/items/traits.html

@
TraitItem:
  OuterAttribute* (
      TraitFunc
    | TraitMethod
    | TraitConst
    | TraitType
    | MacroInvocationSemi
  )
@

https://doc.rust-lang.org/reference/items/implementations.html

@
TraitImplItem:
  OuterAttribute* (
      MacroInvocationSemi
    | ( Visibility? ( TypeAlias | ConstantItem | Function | Method ) )
  )
@
 -}
data TraitItem
  = TF Function
  | TT TraitType
  deriving Show

{-|
https://doc.rust-lang.org/reference/items/traits.html

@
TraitType:
  type IDENTIFIER ( : TypeParamBounds? )? ;
@
 -}
data TraitType
  = TraitType
    { ttName   :: Identifier
    , ttBounds :: TypeParamBounds
    , ttBody   :: Maybe Type
    }
  deriving Show

{-|
https://doc.rust-lang.org/reference/items/implementations.html

@
Implementation:
  InherentImpl | TraitImpl

InherentImpl:
  impl Generics? Type WhereClause? {
    InnerAttribute*
    InherentImplItem*
  }

InherentImplItem:
  OuterAttribute* (
      MacroInvocationSemi
    | ( Visibility? ( ConstantItem | Function | Method ) )
  )

TraitImpl:
  unsafe? impl Generics? !? TypePath for Type
  WhereClause?
  {
    InnerAttribute*
    TraitImplItem*
  }

TraitImplItem:
  OuterAttribute* (
      MacroInvocationSemi
    | ( Visibility? ( TypeAlias | ConstantItem | Function | Method ) )
  )
@
 -}
data TraitImpl
  = TraitImpl
    { tiTParams :: Generics
    , tiTrait   :: Type
    , tiFor     :: Type
    , tiWhere   :: WhereClause
    , tiItems   :: [TraitItem]
    }
  deriving Show

{-|
https://doc.rust-lang.org/reference/items/generics.html

@
Generics:
  \< GenericParams \>

GenericParams:
    LifetimeParams
  | ( LifetimeParam , )* TypeParams

LifetimeParams:
  ( LifetimeParam , )* LifetimeParam?

LifetimeParam:
  OuterAttribute? LIFETIME_OR_LABEL ( : LifetimeBounds )?

TypeParams:
  ( TypeParam , )* TypeParam?

TypeParam:
  OuterAttribute? IDENTIFIER ( : TypeParamBounds? )? ( = Type )?
@
 -}
type Generics = [Identifier]

{-|
https://doc.rust-lang.org/reference/items/generics.html#where-clauses

@
WhereClause:
  where ( WhereClauseItem , )* WhereClauseItem ?

WhereClauseItem:
    LifetimeWhereClauseItem
  | TypeBoundWhereClauseItem

LifetimeWhereClauseItem:
  Lifetime : LifetimeBounds

TypeBoundWhereClauseItem:
  ForLifetimes? Type : TypeParamBounds?

ForLifetimes:
  for \< LifetimeParams \>
@
 -}
type WhereClause = [(Type, TypeParamBounds)]


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
https://doc.rust-lang.org/reference/statements.html

@
Statement:
    ;
  | Item
  | LetStatement
  | ExpressionStatement
  | MacroInvocationSemi
@
-}
data Statement
  {-|
https://doc.rust-lang.org/reference/statements.html#let-statements

@
LetStatement:
  OuterAttribute* let Pattern ( : Type )? (= Expression )? ;
@
   -}
  = SLet Pattern Expression
  deriving Show

{-|
https://doc.rust-lang.org/reference/expressions.html

@
Expression:
    ExpressionWithoutBlock
  | ExpressionWithBlock

ExpressionWithoutBlock:
  OuterAttribute*†
  (
      LiteralExpression
    | PathExpression
    | OperatorExpression
    | GroupedExpression
    | ArrayExpression
    | AwaitExpression
    | IndexExpression
    | TupleExpression
    | TupleIndexingExpression
    | StructExpression
    | EnumerationVariantExpression
    | CallExpression
    | MethodCallExpression
    | FieldExpression
    | ClosureExpression
    | ContinueExpression
    | BreakExpression
    | RangeExpression
    | ReturnExpression
    | MacroInvocation
  )

ExpressionWithBlock:
  OuterAttribute*†
  (
      BlockExpression
    | AsyncBlockExpression
    | UnsafeBlockExpression
    | LoopExpression
    | IfExpression
    | IfLetExpression
    | MatchExpression
  )
@
 -}
data Expression
  = ESimple Text
  {-|
https://doc.rust-lang.org/reference/statements.html#expression-statements

@
PathExpression :
    PathInExpression
  | QualifiedPathInExpression
@

https://doc.rust-lang.org/reference/paths.html#paths-in-expressions

@
PathInExpression:
  ::? PathExprSegment (:: PathExprSegment)*

PathExprSegment:
  PathIdentSegment (:: GenericArgs)?

PathIdentSegment:
  IDENTIFIER | super | self | Self | crate | $crate

GenericArgs:
    \< \>
  | \< GenericArgsLifetimes ,? \>
  | \< GenericArgsTypes ,? \>
  | \< GenericArgsBindings ,? \>
  | \< GenericArgsTypes , GenericArgsBindings ,? \>
  | \< GenericArgsLifetimes , GenericArgsTypes ,? \>
  | \< GenericArgsLifetimes , GenericArgsBindings ,? \>
  | \< GenericArgsLifetimes , GenericArgsTypes , GenericArgsBindings ,? \>

GenericArgsLifetimes:
  Lifetime (, Lifetime)*

GenericArgsTypes:
  Type (, Type)*

GenericArgsBindings:
  GenericArgsBinding (, GenericArgsBinding)*

GenericArgsBinding:
   IDENTIFIER = Type
@
   -}
  | EPath (NonEmpty Identifier)
  {-|
https://doc.rust-lang.org/reference/expressions/operator-expr.html

@
OperatorExpression:
    BorrowExpression
  | DereferenceExpression
  | ErrorPropagationExpression
  | NegationExpression
  | ArithmeticOrLogicalExpression
  | ComparisonExpression
  | LazyBooleanExpression
  | TypeCastExpression
  | AssignmentExpression
  | CompoundAssignmentExpression
@

https://doc.rust-lang.org/reference/expressions/operator-expr.html#borrow-operators

@
BorrowExpression:
    (&|&&) Expression
  | (&|&&) mut Expression
@
   -}
  | EBorrow Expression
  {-|
https://doc.rust-lang.org/reference/expressions/operator-expr.html#the-question-mark-operator

@
ErrorPropagationExpression:
  Expression ?
@
   -}
  | EPropagate Expression
  {-|
https://doc.rust-lang.org/reference/expressions/operator-expr.html#comparison-operators

@
ComparisonExpression:
    Expression == Expression
  | Expression != Expression
  | Expression > Expression
  | Expression < Expression
  | Expression >= Expression
  | Expression <= Expression
@
   -}
  | EBinary Expression Text Expression
  {-|
https://doc.rust-lang.org/reference/expressions/array-expr.html

@
ArrayExpression:
  [ InnerAttribute* ArrayElements? ]

ArrayElements:
    Expression ( , Expression )* ,?
  | Expression ; Expression
@
   -}
  | EArray [Expression]
  {-|
https://doc.rust-lang.org/reference/expressions/tuple-expr.html

@
TupleExpression:
  ( InnerAttribute* TupleElements? )

TupleElements:
  ( Expression , )+ Expression?
@
   -}
  | ETuple [Expression]
  {-|
https://doc.rust-lang.org/reference/expressions/struct-expr.html

@
StructExpression:
    StructExprStruct
  | StructExprTuple
  | StructExprUnit

StructExprStruct:
  PathInExpression { InnerAttribute* (StructExprFields | StructBase)? }

StructExprFields:
  StructExprField (, StructExprField)* (, StructBase | ,?)

StructExprField:
    IDENTIFIER
  | (IDENTIFIER | TUPLE_INDEX) : Expression

StructBase:
  .. Expression

StructExprTuple:
  PathInExpression (
    InnerAttribute*
    ( Expression (, Expression)* ,? )?
  )

StructExprUnit: PathInExpression
@
   -}
  | EStruct Expression [(Identifier, Expression)]
  {-|
https://doc.rust-lang.org/reference/expressions/call-expr.html

@
CallExpression:
  Expression ( CallParams? )

CallParams:
  Expression ( , Expression )* ,?
@
   -}
  | ECall Expression [Expression]
  {-|
https://doc.rust-lang.org/reference/expressions/method-call-expr.html

@
MethodCallExpression:
  Expression . PathExprSegment (CallParams? )
@
   -}
  | EMethod Expression Identifier [Expression]
  {-|
https://doc.rust-lang.org/reference/expressions/field-expr.html

@
FieldExpression:
  Expression . IDENTIFIER
@
   -}
  | EField Expression Identifier
  {-|
https://doc.rust-lang.org/reference/expressions/closure-expr.html

@
ClosureExpression:
  move?
  ( || | | ClosureParameters? | )
  (Expression | -> TypeNoBounds BlockExpression)

ClosureParameters:
  ClosureParam (, ClosureParam)* ,?

ClosureParam:
  OuterAttribute* Pattern ( : Type )?
@
   -}
  | EClosure [Identifier] Expression
  {-|
https://doc.rust-lang.org/reference/expressions/block-expr.html

@
BlockExpression:
  {
    InnerAttribute*
    Statements?
  }

Statements:
    Statement+
  | Statement+ ExpressionWithoutBlock
  | ExpressionWithoutBlock
@
   -}
  | EBlock [Statement] Expression
  {-|
https://doc.rust-lang.org/reference/expressions/match-expr.html

@
MatchExpression:
  match Expressionexcept struct expression {
    InnerAttribute*
    MatchArms?
  }

MatchArms:
  ( MatchArm => ( BlockExpression ,? | Expression , ) )*
  MatchArm => ( BlockExpression | Expression ) ,?

MatchArm:
  OuterAttribute* MatchArmPatterns MatchArmGuard?

MatchArmPatterns:
  |? Pattern ( | Pattern )*

MatchArmGuard:
  if Expression
@
   -}
  | EMatch Expression [(Pattern, Expression)]
  deriving Show


{-|
https://doc.rust-lang.org/reference/patterns.html

@
Pattern:
    LiteralPattern
  | IdentifierPattern
  | WildcardPattern
  | RangePattern
  | ReferencePattern
  | StructPattern
  | TupleStructPattern
  | TuplePattern
  | GroupedPattern
  | SlicePattern
  | PathPattern
  | MacroInvocation
@
 -}
data Pattern
  = PSimple Text
  {-|
https://doc.rust-lang.org/reference/patterns.html#tuple-patterns

@
TuplePattern:
  ( TuplePatternItems? )

TuplePatternItems:
    Pattern ,
  | Pattern (, Pattern)+ ,?
  | (Pattern ,)* .. ( (, Pattern)+ ,? )?
@
   -}
  | PTuple [Pattern]
  {-|
https://doc.rust-lang.org/reference/patterns.html#path-patterns

@
PathPattern:
    PathInExpression
  | QualifiedPathInExpression
@
   -}
  | PPath (NonEmpty Identifier)
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
  {-|
https://doc.rust-lang.org/reference/types/tuple.html#tuple-types

@
TupleType:
    ( )
  | ( ( Type , )+ Type? )
@
   -}
  | TTuple [Type]
  {-|
https://doc.rust-lang.org/reference/types/pointer.html#shared-references-

@
ReferenceType:
  & Lifetime? mut? TypeNoBounds
@
   -}
  | TRef Type
  {-|
https://doc.rust-lang.org/reference/types/slice.html

@
SliceType:
  [ Type ]
@
   -}
  | TSlice Type
  {-|
https://doc.rust-lang.org/reference/types/function-pointer.html

@
BareFunctionType:
  ForLifetimes? FunctionQualifiers fn
    ( FunctionParametersMaybeNamedVariadic? ) BareFunctionReturnType?

BareFunctionReturnType:
  -> TypeNoBounds

FunctionParametersMaybeNamedVariadic:
  MaybeNamedFunctionParameters | MaybeNamedFunctionParametersVariadic

MaybeNamedFunctionParameters:
  MaybeNamedParam ( , MaybeNamedParam )* ,?

MaybeNamedParam:
  OuterAttribute* ( ( IDENTIFIER | _ ) : )? Type

MaybeNamedFunctionParametersVariadic:
  ( MaybeNamedParam , )* MaybeNamedParam , OuterAttribute* ...
@
   -}
  | TFn [Type] Type
  deriving Show

{-|

@
TypeParamBounds:
  TypeParamBound ( + TypeParamBound )* +?

TypeParamBound:
    Lifetime | TraitBound

TraitBound:
    ?? ForLifetimes? TypePath
  | ( ?? ForLifetimes? TypePath )

LifetimeBounds:
  ( Lifetime + )* Lifetime?

Lifetime:
    LIFETIME_OR_LABEL
  | 'static
  | '_
@
-}
type TypeParamBounds = [Type]


-- Utilities

writePubItem :: (Monad m, SrcComp a) => a -> SrcCompT m ()
writePubItem a = withNewLine $ do
  writeText "pub "
  writeSrcComp a

writeGenerics :: Monad m => Generics -> SrcCompT m ()
writeGenerics tps = unless (null tps) $ do
  writeText "<"
  forWithComma_ tps writeText
  writeText ">"

writeWhereClause :: Monad m => WhereClause -> SrcCompT m ()
writeWhereClause wheres = case fmap writeWhereItem wheres of
  []     -> pure ()
  k : ks -> do
    withNewLine $ do
      writeText "where "
      k
    for_ ks $ \k' -> withNewLine $ do
      writeText "    , "
      k'

writeWhereItem :: Monad m => (Type, TypeParamBounds) -> SrcCompT m ()
writeWhereItem (t, bs) = do
  writeSrcComp t
  writeText ": "
  writeTypeParamBounds bs

writeTypeParamBounds :: Monad m => TypeParamBounds -> SrcCompT m ()
writeTypeParamBounds bs = forWith_ " + " bs writeSrcComp


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
    IModule i       -> writePubItem i
    IFunction i     -> writePubItem i
    ITypeAlias i    -> writePubItem i
    IStruct i       -> writePubItem i
    IEnumeration i  -> writePubItem i
    IConstantItem i -> withNewLine $ writeSrcComp i
    ITrait i        -> writePubItem i
    ITraitImpl i    -> withNewLine $ writeSrcComp i

instance SrcComp RsModule where
  writeSrcComp RsModule{..} = do
    writeText "mod "
    writeText mName
    writeText ";"

instance SrcComp Function where
  writeSrcComp Function{..} = do
    writeText "fn "
    writeText fName
    writeGenerics fTParams
    writeText "\n"
    indentBy 2 $ do
      doIndent
      writeText "("
      case fParams of
        []     -> pure ()
        p : ps -> do
          writeText " "
          writeSrcComp p
          writeText "\n"
          for_ ps $ \p' -> withNewLine $ do
            writeText ", "
            writeSrcComp p'
          doIndent
      writeText ") -> "
      writeSrcComp fRType
      writeText "\n"
    writeWhereClause fWhere
    doIndent
    case fBody of
      Nothing -> writeText ";"
      Just e  -> case e of
        EBlock _ _ -> writeSrcComp e
        _          -> writeSrcComp $ EBlock [] e

instance SrcComp FunctionParam where
  writeSrcComp FunctionParam{..} = do
    writeSrcComp fpPattern
    writeText ": "
    writeSrcComp fpType

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

instance SrcComp ConstantItem where
  writeSrcComp ConstantItem{..} = do
    writeText "const "
    writeText ciName
    writeText ": "
    writeSrcComp ciType
    writeText " = "
    writeSrcComp ciBody
    writeText ";"

instance SrcComp Trait where
  writeSrcComp Trait{..} = do
    writeText "trait "
    writeText tName
    writeGenerics tTParams
    writeText "\n"
    withNewLine $ writeText "{"
    indentBy 2 $ traverse_ writeSrcComp tItems
    doIndent
    writeText "}"

instance SrcComp TraitItem where
  writeSrcComp v = case v of
    TF i -> withNewLine $ writeSrcComp i
    TT i -> withNewLine $ writeSrcComp i

instance SrcComp TraitType where
  writeSrcComp TraitType{..} = do
    writeText "type "
    writeText ttName
    unless (null ttBounds) $ do
      writeText ": "
      writeTypeParamBounds ttBounds
    for_ ttBody $ \t -> do
      writeText " = "
      writeSrcComp t
    writeText ";"

instance SrcComp TraitImpl where
  writeSrcComp TraitImpl{..} = do
    writeText "impl"
    writeGenerics tiTParams
    writeText " "
    writeSrcComp tiTrait
    writeText "\n"
    indentBy 2 $ withNewLine $ do
      writeText "for "
      writeSrcComp tiFor
    writeWhereClause tiWhere
    withNewLine $ writeText "{"
    indentBy 2 $ traverse_ writeSrcComp tiItems
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


instance SrcComp Statement where
  writeSrcComp v = case v of
    SLet p e -> do
      writeText "let "
      writeSrcComp p
      writeText " = "
      writeSrcComp e
      writeText ";"

instance SrcComp Expression where
  writeSrcComp v = case v of
    ESimple t        -> writeText t
    EPath ps         -> forWith_ "::" ps writeText
    EBorrow e        -> do
      writeText "&"
      writeSrcComp e
    EPropagate e     -> do
      writeSrcComp e
      writeText "?"
    EBinary e1 op e2 -> do
      writeSrcComp e1
      writeText " "
      writeText op
      writeText " "
      writeSrcComp e2
    EArray es        -> do
      writeText "["
      forWithComma_ es writeSrcComp
      writeText "]"
    ETuple es        -> do
      writeText "("
      forWithComma_ es writeSrcComp
      writeText ")"
    EStruct e fs     -> do
      writeSrcComp e
      writeText " {\n"
      indentBy 2 $ for_ fs $ \(n, v') -> withNewLine $ do
        writeText n
        writeText ": "
        writeSrcComp v'
        writeText ","
      doIndent
      writeText "}"
    ECall e ps       -> do
      writeSrcComp e
      writeText "("
      forWithComma_ ps writeSrcComp
      writeText ")"
    EMethod e n ps   -> do
      writeSrcComp e
      writeText "."
      writeText n
      writeText "("
      forWithComma_ ps writeSrcComp
      writeText ")"
    EField e n       -> do
      writeSrcComp e
      writeText "."
      writeText n
    EClosure ps e    -> do
      writeText "move "
      writeText "|"
      forWithComma_ ps writeText
      writeText "| "
      writeSrcComp e
    EBlock ss e      -> do
      writeText "{\n"
      indentBy 2 $ do
        traverse_ (withNewLine . writeSrcComp) ss
        withNewLine $ writeSrcComp e
      doIndent
      writeText "}"
    EMatch e alts    -> do
      writeText "match "
      writeSrcComp e
      writeText " {\n"
      indentBy 2 $ for_ alts $ \(p, e') -> withNewLine $ do
        writeSrcComp p
        writeText " => "
        writeSrcComp e'
        writeText ","
      doIndent
      writeText "}"


instance SrcComp Pattern where
  writeSrcComp v = case v of
    PSimple t -> writeText t
    PTuple ps -> do
      writeText "("
      forWithComma_ ps writeSrcComp
      writeText ")"
    PPath ps  -> forWith_ "::" ps writeText


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
    TTuple ts       -> do
      writeText "("
      forWithComma_ ts writeSrcComp
      writeText ")"
    TRef t          -> do
      writeText "&"
      writeSrcComp t
    TSlice t        -> do
      writeText "["
      writeSrcComp t
      writeText "]"
    TFn ps r        -> do
      writeText "fn("
      forWithComma_ ps writeSrcComp
      writeText ") -> "
      writeSrcComp r
