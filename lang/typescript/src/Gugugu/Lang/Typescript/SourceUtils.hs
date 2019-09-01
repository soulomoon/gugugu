{-|
Utilities to generate TypeScript sources.

Note, it is a simplified version and not intended for general purpose use.

Vide:

  * https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md#a-grammar
  * http://www.ecma-international.org/ecma-262/6.0/#sec-grammar-summary
 -}
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module Gugugu.Lang.Typescript.SourceUtils
  (
  -- * A.1 Lexical Grammar
  -- | http://www.ecma-international.org/ecma-262/6.0/#sec-lexical-grammar
    IdentifierName

  -- * A.1 Types (TypeScript)
  -- | * https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md#a1-types
  , TypeParameters
  , TypeArguments
  , Type(..)
  , NamespaceName(..)
  , ParameterList
  , Parameter(..)
  , Modifier(..)

  -- * A.2 Expressions
  -- | * http://www.ecma-international.org/ecma-262/6.0/#sec-expressions
  , IdentifierReference
  , BindingIdentifier
  , Identifier

  -- * A.4 Functions and Classes (class part) and A.6 Classes (TypeScript)
  -- | * http://www.ecma-international.org/ecma-262/6.0/#sec-functions-and-classes
  --   * https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md#a6-classes
  , ClassDeclaration(..)
  , ClassElement(..)
  , ConstructorDeclaration(..)

  -- * A.5 Scripts and Modules and A.9 Scripts and Modules (TypeScript)
  -- | * http://www.ecma-international.org/ecma-262/6.0/#sec-scripts-and-modules
  --   * https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md#a9-scripts-and-modules
  , ImplementationModule(..)
  , ImplementationModuleElement(..)
  ) where

import           Control.Monad.Reader
import           Data.Foldable
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Gugugu.Utilities


-- * A.1 Lexical Grammar

{-|
@
IdentifierName ::
        IdentifierStart
        IdentifierName IdentifierPart
IdentifierStart ::
        UnicodeIDStart
        $
        _
        \\ UnicodeEscapeSequence
IdentifierPart ::
        UnicodeIDContinue
        $
        _
        \\ UnicodeEscapeSequence
        \<ZWNJ\>
        \<ZWJ\>
@
-}
type IdentifierName = Text


-- * A.1 Types (TypeScript)

{-|
Typescript:

@
TypeParameters:
        \< TypeParameterList \>
TypeParameterList:
        TypeParameter
        TypeParameterList , TypeParameter
TypeParameter:
        BindingIdentifier Constraint opt
Constraint:
        extends Type
@
 -}
type TypeParameters = [BindingIdentifier]

{-|
Typescript:

@
TypeArguments:
        \< TypeArgumentList \>
TypeArgumentList:
        TypeArgument
        TypeArgumentList , TypeArgument
TypeArgument:
        Type
@
 -}
type TypeArguments = [Type]

{-|
Typescript:

@
Type:
        UnionOrIntersectionOrPrimaryType
        FunctionType
        ConstructorType
UnionOrIntersectionOrPrimaryType:
        UnionType
        IntersectionOrPrimaryType
IntersectionOrPrimaryType:
        IntersectionType
        PrimaryType
PrimaryType:
        ParenthesizedType
        PredefinedType
        TypeReference
        ObjectType
        ArrayType
        TupleType
        TypeQuery
        ThisType
ParenthesizedType:
        ( Type )
PredefinedType:
        any
        number
        boolean
        string
        symbol
        void
TypeReference:
        TypeName [no LineTerminator here] TypeArguments opt
TypeName:
        IdentifierReference
        NamespaceName . IdentifierReference
TupleType:
        [ TupleElementTypes ]
TupleElementTypes:
        TupleElementType
        TupleElementTypes , TupleElementType
TupleElementType:
        Type
UnionType:
        UnionOrIntersectionOrPrimaryType | IntersectionOrPrimaryType
FunctionType:
        TypeParameters opt ( ParameterList opt ) => Type
@
 -}
data Type
  = TParamed NamespaceName TypeArguments
  | TUnion (NonEmpty Type)
  deriving Show

{-|
Typescript:

@
NamespaceName:
        IdentifierReference
        NamespaceName . IdentifierReference
@
 -}
newtype NamespaceName
  = NamespaceName
    { unNamespaceName :: NonEmpty IdentifierReference
    }
  deriving Show

{-|
Typescript:

@
ParameterList:
        RequiredParameterList
        OptionalParameterList
        RestParameter
        RequiredParameterList , OptionalParameterList
        RequiredParameterList , RestParameter
        OptionalParameterList , RestParameter
        RequiredParameterList , OptionalParameterList , RestParameter
RequiredParameterList:
        RequiredParameter
        RequiredParameterList , RequiredParameter
OptionalParameterList:
        OptionalParameter
        OptionalParameterList , OptionalParameter
@
 -}
type ParameterList = [Parameter]

{-|
Typescript:

@
TypeAnnotation:
        : Type
RequiredParameter:
        AccessibilityModifier opt BindingIdentifierOrPattern TypeAnnotation opt
        BindingIdentifier : StringLiteral
BindingIdentifierOrPattern:
        BindingIdentifier
        BindingPattern
OptionalParameter:
        AccessibilityModifier opt BindingIdentifierOrPattern ? TypeAnnotation opt
        AccessibilityModifier opt BindingIdentifierOrPattern TypeAnnotation opt Initializer
        BindingIdentifier ? : StringLiteral
@
 -}
data Parameter
  = Parameter
    { pModifiers :: [Modifier]
    , pName      :: BindingIdentifier
    , pType      :: Type
    }
  deriving Show

{-|
Typescript:

@
AccessibilityModifier:
        public
        private
        protected
@
 -}
data Modifier
  = MExport
  | MPublic
  deriving Show


-- * A.2 Expressions

{-|
@
IdentifierReference[Yield] :
        Identifier
        [~Yield] yield
@
 -}
type IdentifierReference = Identifier

{-|
@
BindingIdentifier[Yield] :
        Identifier
        [~Yield] yield
@
 -}
type BindingIdentifier = Identifier

{-|
@
Identifier :
        IdentifierName but not ReservedWord
@
 -}
type Identifier = IdentifierName


-- * A.4 Functions and Classes (class part) and A.6 Classes (TypeScript)

{-|
@
ClassDeclaration[Yield, Default] :
        class BindingIdentifier[?Yield] ClassTail[?Yield]
        [+Default] class ClassTail[?Yield]
ClassTail[Yield] :
        ClassHeritage[?Yield]opt { ClassBody[?Yield]opt }
ClassHeritage[Yield] :
        extends LeftHandSideExpression[?Yield]
ClassBody[Yield] :
        ClassElementList[?Yield]
ClassElementList[Yield] :
        ClassElement[?Yield]
        ClassElementList[?Yield] ClassElement[?Yield]
@

TypeScript:

@
ClassDeclaration: ( Modified )
        class BindingIdentifier opt TypeParameters opt ClassHeritage { ClassBody }
ClassHeritage: ( Modified )
        ClassExtendsClause opt ImplementsClause opt
ClassExtendsClause:
        extends ClassType
ClassType:
        TypeReference
ImplementsClause:
        implements ClassOrInterfaceTypeList
@
 -}
data ClassDeclaration
  = ClassDeclaration
    { cdModifiers :: [Modifier]
    , cdName      :: BindingIdentifier
    , cdBody      :: [ClassElement]
    }
  deriving Show

{-|
@
ClassElement[Yield] :
        MethodDefinition[?Yield]
        static MethodDefinition[?Yield]
        ;
@

TypeScript:

@
ClassElement: ( Modified )
        ConstructorDeclaration
        PropertyMemberDeclaration
        IndexMemberDeclaration
@
 -}
data ClassElement
  = CEC ConstructorDeclaration
  deriving Show

{-|
TypeScript:

@
ConstructorDeclaration:
        AccessibilityModifier opt constructor ( ParameterList opt ) { FunctionBody }
        AccessibilityModifier opt constructor ( ParameterList opt ) ;
@
 -}
data ConstructorDeclaration
  = ConstructorDeclaration
    { ccdModifiers :: [Modifier]
    , ccdParams    :: ParameterList
    }
  deriving Show


-- * A.5 Scripts and Modules and A.9 Scripts and Modules (TypeScript)

{-|
TypeScript

@
SourceFile:
        ImplementationSourceFile
        DeclarationSourceFile
ImplementationSourceFile:
        ImplementationScript
        ImplementationModule
ImplementationModule:
        ImplementationModuleElements opt
ImplementationModuleElements:
        ImplementationModuleElement
        ImplementationModuleElements ImplementationModuleElement
@
 -}
data ImplementationModule
  = ImplementationModule
    { imBody :: [ImplementationModuleElement]
    }
  deriving Show

{-|
TypeScript

@
ImplementationElement:
        Statement
        LexicalDeclaration
        FunctionDeclaration
        GeneratorDeclaration
        ClassDeclaration
        InterfaceDeclaration
        TypeAliasDeclaration
        EnumDeclaration
        NamespaceDeclaration
        AmbientDeclaration
        ImportAliasDeclaration
ImplementationModuleElement:
        ImplementationElement
        ImportDeclaration
        ImportAliasDeclaration
        ImportRequireDeclaration
        ExportImplementationElement
        ExportDefaultImplementationElement
        ExportListDeclaration
        ExportAssignment
ExportImplementationElement:
        export VariableStatement
        export LexicalDeclaration
        export FunctionDeclaration
        export GeneratorDeclaration
        export ClassDeclaration
        export InterfaceDeclaration
        export TypeAliasDeclaration
        export EnumDeclaration
        export NamespaceDeclaration
        export AmbientDeclaration
        export ImportAliasDeclaration
@
 -}
data ImplementationModuleElement
  = MEC ClassDeclaration
  deriving Show


-- Utilities

writeModifiers :: (Foldable t, Monad m) => t Modifier -> SrcCompT m ()
writeModifiers ms = for_ ms $ \m -> do
  writeSrcComp m
  writeText " "


-- Instances

instance SrcComp Type where
  writeSrcComp v = case v of
    TParamed t ps -> do
      writeSrcComp t
      unless (null ps) $ do
        writeText "<"
        forWithComma_ ps writeSrcComp
        writeText ">"
    TUnion ts     -> forWith_ " | " ts writeSrcComp

instance SrcComp NamespaceName where
  writeSrcComp n = writeText $ T.intercalate "." $ toList $ unNamespaceName n

instance SrcComp Parameter where
  writeSrcComp Parameter{..} = do
    writeModifiers pModifiers
    writeText pName
    writeText ": "
    writeSrcComp pType

instance SrcComp Modifier where
  writeSrcComp v = case v of
    MExport -> writeText "export"
    MPublic -> writeText "public"


instance SrcComp ClassDeclaration where
  writeSrcComp ClassDeclaration{..} = do
    writeModifiers cdModifiers
    writeText "class "
    writeText cdName
    writeText " {\n"
    indentBy 2 $ do
      for_ cdBody $ \ce -> do
        writeText "\n"
        writeSrcComp ce
      writeText "\n"
    doIndent
    writeText "}"

instance SrcComp ClassElement where
  writeSrcComp v = withNewLine $ case v of
    CEC c -> writeSrcComp c

instance SrcComp ConstructorDeclaration where
  writeSrcComp ConstructorDeclaration{..} = do
    writeModifiers ccdModifiers
    writeText "constructor"
    case ccdParams of
      []      -> writeText "("
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
    writeText ") { }"


instance SrcComp ImplementationModule where
  writeSrcComp ImplementationModule{..} = do
    forWith_ "\n" imBody writeSrcComp

instance SrcComp ImplementationModuleElement where
  writeSrcComp v = withNewLine $ case v of
    MEC cd -> writeSrcComp cd
