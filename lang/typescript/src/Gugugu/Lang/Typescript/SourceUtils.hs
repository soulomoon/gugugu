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
  , PropertyName
  , ParameterList
  , Parameter(..)
  , Modifier(..)
  , MethodSignature(..)

  -- * A.2 Expressions and A.2 Expressions (TypeScript)
  -- | * http://www.ecma-international.org/ecma-262/6.0/#sec-expressions
  --   * https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md#a2-expressions
  , IdentifierReference
  , BindingIdentifier
  , Identifier
  , PropertyDefinition(..)
  , Expression(..)

  -- * A.3 Statements
  -- | * http://www.ecma-international.org/ecma-262/6.0/#sec-statements
  , StatementList
  , StatementListItem(..)
  , LexicalDeclaration(..)
  , BindingPattern(..)

  -- * A.4 Functions and Classes (function part) and A.4 Functions (TypeScript)
  -- | * http://www.ecma-international.org/ecma-262/6.0/#sec-functions-and-classes
  --   * https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md#a4-functions
  , ArrowFunction(..)
  , FunctionBody

  -- * A.4 Functions and Classes (class part) and A.5 Interfaces (TypeScript) and A.6 Classes (TypeScript)
  -- | * http://www.ecma-international.org/ecma-262/6.0/#sec-functions-and-classes
  --   * https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md#a5-interfaces
  --   * https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md#a6-classes
  , InterfaceDeclaration(..)
  , ClassDeclaration(..)
  , ClassElement(..)
  , ConstructorDeclaration(..)
  , MemberVariableDeclaration(..)
  , MemberFunctionDeclaration(..)

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
  | TParen Type
  | TFunc TypeParameters ParameterList Type
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
TypeScript:

@
PropertyName:
        IdentifierName
        StringLiteral
        NumericLiteral
@
 -}
type PropertyName = IdentifierName

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
    , pOptional  :: Bool
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
  | MStatic
  | MPublic
  deriving Show

{-|
TypeScript

@
CallSignature:
        TypeParameters opt ( ParameterList opt ) TypeAnnotation opt
MethodSignature:
        PropertyName ?opt CallSignature
@
 -}
data MethodSignature
  = MethodSignature
    { msName    :: PropertyName
    , msTParams :: TypeParameters
    , msParams  :: [Parameter]
    , msRType   :: Type
    }
  deriving Show


-- * A.2 Expressions and A.2 Expressions (TypeScript)

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

{-|
@
PropertyDefinition[Yield] :
        IdentifierReference[?Yield]
        CoverInitializedName[?Yield]
        PropertyName[?Yield] : AssignmentExpression[In, ?Yield]
        MethodDefinition[?Yield]
@

TypeScript

@
PropertyDefinition: ( Modified )
        IdentifierReference
        CoverInitializedName
        PropertyName : AssignmentExpression
        PropertyName CallSignature { FunctionBody }
        GetAccessor
        SetAccessor
@
 -}
data PropertyDefinition
  = PDProperty PropertyName Expression
  | PDMethod MethodSignature FunctionBody
  deriving Show

{-|
@
PrimaryExpression[Yield] :
        this
        IdentifierReference[?Yield]
        Literal
        ArrayLiteral[?Yield]
        ObjectLiteral[?Yield]
        FunctionExpression
        ClassExpression[?Yield]
        GeneratorExpression
        RegularExpressionLiteral
        TemplateLiteral[?Yield]
        CoverParenthesizedExpressionAndArrowParameterList[?Yield]
ArrayLiteral[Yield] :
        [ Elision opt ]
        [ ElementList[?Yield] ]
        [ ElementList[?Yield] , Elisionopt ]
ElementList[Yield] :
        Elision opt AssignmentExpression[In, ?Yield]
        Elision opt SpreadElement[?Yield]
        ElementList[?Yield] , Elision opt AssignmentExpression[In, ?Yield]
        ElementList[?Yield] , Elision opt SpreadElement[?Yield]
ObjectLiteral[Yield] :
        { }
        { PropertyDefinitionList[?Yield] }
        { PropertyDefinitionList[?Yield] , }
PropertyDefinitionList[Yield] :
        PropertyDefinition[?Yield]
        PropertyDefinitionList[?Yield] , PropertyDefinition[?Yield]
MemberExpression[Yield] :
        PrimaryExpression[?Yield]
        MemberExpression[?Yield] [ Expression[In, ?Yield] ]
        MemberExpression[?Yield] . IdentifierName
        MemberExpression[?Yield] TemplateLiteral[?Yield]
        SuperProperty[?Yield]
        MetaProperty
        new MemberExpression[?Yield] Arguments[?Yield]
CallExpression[Yield] :
        MemberExpression[?Yield] Arguments[?Yield]
        SuperCall[?Yield]
        CallExpression[?Yield] Arguments[?Yield]
        CallExpression[?Yield] [ Expression[In, ?Yield] ]
        CallExpression[?Yield] . IdentifierName
        CallExpression[?Yield] TemplateLiteral[?Yield]
Arguments[Yield] :
        ( )
        ( ArgumentList[?Yield] )
ArgumentList[Yield] :
        AssignmentExpression[In, ?Yield]
        ... AssignmentExpression[In, ?Yield]
        ArgumentList[?Yield] , AssignmentExpression[In, ?Yield]
        ArgumentList[?Yield] , ... AssignmentExpression[In, ?Yield]
LeftHandSideExpression[Yield] :
        NewExpression[?Yield]
        CallExpression[?Yield]
PostfixExpression[Yield] :
        LeftHandSideExpression[?Yield]
        LeftHandSideExpression[?Yield] [no LineTerminator here] ++
        LeftHandSideExpression[?Yield] [no LineTerminator here] --
UnaryExpression[Yield] :
        PostfixExpression[?Yield]
        delete UnaryExpression[?Yield]
        void UnaryExpression[?Yield]
        typeof UnaryExpression[?Yield]
        ++ UnaryExpression[?Yield]
        -- UnaryExpression[?Yield]
        + UnaryExpression[?Yield]
        - UnaryExpression[?Yield]
        ~ UnaryExpression[?Yield]
        ! UnaryExpression[?Yield]
MultiplicativeExpression[Yield] :
        UnaryExpression[?Yield]
        MultiplicativeExpression[?Yield] MultiplicativeOperator UnaryExpression[?Yield]
MultiplicativeOperator : one of
        * / %
AdditiveExpression[Yield] :
        MultiplicativeExpression[?Yield]
        AdditiveExpression[?Yield] + MultiplicativeExpression[?Yield]
        AdditiveExpression[?Yield] - MultiplicativeExpression[?Yield]
ShiftExpression[Yield] :
        AdditiveExpression[?Yield]
        ShiftExpression[?Yield] \<\< AdditiveExpression[?Yield]
        ShiftExpression[?Yield] \>\> AdditiveExpression[?Yield]
        ShiftExpression[?Yield] \>\>\> AdditiveExpression[?Yield]
RelationalExpression[In, Yield] :
        ShiftExpression[?Yield]
        RelationalExpression[?In, ?Yield] \< ShiftExpression[?Yield]
        RelationalExpression[?In, ?Yield] \> ShiftExpression[?Yield]
        RelationalExpression[?In, ?Yield] \<= ShiftExpression[? Yield]
        RelationalExpression[?In, ?Yield] \>= ShiftExpression[?Yield]
        RelationalExpression[?In, ?Yield] instanceof ShiftExpression[?Yield]
        [+In] RelationalExpression[In, ?Yield] in ShiftExpression[?Yield]
EqualityExpression[In, Yield] :
        RelationalExpression[?In, ?Yield]
        EqualityExpression[?In, ?Yield] == RelationalExpression[?In, ?Yield]
        EqualityExpression[?In, ?Yield] != RelationalExpression[?In, ?Yield]
        EqualityExpression[?In, ?Yield] === RelationalExpression[?In, ?Yield]
        EqualityExpression[?In, ?Yield] !== RelationalExpression[?In, ?Yield]
BitwiseANDExpression[In, Yield] :
        EqualityExpression[?In, ?Yield]
        BitwiseANDExpression[?In, ?Yield] & EqualityExpression[?In, ?Yield]
BitwiseXORExpression[In, Yield] :
        BitwiseANDExpression[?In, ?Yield]
        BitwiseXORExpression[?In, ?Yield] ^ BitwiseANDExpression[?In, ?Yield]
BitwiseORExpression[In, Yield] :
        BitwiseXORExpression[?In, ?Yield]
        BitwiseORExpression[?In, ?Yield] | BitwiseXORExpression[?In, ?Yield]
LogicalANDExpression[In, Yield] :
        BitwiseORExpression[?In, ?Yield]
        LogicalANDExpression[?In, ?Yield] && BitwiseORExpression[?In, ?Yield]
LogicalORExpression[In, Yield] :
        LogicalANDExpression[?In, ?Yield]
        LogicalORExpression[?In, ?Yield] || LogicalANDExpression[?In, ?Yield]
ConditionalExpression[In, Yield] :
        LogicalORExpression[?In, ?Yield]
        LogicalORExpression[?In,?Yield] ? AssignmentExpression[In, ?Yield] : AssignmentExpression[?In, ?Yield]
AssignmentExpression[In, Yield] :
        ConditionalExpression[?In, ?Yield]
        [+Yield] YieldExpression[?In]
        ArrowFunction[?In, ?Yield]
        LeftHandSideExpression[?Yield] = AssignmentExpression[?In, ?Yield]
        LeftHandSideExpression[?Yield] AssignmentOperator AssignmentExpression[?In, ?Yield]
Expression[In, Yield] :
        AssignmentExpression[?In, ?Yield]
        Expression[?In, ?Yield] , AssignmentExpression[?In, ?Yield]
@

TypeScript

@
Arguments: ( Modified )
        TypeArguments opt ( ArgumentList opt )
@
 -}
data Expression
  = ESimple IdentifierReference
  | EMember Expression IdentifierName
  | ECall Expression TypeArguments [Expression]
  | EArrow ArrowFunction
  | EArray [Expression]
  | ENew Expression [Expression]
  | EObject [PropertyDefinition]
  | EBinary Expression Text Expression
  | EIndex Expression Expression
  deriving Show


-- * A.3 Statements

{-|
@
StatementList[Yield, Return] :
        StatementListItem[?Yield, ?Return]
        StatementList[?Yield, ?Return] StatementListItem[?Yield, ?Return]
@
 -}
type StatementList = [StatementListItem]

{-|
@
Statement[Yield, Return] :
        BlockStatement[?Yield, ?Return]
        VariableStatement[?Yield]
        EmptyStatement
        ExpressionStatement[?Yield]
        IfStatement[?Yield, ?Return]
        BreakableStatement[?Yield, ?Return]
        ContinueStatement[?Yield]
        BreakStatement[?Yield]
        [+Return] ReturnStatement[?Yield]
        WithStatement[?Yield, ?Return]
        LabelledStatement[?Yield, ?Return]
        ThrowStatement[?Yield]
        TryStatement[?Yield, ?Return]
        DebuggerStatement
Declaration[Yield] :
        HoistableDeclaration[?Yield]
        ClassDeclaration[?Yield]
        LexicalDeclaration[In, ?Yield]
BreakableStatement[Yield, Return] :
        IterationStatement[?Yield, ?Return]
        SwitchStatement[?Yield, ?Return]
SwitchStatement[Yield, Return] :
        switch ( Expression[In, ?Yield] ) CaseBlock[?Yield, ?Return]
CaseBlock[Yield, Return] :
        { CaseClauses[?Yield, ?Return]opt }
        { CaseClauses[?Yield, ?Return]opt DefaultClause[?Yield, ?Return] CaseClauses[?Yield, ?Return]opt }
CaseClauses[Yield, Return] :
        CaseClause[?Yield, ?Return]
        CaseClauses[?Yield, ?Return] CaseClause[?Yield, ?Return]
CaseClause[Yield, Return] :
        case Expression[In, ?Yield] : StatementList[?Yield, ?Return]opt
StatementListItem[Yield, Return] :
        Statement[?Yield, ?Return]
        Declaration[?Yield]
ReturnStatement[Yield] :
        return ;
        return [no LineTerminator here] Expression[In, ?Yield] ;
@
 -}
data StatementListItem
  = SIR Expression
  | SID LexicalDeclaration
  | SIf Expression StatementListItem (Maybe StatementListItem)
  | SSwitch Expression [(Expression, StatementList)]
  deriving Show

{-|
@
LexicalDeclaration[In, Yield] :
        LetOrConst BindingList[?In, ?Yield] ;
LetOrConst :
        let
        const
BindingList[In, Yield] :
        LexicalBinding[?In, ?Yield]
        BindingList[?In, ?Yield] , LexicalBinding[?In, ?Yield]
LexicalBinding[In, Yield] :
        BindingIdentifier[?Yield] Initializer[?In, ?Yield]opt
        BindingPattern[?Yield] Initializer[?In, ?Yield]
@
 -}
data LexicalDeclaration
  = LexicalDeclaration
    { ldModifiers :: [Modifier]
    , ldPattern   :: BindingPattern
    , ldDef       :: Expression
    }
  deriving Show

{-|
@
BindingPattern[Yield] :
        ObjectBindingPattern[?Yield]
        ArrayBindingPattern[?Yield]
ArrayBindingPattern[Yield] :
        [ Elision opt BindingRestElement[?Yield]opt ]
        [ BindingElementList[?Yield] ]
        [ BindingElementList[?Yield] , Elision opt BindingRestElement[?Yield]opt ]
BindingElementList[Yield] :
        BindingElisionElement[?Yield]
        BindingElementList[?Yield] , BindingElisionElement[?Yield]
BindingElisionElement[Yield] :
        Elision opt BindingElement[?Yield]
BindingElement[Yield] :
        SingleNameBinding[?Yield]
        BindingPattern[?Yield] Initializer[In, ?Yield]opt
SingleNameBinding[Yield] :
        BindingIdentifier[?Yield] Initializer[In, ?Yield]opt
@
 -}
data BindingPattern
  = PSimple BindingIdentifier
  | PArray [BindingIdentifier]
  deriving Show


-- * A.4 Functions and Classes (function part) and A.4 Functions

{-|
@
ArrowFunction[In, Yield] :
        ArrowParameters[?Yield] [no LineTerminator here] => ConciseBody[?In]
ArrowParameters[Yield] :
        BindingIdentifier[?Yield]
        CoverParenthesizedExpressionAndArrowParameterList[?Yield]
ConciseBody[In] :
        [lookahead ≠ { ] AssignmentExpression[?In]
        { FunctionBody }
@
 -}
data ArrowFunction
  = ArrowFunction
    { afParams :: [BindingIdentifier]
    , afBody   :: Either Expression FunctionBody
    }
  deriving Show

{-|
@
FunctionBody[Yield] :
        FunctionStatementList[?Yield]
FunctionStatementList[Yield] :
        StatementList[?Yield, Return]opt
@
 -}
type FunctionBody = StatementList


-- * A.4 Functions and Classes (class part) and A.5 Interfaces (TypeScript) and A.6 Classes (TypeScript)

{-|
TypeScript

@
ObjectType:
        { TypeBody opt }
TypeBody:
        TypeMemberList ;opt
        TypeMemberList ,opt
TypeMemberList:
        TypeMember
        TypeMemberList ; TypeMember
        TypeMemberList , TypeMember
TypeMember:
        PropertySignature
        CallSignature
        ConstructSignature
        IndexSignature
        MethodSignature
CallSignature:
        TypeParameters opt ( ParameterList opt ) TypeAnnotation opt
MethodSignature:
        PropertyName ?opt CallSignature
InterfaceDeclaration:
        interface BindingIdentifier TypeParameters opt InterfaceExtendsClause opt ObjectType
@
 -}
data InterfaceDeclaration
  = InterfaceDeclaration
    { idModifiers :: [Modifier]
    , idName      :: BindingIdentifier
    , idTParams   :: TypeParameters
    , idMethods   :: [MethodSignature]
    }
  deriving Show

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
    , cdTParams   :: TypeParameters
    , cdImpls     :: [Type]
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
PropertyMemberDeclaration:
        MemberVariableDeclaration
        MemberFunctionDeclaration
        MemberAccessorDeclaration
@
 -}
data ClassElement
  = CEC ConstructorDeclaration
  | CEV MemberVariableDeclaration
  | CEM MemberFunctionDeclaration
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

{-|
@
Initializer[In, Yield] :
        = AssignmentExpression[?In, ?Yield]
@

TypeScript

@
MemberVariableDeclaration:
        AccessibilityModifier opt static opt PropertyName TypeAnnotation opt Initializer opt ;
@
 -}
data MemberVariableDeclaration
  = MemberVariableDeclaration
    { mvdModifiers :: [Modifier]
    , mvdName      :: PropertyName
    , mvdType      :: Type
    , mvdDef       :: Expression
    }
  deriving Show

{-|
@
Initializer[In, Yield] :
        = AssignmentExpression[?In, ?Yield]
@

TypeScript

@
MemberFunctionDeclaration:
        AccessibilityModifier opt static opt PropertyName CallSignature { FunctionBody }
        AccessibilityModifier opt static opt PropertyName CallSignature ;
@
 -}
data MemberFunctionDeclaration
  = MemberFunctionDeclaration
    { mfdModifiers :: [Modifier]
    , mfdSig       :: MethodSignature
    , mfdBody      :: FunctionBody
    }
  deriving Show


-- * A.5 Scripts and Modules and A.9 Scripts and Modules (TypeScript)

{-|
@
ImportDeclaration :
        import ImportClause FromClause ;
        import ModuleSpecifier ;
ImportClause :
        ImportedDefaultBinding
        NameSpaceImport
        NamedImports
        ImportedDefaultBinding , NameSpaceImport
        ImportedDefaultBinding , NamedImports
NameSpaceImport :
        * as ImportedBinding
ImportedBinding :
        BindingIdentifier
FromClause :
        from ModuleSpecifier
ModuleSpecifier :
        StringLiteral
@

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
    { imImports :: [(BindingIdentifier, Text)]
    , imBody    :: [ImplementationModuleElement]
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
  | MEI InterfaceDeclaration
  | MED LexicalDeclaration
  deriving Show


-- Utilities

writeParams :: (Foldable t, Monad m) => t Expression -> SrcCompT m ()
writeParams es = do
  writeText "("
  forWithComma_ es writeSrcComp
  writeText ")"

writeModifiers :: (Foldable t, Monad m) => t Modifier -> SrcCompT m ()
writeModifiers ms = for_ ms $ \m -> do
  writeSrcComp m
  writeText " "


-- Instances

instance SrcComp Type where
  writeSrcComp v = case v of
    TParamed t ps   -> do
      writeSrcComp t
      unless (null ps) $ do
        writeText "<"
        forWithComma_ ps writeSrcComp
        writeText ">"
    TUnion ts       -> forWith_ " | " ts writeSrcComp
    TParen t        -> do
      writeText "("
      writeSrcComp t
      writeText ")"
    TFunc tps ps rt -> do
      case tps of
        [] -> pure ()
        _  -> do
          writeText "<"
          forWithComma_ tps writeText
          writeText ">"
      writeText "("
      forWithComma_ ps writeSrcComp
      writeText ") => "
      writeSrcComp rt

instance SrcComp NamespaceName where
  writeSrcComp n = writeText $ T.intercalate "." $ toList $ unNamespaceName n

instance SrcComp Parameter where
  writeSrcComp Parameter{..} = do
    writeModifiers pModifiers
    writeText pName
    when pOptional $ writeText "?"
    writeText ": "
    writeSrcComp pType

instance SrcComp Modifier where
  writeSrcComp v = case v of
    MExport -> writeText "export"
    MStatic -> writeText "static"
    MPublic -> writeText "public"

instance SrcComp MethodSignature where
  writeSrcComp MethodSignature{..} = do
    writeText msName
    unless (null msTParams) $ do
      writeText "<"
      forWithComma_ msTParams writeText
      writeText ">"
    writeText "("
    forWithComma_ msParams writeSrcComp
    writeText "): "
    writeSrcComp msRType


instance SrcComp PropertyDefinition where
  writeSrcComp v = case v of
    PDProperty n e    -> do
      writeText n
      writeText ": "
      writeSrcComp e
    PDMethod sig body -> do
      writeSrcComp sig
      writeText " {\n"
      indentBy 2 $ traverse_ (withNewLine . writeSrcComp) body
      doIndent
      writeText "}"

instance SrcComp Expression where
  writeSrcComp v = case v of
    ESimple n        -> writeText n
    EMember this n   -> do
      writeSrcComp this
      writeText "."
      writeText n
    ECall f ts as    -> do
      writeSrcComp f
      unless (null ts) $ do
        writeText "<"
        forWithComma_ ts writeSrcComp
        writeText ">"
      writeParams as
    EArrow af        -> writeSrcComp af
    EArray es        -> do
      writeText "["
      forWithComma_ es writeSrcComp
      writeText "]"
    ENew c as        -> do
      writeText "new "
      writeSrcComp c
      writeParams as
    EObject ps       -> do
      writeText "{\n"
      indentBy 2 $ for_ ps $ \p -> withNewLine $ do
        writeSrcComp p
        writeText ","
      doIndent
      writeText "}"
    EBinary e1 op e2 -> do
      writeSrcComp e1
      writeText " "
      writeText op
      writeText " "
      writeSrcComp e2
    EIndex e i       -> do
      writeSrcComp e
      writeText "["
      writeSrcComp i
      writeText "]"


instance SrcComp StatementListItem where
  writeSrcComp v = case v of
    SIR e           -> do
      writeText "return "
      writeSrcComp e
      writeText ";"
    SID c           -> writeSrcComp c
    SIf cond s1 s2  -> do
      writeText "if ("
      writeSrcComp cond
      writeText ") "
      writeSrcComp s1
      for_ s2 $ \s -> do
        writeText " else "
        writeSrcComp s
    SSwitch e cases -> do
      writeText "switch ("
      writeSrcComp e
      writeText ") {\n"
      indentBy 2 $ for_ cases $ \(e', ss) -> do
        withNewLine $ do
          writeText "case "
          writeSrcComp e'
          writeText ":"
        indentBy 2 $ traverse_ (withNewLine . writeSrcComp) ss
      doIndent
      writeText "}"

instance SrcComp LexicalDeclaration where
  writeSrcComp LexicalDeclaration{..} = do
    writeModifiers ldModifiers
    writeText "const "
    writeSrcComp ldPattern
    writeText " = "
    writeSrcComp ldDef
    writeText ";"

instance SrcComp BindingPattern where
  writeSrcComp v = case v of
    PSimple n -> writeText n
    PArray ns -> do
      writeText "["
      forWithComma_ ns writeText
      writeText "]"


instance SrcComp ArrowFunction where
  writeSrcComp ArrowFunction{..} = do
    case afParams of
      [p] -> writeText p
      _   -> do
        writeText "("
        forWithComma_ afParams writeText
        writeText ")"
    writeText " => "
    case afBody of
      Left e   -> writeSrcComp e
      Right ss -> do
        writeText "{\n"
        indentBy 2 $ traverse_ (withNewLine . writeSrcComp) ss
        doIndent
        writeText "}"


instance SrcComp InterfaceDeclaration where
  writeSrcComp InterfaceDeclaration{..} = do
    writeModifiers idModifiers
    writeText "interface "
    writeText idName
    unless (null idTParams) $ do
      writeText "<"
      forWithComma_ idTParams writeText
      writeText ">"
    writeText " {\n"
    indentBy 2 $ for_ idMethods $ \ms -> do
      doIndent
      writeSrcComp ms
      writeText ";\n"
    doIndent
    writeText "}"

instance SrcComp ClassDeclaration where
  writeSrcComp ClassDeclaration{..} = do
    writeModifiers cdModifiers
    writeText "class "
    writeText cdName
    case cdTParams of
      [] -> pure ()
      _  -> do
        writeText "<"
        forWithComma_ cdTParams writeText
        writeText ">"
    unless (null cdImpls) $ do
      writeText " implements "
      forWithComma_ cdImpls writeSrcComp
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
    CEV c -> writeSrcComp c
    CEM c -> writeSrcComp c

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

instance SrcComp MemberVariableDeclaration where
  writeSrcComp MemberVariableDeclaration{..} = do
    writeModifiers mvdModifiers
    writeText mvdName
    writeText ": "
    writeSrcComp mvdType
    writeText " = "
    writeSrcComp mvdDef
    writeText ";"

instance SrcComp MemberFunctionDeclaration where
  writeSrcComp MemberFunctionDeclaration{..} = do
    writeModifiers mfdModifiers
    writeSrcComp mfdSig
    writeText " {\n"
    indentBy 2 $ traverse_ (withNewLine . writeSrcComp) mfdBody
    doIndent
    writeText "}"


instance SrcComp ImplementationModule where
  writeSrcComp ImplementationModule{..} = do
    unless (null imImports) $ do
      for_ imImports $ \(alias, path) -> withNewLine $ do
        writeText "import * as "
        writeText alias
        writeText " from \""
        writeText path
        writeText "\";"
      writeText "\n"
    forWith_ "\n" imBody writeSrcComp

instance SrcComp ImplementationModuleElement where
  writeSrcComp v = withNewLine $ case v of
    MEC cd  -> writeSrcComp cd
    MEI id' -> writeSrcComp id'
    MED ld  -> writeSrcComp ld
