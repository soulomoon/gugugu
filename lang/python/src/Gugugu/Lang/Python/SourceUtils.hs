{-|
Utilities to generate Python sources.

Note, it is a simplified version and not intended for general purpose use.

Vide: https://docs.python.org/3/reference/index.html
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module Gugugu.Lang.Python.SourceUtils
  ( Identifier
  -- * Expressions
  -- | * https://docs.python.org/3/reference/expressions.html
  , Expr(..)
  , LambdaExpr(..)
  , ArgumentList(..)
  -- * Simple statements
  -- | * https://docs.python.org/3/reference/simple_stmts.html
  , Target(..)
  , AssignmentStmt(..)
  , AnnotatedAssignmentStmt(..)
  , ReturnStmt(..)
  , RaiseStmt(..)
  , ImportStmt(..)
  -- * Compound statements
  -- | * https://docs.python.org/3/reference/compound_stmts.html
  , Suite
  , IfStmt(..)
  , TryStmt(..)
  , FuncDef(..)
  , ClassDef(..)
  , ParameterList
  -- * Top-level components
  -- | * https://docs.python.org/3/reference/toplevel_components.html
  , FileInput(..)
  , Statement(..)
  ) where

import           Control.Monad
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text          (Text)

import           Gugugu.Utilities


-- | Python Identifier
type Identifier = Text


-- * Expressions

{-|
@
atom      ::=  identifier | literal | enclosure
enclosure ::=  parenth_form | list_display | dict_display | set_display
               | generator_expression | yield_atom
literal ::=  stringliteral | bytesliteral
             | integer | floatnumber | imagnumber

primary ::=  atom | attributeref | subscription | slicing | call

await_expr ::=  "await" primary

power ::=  (await_expr | primary) ["**" u_expr]

u_expr ::=  power | "-" u_expr | "+" u_expr | "~" u_expr

m_expr ::=  u_expr | m_expr "*" u_expr | m_expr "@" m_expr |
            m_expr "\/\/" u_expr | m_expr "\/" u_expr |
            m_expr "%" u_expr
a_expr ::=  m_expr | a_expr "+" m_expr | a_expr "-" m_expr

shift_expr ::=  a_expr | shift_expr ("\<\<" | "\>\>") a_expr

and_expr ::=  shift_expr | and_expr "&" shift_expr
xor_expr ::=  and_expr | xor_expr "^" and_expr
or_expr  ::=  xor_expr | or_expr "|" xor_expr

comparison    ::=  or_expr (comp_operator or_expr)*
comp_operator ::=  "\<" | "\>" | "==" | ">=" | "<=" | "!="
                   | "is" ["not"] | ["not"] "in"

or_test  ::=  and_test | or_test "or" and_test
and_test ::=  not_test | and_test "and" not_test
not_test ::=  comparison | "not" not_test

conditional_expression ::=  or_test ["if" or_test "else" expression]
expression             ::=  conditional_expression | lambda_expr
expression_nocond      ::=  or_test | lambda_expr_nocond
@
 -}
data Expr
  = ESimple Text
  | EAttr Expr Identifier
  | ECall Expr ArgumentList
  | ESub Expr [Expr]
  | EBinary Expr Text Expr
  | ELambda LambdaExpr
  | EList [Expr]
  | EDict [(Expr, Expr)]
  deriving Show

{-|
@
lambda_expr        ::=  "lambda" [parameter_list] ":" expression
lambda_expr_nocond ::=  "lambda" [parameter_list] ":" expression_nocond
@
 -}
data LambdaExpr
  = LambdaExpr
    { leParams :: [Identifier]
    , leExpr   :: Expr
    }
  deriving Show

{-|
@
argument_list        ::=  positional_arguments ["," starred_and_keywords]
                            ["," keywords_arguments]
                          | starred_and_keywords ["," keywords_arguments]
                          | keywords_arguments
positional_arguments ::=  ["*"] expression ("," ["*"] expression)*
starred_and_keywords ::=  ("*" expression | keyword_item)
                          ("," "*" expression | "," keyword_item)*
keywords_arguments   ::=  (keyword_item | "**" expression)
                          ("," keyword_item | "," "**" expression)*
keyword_item         ::=  identifier "=" expression
@
 -}
data ArgumentList
  = ArgumentList
    { alPositional :: [Expr]
    }
  deriving Show


-- * Simple Statement

{-|
@
target_list     ::=  target ("," target)* [","]
target          ::=  identifier
                     | "(" [target_list] ")"
                     | "[" [target_list] "]"
                     | attributeref
                     | subscription
                     | slicing
                     | "*" target

augtarget                 ::=  identifier | attributeref | subscription | slicing
augop                     ::=  "+=" | "-=" | "*=" | "@=" | "\/=" | "\/\/=" | "%=" | "**="
                               | ">>=" | "<<=" | "&=" | "^=" | "|="
@
 -}
data Target
  = TSimple Identifier
  | TAttr Expr Identifier
  | TTwo Identifier Identifier
  deriving Show

{-|
@
assignment_stmt ::=  (target_list "=")+ (starred_expression | yield_expression)
@
 -}
data AssignmentStmt
  = AssignmentStmt
    { asTarget :: Target
    , asValue  :: Expr
    }
  deriving Show

{-|
@
annotated_assignment_stmt ::=  augtarget ":" expression
                               ["=" (starred_expression | yield_expression)]
@
 -}
data AnnotatedAssignmentStmt
  = AnnotatedAssignmentStmt
    { aasTarget     :: Target
    , aasAnnotation :: Expr
    }
  deriving Show

{-|
@
return_stmt ::=  "return" [expression_list]
@
 -}
newtype ReturnStmt
  = ReturnStmt
    { rsValues :: [Expr]
    }
  deriving Show

{-|
@
raise_stmt ::=  "raise" [expression ["from" expression]]
@
 -}
newtype RaiseStmt
  = RaiseStmt
    { rsExc :: Expr
    }
  deriving Show

{-|
@
import_stmt     ::=  "import" module ["as" identifier] ("," module ["as" identifier])*
                     | "from" relative_module "import" identifier ["as" identifier]
                     ("," identifier ["as" identifier])*
                     | "from" relative_module "import" "(" identifier ["as" identifier]
                     ("," identifier ["as" identifier])* [","] ")"
                     | "from" module "import" "*"
module          ::=  (identifier ".")* identifier
relative_module ::=  "."* module | "."+
@
 -}
data ImportStmt
  = ImportAs (NonEmpty Identifier) Identifier
  | ImportFrom (NonEmpty Identifier) (NonEmpty Identifier)
  deriving (Eq, Ord, Show)


-- * Compound Statement

{-|
@
suite         ::=  stmt_list NEWLINE | NEWLINE INDENT statement+ DEDENT
@
 -}
type Suite = [Statement]

{-|
@
if_stmt ::=  "if" expression ":" suite
             ("elif" expression ":" suite)*
             ["else" ":" suite]
@
 -}
data IfStmt
  = IfStmt
    { isCond  :: Expr
    , isFirst :: Suite
    }
  deriving Show

{-|
@
try_stmt  ::=  try1_stmt | try2_stmt
try1_stmt ::=  "try" ":" suite
               ("except" [expression ["as" identifier]] ":" suite)+
               ["else" ":" suite]
               ["finally" ":" suite]
try2_stmt ::=  "try" ":" suite
               "finally" ":" suite
@
 -}
data TryStmt
  = TryStmt
    { tsBody    :: Suite
    , tsExcepts :: [(Expr, Maybe Identifier, Suite)]
    , tsElse    :: Maybe Suite
    , tsFinally :: Maybe Suite
    }
  deriving Show

{-|
@
funcdef                   ::=  [decorators] "def" funcname "(" [parameter_list] ")"
                               ["->" expression] ":" suite
decorators                ::=  decorator+
decorator                 ::=  "@" dotted_name ["(" [argument_list [","]] ")"] NEWLINE
dotted_name               ::=  identifier ("." identifier)*
funcname                  ::=  identifier
@
 -}
data FuncDef
  = FuncDef
    { fdDecorators :: [Expr]
    , fdName       :: Identifier
    , fdParams     :: ParameterList     -- ^ Pair of name, annotation
    , fdRType      :: Maybe Expr
    , fdSuite      :: [Statement]
    }
  deriving Show

{-|
@
classdef    ::=  [decorators] "class" classname [inheritance] ":" suite
inheritance ::=  "(" [argument_list] ")"
classname   ::=  identifier
@
 -}
data ClassDef
  = ClassDef
    { cdDecorators :: [Expr]
    , cdName       :: Identifier
    , cdArgs       :: ArgumentList
    , cdSuite      :: Suite
    }
  deriving Show

{-|
Pair of name, annotation
@
parameter_list            ::=  defparameter ("," defparameter)* "," "/" ["," [parameter_list_no_posonly]]
                               | parameter_list_no_posonly
parameter_list_no_posonly ::=  defparameter ("," defparameter)* ["," [parameter_list_starargs]]
                               | parameter_list_starargs
parameter_list_starargs   ::=  "*" [parameter] ("," defparameter)* ["," ["**" parameter [","]]]
                               | "**" parameter [","]
parameter                 ::=  identifier [":" expression]
defparameter              ::=  parameter ["=" expression]
@
-}
type ParameterList = [(Identifier, Maybe Expr)]


-- * Top-level components

{-|
@
file_input ::=  (NEWLINE | statement)*
@
 -}
data FileInput
  = FileInput
    { fiImports :: [ImportStmt]
    , fiContent :: [Statement]
    }
  deriving Show

{-|
@
compound_stmt ::=  if_stmt
                   | while_stmt
                   | for_stmt
                   | try_stmt
                   | with_stmt
                   | funcdef
                   | classdef
                   | async_with_stmt
                   | async_for_stmt
                   | async_funcdef

simple_stmt ::=  expression_stmt
                 | assert_stmt
                 | assignment_stmt
                 | augmented_assignment_stmt
                 | annotated_assignment_stmt
                 | pass_stmt
                 | del_stmt
                 | return_stmt
                 | yield_stmt
                 | raise_stmt
                 | break_stmt
                 | continue_stmt
                 | import_stmt
                 | future_stmt
                 | global_stmt
                 | nonlocal_stmt

statement     ::=  stmt_list NEWLINE | compound_stmt
stmt_list     ::=  simple_stmt (";" simple_stmt)* [";"]
@
 -}
data Statement
  = SA AssignmentStmt
  | SAA AnnotatedAssignmentStmt
  | SR ReturnStmt
  | SRA RaiseStmt
  | SI IfStmt
  | ST TryStmt
  | SFD FuncDef
  | SCD ClassDef
  deriving Show


-- Utilities

isEmptyArgumentList :: ArgumentList -> Bool
isEmptyArgumentList ArgumentList{..} = null alPositional

writeSuite :: Monad m => Suite -> SrcCompT m ()
writeSuite suite = indentBy 4 $ if (null suite)
  then withNewLine $ writeText "pass"
  else traverse_ writeSrcComp suite


-- Instances

instance SrcComp Expr where
  writeSrcComp v = case v of
    ESimple t      -> writeText t
    EAttr p n      -> do
      writeSrcComp p
      writeText "."
      writeText n
    ECall p arg    -> do
      writeSrcComp p
      writeText "("
      writeSrcComp arg
      writeText ")"
    ESub p ks      -> do
      writeSrcComp p
      writeText "["
      forWithComma_ ks writeSrcComp
      writeText "]"
    EBinary l op r -> do
      writeSrcComp l
      writeText " "
      writeText op
      writeText " "
      writeSrcComp r
    ELambda le     -> writeSrcComp le
    EList vs       -> do
      writeText "[\n"
      indentBy 4 $ for_ vs $ \v' -> withNewLine $ do
        writeSrcComp v'
        writeText ","
      doIndent
      writeText "]"
    EDict kvs      -> do
      writeText "{\n"
      indentBy 4 $ for_ kvs $ \(k, v') -> withNewLine $ do
        writeSrcComp k
        writeText ": "
        writeSrcComp v'
        writeText ","
      doIndent
      writeText "}"

instance SrcComp LambdaExpr where
  writeSrcComp LambdaExpr{..} = do
    writeText "lambda"
    for_ leParams $ \name -> do
      writeText " "
      writeText name
    writeText ": "
    writeSrcComp leExpr

instance SrcComp ArgumentList where
  writeSrcComp ArgumentList{..} = forWithComma_ alPositional writeSrcComp


instance SrcComp Target where
  writeSrcComp v = case v of
    TSimple t       -> writeText t
    TAttr expr attr -> do
      writeSrcComp expr
      writeText "."
      writeText attr
    TTwo t1 t2      -> do
      writeText t1
      writeText ", "
      writeText t2

instance SrcComp AssignmentStmt where
  writeSrcComp AssignmentStmt{..} = withNewLine $ do
    writeSrcComp asTarget
    writeText " = "
    writeSrcComp asValue

instance SrcComp AnnotatedAssignmentStmt where
  writeSrcComp AnnotatedAssignmentStmt{..} = withNewLine $ do
    writeSrcComp aasTarget
    writeText ": "
    writeSrcComp aasAnnotation

instance SrcComp ReturnStmt where
  writeSrcComp ReturnStmt{..} = withNewLine $ do
    writeText "return "
    forWithComma_ rsValues writeSrcComp

instance SrcComp RaiseStmt where
  writeSrcComp RaiseStmt{..} = withNewLine $ do
    writeText "raise "
    writeSrcComp rsExc

instance SrcComp ImportStmt where
  writeSrcComp v = withNewLine $ case v of
    ImportAs mod' as      -> do
      writeText "import "
      forWith_ "." mod' writeText
      writeText " as "
      writeText as
    ImportFrom mod' items -> do
      writeText "from "
      forWith_ "." mod' writeText
      writeText " import "
      forWithComma_ items writeText


instance SrcComp IfStmt where
  writeSrcComp IfStmt{..} = do
    withNewLine $ do
      writeText "if "
      writeSrcComp isCond
      writeText ":"
    writeSuite isFirst

instance SrcComp TryStmt where
  writeSrcComp TryStmt{..} = do
    withNewLine $ writeText "try:"
    writeSuite tsBody
    for_ tsExcepts $ \(excT, mExcB, excBody) -> do
      withNewLine $ do
        writeText "except "
        writeSrcComp excT
        for_ mExcB $ \excB -> do
          writeText " as "
          writeText excB
        writeText ":"
      writeSuite excBody
    for_ tsElse $ \suite -> do
      withNewLine $ writeText "else:"
      writeSuite suite
    for_ tsFinally $ \suite -> do
      withNewLine $ writeText "finally:"
      writeSuite suite

instance SrcComp FuncDef where
  writeSrcComp FuncDef{..} = do
    for_ fdDecorators $ \d -> withNewLine $ do
      writeText "@"
      writeSrcComp d
    withNewLine $ do
      writeText "def "
      writeText fdName
      writeText "("
      forWithComma_ fdParams $ \(name, mAnno) -> do
        writeText name
        for_ mAnno $ \anno -> do
          writeText ": "
          writeSrcComp anno
      writeText ")"
      for_ fdRType $ \rt -> do
        writeText " -> "
        writeSrcComp rt
      writeText ":"
    writeSuite fdSuite

instance SrcComp ClassDef where
  writeSrcComp ClassDef{..} = do
    for_ cdDecorators $ \d -> withNewLine $ do
      writeText "@"
      writeSrcComp d
    withNewLine $ do
      writeText "class "
      writeText cdName
      unless (isEmptyArgumentList cdArgs) $ do
        writeText "("
        writeSrcComp cdArgs
        writeText ")"
      writeText ":"
    writeSuite cdSuite


instance SrcComp FileInput where
  writeSrcComp FileInput{..} = do
    traverse_ writeSrcComp fiImports
    unless (null fiImports || null fiContent) $ do
      writeText "\n\n"
    forWith_ "\n\n" fiContent writeSrcComp

instance SrcComp Statement where
  writeSrcComp v = case v of
    SA c  -> writeSrcComp c
    SAA c -> writeSrcComp c
    SR c  -> writeSrcComp c
    SRA c -> writeSrcComp c
    SI c  -> writeSrcComp c
    ST c  -> writeSrcComp c
    SFD c -> writeSrcComp c
    SCD c -> writeSrcComp c
