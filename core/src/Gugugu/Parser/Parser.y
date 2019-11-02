{
{-# OPTIONS_GHC -Wmissing-fields #-}
{-# OPTIONS_HADDOCK hide         #-}
module Gugugu.Parser.Parser
  ( parseModuleDec
  ) where

import           Control.Lens.Setter
import           Control.Monad
import           Control.Monad.Except
import           Data.List
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)

import           Gugugu.Parser.Lexer
import           Gugugu.Parser.Types

}

%monad { P } { (>>=) } { pure }
%lexer { lexer } { TEOF }

%name parseModuleDec moduleDec
%tokentype { Token }
%error { parseError }

%token
  "module"                              { TModule }
  "where"                               { TWhere }
  "import"                              { TImport }
  "data"                                { TData }

  "="                                   { TEq }
  "|"                                   { TVBar }
  ","                                   { TComma }
  "::"                                  { TDColon }
  "->"                                  { TRArrow }

  conid                                 { TConId $$ }
  varid                                 { TVarId $$ }

  "("                                   { TLParen }
  ")"                                   { TRParen }

  "{"                                   { TLBrace }
  "}"                                   { TRBrace }

  "{{"                                  { TvLBrace }
  "}}"                                  { TvRBrace }
  ";;"                                  { TvSemi }

  "{-# FOREIGN"                         { TPForeign }
  "#-}"                                 { TPClose }
  pComp                                 { TPComp $$ }

%%

moduleDec :: { ModuleDec }
moduleDec : "module" conid "where"
              "{{"
                imports
                body
              vClose                    { ModuleDec
                                          { moduleDecName    = $2
                                          , moduleDecImports = $5
                                          , moduleDecBody    = $6
                                          }
                                        }

imports :: { [ImportStmt] }
imports   : {- empty -}                 { [] }
          | imports importStmt          { $1 `snocList` $2 }

importStmt :: { ImportStmt }
importStmt: "import" conid ";;"         { ImportStmt
                                          { importStmtModuleName = $2
                                          }
                                        }

body :: { [Dec] }
body      : {- empty -}                 { [] }
          | body dec                    { $1 `snocList` $2 }

dec :: { Dec }
dec       : dataDec                     { DData $1 }
          | funcDec                     { DFunc $1 }


dataDec :: { DataDec }
dataDec   : "data" conid
              dataPragmas
              maybeDataCon ";;"         { DataDec
                                          { dataDecName    = $2
                                          , dataDecPragmas = $3
                                          , dataDecDef     = $4
                                          }
                                        }

maybeDataCon :: { Maybe DataCon }
maybeDataCon
          : {- empty -}                 { Nothing }
          | "=" dataCon                 { Just $2 }

funcDec :: { FuncDec }
funcDec   : varid
              "::" typeExpr
              "->" typeExpr
              ";;"                      { FuncDec
                                          { funcDecName     = $1
                                          , funcDecDomain   = $3
                                          , funcDecCodomain = $5
                                          }
                                        }

dataCon :: { DataCon }
dataCon   : recordCon                   { DRecord $1 }
          | enumCons                    { DEnum $1 }


enumCons :: { NonEmpty Text }
enumCons  : conid                       { $1 :| [] }
          | enumCons "|" conid          { $1 <> ( $3 :| [] )  }

recordCon :: { RecordCon }
recordCon : conid
              "{" recordFields "}"      { RecordCon
                                          { recordConName   = $1
                                          , recordConFields = $3
                                          }
                                        }

recordFields :: { [RecordField] }
recordFields
          : recordField                 { [ $1 ] }
          | recordFields
              "," recordField           { $1 `snocList` $3 }

recordField :: { RecordField }
recordField
          : varid "::" typeExpr         { RecordField
                                          { recordFieldName = $1
                                          , recordFieldType = $3
                                          }
                                        }


typeExpr :: { TypeExpr }
typeExpr  : conid                       { simpleType $1 }
          | typeExpr conid              { typeExprParams
                                            %~ (`snocList` simpleType $2 )
                                            $ $1
                                        }
          | typeExpr "(" typeExpr ")"   { typeExprParams
                                            %~ (`snocList` $3 )
                                            $ $1
                                        }


dataPragmas :: { [PragmaToData] }
dataPragmas
          : {- empty -}                 { [] }
          | dataPragmas dataPragma      { $1 `snocList` $2 }

dataPragma :: { PragmaToData }
dataPragma: foreignPragma               { PDForeign $1 }

foreignPragma :: { ForeignPragma }
foreignPragma
          : "{-# FOREIGN"
              pComp pComp
            "#-}"                       { ForeignPragma
                                            { foreignPragmaTarget  = $2
                                            , foreignPragmaContent = $3
                                            }
                                        }


vClose :: { () }
vClose    : "}}"                        { () }
          | error                       { %popLayoutContext }


{

lexer :: (Token -> P a) -> P a
lexer k = lexToken >>= k

parseError :: Token -> P a
parseError t = throwError $ "Parse Error: " <> show t


simpleType :: Text -> TypeExpr
simpleType name = TypeExpr
  { typeExprFirst   = name
  , _typeExprParams = []
  }

snocList :: [a] -> a -> [a]
snocList xs x = xs <> [x]

}
