{
{-# OPTIONS_GHC -Wmissing-fields #-}
{-# OPTIONS_HADDOCK hide         #-}
module Gugugu.Parser.Parser
  ( parseModuleDec
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Data.List
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
  "data"                                { TData }

  "="                                   { TEq }
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

%%

moduleDec :: { ModuleDec }
moduleDec : "module" conid "where"
              "{{"
                body
              vClose                    { ModuleDec
                                          { moduleDecName = $2
                                          , moduleDecBody = $5
                                          }
                                        }

body :: { [Dec] }
body      : {- empty -}                 { [] }
          | body dec                    { $1 <> [ $2 ] }

dec :: { Dec }
dec       : dataDec                     { DData $1 }
          | funcDec                     { DFunc $1 }


dataDec :: { DataDec }
dataDec   : "data" conid
              "=" dataCon ";;"          { DataDec
                                          { dataDecName = $2
                                          , dataDecDef  = $4
                                          }
                                        }

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


recordCon :: { RecordCon }
recordCon
          : conid
              "{" recordFields "}"      { RecordCon
                                          { recordConName   = $1
                                          , recordConFields = $3
                                          }
                                        }

recordFields :: { [RecordField] }
recordFields
          : recordField                 { [ $1 ] }
          | recordFields
              "," recordField           { $1 <> [ $3 ] }

recordField :: { RecordField }
recordField
          : varid "::" typeExpr         { RecordField
                                          { recordFieldName = $1
                                          , recordFieldType = $3
                                          }
                                        }


typeExpr :: { TypeExpr }
typeExpr  : conid                       { TypeExpr
                                          { typeExprFirst  = $1
                                          , typeExprParams = []
                                          }
                                        }
          | typeExpr conid              { TypeExpr
                                          { typeExprFirst  = typeExprFirst $1
                                          , typeExprParams =
                                              typeExprParams $1 <>
                                                [ TypeExpr
                                                  { typeExprFirst  = $2
                                                  , typeExprParams = []
                                                  }
                                                ]
                                          }
                                        }
          | typeExpr "(" typeExpr ")"   { TypeExpr
                                            { typeExprFirst  = typeExprFirst $1
                                            , typeExprParams =
                                                typeExprParams $1 <> [ $3 ]
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

}
