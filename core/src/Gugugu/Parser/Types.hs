{-|
Parser types
 -}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
module Gugugu.Parser.Types
  (
  -- * Parsing Environment
    P
  , unP
  , PState(..)
  , makePState
  , LayoutContext(..)
  , PInput(..)
  , SourceLocation(..)
  , formatSourceLocation

  -- * Tokens
  , Token(..)

  -- * AST Nodes
  , ModuleDec(..)
  , Dec(..)
  , DataDec(..)
  , DataCon(..)
  , RecordCon(..)
  , RecordField(..)
  , TypeExpr(..)

  -- * Utilities required by the parser
  , popLayoutContext
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Functor.Identity
import           Data.List.NonEmpty    (NonEmpty (..))
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Vector.Unboxed   (Vector)
import qualified Data.Vector.Unboxed   as V
import           Data.Word
import           Text.Printf


-- | The parsing monad
type P = ExceptT String (StateT PState Identity)

-- | Run the parser
unP :: P a -> PState -> (Either String a, PState)
unP m = runIdentity . runStateT (runExceptT m)

-- | The parsing state
data PState
  = PState
    { psInput          :: PInput            -- ^ Current input
    , psLayoutContext  :: [LayoutContext]   -- ^ 'LayoutContext' stack
    , psAlexStartCodes :: NonEmpty Int      -- ^ Alex start code stack
    }
  deriving Show

-- | Make the 'PState' from file name and file contents
makePState :: FilePath -> Text -> PState
makePState path input = PState
  { psInput          = PInput
      { pSrc      = chs
      , pLen      = V.length chs
      , pLoc      = SourceLocation
          { slSrcPath = path
          , slOffset  = 0
          , slLine    = 1
          , slColumn  = 1
          }
      , pPending  = []
      , pPrevChar = '\n'
      }
  , psLayoutContext  = []
  , psAlexStartCodes = 0 :| []
  }
  where chs = V.unfoldr T.uncons input

-- | In layout or not
data LayoutContext
  = NoLayout      -- ^ Not in a layout
  | Layout Int    -- ^ In a layout and the indentation level
  deriving Show

-- | The lexer input
data PInput
  = PInput
    { pSrc      :: Vector Char      -- ^ Full sources
    , pLen      :: Int              -- ^ Length of the sources in characters
    , pLoc      :: SourceLocation   -- ^ Current location
    , pPending  :: [Word8]          -- ^ Pending bytes
    , pPrevChar :: Char             -- ^ Previous character
    }
  deriving Show

-- | Source location
data SourceLocation
  = SourceLocation
    { slOffset  :: Int        -- ^ Offset in characters
    , slLine    :: Int        -- ^ Line number, from 1
    , slColumn  :: Int        -- ^ Column number, from 1
    , slSrcPath :: FilePath   -- ^ Source file name
    }
  deriving Show

-- | Format to string like @path\/to\/source:line:column@
formatSourceLocation :: SourceLocation -> String
formatSourceLocation SourceLocation{..} =
  printf "%s:%d:%d" slSrcPath slLine slColumn


-- Tokens

-- | The token type
data Token
  = TEOF                -- ^ EOF
  | TModule             -- ^ Keyword @module@
  | TWhere              -- ^ Keyword @where@
  | TData               -- ^ Keyword @data@

  | TEq                 -- ^ Symbol, equal sign @=@
  | TComma              -- ^ Symbol, comma @,@
  | TDColon             -- ^ Symbol, double colon @::@

  | TConId Text         -- ^ Constructor identifier
  | TVarId Text         -- ^ Variable identifier

  | TLBrace             -- ^ Symbol, left brace @{@
  | TRBrace             -- ^ Symbol, right brace @}@
  | TLParen             -- ^ Symbol, left parenthesis @(@
  | TRParen             -- ^ Symbol, right parenthesis @)@

  | TvSemi              -- ^ Virtual token, denoted by double semicolon @;;@
  | TvLBrace            -- ^ Virtual token, denoted by double left brace @{{@
  | TvRBrace            -- ^ Virtual token, denoted by double right brace @}}@
  deriving Show


-- AST Nodes

-- | Module declaration
--
-- @
-- module 'moduleDecName' where
-- {{ 'moduleDecBody'
-- }}
-- @
data ModuleDec
  = ModuleDec
    { moduleDecName :: Text
    , moduleDecBody :: [Dec]
    }
  deriving Show

-- | Declaration
data Dec
  = DData DataDec
  deriving Show


-- | Data declaration
--
-- @
-- data 'dataDecName' = 'dataDecDef'
-- @
data DataDec
  = DataDec
    { dataDecName :: Text
    , dataDecDef  :: DataCon
    }
  deriving Show

-- | Data constructor
data DataCon
  = DRecord RecordCon
  deriving Show


-- | Record data constructor
--
-- @
-- 'recordConName'
-- { 'recordConFields'
-- }
-- @
data RecordCon
  = RecordCon
    { recordConName   :: Text
    , recordConFields :: [RecordField]
    }
  deriving Show

-- | Record field
--
-- @
-- 'recordFieldName' :: 'recordFieldType'
-- @
data RecordField
  = RecordField
    { recordFieldName :: Text
    , recordFieldType :: TypeExpr
    }
  deriving Show


-- | Type expression
--
-- @
-- 'typeExprFirst' 'typeExprParams'
-- @
data TypeExpr
  = TypeExpr
    { typeExprFirst  :: Text
    , typeExprParams :: [TypeExpr]
    }
  deriving Show


-- Utilities required by parser

-- | Pop the current layout context
popLayoutContext :: P ()
popLayoutContext = do
  s@PState{ psLayoutContext } <- get
  case psLayoutContext of
    []      -> throwError "layout expected but no layout available"
    _ : lcs -> put s{ psLayoutContext = lcs }
