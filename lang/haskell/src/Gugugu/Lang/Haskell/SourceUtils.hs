{-|
Utilities to generate Haskell sources.

Note, it is a simplified version and not intended for general purpose use.

Vide:
https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-18000010.5
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
module Gugugu.Lang.Haskell.SourceUtils
  ( Id
  , QualId
  , HaskellModule(..)
  , ImportDecl(..)
  , TopDecl(..)
  , TypeDecl(..)
  , DataDecl(..)
  , Type(..)
  , Constr(..)
  ) where

import           Control.Monad
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text          (Text)

import           Gugugu.Utilities

-- | Haskell identifier, we do not distinguish varid and conid here.
type Id = Text

-- | Qualified Id
type QualId = NonEmpty Id


{-|
@
module    → module modid [exports] where body
          | body
body      → { impdecls ; topdecls }
          | { impdecls }
          | { topdecls }
impdecls  → impdecl1 ; … ; impdecln                         (n ≥ 1)
exports   → ( export1 , … , exportn [ , ] )                 (n ≥ 0)
export    → qvar
          | qtycon [(..) | ( cname1 , … , cnamen )]         (n ≥ 0)
          | qtycls [(..) | ( qvar1 , … , qvarn )]           (n ≥ 0)
          | module modid
topdecls  → topdecl1 ; … ; topdecln                         (n ≥ 0)
@
 -}
data HaskellModule
  = HaskellModule
    { hmId      :: QualId
    , hmImports :: [ImportDecl]
    , hmDecls   :: [TopDecl]
    }
  deriving Show

{-|
@
impdecl   → import [qualified] modid [as modid] [impspec]
          |                                                 (empty declaration)
impspec   → ( import1 , … , importn [ , ] )                 (n ≥ 0)
          | hiding ( import1 , … , importn [ , ] )          (n ≥ 0)
import    → var
          | tycon [ (..) | ( cname1 , … , cnamen )]         (n ≥ 0)
          | tycls [(..) | ( var1 , … , varn )]              (n ≥ 0)
cname     → var | con
@
 -}
newtype ImportDecl
  = ImportDecl
    { idModuleId  :: QualId
    }
  deriving (Eq, Ord, Show)

{-|
@
topdecl   → type simpletype = type
          | data [context =>] simpletype [= constrs] [deriving]
          | newtype [context =>] simpletype = newconstr [deriving]
          | class [scontext =>] tycls tyvar [where cdecls]
          | instance [scontext =>] qtycls inst [where idecls]
          | default (type1 , … , typen)                     (n ≥ 0)
          | foreign fdecl
          | decl
@
 -}
data TopDecl
  = TdType TypeDecl
  | TdData DataDecl
  deriving Show


{-|
@
type simpletype = type
@
 -}
data TypeDecl
  = TypeDecl
    { tdName :: Id
    , tdType :: Type
    }
  deriving Show

{-|
@
            data [context =>] simpletype [= constrs] [deriving]
constrs   → constr1 | … | constrn                           (n ≥ 1)
deriving  → deriving (dclass | (dclass1, … , dclassn))      (n ≥ 0)
dclass    → qtycls
@
 -}
data DataDecl
  = DataDecl
    { ddName      :: Id
    , ddCons      :: [Constr]
    , ddDerivings :: [QualId]
    }
  deriving Show


{-|
@
type      → btype [-> type]                                 (function type)
btype     → [btype] atype                                   (type application)
atype     → gtycon
          | tyvar
          | ( type1 , … , typek )                           (tuple type, k ≥ 2)
          | [ type ]                                        (list type)
          | ( type )                              (parenthesized constructor)
gtycon    → qtycon
          | ()                                              (unit type)
          | []                                              (list constructor)
          | (->)                                  (function constructor)
          | (,{,})                                (tupling constructors)
@
 -}
data Type
  = TSimple Id
  | TQCon (QualId)
  | TApp Type Type
  | TParen Type
  deriving Show

{-|
@
constr    → con [!] atype1 … [!] atypek           (arity con = k, k ≥ 0)
          | (btype | ! atype) conop (btype | ! atype)       (infix conop)
          | con { fielddecl1 , … , fielddecln }             (n ≥ 0)
fielddecl → vars :: (type | ! atype)
@
 -}
data Constr
  = Constr Id
  | CRecord Id [(Id, Type)]
  deriving Show


-- Utilities

writeQualId :: Monad m => QualId -> SrcCompT m ()
writeQualId = flip (forWith_ ".") writeText


-- Instances

instance SrcComp HaskellModule where
  writeSrcComp HaskellModule{..} = do
    withNewLine $ do
      writeText "module "
      writeQualId hmId
      writeText " where"
    unless (null hmImports) $ do
      writeText "\n"
      traverse_ writeSrcComp hmImports
    unless (null hmDecls) $ do
      writeText "\n"
      for_ hmDecls $ \d -> do
        writeText "\n"
        writeSrcComp d

instance SrcComp ImportDecl where
  writeSrcComp ImportDecl{..} = withNewLine $ do
    writeText "import qualified "
    writeQualId idModuleId

instance SrcComp TopDecl where
  writeSrcComp v = case v of
    TdType d -> writeSrcComp d
    TdData d -> writeSrcComp d


instance SrcComp TypeDecl where
  writeSrcComp TypeDecl{..} = withNewLine $ do
    writeText "type "
    writeText tdName
    writeText " = "
    writeSrcComp tdType

instance SrcComp DataDecl where
  writeSrcComp DataDecl{..} = do
    doIndent
    writeText "data "
    writeText ddName
    let writeDerivings  = do
          writeText "deriving "
          case ddDerivings of
            d : [] -> writeQualId d
            _      -> do
              writeText "("
              forWithComma_ ddDerivings writeQualId
    case ddCons of
      []     -> do
        writeText " "
        unless (null ddDerivings) writeDerivings
        writeText "\n"
      c : cs -> do
        writeText "\n"
        indentBy 2 $ do
          withNewLine $ do
            writeText "= "
            writeSrcComp c
          for_ cs $ \c' -> withNewLine $ do
            writeText "| "
            writeSrcComp c'
          unless (null ddDerivings) $ do
            doIndent
            writeDerivings
            writeText "\n"


instance SrcComp Type where
  writeSrcComp v = case v of
    TSimple n -> writeText n
    TQCon qn  -> writeQualId qn
    TApp c p  -> do
      writeSrcComp c
      writeText " "
      writeSrcComp p
    TParen t  -> do
      writeText "("
      writeSrcComp t
      writeText ")"


instance SrcComp Constr where
  writeSrcComp v = case v of
    Constr n         -> writeText n
    CRecord n fields -> do
      writeText n
      let writeField (fn, t) = do
            writeText fn
            writeText " :: "
            writeSrcComp t
      case fields of
        [] -> writeText " { }"
        f : fs -> do
          writeText "\n"
          indentBy 2 $ do
            withNewLine $ do
              writeText "{ "
              writeField f
            for_ fs $ \f' -> withNewLine $ do
              writeText ", "
              writeField f'
            doIndent
            writeText "}"
