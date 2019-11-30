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
  , ClassDecl(..)
  , InstDecl(..)
  , Decl(..)
  , TypeSig(..)
  , Def(..)
  , Type(..)
  , Exp(..)
  , Pat(..)
  , Constr(..)
  ) where

import           Control.Monad
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text          (Text)
import qualified Data.Text          as T

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
    { hmExts    :: [Text]
    , hmId      :: QualId
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
  | TdClass ClassDecl
  | TdInst InstDecl
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
            class [scontext =>] tycls tyvar [where cdecls]
tycls     → conid                                           (type classes)
cdecls    → { cdecl1 ; … ; cdecln }                         (n ≥ 0)
cdecl     → gendecl
          | (funlhs | var) rhs
@
 -}
data ClassDecl
  = ClassDecl
    { cdName  :: Id
    , cdTVars :: [Id]
    , cdDecls :: [Decl]
    }
  deriving Show

{-|
@
            instance [scontext =>] qtycls inst [where idecls]
inst      → gtycon
          | ( gtycon tyvar1 … tyvark )            (k ≥ 0, tyvars distinct)
          | ( tyvar1 , … , tyvark )               (k ≥ 2, tyvars distinct)
          | [ tyvar ]
          | ( tyvar1 -> tyvar2 )                  tyvar1 and tyvar2 distinct
idecls    → { idecl1 ; … ; idecln }                         (n ≥ 0)
idecl     → (funlhs | var) rhs
          |                                                 (empty)
@
 -}
data InstDecl
  = InstDecl
    { idClass :: QualId
    , idTypes :: [Type]
    , idDecls :: [Decl]
    }
  deriving Show

{-|
@
decl      → gendecl
          | (funlhs | pat) rhs
@
 -}
data Decl
  = DTS TypeSig
  | DDef Def
  deriving Show

{-|
@
            vars :: [context =>] type                       (type signature)
@
-}
data TypeSig
  = TypeSig
    { tsVar  :: Id
    , tsType :: Type
    }
  deriving Show

{-|
@
            (funlhs | pat) rhs
@
-}
data Def
  = Def
    { dLhs    :: Id
    , dParams :: [Id]
    , dRhs    :: Exp
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
  | TArrow Type Type
  deriving Show

infixr 9 `TArrow`


{-|
@
exp       → infixexp :: [context =>] type         (expression type signature)
          | infixexp
infixexp  → lexp qop infixexp                     (infix operator application)
          | - infixexp                                      (prefix negation)
          | lexp
lexp      → \ apat1 … apatn -> exp                (lambda abstraction, n ≥ 1)
          | let decls in exp                                (let expression)
          | if exp [;] then exp [;] else exp                (conditional)
          | case exp of { alts }                            (case expression)
          | do { stmts }                                    (do expression)
          | fexp
fexp      → [fexp] aexp                           (function application)
aexp      → qvar                                            (variable)
          | gcon                                  (general constructor)
          | literal
          | ( exp )                               (parenthesized expression)
          | ( exp1 , … , expk )                             (tuple, k ≥ 2)
          | [ exp1 , … , expk ]                             (list, k ≥ 1)
          | [ exp1 [, exp2] .. [exp3] ]           (arithmetic sequence)
          | [ exp | qual1 , … , qualn ]           (list comprehension, n ≥ 1)
          | ( infixexp qop )                                (left section)
          | ( qop⟨-⟩ infixexp )                             (right section)
          | qcon { fbind1 , … , fbindn }          (labeled construction, n ≥ 0)
          | aexp⟨qcon⟩ { fbind1 , … , fbindn }    (labeled update, n  ≥  1)
qual      →  pat <- exp                                     (generator)
          | let decls                                       (local declaration)
          | exp                                             (guard)
alts      → alt1 ; … ; altn                                 (n ≥ 1)
alt       → pat -> exp [where decls]
          | pat gdpat [where decls]
          |                                                 (empty alternative)
@
 -}
data Exp
  = ESimple Text
  | EQual QualId
  | EApp Exp Exp
  | ELet [Decl] Exp
  | ECase Exp [(Pat, Exp)]
  | EBinary Exp Text Exp
  deriving Show

{-|
@
pat       → lpat qconop pat                                 (infix constructor)
          | lpat
lpat      → apat
          | - (integer | float)                             (negative literal)
          | gcon apat1 … apatk                    (arity gcon  =  k, k ≥ 1)
apat      → var [ @ apat]                                   (as pattern)
          | gcon                                            (arity gcon  =  0)
          | qcon { fpat1 , … , fpatk }            (labeled pattern, k ≥ 0)
          | literal
          | _                                               (wildcard)
          | ( pat )                               (parenthesized pattern)
          | ( pat1 , … , patk )                   (tuple pattern, k ≥ 2)
          | [ pat1 , … , patk ]                   (list pattern, k ≥ 1)
          | ~ apat                                (irrefutable pattern)
@
 -}
data Pat
  = PSimple Text
  | PCon QualId [Id]
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
    let maxLen = maximum $ fmap T.length hmExts
    for_ hmExts $ \ext -> withNewLine $ do
      writeText "{-# LANGUAGE "
      writeText $ T.justifyLeft maxLen ' ' ext
      writeText " #-}"
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
    TdType d  -> writeSrcComp d
    TdData d  -> writeSrcComp d
    TdClass d -> writeSrcComp d
    TdInst d  -> writeSrcComp d


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

instance SrcComp ClassDecl where
  writeSrcComp ClassDecl{..} = do
    withNewLine $ do
      writeText "class "
      writeText cdName
      for_ cdTVars $ \t -> do
        writeText " "
        writeText t
      writeText " where"
    indentBy 2 $ for_ cdDecls (withNewLine . writeSrcComp)

instance SrcComp InstDecl where
  writeSrcComp InstDecl{..} = do
    withNewLine $ do
      writeText "instance "
      writeQualId idClass
      for_ idTypes $ \t -> do
        writeText " "
        writeSrcComp t
      writeText " where"
    indentBy 2 $ for_ idDecls (withNewLine . writeSrcComp)

instance SrcComp Decl where
  writeSrcComp v = case v of
    DTS  d -> writeSrcComp d
    DDef d -> writeSrcComp d

instance SrcComp TypeSig where
  writeSrcComp TypeSig{..} = do
    writeText tsVar
    writeText " :: "
    writeSrcComp tsType

instance SrcComp Def where
  writeSrcComp Def{..} = do
    writeText dLhs
    for_ dParams $ \p -> do
      writeText " "
      writeText p
    writeText " = "
    writeSrcComp dRhs

instance SrcComp Type where
  writeSrcComp v = case v of
    TSimple n    -> writeText n
    TQCon qn     -> writeQualId qn
    TApp c p     -> do
      writeSrcComp c
      writeText " "
      writeSrcComp p
    TParen t     -> do
      writeText "("
      writeSrcComp t
      writeText ")"
    TArrow t1 t2 -> do
      writeSrcComp t1
      writeText " -> "
      writeSrcComp t2

instance SrcComp Exp where
  writeSrcComp v = case v of
    ESimple t      -> writeText t
    EQual qn       -> writeQualId qn
    EApp e1 e2     -> do
      writeSrcComp e1
      writeText " "
      writeSrcComp e2
    ELet ds e      -> do
      writeText "let\n"
      indentBy 2 $ do
        indentBy 2 $ do
          for_ ds (withNewLine . writeSrcComp)
        doIndent
        writeText "in "
        writeSrcComp e
    ECase e alts   -> do
      writeText "case "
      writeSrcComp e
      writeText " of"
      indentBy 2 $ for_ alts $ \(p, e') -> do
        writeText "\n"
        doIndent
        writeSrcComp p
        writeText " -> "
        writeSrcComp e'
    EBinary l op r -> do
      writeSrcComp l
      writeText " "
      writeText op
      writeText " "
      writeSrcComp r

instance SrcComp Pat where
  writeSrcComp v = case v of
    PSimple t -> writeText t
    PCon c vs -> do
      writeQualId c
      for_ vs $ \v' -> do
        writeText " "
        writeText v'

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
