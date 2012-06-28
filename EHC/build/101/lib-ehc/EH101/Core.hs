

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Core.ag)
module EH101.Core(module EH101.AbstractCore
, module EH101.AnaDomain
, module EH101.Base.Target
, CodeAGItf (..), CModule (..), CExpr (..), MbCExpr, CBind (..), CBound (..), CMetaVal (..), CMetaBind (..), CMetas, CBindL, CBoundL, CPatRest (..), CAlt (..), CAltL, CPat (..), CPatFld (..), CPatFldL
, CBindAnn (..), CBindAnnL, CExprAnn (..)
, RAlt, RPat, RPatConBind, RPatFld
, cmetasDefault
, cmetasVal
, cmetasMapVal
, CBindCateg (..)
, EvalCtx (..), isStrict
, cexprIsLam
, cbindNm
, mkCMod, emptyCModule
, cexprMbVar, cexprVar
, cexprTupFld
, cexprIsEvaluated
, CVarIntro (..), emptyCVarIntro
, CVarIntroMp, CVarIntroL, cviLookup
, cLevModule, cLevExtern
, CVarRepl (..)
, CVarReplMp
, CVarReplNm, emptyCVarReplNm
, CVarReplNmMp, CVarReplNmL
, cvrFromCvi
, fvLev, fvsLev
, cbindLNub
, cTupLbl
, cTupTag
, cTupOff
, cbindAspectMbExpr, cbindExprs
, cLevIntern
, HsName2OffsetMp, HsName2OffsetMpMp
, offMpKeysSorted, offMpMpKeysSet
, cModMerge
, CDbBindLetInfo, CDbBindLetInfo'2
, CDbBindArray, CDbBindRef, CDbModuleBindMp
, CModuleDatabase (..), emptyCModuleDatabase
, cmoddbLookup
, module EH101.Foreign) where

import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Opts.Base
import EH101.AbstractCore
import EH101.AnaDomain
import EH101.Base.Target (FFIWay (..),TargetFlavor (..))
import Data.Maybe
import Data.Char
import Data.List
import EH.Util.Pretty
import EH.Util.Utils
import qualified Data.Map as Map
import qualified Data.Set as Set
import EH101.Ty
import EH101.Base.Debug
import Data.Array
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize
import Data.Typeable (Typeable)
import Data.Generics (Data)
import EH101.Foreign






























type RAlt 			= RAlt' 		CExpr Ty CBind CPatRest
type RPat 			= RPat' 		CExpr Ty CBind CPatRest
type RPatConBind 	= RPatConBind' 	CExpr Ty CBind CPatRest
type RPatFld 		= RPatFld' 		CExpr Ty CBind CPatRest



cTupLbl :: CExpr -> HsName
cTupLbl e
  =  case e of
         CExpr_TupIns _ _ l _ _ -> l
         CExpr_TupUpd _ _ l _ _ -> l
         CExpr_TupDel _ _ l _   -> l



cTupTag :: CExpr -> CTag
cTupTag e
  =  case e of
         CExpr_TupIns _ t _ _ _ -> t
         CExpr_TupUpd _ t _ _ _ -> t
         CExpr_TupDel _ t _ _   -> t



cTupOff :: CExpr -> CExpr
cTupOff e
  =  case e of
         CExpr_TupIns _ _ _ o _ -> o
         CExpr_TupUpd _ _ _ o _ -> o
         CExpr_TupDel _ _ _ o   -> o



cmetasDefault :: CMetas
cmetasDefault = (CMetaBind_Plain,CMetaVal_Val)



cmetasVal :: CMetas -> CMetaVal
cmetasVal (_,v) = v



cmetasMapVal :: (CMetaVal -> CMetaVal) -> CMetas -> CMetas
cmetasMapVal f (b,v) = (b,f v)



data CBindCateg
  = CBindCateg_Rec				-- mutually recursive
  | CBindCateg_Strict			-- strictly evaluated
  | CBindCateg_Plain				-- plain
  | CBindCateg_FFI				-- imported function
  | CBindCateg_FFE				-- exported function (not implemented yet)
  deriving (Show,Eq,Enum)



deriving instance Typeable CBindCateg
deriving instance Data CBindCateg



data EvalCtx
  = EvalCtx_None         -- nothing known, no strictness required
  | EvalCtx_Eval         -- strictness (thus eval) required
  | EvalCtx_EvalUnbox    -- strictness (thus eval) + unboxing required
  deriving Eq

isStrict :: EvalCtx -> Bool
isStrict EvalCtx_Eval        = True
isStrict EvalCtx_EvalUnbox   = True
isStrict _                   = False



cexprIsLam :: CExpr -> Bool
cexprIsLam (CExpr_Lam _ _) = True
cexprIsLam _               = False



cbindNm :: CBind -> HsName
cbindNm (CBind_Bind      n _) = n
-- cbindNm (CBind_FFI _ _ _ n _  ) = n



-- | extract expr for aspect, relevant for later use/analysis/...
cbindAspectMbExpr :: CBound -> Maybe CExpr
cbindAspectMbExpr (CBound_Bind _ e) = Just e
cbindAspectMbExpr (CBound_Val  _ e) = Just e
cbindAspectMbExpr _                 = Nothing

-- | extract exprs of a binding which are relevant for use/analysis/...
cbindExprs :: CBind -> [CExpr]
cbindExprs (CBind_Bind _ a) = catMaybes $ map cbindAspectMbExpr a



cbindLNub :: CBindL -> CBindL
cbindLNub = nubBy (\b1 b2 -> cbindNm b1 == cbindNm b2)



mkCMod :: CExpr -> CModule
mkCMod e = CModule_Mod (hsnFromString "") e []

emptyCModule :: CModule
emptyCModule = mkCMod (acoreInt 0)



cexprMbVar :: CExpr -> Maybe HsName
cexprMbVar (CExpr_Var r) = Just (acbrefNm r)
cexprMbVar _             = Nothing

cexprVar :: CExpr -> HsName
cexprVar = maybe hsnUnknown id . cexprMbVar



cexprTupFld :: CExpr -> CExpr
cexprTupFld (CExpr_TupIns _ _ _ _ e) = e
cexprTupFld _                        = acoreVar hsnUnknown



cexprIsEvaluated :: CExpr -> Bool
cexprIsEvaluated (CExpr_Int  _) = True
cexprIsEvaluated (CExpr_Char _) = True
cexprIsEvaluated _              = False



data CVarIntro
  = CVarIntro
      { cviLev		:: Int		-- lexical level
      , cviMeta		:: CMetaVal	-- meta info
      }

emptyCVarIntro :: CVarIntro
emptyCVarIntro
  = CVarIntro cLevExtern CMetaVal_Val



type CVarIntroMp = Map.Map HsName CVarIntro
type CVarIntroL  = AssocL  HsName CVarIntro

cviLookup :: HsName -> CVarIntroMp -> CVarIntro
cviLookup n m = Map.findWithDefault emptyCVarIntro n m



cLevModule, cLevExtern :: Int
cLevModule = 0
cLevExtern = 0



cLevIntern :: Int
cLevIntern = 1



data CVarRepl r
  = CVarRepl
      { cvrRepl		:: r		-- replacement
      , cvrMeta		:: CMetaVal	-- meta info
      }



type CVarReplMp  r = Map.Map HsName (CVarRepl r)
type CVarReplAsc r = AssocL  HsName (CVarRepl r)



type CVarReplNm = CVarRepl HsName

emptyCVarReplNm :: CVarReplNm
emptyCVarReplNm = CVarRepl hsnUnknown CMetaVal_Val



type CVarReplNmMp = CVarReplMp  HsName
type CVarReplNmL  = CVarReplAsc HsName



cvrFromCvi :: CVarIntro -> CVarReplNm
cvrFromCvi i
  = emptyCVarReplNm
      { cvrMeta 	= cviMeta i
      }



fvLev :: HsName -> CVarIntroMp -> Int
fvLev n m = cviLev $ cviLookup n m

fvsLev :: CVarIntroMp -> Int -> FvS -> Int
fvsLev lm lDflt fvs = foldr (\n l -> fvLev n lm `max` l) lDflt $ Set.toList $ fvs



type HsNameOffset      = Int
type HsName2OffsetMp   = Map.Map HsName HsNameOffset
type HsName2OffsetMpMp = Map.Map HsName (Int,HsName2OffsetMp)



-- | Module names, sorted on import order, which is included as 0-based offset (used as index in import entry table)
offMpKeysSorted :: HsName2OffsetMpMp -> AssocL HsName Int
offMpKeysSorted m = sortOn snd [ (n,o) | (n,(o,_)) <- Map.toList m ]

offMpMpKeysSet :: HsName2OffsetMpMp -> HsNameS
offMpMpKeysSet m = Set.unions [ Map.keysSet m' | (_,m') <- Map.elems m ]



-- | merge by concatenation
cModMerge :: [CModule] -> CModule
cModMerge mL
  = foldr1 cmb mL
  where get (CExpr_Let c b e) = CExpr_Let c b . get e
        get  _                = id
        cmb (CModule_Mod m1 e1 t1) (CModule_Mod m2 e2 t2)
          = CModule_Mod m2 (get e1 e2) (t1++t2)



-- | the binding info required for let bind
type CDbBindLetInfo'' f cat bind = (cat,f bind)
type CDbBindLetInfo'  f          = CDbBindLetInfo'' f  CBindCateg CBind
type CDbBindLetInfo'2   cat bind = CDbBindLetInfo'' [] cat            bind
type CDbBindLetInfo              = CDbBindLetInfo'  []



-- | actual bindings stored in separate array to allow for sharing
type CDbBindArray = Array Int (CDbBindLetInfo' (Array Int))

-- | reference into database of bindings, agnostic of name given to it
type CDbBindRef = (Int,Int)

-- | binding map of global names to individual bindings
type CDbModuleBindMp = Map.Map HsName CDbBindRef



-- | the full module represented in a map/database like format (20101004 AD: to be made into persistent db soon)
data CModuleDatabase
  = CModuleDatabase
      { cmoddbModNm			:: !HsName				-- module name
      , cmoddbBindArr		:: !CDbBindArray		-- bindings
      , cmoddbBindMp		:: !CDbModuleBindMp		-- map of name to bindings
      , cmoddbMainExpr		:: !CExpr				-- the final expr of the module's let expr
      , cmoddbTagsMp		:: !CTagsMp				-- datatype info
      }

emptyCModuleDatabase :: CModuleDatabase
emptyCModuleDatabase = CModuleDatabase hsnUnknown (array (1,0) []) Map.empty (acoreInt 0) emptyCTagsMp



cmoddbLookup :: HsName -> CModuleDatabase -> Maybe CDbBindRef
cmoddbLookup n db = Map.lookup n $ cmoddbBindMp db



instance Serialize CModule where
  sput (CModule_Mod   a b c   ) = sputWord8 0 >> sput a >> sput b >> sput c
  sget
    = do t <- sgetWord8
         case t of
            0 -> liftM3 CModule_Mod         sget sget sget

instance Serialize CExpr where
  sput (CExpr_Let       a b c       ) = sputWord8  0 >> sput a >> sput b >> sput c
  sput (CExpr_App       a b         ) = sputWord8  1 >> sput a >> sput b
  sput (CExpr_Lam       a b         ) = sputWord8  2 >> sput a >> sput b
  sput (CExpr_Case      a b c       ) = sputWord8  3 >> sput a >> sput b >> sput c
  sput (CExpr_Var       a           ) = sputWord8  4 >> sput a
  sput (CExpr_Int       a           ) = sputWord8  5 >> sput a
  sput (CExpr_Char      a           ) = sputWord8  6 >> sput a
  sput (CExpr_String    a           ) = sputWord8  7 >> sput a
  sput (CExpr_Tup       a           ) = sputWord8  8 >> sput a
  sput (CExpr_TupDel    a b c d     ) = sputWord8  9 >> sput a >> sput b >> sput c >> sput d
  sput (CExpr_TupIns    a b c d e   ) = sputWord8 10 >> sput a >> sput b >> sput c >> sput d >> sput e
  sput (CExpr_TupUpd    a b c d e   ) = sputWord8 11 >> sput a >> sput b >> sput c >> sput d >> sput e
  sput (CExpr_CaseAltFail
                        a b         ) = sputWord8 12 >> sput a >> sput b
  sput (CExpr_Hole      a           ) = sputWord8 13 >> sput a
  sput (CExpr_HoleLet   a b         ) = sputWord8 14 >> sput a >> sput b
  sput (CExpr_ImplsApp  a b         ) = sputWord8 15 >> sput a >> sput b
  sput (CExpr_ImplsLam  a b         ) = sputWord8 16 >> sput a >> sput b
  sput (CExpr_CoeArg                ) = sputWord8 17
  sput (CExpr_Integer   a           ) = sputWord8 18 >> sput a
  sput (CExpr_Ann       a b         ) = sputWord8 19 >> sput a >> sput b
  sput (CExpr_FFI       a b c d     ) = sputWord8 20 >> sput a >> sput b >> sput c >> sput d
  sget
    = do t <- sgetWord8
         case t of
           0  -> liftM3 CExpr_Let           sget sget sget
           1  -> liftM2 CExpr_App           sget sget
           2  -> liftM2 CExpr_Lam           sget sget
           3  -> liftM3 CExpr_Case          sget sget sget
           4  -> liftM  CExpr_Var           sget
           5  -> liftM  CExpr_Int           sget
           6  -> liftM  CExpr_Char          sget
           7  -> liftM  CExpr_String        sget
           8  -> liftM  CExpr_Tup           sget
           9  -> liftM4 CExpr_TupDel        sget sget sget sget
           10 -> liftM5 CExpr_TupIns        sget sget sget sget sget
           11 -> liftM5 CExpr_TupUpd        sget sget sget sget sget
           12 -> liftM2 CExpr_CaseAltFail   sget sget
           13 -> liftM  CExpr_Hole          sget
           14 -> liftM2 CExpr_HoleLet       sget sget
           15 -> liftM2 CExpr_ImplsApp      sget sget
           16 -> liftM2 CExpr_ImplsLam      sget sget
           17 -> return CExpr_CoeArg
           18 -> liftM  CExpr_Integer       sget
           19 -> liftM2 CExpr_Ann           sget sget
           20 -> liftM4 CExpr_FFI           sget sget sget sget

instance Serialize CMetaVal where
  sput (CMetaVal_Val                ) = sputWord8 0
  sput (CMetaVal_Dict               ) = sputWord8 1
  sput (CMetaVal_DictClass      a   ) = sputWord8 2 >> sput a
  sput (CMetaVal_DictInstance   a   ) = sputWord8 3 >> sput a
  sput (CMetaVal_Track          a   ) = sputWord8 4 >> sput a
  sget
    = do t <- sgetWord8
         case t of
            0 -> return CMetaVal_Val
            1 -> return CMetaVal_Dict
            2 -> liftM  CMetaVal_DictClass          sget
            3 -> liftM  CMetaVal_DictInstance       sget
            4 -> liftM  CMetaVal_Track              sget

instance Serialize CExprAnn where
  sput (CExprAnn_Ty    a) = sputWord8 0 >> sput a
  sput (CExprAnn_Coe   a) = sputWord8 1 >> sput a
  sput (CExprAnn_Debug _) = sputWord8 2
  sget
    = do t <- sgetWord8
         case t of
            0 -> liftM  CExprAnn_Ty         sget
            1 -> liftM  CExprAnn_Coe        sget
            2 -> return (CExprAnn_Debug     "")

instance Serialize CBindAnn where
  sput (CBindAnn_Coe   a) = sputWord8 0 >> sput a
  sget
    = do t <- sgetWord8
         case t of
            0 -> liftM  CBindAnn_Coe         sget

instance Serialize CBound where
  sput (CBound_Bind 	a b      ) = sputWord8 0 >> sput a >> sput b
  -- sput (CBound_FFI  	a b c d  ) = sputWord8 1 >> sput a >> sput b >> sput c >> sput d
  sput (CBound_FFE  	a b c d  ) = sputWord8 2 >> sput a >> sput b >> sput c >> sput d
  sput (CBound_RelevTy a b      ) = sputWord8 3 >> sput a >> sput b
  sput (CBound_Meta    a b      ) = sputWord8 4 >> sput a >> sput b
  sput (CBound_Val     a b      ) = sputWord8 5 >> sput a >> sput b
  sput (CBound_Ty      a b      ) = sputWord8 6 >> sput a >> sput b
  sget
    = do t <- sgetWord8
         case t of
            0 -> liftM2 CBound_Bind          sget sget
            -- 1 -> liftM4 CBound_FFI           sget sget sget sget
            2 -> liftM4 CBound_FFE           sget sget sget sget
            3 -> liftM2 CBound_RelevTy       sget sget
            4 -> liftM2 CBound_Meta          sget sget
            5 -> liftM2 CBound_Val           sget sget
            6 -> liftM2 CBound_Ty            sget sget

instance Serialize CBind where
  sput (CBind_Bind a b      ) = sputWord8 0 >> sput a >> sput b
  sget
    = do t <- sgetWord8
         case t of
            0 -> liftM2 CBind_Bind          sget sget

instance Serialize CAlt where
  sput (CAlt_Alt   a b      ) = sputWord8 0 >> sput a >> sput b
  sget
    = do t <- sgetWord8
         case t of
            0 -> liftM2 CAlt_Alt         sget sget

instance Serialize CPat where
  sput (CPat_Var       a        ) = sputWord8 0 >> sput a
  sput (CPat_Con       a b c    ) = sputWord8 1 >> sput a >> sput b >> sput c
  sput (CPat_Int       a        ) = sputWord8 2 >> sput a
  sput (CPat_Char      a        ) = sputWord8 3 >> sput a
  sput (CPat_BoolExpr  a        ) = sputWord8 4 >> sput a
  sget
    = do t <- sgetWord8
         case t of
            0 -> liftM  CPat_Var         sget
            1 -> liftM3 CPat_Con         sget sget sget
            2 -> liftM  CPat_Int         sget
            3 -> liftM  CPat_Char        sget
            4 -> liftM  CPat_BoolExpr    sget

instance Serialize CPatRest where
  sput (CPatRest_Var   a        ) = sputWord8 0 >> sput a
  sput (CPatRest_Empty          ) = sputWord8 1
  sget
    = do t <- sgetWord8
         case t of
            0 -> liftM  CPatRest_Var           sget
            1 -> return CPatRest_Empty

instance Serialize CPatFld where
  sput (CPatFld_Fld   a b c d ) = sputWord8 0 >> sput a >> sput b >> sput c >> sput d
  sget
    = do t <- sgetWord8
         case t of
            0 -> liftM4 CPatFld_Fld         sget sget sget sget

instance Serialize CBindCateg where
  sput = sputEnum8
  sget = sgetEnum8

instance Serialize CMetaBind where
  sput = sputEnum8
  sget = sgetEnum8



instance AbstractCore CExpr CMetaVal CBind CBound CBindCateg CMetaBind Ty CPat CPatRest CPatFld CAlt where
  -- expr
  acoreApp1Bound   f a      			= CExpr_App f a
  -- acoreLam1Ty a _ e 				    = CExpr_Lam (acoreBind1 a) e
  acoreLam1Bind b e 				    = CExpr_Lam b e
  acoreTagTupTy   tg _ es 				= acoreApp (CExpr_Tup tg) es
  acoreBoundVal1CatLevMetasTy _ _ _ m _ e
  										= CBound_Bind m e
  acoreBound1AspkeyVal a e  			= CBound_Val a e
  acoreBoundValTy1CatLev _ _ _ t		= CBound_Ty acbaspkeyDefaultTy t
  acoreBind1Asp n as					= CBind_Bind n as
  acoreBind1CatLevMetasTy bcat n mlev mb t e
  										= acoreBind1Asp n [acoreBoundValTy1CatLev bcat n mlev t, acoreBoundVal1CatLevMetasTy bcat n mlev mb t e]
  acoreLetBase							= CExpr_Let
  acoreCaseDflt	e as d					= CExpr_Case e as (maybe (acoreVar hsnUnknown) id d)
  acoreVar n							= CExpr_Var (ACoreBindRef n Nothing)
  acoreStringTy _ i						= CExpr_String i
  acoreCharTy _ i						= CExpr_Char i
  acoreIntTy _ i						= CExpr_Int i
  acoreIntTy2 _ i						= CExpr_Int (fromInteger i)
  acoreUidHole							= CExpr_Hole
  acoreHoleLet							= CExpr_HoleLet

  -- ty constants
  acoreTyBool o                         = semCon (ehcOptBuiltin o ehbnDataBool)

  -- ty
  -- acoreTyInt2							= tyInt
  acoreTy2ty							= id

  -- pat
  acorePatVarTy n _						= CPat_Var n
  acorePatCon 							= CPat_Con
  acorePatIntTy _ i						= CPat_Int i
  acorePatIntTy2 _ i					= CPat_Int (fromInteger i)
  acorePatCharTy _ i					= CPat_Char i
  acorePatBoolExpr 						= CPat_BoolExpr

  -- patfld
  acorePatFldBind (lbl,off) b			= CPatFld_Fld lbl off b []
  -- acorePatFldTy _ (lbl,off) n 			= CPatFld_Fld lbl off n []

  -- patrest
  acorePatRestEmpty  					= CPatRest_Empty
  acorePatRestVar						= CPatRest_Var

  -- alt
  acoreAlt                             	= CAlt_Alt

  -- acoreTy2ty            t = t

  -- defaults
  acoreMetavalDflt       				= CMetaVal_Val
  acoreMetavalDfltDict   				= CMetaVal_Dict
  acoreMetabindDflt      				= CMetaBind_Plain
  acoreTyErr             				= Ty_Dbg
  acoreTyNone            				= acoreTyErr "acoreTyNone"
  acoreTyChar 							= tyChar
  acoreTyInt 							= tyInt
  acoreTyString o 						= tyString o

  -- bindcateg
  acoreBindcategRec 					= CBindCateg_Rec
  acoreBindcategStrict 					= CBindCateg_Strict
  acoreBindcategPlain					= CBindCateg_Plain

  -- inspecting
  acoreExprMbLam (CExpr_Lam b e)        = Just (b,e)
  acoreExprMbLam _                      = Nothing

  acoreExprMbLet (CExpr_Let c b e)      = Just (c,b,e)
  acoreExprMbLet _                      = Nothing

  acoreExprMbVar (CExpr_Var r)          = Just (acbrefNm r)
  acoreExprMbVar _                      = Nothing

  acoreExprMbInt (CExpr_Int i)          = Just (tyInt,toInteger i)
  acoreExprMbInt _                      = Nothing

  acoreBindcategMbRec CBindCateg_Rec    = Just CBindCateg_Rec
  acoreBindcategMbRec _                 = Nothing

  acoreBindcategMbStrict CBindCateg_Strict  = Just CBindCateg_Strict
  acoreBindcategMbStrict _                  = Nothing

  acorePatMbCon (CPat_Con tg r fs)		= Just (tg,r,fs)
  acorePatMbCon _                 		= Nothing

  acorePatMbInt (CPat_Int i)			= Just (tyInt,toInteger i)
  acorePatMbInt _                 		= Nothing

  acorePatMbChar (CPat_Char i)			= Just (tyChar,i)
  acorePatMbChar _                 		= Nothing

  acoreUnAlt (CAlt_Alt p e)				= (p,e)
  acoreUnPatFld (CPatFld_Fld l o b _)   = ((l,o),b)
  acoreUnBind (CBind_Bind n as)         = (n,as)

  acoreBoundMbVal (CBound_Val a e)		= Just (a,e)
  acoreBoundMbVal _                 	= Nothing

  -- coercion
  acoreCoeArg 							= CExpr_CoeArg
  acoreExprIsCoeArg						= (== CExpr_CoeArg)

-- CAlt --------------------------------------------------------
data CAlt  = CAlt_Alt !(CPat ) !(CExpr ) 
           deriving ( Data,Eq,Show,Typeable)
-- CAltL -------------------------------------------------------
type CAltL  = [CAlt ]
-- CBind -------------------------------------------------------
data CBind  = CBind_Bind !(HsName) !(CBoundL ) 
            deriving ( Data,Eq,Show,Typeable)
-- CBindAnn ----------------------------------------------------
data CBindAnn  = CBindAnn_Coe !(RelevCoe) 
               deriving ( Data,Eq,Show,Typeable)
-- CBindAnnL ---------------------------------------------------
type CBindAnnL  = [CBindAnn ]
-- CBindL ------------------------------------------------------
type CBindL  = [CBind ]
-- CBound ------------------------------------------------------
data CBound  = CBound_Bind !(CMetas ) !(CExpr ) 
             | CBound_FFE !(FFIWay) !(ForeignEnt) !(CExpr ) !(Ty) 
             | CBound_Meta !(ACoreBindAspectKeyS) !(CMetas ) 
             | CBound_RelevTy !(ACoreBindAspectKeyS) !(RelevTy) 
             | CBound_Ty !(ACoreBindAspectKeyS) !(Ty) 
             | CBound_Val !(ACoreBindAspectKeyS) !(CExpr ) 
             deriving ( Data,Eq,Show,Typeable)
-- CBoundL -----------------------------------------------------
type CBoundL  = [CBound ]
-- CExpr -------------------------------------------------------
data CExpr  = CExpr_Ann !(CExprAnn ) !(CExpr ) 
            | CExpr_App !(CExpr ) !(CBound ) 
            | CExpr_Case !(CExpr ) !(CAltL ) !(CExpr ) 
            | CExpr_CaseAltFail !(CaseAltFailReason) !(CExpr ) 
            | CExpr_Char !(Char) 
            | CExpr_CoeArg 
            | CExpr_FFI !(FFIWay) !(String) !(ForeignEnt) !(Ty) 
            | CExpr_Hole !(UID) 
            | CExpr_HoleLet !(UID) !(CExpr ) 
            | CExpr_ImplsApp !(CExpr ) !(ImplsVarId) 
            | CExpr_ImplsLam !(ImplsVarId) !(CExpr ) 
            | CExpr_Int !(Int) 
            | CExpr_Integer !(Integer) 
            | CExpr_Lam !(CBind ) !(CExpr ) 
            | CExpr_Let !(CBindCateg) !(CBindL ) !(CExpr ) 
            | CExpr_String !(String) 
            | CExpr_Tup !(CTag) 
            | CExpr_TupDel !(CExpr ) !(CTag) !(HsName) !(CExpr ) 
            | CExpr_TupIns !(CExpr ) !(CTag) !(HsName) !(CExpr ) !(CExpr ) 
            | CExpr_TupUpd !(CExpr ) !(CTag) !(HsName) !(CExpr ) !(CExpr ) 
            | CExpr_Var !(ACoreBindRef) 
            deriving ( Data,Eq,Show,Typeable)
-- CExprAnn ----------------------------------------------------
data CExprAnn  = CExprAnn_Coe !(RelevCoe) 
               | CExprAnn_Debug !(String) 
               | CExprAnn_Ty !(Ty) 
               deriving ( Data,Eq,Show,Typeable)
-- CMetaBind ---------------------------------------------------
data CMetaBind  = CMetaBind_Apply0 
                | CMetaBind_Function0 
                | CMetaBind_Function1 
                | CMetaBind_Plain 
                deriving ( Data,Enum,Eq,Show,Typeable)
-- CMetaVal ----------------------------------------------------
data CMetaVal  = CMetaVal_Dict 
               | CMetaVal_DictClass !(([Track])) 
               | CMetaVal_DictInstance !(([Track])) 
               | CMetaVal_Track !(Track) 
               | CMetaVal_Val 
               deriving ( Data,Eq,Show,Typeable)
-- CMetas ------------------------------------------------------
type CMetas  = ( CMetaBind ,CMetaVal )
-- CModule -----------------------------------------------------
data CModule  = CModule_Mod !(HsName) !(CExpr ) !(CTagsMp) 
              deriving ( Data,Eq,Show,Typeable)
-- CPat --------------------------------------------------------
data CPat  = CPat_BoolExpr !(CExpr) 
           | CPat_Char !(Char) 
           | CPat_Con !(CTag) !(CPatRest ) !(CPatFldL ) 
           | CPat_Int !(Int) 
           | CPat_Var !(HsName) 
           deriving ( Data,Eq,Show,Typeable)
-- CPatFld -----------------------------------------------------
data CPatFld  = CPatFld_Fld !(HsName) !(CExpr ) !(CBind ) !(CBindAnnL ) 
              deriving ( Data,Eq,Show,Typeable)
-- CPatFldL ----------------------------------------------------
type CPatFldL  = [CPatFld ]
-- CPatRest ----------------------------------------------------
data CPatRest  = CPatRest_Empty 
               | CPatRest_Var !(HsName) 
               deriving ( Data,Eq,Show,Typeable)
-- CodeAGItf ---------------------------------------------------
data CodeAGItf  = CodeAGItf_AGItf !(CModule ) 
                deriving ( Data,Eq,Show,Typeable)
-- MbCExpr -----------------------------------------------------
type MbCExpr  = Maybe CExpr 