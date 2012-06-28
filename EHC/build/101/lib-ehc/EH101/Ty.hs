

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/Ty.ag)
module EH101.Ty(TyAGItf (..), Ty (..), TyAnn (..), TyL, tyInt, tyChar, mkTyCon
, tyLHdAndTl, tyArrowArgRes, tyArrowArgsRes, tyArrowArgs, tyArrowRes, tyAppFunArgs, tyProdArgs, tyAppArgs
, tyUnAnn
, tyCanonAnn
, TyVarId, mkTyVar, mkNewTyVar, mkNewUIDTyVarL, mkNewTyVarL, mkTyFreshProd, mkTyFreshProdFrom, tyEnsureNonAny
, TyVarIdL, TyVarIdS
, TyVarWild (..), TyVarWildMp
, tvwmpNoQuantS
, mkTyQu, TyVarCateg (..)
, tyMbVar
, tyVar
, TyQu (..)
, tvCatIsPlain, tvCatIsFixed
, TvInfo (..)
, emptyTvInfo
, mkTvInfo, mkTvInfoTy
, TvCatMp, emptyTvCatMp
, tyQu_Forall, tyQu_Exists
, tyquIsExists, tyquIsForall
, tyquMetaLev
, tyquExists
, showTyQu
, tyAnnMono
, tyAnnDecomposeMk
, tyIsVar, tyIsCon
, tvIsPlain, tyIsVarPlain
, tyIsSimple
, tyMbCon, tyConNm
, Polarity
, polCovariant, polContravariant, polInvariant
, polIsCovariant, polIsContravariant, polIsInvariant
, polOpp
, FIMode (..)
, fimOpp
, fimSwapPol
, InstTo (..)
, instToSplitQu
, kiStar
, tyQu_KiForall, tyQu_KiExists
, tyAppFunArgsWithLkup
, TyKiKey (..)
, tyKiKeyMbName
, tyKiKeyIsName
, instToL1AssocL
, kiRow, tyRowEmpty, tyRecEmpty, tySumEmpty, mkTyRow, mkTyRec, mkTySum, mkTyRecExt
, tyAppFunArg
, FldTyL
, tyRowUnAnn, tyRowExtsUnAnn
, tyMbConWithLkup
, tyAppFunMbConNm, tyAppFunConNm
, tyRowExtsWithLkup, tyRecExtrWithLkup, tyRecExtsWithLkup
, tyRowExtr, tyRecExtr, tyRecExts, tyMbRecRow, tyRecRow, tyRowExts
, rowExtCmp, tyRowCanonOrder, tyRowCanonOrderBy, tyRowCanonOrderOn, tyRowCanonLblOrder
, tyRecOffset
, SysfTy, SysfTySeq, SysfTySeq1
, mkTyThunk
, tyArrowInverse
, tyRecExts2
, tyRowOffsetOrder
, tyExtsOffset
, tyRecOffsetWithLkup
, tyErr
, Pred (..)
, Impls (..), ImplsVarId
, TyCtxt (..)
, tyEnsureNonAnyImpl
, tvCatIsMeta
, TvPurpose (..)
, tvpurposeIsTy
, mkTvInfoPlain
, PredScope (..), initPredScope
, pscpMbVar
, pscpEnter, pscpLeave
, pscpEnter', pscpLeave', pscpMk'
, pscpIsVisibleIn, pscpCommon
, pscpParents
, pscpCmp, pscpCmpByLen
, ImplsProveOcc (..), mkImplsProveOcc
, PredOcc (..), poId, mkPredOcc
, CHRPredOccCxt (..)
, CHRPredOcc' (..), CHRPredOcc, cpoScope, mkCHRPredOcc
, cpo2PredOcc
, tyUnNoiseForVarBind
, mkTyMetaVar
, mkImplsTail, mkImplsVar, mkImplsNil
, mkTyImpls
, mkTyPr
, tyArrowArity
, tyArrowArgResWithLkup
, tyArrowImplsResWithLkup, tyArrowImplsArgResWithLkup
, tyArrowImplsRes, tyArrowImplsArgRes
, tyMbAppConArgs
, tyQuant
, tyLImplsPreds
, tyPred, predNm, tyPredNm, tyPrArrowArgsRes
, tyPredMatchNmArgs, predMatchNmArgs
, tyPred2DataTy, pred2DataTy
, predTy
, predHasRuntimeEvidence
, implsPredsTailWithLkup, tyImplsWithLkup, implsPrIdsWithLkup
, tyImpls, implsPredsTail, implsPredsMbTail, implsIsTail, tyIsImplsTail, tyImplsPreds, implsPrIds
, implsMbVar, implsTailVar
, implsIsEmpty
, tyIsPredicated, tyIsPredicatedWithLkup
, Label (..), LabelAGItf (..)
, LabelVarId
, LabelOffset (..)
, tyIsEmptyRow
, tyRowIsCanonOrdered
, labelMbVar
, tyLamArgsRes
, tyIsLam
, mkTyLam
, tyString
, tyAppFunArgsMk
, tyLamEtaRed
, tyDecomposeMk
, tyRecMap
, PredSeq (..)
, predSeqToList, predLFlatten
, mkPolNegate, mkPolVar
, tyIntUnboxed
, kiUnboxed
, tyAllConS
, tyInteger
, tyMbRecExts
, mkPredOccRng
, mkCHRPredOccRng
, tyIsA) where

import EH.Util.Utils
import EH101.Base.Common
import EH101.Base.Builtin
import EH101.Opts.Base
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import EH.Util.Pretty
import EH.Util.Utils
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize



{-|
The module EH101.Ty contains the Haskell interface to the internal representation of types
used by EHC. The AST is described in Ty/AbsSyn.

-}










































type SysfTy     	= Ty			-- base ty
type SysfTySeq    	= Ty			-- sequence
type SysfTySeq1    	= Ty			-- singleton



type TyL        = [Ty]



type TyVarId    = UID



type TyVarIdL   = [TyVarId]
type TyVarIdS   = Set.Set TyVarId



type FldTyL = AssocL (Maybe HsName) Ty



type ImplsVarId    = UID



type LabelVarId      = UID



-- | tyvars may act as wildcard, that is, they act as placeholders inside partial type signatures, providing
--   some structure. Such wildcards come in flavors:
data TyVarWild
  = TyVarWild_NoQuantTyExpr_YesQuantLetBinding		-- quantify: not in TyExpr, yes in let binding
  | TyVarWild_NoQuantTyExpr_NoQuantLetBinding		-- quantify: not in TyExpr, not in let binding (enforce monomorphism)
  deriving Eq

type TyVarWildMp = Map.Map TyVarId TyVarWild



tvwmpNoQuantS :: TyVarWildMp -> UIDS
tvwmpNoQuantS = Map.keysSet . Map.filter (== TyVarWild_NoQuantTyExpr_NoQuantLetBinding)



tvCatIsPlain :: TyVarCateg -> Bool
tvCatIsPlain TyVarCateg_Plain  = True
tvCatIsPlain _                 = False

tvCatIsFixed :: TyVarCateg -> Bool
tvCatIsFixed TyVarCateg_Fixed  = True
tvCatIsFixed _                 = False



tvCatIsMeta :: TyVarCateg -> Bool
tvCatIsMeta TyVarCateg_Meta   = True
tvCatIsMeta _                 = False



data TvPurpose
  = TvPurpose_Ty TyVarCateg     -- stands for unknown type, the known humble type variable
  | TvPurpose_Impls             -- possibly empty sequence of implicit parameters
  | TvPurpose_Scope             -- predicate scope
  | TvPurpose_Pred              -- predicate
  | TvPurpose_AssNm             -- assumed name, in CHR
  | TvPurpose_Label             -- label, in CHR, for ext records
  | TvPurpose_Offset            -- offset, in CHR, for ext records
  | TvPurpose_PredSeq           -- experimental still
  deriving (Eq,Ord)



instance Show TvPurpose where
  show (TvPurpose_Ty TyVarCateg_Fixed)  = "c"
  show (TvPurpose_Ty TyVarCateg_Meta )  = "m"
  show (TvPurpose_Ty _               )  = "v"
  show TvPurpose_Impls                  = "i"
  show TvPurpose_Scope                  = "s"
  show TvPurpose_Pred                   = "p"
  show TvPurpose_AssNm                  = "a"
  show TvPurpose_Label                  = "l"
  show TvPurpose_Offset                 = "o"
  show TvPurpose_PredSeq                = "s"



tvpurposeIsTy (TvPurpose_Ty _) = True
tvpurposeIsTy _                = False



data TvInfo
  = TvInfo
      { tvinfoCateg     :: !TyVarCateg
      , tvinfoPurpose   :: !TvPurpose
      }



emptyTvInfo
  = TvInfo TyVarCateg_Plain
           (TvPurpose_Ty TyVarCateg_Plain)



mkTvInfo = TvInfo

mkTvInfoTy :: TyVarCateg -> TvInfo
mkTvInfoTy c = mkTvInfo c (TvPurpose_Ty c)



instance Show TvInfo where
  show i | pur == TvPurpose_Ty TyVarCateg_Fixed && cat == TyVarCateg_Fixed = "c"
         | otherwise                                                       = show pur
    where cat = tvinfoCateg i
          pur = tvinfoPurpose i



mkTvInfoPlain :: TvPurpose -> TvInfo
mkTvInfoPlain p = mkTvInfo TyVarCateg_Plain p



type TvCatMp = Map.Map TyVarId TvInfo
emptyTvCatMp = Map.empty



data LabelOffset
  = LabelOffset_Off !Int
  | LabelOffset_Var !UID
  deriving
    ( Eq, Ord
    , Typeable, Data
    )



instance Show LabelOffset where
  show (LabelOffset_Off o) = show o
  show (LabelOffset_Var v) = "off_" ++ show v



data PredScope
  = PredScope_Lev !(RLList Int)
  | PredScope_Var !TyVarId
  deriving (Eq,Ord)

initPredScope :: PredScope
initPredScope = PredScope_Lev rllEmpty



deriving instance Typeable PredScope
deriving instance Data PredScope



pscpMbVar :: PredScope -> Maybe TyVarId
pscpMbVar (PredScope_Var v) = Just v
pscpMbVar _                 = Nothing



instance Show PredScope where
  show (PredScope_Lev l) = show l
  show (PredScope_Var v) = "[sc_" ++ show v ++ "]"



pscpEnter :: Int -> PredScope -> (Int,PredScope)
pscpEnter x (PredScope_Lev s) = (x+1,PredScope_Lev (s `rllConcat` rllSingleton x))

pscpLeave :: PredScope -> PredScope
pscpLeave (PredScope_Lev s) = PredScope_Lev $ fst $ fromJust $ rllInitLast s



-- enter yes/no scope, give back the threaded counter to outside the new scope and inside
-- use in conjunction with pscpLeave'
pscpEnter' :: Bool -> Int -> (Int,Int)
pscpEnter' yesEnter x
  = if yesEnter
    then (fst $ pscpEnter x initPredScope,0)
    else (x,x)

-- leave scope, on previous entering yes/no scope, give back the threaded counter to outside the new scope and inside
-- use in conjunction with pscpEnter'
pscpLeave' :: Bool -> Int -> Int -> Int
pscpLeave' yesEnter newScopeCounter innerScopeCounter
  = if yesEnter
    then newScopeCounter
    else innerScopeCounter

-- make scope, depending on yes/no entering
-- use in conjunction with pscpEnter'
pscpMk' :: Bool -> Int -> PredScope -> PredScope
pscpMk' yesEnter x s
  = if yesEnter
    then snd $ pscpEnter x s
    else s



pscpIsVisibleIn :: PredScope -> PredScope -> Bool
pscpIsVisibleIn (PredScope_Lev sOuter) (PredScope_Lev sInner) = sOuter `rllIsPrefixOf` sInner
pscpIsVisibleIn _                      _                      = False

pscpCommon :: PredScope -> PredScope -> Maybe PredScope
pscpCommon (PredScope_Lev s1) (PredScope_Lev s2)
  = Just $ PredScope_Lev $ commonPrefix s1 s2
  where commonPrefix xxs     yys     | isJust ht1 && isJust ht2 && x == y     = rllSingleton x `rllConcat` commonPrefix xs ys
                                     | otherwise                              = rllEmpty
                                     where ht1 = rllHeadTail xxs
                                           ht2 = rllHeadTail yys
                                           (x,xs) = fromJust ht1
                                           (y,ys) = fromJust ht2
        -- commonPrefix _       _                    = rllEmpty
pscpCommon _                  _
  = Nothing



pscpParents :: PredScope -> [PredScope]
pscpParents (PredScope_Lev s) | not (rllNull s) = map PredScope_Lev $ rllInits $ rllInit s
pscpParents _                                   = []



pscpCmp :: PredScope -> PredScope -> Maybe Ordering
pscpCmp (PredScope_Lev s) (PredScope_Lev t) = Just $ s `compare` t
pscpCmp _                 _                 = Nothing

pscpCmpByLen :: PredScope -> PredScope -> Ordering
pscpCmpByLen (PredScope_Lev s) (PredScope_Lev t) = (rllLength s) `compare` (rllLength t)



data ImplsProveOcc
  = ImplsProveOcc
      { ipoId       :: !UID
      , ipoScope    :: !PredScope
      }
  deriving (Eq,Show,Ord)

mkImplsProveOcc :: UID -> PredScope -> ImplsProveOcc
mkImplsProveOcc = ImplsProveOcc



deriving instance Typeable ImplsProveOcc
deriving instance Data ImplsProveOcc



data PredOcc
  =  PredOcc
       { poPr               :: !Pred
       , poPoi              :: !PredOccId
       , poScope            :: !PredScope
       , poRange            :: !Range
       }
  deriving (Show,Eq,Ord)

poId :: PredOcc -> UID
poId = poiId . poPoi

mkPredOcc :: Pred -> PredOccId -> PredScope -> PredOcc
mkPredOcc p i sc = rngLift emptyRange mkPredOccRng p i sc



mkPredOccRng :: Range -> Pred -> PredOccId -> PredScope -> PredOcc
mkPredOccRng r p i sc = PredOcc p i sc r



data CHRPredOccCxt
  = CHRPredOccCxt_Scope1
      { cpocxScope			:: !PredScope			-- default, only allowed value for occurring preds
      --											-- others for solving and CHR's only
      }
  deriving (Show,Eq,Ord)




deriving instance Typeable CHRPredOccCxt
deriving instance Data CHRPredOccCxt



data CHRPredOcc' p
  =  CHRPredOcc
       { cpoPr               :: !p
       -- , cpoScope            :: !PredScope
       , cpoCxt              :: !CHRPredOccCxt
       , cpoRange            :: !Range
       }
  deriving (Show,Eq,Ord)

type CHRPredOcc = CHRPredOcc' Pred

mkCHRPredOcc :: Pred -> PredScope -> CHRPredOcc
mkCHRPredOcc p sc = mkCHRPredOccRng emptyRange p sc

cpoScope :: CHRPredOcc -> PredScope
cpoScope = cpocxScope . cpoCxt



cpo2PredOcc :: PredOccId -> CHRPredOcc -> PredOcc
cpo2PredOcc i o
  = mkPredOccRng
      (cpoRange o)
      (cpoPr o)
      i
      (cpoScope o)



deriving instance Typeable1 CHRPredOcc'
deriving instance Data CHRPredOcc



mkCHRPredOccRng :: Range -> Pred -> PredScope -> CHRPredOcc
mkCHRPredOccRng r p sc = CHRPredOcc p (CHRPredOccCxt_Scope1 sc) r



tyQu_Forall = TyQu_Forall metaLevVal
tyQu_Exists = TyQu_Exists metaLevVal



tyQu_KiForall = TyQu_Forall metaLevTy
tyQu_KiExists = TyQu_Exists metaLevTy



tyquIsExists, tyquIsForall :: TyQu -> Bool



tyquIsForall (TyQu_Forall _)         = True
tyquIsForall _                       = False

tyquIsExists (TyQu_Exists _)         = True
tyquIsExists _                       = False



tyquMetaLev (TyQu_Forall l)         = l
tyquMetaLev (TyQu_Exists l)         = l
tyquMetaLev (TyQu_Plain  l)         = l



tyquExists, tyquForall :: TyQu -> TyQu



tyquForall   (TyQu_Exists l)         = TyQu_Forall l
tyquForall   q                       = q

tyquExists   (TyQu_Forall l)         = TyQu_Exists l
tyquExists   q                       = q



showTyQu  (TyQu_Forall 0) =  "forall"
showTyQu  (TyQu_Forall 1) =  "forall" ++ [charKindStar]
showTyQu  (TyQu_Forall l) =  "forall" ++ [charKindStar] ++ show l
showTyQu  (TyQu_Exists 0) =  "exists"
showTyQu  (TyQu_Exists 1) =  "exists" ++ [charKindStar]
showTyQu  (TyQu_Exists l) =  "exists" ++ [charKindStar] ++ show l
showTyQu  (TyQu_Plain  0) =  ""
showTyQu  (TyQu_Plain  1) =  [charKindStar]
showTyQu  (TyQu_Plain  l) =  [charKindStar] ++ show l



tyIsA :: Ty -> String
tyIsA (Ty_Con   a      ) = "CON"
tyIsA (Ty_App   a b    ) = "APP"
tyIsA (Ty_Ann   a b    ) = "ANN"
tyIsA (Ty_Var   a b    ) = "VAR"
tyIsA (Ty_Any          ) = "ANY"
tyIsA (Ty_TBind a b c d) = "QUANT"
tyIsA (Ty_Ext   a b c  ) = "EXT"
tyIsA (Ty_Pred  a      ) = "PRED"
tyIsA (Ty_Lam   a b    ) = "LAM"
tyIsA (Ty_Impls a      ) = "IMPLS"



-- | Remove TyAnn's
tyUnAnn :: Ty -> Ty
tyUnAnn (Ty_Ann _ t) = tyUnAnn t
tyUnAnn t            = t
{-# INLINE tyUnAnn #-}



tyRowUnAnn :: AssocL HsName Ty -> AssocL HsName Ty
tyRowUnAnn = assocLMapElt tyUnAnn

tyRowExtsUnAnn :: (Ty,AssocL HsName Ty) -> (Ty,AssocL HsName Ty)
tyRowExtsUnAnn (x,y) = (tyUnAnn x,tyRowUnAnn y)



-- | Canonicalize TyAnn's
tyCanonAnn :: Ty -> Ty
tyCanonAnn t
  = c [] t
  where c seen t@(Ty_Ann a ta) | a `elem` seen = c seen ta
                               | otherwise     = Ty_Ann a $ c (a:seen) ta
        c _    t                               = t



-- | Add TyAnn
tyAnn :: TyAnn -> Ty -> Ty
tyAnn a t@(Ty_Ann a' _) | a == a' = t
tyAnn a t                         = Ty_Ann a t

-- | Add TyAnn_Mono
tyAnnMono :: Ty -> Ty
tyAnnMono = tyAnn TyAnn_Mono



-- | Decompose type for annotation into unannotated type + reconstruction
tyAnnDecomposeMk    :: Ty -> (Ty, [TyAnn], Ty -> Ty)
tyAnnDecomposeMk t
  = case t of
      Ty_Ann a t'       -> (ta,as,\ta' -> tyAnn a $ mk ta')
                        where (ta,as,mk) = tyAnnDecomposeMk t'
      _                 -> (t ,[],id)



-- | un-noise with the purpose of finding out whether we end up with Ty_Var
tyUnNoiseForVarBind :: Ty -> Maybe Ty
tyUnNoiseForVarBind t
  = un t
  where un t | not (null impls) && all isNil impls
             = tv t'
             where (impls,t') = tyArrowImplsRes t
                   isNil (Ty_Impls (Impls_Nil)) = True
                   isNil _                      = False
        un t = tv t
        tv t | tyIsVar t = Just t
             | otherwise = Nothing



tyIsVar :: Ty -> Bool
tyIsVar = isJust . tyMbVar

tyIsCon :: Ty -> Bool
tyIsCon = isJust . tyMbCon

tyIsQu :: Ty -> Bool
tyIsQu = isJust . tyMbQu



tyIsVarPlain :: Ty -> Bool
tyIsVarPlain = maybe False (tvCatIsPlain . snd) . tyMbVar'

tvIsPlain :: TvCatMp -> TyVarId -> Bool
tvIsPlain m v = maybe False (tvCatIsPlain . tvinfoCateg) $ Map.lookup v m



tyIsEmptyRow :: Ty -> Bool
tyIsEmptyRow = maybe False (== hsnRowEmpty) . tyMbCon



tyIsSimple :: Ty -> Bool
tyIsSimple t = tyIsVar t || tyIsCon t



tyIsLam :: Ty -> Bool
tyIsLam ty
  = not (null as)
  where (as,_) = tyLamArgsRes ty



instance SemApp Ty where
  semApp         = Ty_App
  semAppTop      = id
  semCon         = Ty_Con . mkHNm
  semParens      = id


  semRngVar _    = panic "Ty:semRngVar"


  mkProdApp tyL  = mkTyRec (zip positionalFldNames tyL)



mkTyCon :: String -> Ty
mkTyCon n = semCon (hsnFromString n)



mkTyVar :: TyVarId -> Ty
mkTyVar tv = Ty_Var tv TyVarCateg_Plain



mkTyMetaVar :: TyVarId -> Ty
mkTyMetaVar tv = Ty_Var tv TyVarCateg_Meta



mkNewTyVar :: UID -> Ty
mkNewTyVar u = let  (_,v) = mkNewUID u in mkTyVar v



mkNewUIDTyVarL :: Int -> UID -> ([UID],TyL)
mkNewUIDTyVarL sz u = let vs = mkNewUIDL sz u in (vs,map mkTyVar vs)

mkNewTyVarL :: Int -> UID -> TyL
mkNewTyVarL sz u = snd (mkNewUIDTyVarL sz u)



tyEnsureNonAny :: UID -> Ty -> Ty
tyEnsureNonAny u t = if t /= Ty_Any then t else mkNewTyVar u



-- | (placeholder for) make type level repr of thunk (i.e. laziness)
mkTyThunk :: Ty -> Ty
mkTyThunk = id



tyEnsureNonAnyImpl :: UID -> Ty -> Ty
tyEnsureNonAnyImpl u t
  = if t /= Ty_Any then  t
                   else  let  [i,r] = mkNewUIDL 2 u
                         in   [mkImplsVar i] `mkArrow` mkTyVar r



mkTyQu :: TyQu -> [(TyVarId,Ty)] -> Ty -> Ty
mkTyQu q tvL t = foldr (\(tv,k) t -> Ty_TBind q tv k t) t tvL



mkTyFreshProdFrom :: UID -> Int -> Ty
mkTyFreshProdFrom uid arity =  mkProdApp . map mkTyVar . mkNewUIDL arity $ uid

mkTyFreshProd :: Int -> Ty
mkTyFreshProd = mkTyFreshProdFrom uidStart



mkTyLam :: [TyVarId] -> Ty -> Ty
mkTyLam args body = foldr Ty_Lam body args



mkImplsTail :: ImplsVarId -> Impls
mkImplsTail v = Impls_Tail v []

mkImplsVar :: ImplsVarId -> Ty
mkImplsVar v = Ty_Impls (mkImplsTail v)

mkImplsNil :: Ty
mkImplsNil = Ty_Impls Impls_Nil



mkTyImpls :: [Pred] -> Ty -> Ty
mkTyImpls prL t = map mkTyPr prL `mkArrow` t



mkTyPr :: Pred -> Ty
mkTyPr p
  =  case p of
       Pred_Pred t  -> t
       _            -> Ty_Pred p



tyInt   = Ty_Con hsnInt



tyChar  = Ty_Con hsnChar





kiStar  = Ty_Con hsnKindStar



kiRow       = Ty_Con hsnKindRow
tyRowEmpty  = Ty_Con hsnRowEmpty
tyRecEmpty  = mkTyRec []
tySumEmpty  = mkTySum []



tyIntUnboxed   = Ty_Con hsnIntUnboxed



kiUnboxed  = Ty_Con hsnKindUnboxed



tyInteger      	  = Ty_Con hsnInteger





tyString :: EHCOpts -> Ty
tyString o = Ty_Con (ehcOptBuiltin o ehbnPrelString)



tyArrowArgRes   :: Ty -> (Ty,Ty)



tyArrowArgsRes  :: Ty -> (TyL,Ty)
tyAppFunArgs    :: Ty -> (Ty,TyL)
tyAppArgs       :: Ty -> TyL
tyArrowArgs     :: Ty -> TyL
tyArrowRes      :: Ty -> Ty



-- | if an arrow, split into single arg + res
tyMbArrowArgRes   :: Ty -> Maybe (Ty,Ty)
tyMbArrowArgRes t
  =  case tyUnAnn t of
       Ty_TBind _ _ _ t   -> tyMbArrowArgRes t
       Ty_App a1 r
         -> case tyUnAnn a1 of
              Ty_App a2 a
                -> case tyUnAnn a2 of
                     Ty_Con nm | hsnIsArrow nm  -> Just (a,r)
                     _                          -> Nothing
              _ -> Nothing
       _ -> Nothing

tyArrowArgRes t = maybe (Ty_Any,t) id $ tyMbArrowArgRes t



-- | split into arguments and result, and give rebuilding function of outer layer
tyArrowArgsRes' :: Ty -> (([Ty],Ty),Ty->Ty)
tyArrowArgsRes' t
  =  case tyUnAnn t of
       Ty_TBind q tv l t  -> let (asr,mk) = tyArrowArgsRes' t in (asr, Ty_TBind q tv l . mk)
       t | isJust mbAR    -> let ((as,r'),_) = tyArrowArgsRes' r in ((a:as,r'),id)
         | otherwise      -> (([],t),id)
         where mbAR@(~(Just (a,r))) = tyMbArrowArgRes t

-- | split into arguments and result
tyArrowArgsRes = fst . tyArrowArgsRes'



tyArrowArgs  = fst . tyArrowArgsRes
tyArrowRes   = snd . tyArrowArgsRes



-- | the arity of an arrow type
tyArrowArity :: Ty -> Int
tyArrowArity = length . tyArrowArgs



-- |  inverse type, i.e. a->b gives b->a, a->b->c gives c->(a,b)
tyArrowInverse :: Ty -> Ty
tyArrowInverse t
  = case tyArrowArgsRes' t of
      ((   [a]  ,r),mk) -> mk $ [r] `mkArrow` a
      ((as@(_:_),r),mk) -> mk $ [r] `mkArrow` mkProdApp as
      _                 -> t



tyArrowArgResWithLkup :: (TyVarId -> Maybe Ty) -> Ty -> (Ty,Ty)
tyArrowArgResWithLkup lookup = tyVarChkVisitLift lookup tyArrowArgRes tyArrowArgRes



tyArrowImplsArgResWithLkup :: (TyVarId -> Maybe Ty) -> Ty -> (TyL,Ty,Ty)
tyArrowImplsArgResWithLkup lookup t
  = (i,a,r)
  where (i,t')  = tyArrowImplsResWithLkup lookup t
        (a,r)   = tyArrowArgResWithLkup   lookup t'

tyArrowImplsResWithLkup :: (TyVarId -> Maybe Ty) -> Ty -> (TyL,Ty)
tyArrowImplsResWithLkup lookup t
  = extr t
  where extr t = case tyMbArrowArgRes t of
                   Just (a,r)
                     | isImpls a'
                       -> let (as,r') = extr r in (a':as,r')
                     where  a' = tyUnAnn a
                            isImpls (Ty_Pred  _)  = True
                            isImpls (Ty_Impls _)  = True
                            isImpls _             = False
                   _   -> tyVarLift lookup extr ((,) []) t



tyArrowImplsRes :: Ty -> (TyL,Ty)
tyArrowImplsRes = tyArrowImplsResWithLkup (const Nothing)

tyArrowImplsArgRes  :: Ty -> (TyL,Ty,Ty)
tyArrowImplsArgRes = tyArrowImplsArgResWithLkup (const Nothing)



tyProdArgs      :: Ty -> TyL
tyLHdAndTl      :: [Ty] -> (Ty,TyL)



tyAppFunArgs
  =  extr []
  where  extr as t
           =  case tyUnAnn t of
                Ty_TBind _ qv _ t -> extr as t
                Ty_App f a        -> extr (a:as) f
                _                 -> (t,as)



-- Substitution aware
tyAppFunArgsWithLkup :: (TyVarId -> Maybe Ty) -> Ty -> (Ty,TyL)
tyAppFunArgsWithLkup lookup = tyVarChkVisitLift lookup tyAppFunArgs tyAppFunArgs



tyAppFunArgsMk    :: Ty -> (Ty, TyL, Ty -> TyL -> Ty)
tyAppFunArgsMk
  =  extr []
  where  extr as t
           =  case tyUnAnn t of
                Ty_TBind q v k t  -> (f,as,\f as -> Ty_TBind q v k $ mk f as)
                                  where (f,as,mk) = tyAppFunArgsMk t
                Ty_App f a        -> extr (a:as) f
                _                 -> (t,as,\f as -> mkApp (f:as))



tyAppArgs    = snd . tyAppFunArgs



tyLHdAndTl   = hdAndTl' Ty_Any



tyMbCon :: Ty -> Maybe HsName
tyMbCon t = case tyUnAnn t of {Ty_Con nm -> Just nm ; _ -> Nothing}

tyConNm :: Ty -> HsName
tyConNm = maybe hsnUnknown id . tyMbCon



tyMbConWithLkup :: (TyVarId -> Maybe Ty) -> Ty -> Maybe HsName
tyMbConWithLkup lookup = tyVarChkVisitLift lookup tyMbCon tyMbCon



tyMbVar' :: Ty -> Maybe (TyVarId,TyVarCateg)
tyMbVar' t = case tyUnAnn t of {Ty_Var v c -> Just (v,c) ; _ -> Nothing}

tyMbVar :: Ty -> Maybe TyVarId
tyMbVar = fmap fst . tyMbVar'



tyMbQu :: Ty -> Maybe TyQu
tyMbQu t
  = case tyUnAnn t of
      Ty_TBind q _ _ _
        -> Just q
      _ -> Nothing



tyVar :: Ty -> TyVarId
tyVar = maybe uidStart id . tyMbVar



tyProdArgs ty = let (t,al) = tyRecExts ty in map snd al



tyAppFunArg :: Ty -> (Ty,Ty)
tyAppFunArg t =  case tyUnAnn t of {Ty_App f a -> (f,a); _ -> (Ty_Any,Ty_Any)}



tyMbAppConArgs    :: Ty -> Maybe (HsName,TyL)
tyMbAppConArgs t
  =  fmap (\c -> (c,a)) $ tyMbCon f
  where (f,a) = tyAppFunArgs t



tyAppFunMbConNm :: Ty -> Maybe HsName
tyAppFunMbConNm = tyMbCon . fst . tyAppFunArgs

tyAppFunConNm :: Ty -> HsName
tyAppFunConNm = tyConNm . fst . tyAppFunArgs



-- | All constructors occurring in a type
tyAllConS    :: Ty -> Set.Set HsName
tyAllConS t
  =  Set.unions $ (maybe Set.empty Set.singleton $ tyMbCon f) : map tyAllConS a
  where (f,a,_) = tyDecomposeMk t



tyQuant :: Ty -> Ty
tyQuant t
  =  case tyUnAnn t of
       Ty_TBind _ _ _ t'  -> tyQuant t'
       _                  -> t



tyLImplsPreds :: TyL -> ([Pred],Impls)
tyLImplsPreds = foldr (\t (ps,i) -> case tyUnAnn t of {Ty_Pred p -> (p:ps,i); Ty_Impls i -> (ps,i)}) ([],Impls_Nil)



tyLamArgsRes :: Ty -> ([TyVarId],Ty)
tyLamArgsRes
  =  extr
  where  extr t
           =  case tyUnAnn t of
                Ty_Lam a r  -> (a:as',r')
                            where (as',r') = extr r
                _           -> ([],t)



-- Eta reduce when the full list of args can be eliminated thus, i.e. it matches with the tail of the args applied in the app (if any) of the body
tyLamEtaRed :: Ty -> Maybe Ty
tyLamEtaRed t
  | llas > 0 && rlDiff >= 0 && map mkTyVar las == rTl = Just (mkApp (f:rHd))
  | otherwise                                         = Nothing
  where (las,r  ) = tyLamArgsRes t
        (f  ,ras) = tyAppFunArgs r
        llas      = length las
        rlas      = length ras
        rlDiff	  = rlas - llas
        (rHd,rTl) = splitAt rlDiff ras



mkTyRow :: Ty -> AssocL HsName Ty -> Ty
mkTyRow r = foldl (\t (n,e) -> Ty_Ext t n e) r

mkTyRec :: AssocL HsName Ty -> Ty
mkTyRec al = hsnRec `mkConApp` [tyRowEmpty `mkTyRow` al]

mkTyRecExt :: Ty -> AssocL HsName Ty -> Ty
mkTyRecExt recd al
  =  let  (row,exts) = tyRowExts (tyRecRow recd)
     in   hsnRec `mkConApp` [row `mkTyRow` (exts ++ al)]

mkTySum :: AssocL HsName Ty -> Ty
mkTySum al = hsnSum `mkConApp` [tyRowEmpty `mkTyRow` al]



tyVarChkVisitLift :: (TyVarId -> Maybe Ty) -> (Ty -> x) -> (Ty -> x) -> Ty -> x
tyVarChkVisitLift
  = withLkupChkVisitLift tyMbVar (noVisit . tyUnAnn)
  where noVisit (Ty_TBind _ qv _ _) = Set.singleton qv
        noVisit _                   = Set.empty

tyVarLift :: (TyVarId -> Maybe Ty) -> (Ty -> x) -> (Ty -> x) -> Ty -> x
tyVarLift = withLkupLift tyMbVar



implsTailVarLiftCyc :: (TyVarId -> Maybe Impls) -> (TyVarIdS -> Impls -> x) -> (Impls -> x) -> TyVarIdS -> Impls -> x
implsTailVarLiftCyc = withLkupLiftCyc1 implsMbVar (const Set.empty)



tyRowExtsWithLkup :: (TyVarId -> Maybe Ty) -> Ty -> (Ty,AssocL HsName Ty)
tyRowExtsWithLkup lookup
  =  extr []
  where  extr as t
           =  case tyUnAnn t of
                (Ty_Ext r l e) -> extr ((l,e):as) r
                t'             -> tyVarLift lookup (extr as) (flip (,) as) t'

tyRecExtsWithLkup :: (TyVarId -> Maybe Ty) -> Ty -> (Ty,AssocL HsName Ty)
tyRecExtsWithLkup lookup t
  =  case tyRecRowWithLkup lookup t of
       Ty_Any  -> (Ty_Any,[])
       row     -> tyRowExtsWithLkup lookup row

tyRecRowWithLkup :: (TyVarId -> Maybe Ty) -> Ty -> Ty
tyRecRowWithLkup lookup = maybe Ty_Any id . tyMbRecRowWithLkup lookup

tyRowExtrWithLkup :: (TyVarId -> Maybe Ty) -> HsName -> Ty -> Maybe (Ty,Ty)
tyRowExtrWithLkup lookup lbl t
  = extr t
  where extr t
          = case tyUnAnn t of
              (Ty_Ext r l e) | lbl == l   -> Just (r,e)
                             | otherwise  -> maybe Nothing (\(r',e') -> Just (Ty_Ext r' l e,e')) (extr r)
              t'                          -> tyVarLift lookup extr (const Nothing) t'

tyRecExtrWithLkup :: (TyVarId -> Maybe Ty) -> HsName -> Ty -> Maybe (Ty,Ty)
tyRecExtrWithLkup lookup lbl t
  =  case tyRowExtrWithLkup lookup lbl (tyRecRowWithLkup lookup t) of
       Nothing    -> Nothing
       Just (r,e) -> Just (hsnRec `mkConApp` [r],e)

tyMbRecRowWithLkup :: (TyVarId -> Maybe Ty) -> Ty -> Maybe Ty
tyMbRecRowWithLkup lookup t
  =  case tyAppFunArgsWithLkup lookup t of
       (f,[row])
         -> case tyMbConWithLkup lookup f of
              Just n | hsnIsRec n || hsnIsSum n -> Just row
              _                                 -> Nothing
       _                                        -> Nothing






tyMbRecRow :: Ty -> Maybe Ty
tyMbRecRow = tyMbRecRowWithLkup (const Nothing)
{-
  =  case tyAppFunArgs t of
       (Ty_Con n,[row]) | hsnIsRec n || hsnIsSum n -> Just row
       _                                           -> Nothing
-}

tyRecRow :: Ty -> Ty
tyRecRow = tyRecRowWithLkup (const Nothing) -- maybe Ty_Any id . tyMbRecRow

tyRowExts :: Ty -> (Ty,AssocL HsName Ty)
tyRowExts = tyRowExtsWithLkup (const Nothing)

tyRecExts :: Ty -> (Ty,AssocL HsName Ty)
tyRecExts = tyRecExtsWithLkup (const Nothing)

tyRowExtr :: HsName -> Ty -> Maybe (Ty,Ty)
tyRowExtr = tyRowExtrWithLkup (const Nothing)

tyRecExtr :: HsName -> Ty -> Maybe (Ty,Ty)
tyRecExtr = tyRecExtrWithLkup (const Nothing)



tyMbRecExts :: Ty -> Maybe (Ty,AssocL HsName Ty)
tyMbRecExts = fmap tyRowExts . tyMbRecRow



tyRecExts2 :: Ty -> AssocL HsName (AssocL HsName Ty)
tyRecExts2
  = assocLMapElt (snd . tyRecExts) . snd . tyRecExts



tyDecomposeMk    :: Ty -> (Ty, TyL, Ty -> TyL -> Ty)
tyDecomposeMk t
  = case t of
      Ty_TBind q v k t' -> (f,as,\f' as' -> Ty_TBind q v k $ mk f' as')
                        where (f,as,mk) = tyDecomposeMk t'
      Ty_App _ _        -> (f,as,\f' as' -> mkApp (f':as'))
                        where (f,as) = tyAppFunArgs t
      Ty_Ext _ _ _      -> (r,assocLElts e,\r' e' -> mkTyRow r' $ zip (assocLKeys e) e')
                        where (r,e) = tyRowExts t
      _ | not (null an) -> (t2,as,\f' as' -> mk1 $ mk2 f' as')
        | otherwise     -> (t,[],const)
                        where (t1,an,mk1) = tyAnnDecomposeMk t
                              (t2,as,mk2) = tyDecomposeMk    t1



-- | Map 'f' over fields of record
tyRecMap :: (Ty -> Ty) -> Ty -> Ty
tyRecMap f r
  = mk rem (map f fs)
  where (rem,fs,mk) = tyDecomposeMk r



tyRowIsCanonOrdered :: AssocL HsName a -> Bool
tyRowIsCanonOrdered = isSortedByOn rowLabCmp fst



rowExtCmp :: (HsName,a) -> (HsName,a) -> Ordering
rowExtCmp (n1,_) (n2,_) = n1 `rowLabCmp` n2

tyRowCanonOrderOn :: (o -> HsName) -> AssocL o a -> AssocL o a
tyRowCanonOrderOn sel = sortByOn rowLabCmp (sel . fst)

tyRowCanonOrderBy :: (o -> o -> Ordering) -> AssocL o a -> AssocL o a
tyRowCanonOrderBy = rowCanonOrderBy

tyRowCanonOrder :: AssocL HsName a -> AssocL HsName a
tyRowCanonOrder = tyRowCanonOrderBy rowLabCmp

tyRowCanonLblOrder :: [HsName] -> [HsName]
tyRowCanonLblOrder = sortBy rowLabCmp



tyRowOffsetOrder :: (a -> Int) -> AssocL HsName a -> AssocL HsName a
tyRowOffsetOrder off = sortOn (off . snd)



tyExtsOffset :: HsName -> AssocL HsName a -> (Int,Presence)
tyExtsOffset lbl exts
  = find 0 lbl exts
  where find o l (e:es) = case (l,panic "Ty.tyExtsOffset") `rowExtCmp` e of
                            GT -> find (o+1) l es
                            EQ -> (o,Present)
                            LT -> (o,Absent)
        find o _ []     = (o,Absent)

tyRecOffset :: HsName -> Ty -> Int
tyRecOffset lbl t
  = fst $ tyExtsOffset lbl $ tyRowCanonOrder exts
  where (_,exts) = tyRecExts t



tyRecOffsetWithLkup :: (TyVarId -> Maybe Ty) -> HsName -> Ty -> Int
tyRecOffsetWithLkup lookup nm
  = tyVarLift lookup o o
  where o = tyRecOffset nm



tyPred :: Ty -> Pred
tyPred t
  =  case tyUnAnn t of
       Ty_Pred pt  -> pt
       _           -> Pred_Pred t

predNm :: Pred -> HsName
predNm = tyAppFunConNm . predTy

tyPredNm :: Ty -> HsName
tyPredNm = predNm . tyPred

tyPrArrowArgsRes :: Ty -> ([Pred],Pred)
tyPrArrowArgsRes tp = let (tl,t) = tyArrowArgsRes tp in (map tyPred tl, tyPred t)



-- | extract class name and class args of predicate packages as Ty
tyPredMatchNmArgs :: Ty -> (HsName,[Ty])
tyPredMatchNmArgs = predMatchNmArgs . tyPred

-- | extract class name and class args of predicate
predMatchNmArgs :: Pred -> (HsName,[Ty])
predMatchNmArgs p
  =  case p of
       Pred_Class t    -> (tyAppFunConNm t, tyAppArgs t)
       Pred_Pred  t    -> predMatchNmArgs $ snd $ tyPrArrowArgsRes t
       Pred_Lacks _ (Label_Lab l)  -> (hsnUniqify HsNameUniqifier_LacksLabel l, [])
       Pred_Lacks _ _              -> (mkHNm "_LabVar_", [])      -- necessary? only used by CHR's
       Pred_Eq t1 t2   -> (hsnEqTilde,[t1,t2])



-- | construct for a predicate its corresponding data type
pred2DataTy :: Pred -> Ty
pred2DataTy p
  = mkConApp (hsnClass2Dict n) as
  where (n,as) = predMatchNmArgs p

-- | construct for a predicate packaged as a Ty its corresponding data type
tyPred2DataTy :: Ty -> Ty
tyPred2DataTy = pred2DataTy . tyPred



predTy :: Pred -> Ty
predTy p
  =  case p of
       Pred_Class t    -> t
       Pred_Pred  t    -> t
       Pred_Lacks    t _  -> t
       Pred_Eq t _ -> t  -- does it matter if we return the left or the right type?



predSeqToList :: PredSeq -> [Pred]
predSeqToList (PredSeq_Cons h t) = h : predSeqToList t
predSeqToList _                  = []

predLFlatten :: [Pred] -> [Pred]
predLFlatten
  = concatMap fl
  where fl (Pred_Preds s) = predSeqToList s
        fl p              = [p]



-- | Is runtime evidence required for the predicate?
predHasRuntimeEvidence :: Pred -> Bool
predHasRuntimeEvidence p
  =  case p of
       Pred_Eq _ _ -> False
       _           -> True



implsPredsTailWithLkup' :: (TyVarId -> Maybe Impls) -> PredScope -> Impls -> ([(PredOcc,[ImplsProveOcc])],Impls)
implsPredsTailWithLkup' lookup sc i
  = extr Set.empty i
  where extr vsVisited i
          = case i of
              Impls_Cons _ p pv prange ipos t
                -> ((mkPredOccRng prange p pv sc,ipos) : p',mi)
                where (p',mi) = extr vsVisited t
              _ -> implsTailVarLiftCyc lookup extr ((,) []) vsVisited i

implsPredsTailWithLkup :: (TyVarId -> Maybe Impls) -> PredScope -> Impls -> ([PredOcc],Impls)
implsPredsTailWithLkup lookup sc i
  = (map fst is,t)
  where (is,t) = implsPredsTailWithLkup' lookup sc i

tyImplsWithLkup :: (TyVarId -> Maybe Ty) -> Ty -> Impls
tyImplsWithLkup lookup = tyVarLift lookup tyImpls tyImpls

implsPrIdsWithLkup :: (TyVarId -> Maybe Impls) -> Impls -> [PredOccId]
implsPrIdsWithLkup lookup = map poPoi . fst . implsPredsTailWithLkup lookup initPredScope




tyImpls :: Ty -> Impls
tyImpls
  = extr . tyUnAnn
  where extr (Ty_Impls i) = i

implsPredsTail' :: PredScope -> Impls -> ([(PredOcc,[ImplsProveOcc])],Impls)
implsPredsTail' = implsPredsTailWithLkup' (const Nothing)

implsPredsTail :: PredScope -> Impls -> ([PredOcc],Impls)
implsPredsTail = implsPredsTailWithLkup (const Nothing)

implsPredsMbTail :: Impls -> ([(PredOcc,[ImplsProveOcc])],Maybe Impls)
implsPredsMbTail i =  case implsPredsTail' initPredScope i of
                        (i',t@(Impls_Tail _ _)) -> (i',Just t)
                        (i',   Impls_Nil      ) -> (i',Nothing)

tyImplsPreds :: PredScope -> Ty -> [PredOcc]
tyImplsPreds sc = fst . implsPredsTail sc . tyImpls

implsIsTail :: Impls -> Bool
implsIsTail = isJust . implsMbVar

tyIsImplsTail :: Ty -> Bool
tyIsImplsTail = implsIsTail . tyImpls

implsPrIds :: Impls -> [PredOccId]
implsPrIds = map poPoi . fst . implsPredsTail initPredScope



implsMbVar :: Impls -> Maybe TyVarId
implsMbVar (Impls_Tail v _)  = Just v
implsMbVar _                 = Nothing

implsTailVar :: Impls -> ImplsVarId
implsTailVar = panicJust "implsTailVar" . implsMbVar



implsIsEmpty :: Impls -> Bool
implsIsEmpty (Impls_Cons _ _ _ _ _ _) = False
implsIsEmpty _                        = True



tyIsPredicated :: Ty -> Bool
tyIsPredicated (Ty_Impls i) = not $ implsIsEmpty i
tyIsPredicated t            = isPr a
                            where a = map tyUnAnn $ tyArrowArgs t
                                  isPr (Ty_Pred p:_) = True
                                  isPr _             = False

tyIsPredicatedWithLkup :: (TyVarId -> Maybe Ty) -> Ty -> Bool
tyIsPredicatedWithLkup lookup = tyVarLift lookup tyIsPredicated tyIsPredicated



labelMbVar :: Label -> Maybe TyVarId
labelMbVar (Label_Var v)  = Just v
labelMbVar _              = Nothing



data TyKiKey
  = TyKiKey_Name    !HsName
  | TyKiKey_TyVar   !TyVarId
  deriving (Eq,Ord)

instance Show TyKiKey where
  show (TyKiKey_Name  n) = show n
  show (TyKiKey_TyVar v) = show v



deriving instance Typeable TyKiKey
deriving instance Data TyKiKey



tyKiKeyMbName :: TyKiKey -> Maybe HsName
tyKiKeyMbName (TyKiKey_Name n) = Just n
tyKiKeyMbName _                = Nothing



tyKiKeyIsName :: TyKiKey -> Bool
tyKiKeyIsName = isJust . tyKiKeyMbName



data TyCtxt = TyCtxt_Ty | TyCtxt_Pred | TyCtxt_Class deriving (Show,Eq)





type Polarity = Ty



polCovariant :: Polarity
polCovariant = semCon hsnCovariant

polContravariant :: Polarity
polContravariant = semCon hsnContravariant

polInvariant :: Polarity
polInvariant = semCon hsnInvariant



mkPolNegate :: Polarity -> Polarity
mkPolNegate = mk1ConApp hsnPolNegation

mkPolVar :: UID -> Polarity
mkPolVar = mkTyVar



polIs :: HsName -> Polarity -> Bool
polIs nm = maybe False (== nm) . tyMbCon

polIsCovariant, polIsContravariant, polIsInvariant :: Polarity -> Bool
polIsCovariant     = polIs hsnCovariant
polIsContravariant = polIs hsnContravariant
polIsInvariant     = polIs hsnInvariant



polOpp :: Polarity -> Polarity
polOpp pol | polIsCovariant     pol = polContravariant
           | polIsContravariant pol = polCovariant
           | otherwise              = polInvariant



data FIMode  =  FitSubLR
             |  FitSubRL
             |  FitUnify
             deriving (Eq,Ord)



fimOpp :: FIMode -> FIMode
fimOpp m
  =  case m of
       FitSubLR  -> FitSubRL
       FitSubRL  -> FitSubLR
       _         -> m



fimSwapPol :: Polarity -> FIMode -> FIMode
fimSwapPol pol m = if polIsContravariant pol then fimOpp m else m



instance Show FIMode where
  show FitSubLR  = "<="
  show FitSubRL  = ">="
  show FitUnify  = "=="



data InstTo
  = InstTo_Plain        				-- a plain value
  | InstTo_Qu                           -- the fresh type (tyvar) instantiated to
      { instoQu     	:: TyQu			-- how tvar was quantified, also includes the meta level
      , instoFrom   	:: TyVarId		-- the tvar from which is instantiated
      , instoTo     	:: TyVarId		-- the new tvar to which is instantiated
      , instoL1     	:: Ty
      }
{-
  | InstTo_Lam                          -- a lambda
      { instoLam    :: [InstTo]
      }
-}
  deriving Show



instToIsQu :: InstTo -> Bool
instToIsQu (InstTo_Qu _ _ _ _) = True
instToIsQu _                   = False



-- split of initial quantifier instantiations, to be used for Sys F generation for type parameterization
instToSplitQu :: [InstTo] -> ([InstTo],[InstTo])
instToSplitQu = span instToIsQu



-- get tvar -> kind bindings of instantiation
instToL1AssocL :: [InstTo] -> AssocL TyVarId (MetaLev,Ty)
instToL1AssocL l = [ (v,(tyquMetaLev q,k)) | (InstTo_Qu q _ v k) <- l ]



-- a for the time being placeholder for yet unresolved/unpropagated type
tyErr :: String -> Ty
tyErr s = Ty_Dbg $ "*ERR: " ++ s ++ " :*"



instance Serialize TyKiKey where
  sput (TyKiKey_Name  a) = sputWord8 0 >> sput a
  sput (TyKiKey_TyVar a) = sputWord8 1 >> sput a
  sget = do t <- sgetWord8
            case t of
              0 -> liftM TyKiKey_Name  sget
              1 -> liftM TyKiKey_TyVar sget



instance Binary ImplsProveOcc where
  put (ImplsProveOcc a b) = put a >> put b
  get = liftM2 ImplsProveOcc get get

instance Serialize ImplsProveOcc where
  sput = sputPlain
  sget = sgetPlain

instance Binary PredScope where
  put (PredScope_Lev   a      ) = putWord8 0  >> put a
  put (PredScope_Var   a      ) = putWord8 1  >> put a
  get = do tag <- getWord8
           case tag of
             0  -> liftM  PredScope_Lev   get
             1  -> liftM  PredScope_Var   get

instance Serialize PredScope where
  sput = sputPlain
  sget = sgetPlain

instance Binary CHRPredOccCxt where
  put (CHRPredOccCxt_Scope1 a) = put a
  get = liftM CHRPredOccCxt_Scope1 get

instance Serialize CHRPredOccCxt where
  sput = sputPlain
  sget = sgetPlain

instance Binary LabelOffset where
  put (LabelOffset_Off a) = putWord8 0 >> put a
  put (LabelOffset_Var a) = putWord8 1 >> put a
  get = do t <- getWord8
           case t of
             0 -> liftM LabelOffset_Off get
             1 -> liftM LabelOffset_Var get

instance Serialize LabelOffset where
  sput = sputPlain
  sget = sgetPlain




instance Serialize Ty where
  sput (Ty_Con   a      ) = sputWord8 0  >> sput a
  sput (Ty_App   a b    ) = sputWord8 1  >> sput a >> sput b
  sput (Ty_Ann   a b    ) = sputWord8 2  >> sput a >> sput b
  sput (Ty_Var   a b    ) = sputWord8 3  >> sput a >> sput b
  sput (Ty_Any          ) = sputWord8 4
  sput (Ty_TBind a b c d) = sputWord8 5  >> sput a >> sput b >> sput c >> sput d
  sput (Ty_Ext   a b c  ) = sputWord8 6  >> sput a >> sput b >> sput c
  sput (Ty_Pred  a      ) = sputWord8 7  >> sput a
  sput (Ty_Lam   a b    ) = sputWord8 8  >> sput a >> sput b
  sput (Ty_Impls a      ) = sputWord8 9  >> sput a
  sput (Ty_Dbg   a      ) = sputWord8 10 >> sput a
  sget = do tag <- sgetWord8
            case tag of
              0  -> liftM  Ty_Con   sget
              1  -> liftM2 Ty_App   sget sget
              2  -> liftM2 Ty_Ann   sget sget
              3  -> liftM2 Ty_Var   sget sget
              4  -> return Ty_Any
              5  -> liftM4 Ty_TBind sget sget sget sget
              6  -> liftM3 Ty_Ext   sget sget sget
              7  -> liftM  Ty_Pred  sget
              8  -> liftM2 Ty_Lam   sget sget
              9  -> liftM  Ty_Impls sget
              10 -> liftM  Ty_Dbg   sget

instance Serialize TyAnn where
  sput (TyAnn_Empty       ) = sputWord8 0
  sput (TyAnn_Mono        ) = sputWord8 1
  sput (TyAnn_Strictness a) = sputWord8 2  >> sput a
  sget = do tag <- sgetWord8
            case tag of
              0 -> return TyAnn_Empty
              1 -> return TyAnn_Mono
              2 -> liftM  TyAnn_Strictness sget

instance Binary TyVarCateg where
  put = putEnum8
  get = getEnum8

instance Serialize TyVarCateg where
  sput = sputPlain
  sget = sgetPlain

instance Binary TyQu where
  put (TyQu_Forall a) = putWord8 0 >> put a
  put (TyQu_Exists a) = putWord8 1 >> put a
  put (TyQu_Plain  a) = putWord8 1 >> put a
  get = do tag <- getWord8
           case tag of
             0 -> liftM  TyQu_Forall   get
             1 -> liftM  TyQu_Exists   get
             2 -> liftM  TyQu_Plain    get

instance Serialize TyQu where
  sput = sputPlain
  sget = sgetPlain

instance Serialize Pred where
  sput (Pred_Class  a  ) = sputWord8 0 >> sput a
  sput (Pred_Pred   a  ) = sputWord8 1 >> sput a
  sput (Pred_Lacks  a b) = sputWord8 2 >> sput a >> sput b
  sput (Pred_Arrow  a b) = sputWord8 3 >> sput a >> sput b
  sput (Pred_Eq     a b) = sputWord8 4 >> sput a >> sput b
  sput (Pred_Var    a  ) = sputWord8 5 >> sput a
  sput (Pred_Preds  a  ) = sputWord8 6 >> sput a
  sget = do tag <- sgetWord8
            case tag of
              0 -> liftM  Pred_Class   sget
              1 -> liftM  Pred_Pred    sget
              2 -> liftM2 Pred_Lacks   sget sget
              3 -> liftM2 Pred_Arrow   sget sget
              4 -> liftM2 Pred_Eq      sget sget
              5 -> liftM  Pred_Var     sget
              6 -> liftM  Pred_Preds   sget

instance Serialize Label where
  sput (Label_Lab  a) = sputWord8 0 >> sput a
  sput (Label_Var  a) = sputWord8 1 >> sput a
  sget = do tag <- sgetWord8
            case tag of
              0 -> liftM  Label_Lab   sget
              1 -> liftM  Label_Var   sget

instance Serialize PredSeq where
  sput (PredSeq_Nil          ) = sputWord8 0
  sput (PredSeq_Cons  a b    ) = sputWord8 1  >> sput a >> sput b
  sput (PredSeq_Var   a      ) = sputWord8 2  >> sput a
  sget = do tag <- sgetWord8
            case tag of
              0  -> return PredSeq_Nil
              1  -> liftM2 PredSeq_Cons  sget sget
              2  -> liftM  PredSeq_Var   sget

{-
instance Binary PredSeq where
  put = putList (PredSeq_Nil ==) (\(PredSeq_Cons a b) -> (a,b))
  get = getList PredSeq_Nil PredSeq_Cons
-}

instance Serialize CHRPredOcc where
  -- Range info is stripped on the fly
  sput (CHRPredOcc a b _) = sput a >> sput b
  -- Range info is stripped on the fly
  sget = liftM2 (\a b -> CHRPredOcc a b emptyRange) sget sget

instance Serialize Impls where
  sput (Impls_Nil             ) = sputWord8 0
  sput (Impls_Tail a b        ) = sputWord8 1  >> sput a >> sput b
  sput (Impls_Cons a b c d e f) = sputWord8 2  >> sput a >> sput b >> sput c >> sput d >> sput e >> sput f
  sget = do tag <- sgetWord8
            case tag of
              0  -> return Impls_Nil
              1  -> liftM2 Impls_Tail  sget sget
              2  ->
                    liftM6 Impls_Cons  sget sget sget sget sget sget




{-|
There are some conventions/restrictions on the structure of types that are not enforced
by the abstract syntax:

Encoding of prove-constraints:
  concrete syntax:
    {! impls !} -> ty
  abstract syntax:
    Ty_App (Ty_App (Ty_Con "->") (Ty_Impls impls)) ty

Encoding of assume-constraints:
  concrete syntax:
    (ty, {! pred1 !}, ..., {! predn !})
  abstract syntax:
    Ty_Ext (... (Ty_Ext ty (prod m+1) (Ty_Pred pred_1) ) ...) (prod m+n) (Ty_Pred pred_n)

  In other words: the predicates are at the outset of a product, pred_n "more outermost"
  than pred_{n-1}.

-}


{-|
The basic alternatives encode the following:
- Con: data type constructors, including tuple constructors
- App: application to 1 argument, for example 'a -> b' is encoded as (App (App -> a) b)
- Any: representing Bot/Top depending on context: (1) unknown expected type, (2) error type
- Var: type variables, including a category: plain tyvars, fixed tyvars (aka skolems)

-}
-- Impls -------------------------------------------------------
data Impls  = Impls_Cons !(ImplsVarId) !(Pred ) !(PredOccId) !(Range) !(([ImplsProveOcc])) !(Impls ) 
            | Impls_Nil 
            | Impls_Tail !(ImplsVarId) !(([ImplsProveOcc])) 
            deriving ( Data,Eq,Ord,Show,Typeable)
-- Label -------------------------------------------------------
data Label  = Label_Lab !(HsName) 
            | Label_Var !(LabelVarId) 
            deriving ( Data,Eq,Ord,Show,Typeable)
-- LabelAGItf --------------------------------------------------
data LabelAGItf  = LabelAGItf_AGItf !(Label ) 
                 deriving ( Data,Eq,Ord,Show,Typeable)
-- Pred --------------------------------------------------------
data Pred  = Pred_Arrow !(PredSeq ) !(Pred ) 
           | Pred_Class !(Ty ) 
           | Pred_Eq !(Ty ) !(Ty ) 
           | Pred_Lacks !(Ty ) !(Label ) 
           | Pred_Pred !(Ty ) 
           | Pred_Preds !(PredSeq ) 
           | Pred_Var !(TyVarId) 
           deriving ( Data,Eq,Ord,Show,Typeable)
-- PredSeq -----------------------------------------------------
data PredSeq  = PredSeq_Cons !(Pred ) !(PredSeq ) 
              | PredSeq_Nil 
              | PredSeq_Var !(TyVarId) 
              deriving ( Data,Eq,Ord,Show,Typeable)
-- Ty ----------------------------------------------------------
data Ty  = Ty_Ann !(TyAnn ) !(Ty ) 
         | Ty_Any 
         | Ty_App !(Ty ) !(Ty ) 
         | Ty_Con !(HsName) 
         | Ty_Dbg !(String) 
         | Ty_Ext !(Ty ) !(HsName) !(Ty ) 
         | Ty_Impls !(Impls ) 
         | Ty_Lam !(TyVarId) !(Ty ) 
         | Ty_Pred !(Pred ) 
         | Ty_TBind !(TyQu ) !(TyVarId) !(Ty) !(Ty ) 
         | Ty_Var !(TyVarId) !(TyVarCateg ) 
         deriving ( Data,Eq,Ord,Show,Typeable)
-- TyAGItf -----------------------------------------------------
data TyAGItf  = TyAGItf_AGItf !(Ty ) 
              deriving ( Data,Eq,Ord,Show,Typeable)
-- TyAnn -------------------------------------------------------
data TyAnn  = TyAnn_Empty 
            | TyAnn_Mono 
            | TyAnn_Strictness !(Strictness) 
            deriving ( Data,Eq,Ord,Show,Typeable)
-- TyQu --------------------------------------------------------
data TyQu  = TyQu_Exists !(MetaLev) 
           | TyQu_Forall !(MetaLev) 
           | TyQu_Plain !(MetaLev) 
           deriving ( Data,Eq,Ord,Show,Typeable)
-- TyVarCateg --------------------------------------------------
data TyVarCateg  = TyVarCateg_Fixed 
                 | TyVarCateg_Meta 
                 | TyVarCateg_Plain 
                 deriving ( Data,Enum,Eq,Ord,Show,Typeable)