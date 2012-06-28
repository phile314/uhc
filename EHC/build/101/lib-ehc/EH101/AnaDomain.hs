

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/AnaDomain.ag)
module EH101.AnaDomain(QualAGItf (..), TyAGItf (..), CoeAGItf (..)
, AnaEval (..), AnaEvalL, RelevTy (..), RelevTyL, MbRelevTy, RelevQual (..), RelevQualL, RelevCoe (..), RelevCoeL
, RQuant (..)
, RelevQualS
, (<.>)
, relevtyAnaEval
, AnaLattice (..)
, freshStrict, freshLazy
, anaMkBotFun
, relevCoeUnCompose
, relevCoeToComposeList
, RVarMpInfo (..)
, rvmiMbEval
, RVarMp, emptyRVarMp
, rvarmpEvalUnit
, rvarmpKeysSet, rvarmpUnions
, AnaMatchState (..), AMS, emptyAnaMatchState
, AMSOut (..), emptyAMSOut, amsoMkOk
, amsLE, amsRunMb) where

import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Opts.Base
import EH101.VarMp
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Control.Monad.State hiding (join)
import Control.Applicative
import EH.Util.Utils
import EH101.Base.Debug
import EH.Util.Pretty
import Control.Monad hiding (join)
import EH101.Base.Serialize
import EH101.Base.Binary (liftM6)
import Data.Typeable (Typeable)
import Data.Generics (Data)





















-- | quantifier for RelevTy_Fun
data RQuant
  = RQuant_Forall		-- the usual universal instantiation
  | RQuant_Rec			-- quantified for mutual recursive use, instantiates differently
  | RQuant_None			-- not quantified
  deriving
    ( Eq, Ord, Enum
    , Typeable, Data
    )



instance Show RQuant where
  show RQuant_Forall 	= "A"
  show RQuant_Rec 		= "R"
  show RQuant_None    	= ""



type RelevQualS = Set.Set RelevQual



infixl 3 `RelevCoe_Comp`
infixl 3 <.>

-- | smart variant of RelevCoe_Comp
(<.>) :: RelevCoe -> RelevCoe -> RelevCoe
RelevCoe_Id           <.> r                   = r
l                     <.> RelevCoe_Id         = l
RelevCoe_Eval   ll lr <.> RelevCoe_Eval rl rr
  | lr == rl                                  = RelevCoe_Eval ll rr
RelevCoe_CastTy ll (RelevTy_Ana lr)
                      <.> RelevCoe_Eval rl rr
  | lr == rl                                  = RelevCoe_CastTy ll (RelevTy_Ana rr)
l                     <.> RelevCoe_Comp rl rr = (l <.> rl) `RelevCoe_Comp` rr
RelevCoe_Comp   ll lr <.> r                   = ll `RelevCoe_Comp` (lr <.> r)
l                     <.> r                   = RelevCoe_Comp l r



relevtyAnaEval (RelevTy_Ana e) = e
relevtyAnaEval _               = panic "AnaDomain.relevtyAnaEval"



class AnaLattice a where
  top   :: a
  bot   :: a

  isTop :: a -> Bool
  isBot :: a -> Bool
  isVar :: a -> Maybe UID

  fresh :: UID -> a

  meet  :: a -> a -> AMS (Maybe a)
  join  :: a -> a -> AMS (Maybe a)

  -- base <= base
  leBB_l :: a -> a -> AMS (AMSOut a)
  leBB_r :: a -> a -> AMS (AMSOut a)
  leBB_l = leBB
  leBB_r = leBB

  -- base <= base
  leBB :: a -> a -> AMS (AMSOut a)
  leBB = leBB_l

  -- base <= fun, default compares to fun result
  leBF :: a -> [a] -> a -> AMS (AMSOut a)
  leBF b fa fr = leBB b fr



-- | wrap action inside var looking up
anaVar
  :: AnaLattice x
     => (UID -> AnaMatchState -> Maybe x)       -- lookup
     -> res                          			-- when vars are equal anyway
     -> (x -> AMS res)                          -- when var found
     -> (x -> x -> AMS res)                     -- when var is not found
     -> UID -> x -> x
     -> AMS res
anaVar lkupVar dflt found notfound v1 a1 a2
  = do { s <- get
       ; let isV2@(~(Just v2)) = isVar a2
       ; if isJust isV2 && v1 == v2
         then return dflt
         else case lkupVar v1 s of
                Just a1' -> found    a1'
                _        -> notfound a1 a2
       }

anaBind
  :: (UID -> x -> RVarMp)
     -> (x -> res)
     -> UID -> x
     -> AMS res
anaBind mkVM mkRes v1 a2
  = do { s <- get
       ; put (amsBind (mkVM v1) a2 s)
       ; return $ mkRes a2
       }



instance AnaLattice AnaEval where
  top   = AnaEval_Lazy
  bot   = AnaEval_WHNF

  isTop AnaEval_Lazy = True
  isTop _            = False

  isBot AnaEval_WHNF = True
  isBot _            = False

  isVar (AnaEval_Var v) = Just v
  isVar _               = Nothing

  fresh = AnaEval_Var

  meet a1@(AnaEval_Var v1) a2               = anaVar amsLookupEval (Just a1) (meet a2) anaEvalMeet v1 a1 a2
  meet a1 a2@(AnaEval_Var v2)               = anaVar amsLookupEval (Just a1) (meet a1) anaEvalMeet v2 a2 a1
  meet a1 a2                                = anaEvalMeet a1 a2

  join a1@(AnaEval_Var v1) a2               = anaVar amsLookupEval (Just a1) (join a2) anaEvalJoin v1 a1 a2
  join a1 a2@(AnaEval_Var v2)               = anaVar amsLookupEval (Just a1) (join a1) anaEvalJoin v2 a2 a1
  join a1 a2                                = anaEvalJoin a1 a2

  leBB_l a1@(AnaEval_Var v1) a2             = anaVar amsLookupEval (amsoMkOk a1 a1 $ RelevCoe_Eval a1 a1) (flip leBB_l a2)       leBB_r     v1 a1 a2
  leBB_l a1 a2                              = leBB_r a1 a2
  leBB_r a1 a2@(AnaEval_Var v2)             = anaVar amsLookupEval (amsoMkOk a1 a1 $ RelevCoe_Eval a1 a1) (     leBB_r a1) (flip anaEvalLE) v2 a2 a1
  leBB_r a1 a2                              = anaEvalLE a1 a2




-- | meet or join for 2 element lattice (only bot+top), nothing in between
anaMeetOrJoin
  :: AnaLattice x
     => (UID -> x -> RVarMp)		-- make var mapping (subst)
     -> Bool						-- is meet (True) or join (False)
     -> x -> x
     -> AMS (Maybe x)
anaMeetOrJoin mkVM isM a1 a2
  = mt False a1 a2
  where mt flip a1 a2
          | isB && isM  = return $ Just a1
          | isB         = return $ Just a2
          | isT && isM  = return $ Just a2
          | isT         = return $ Just a1
          | not flip    = mt True a2 a1
          | isJust m1 && isJust m2
                        = do { s <- get
                             ; let bound = amsBoundS s
                             ;      if not (v1 `Set.member` bound) then bind v1 a2
                               else if not (v2 `Set.member` bound) then bind v2 a1
                               else return Nothing
                             }
          | otherwise   = return Nothing
                        where m1@(~(Just v1)) = isVar a1
                              m2@(~(Just v2)) = isVar a2
                              -- areV = isJust m1 && isJust m2
                              isB  = isBot a1
                              isT  = isTop a1
        bind = anaBind mkVM Just

-- | meet, join, specialized for AnaEval
anaEvalMeet, anaEvalJoin :: AnaEval -> AnaEval -> AMS (Maybe AnaEval)
anaEvalMeet = anaMeetOrJoin rvarmpEvalUnit True
anaEvalJoin = anaMeetOrJoin rvarmpEvalUnit False

anaEvalLE :: AnaEval -> AnaEval -> AMS (AMSOut AnaEval)
anaEvalLE a1 a2
  = le a1 a2
  where le a1@(AnaEval_Var v1) a2@(AnaEval_WHNF  )   = bind  v1 a2                        -- can't get lower than bot
        le a1@(AnaEval_Var v1) a2                    = delay a1 a2                        -- rest is delayed
        le a1@(AnaEval_Lazy  ) a2@(AnaEval_Var v2)   = bind  v2 a1                        -- can't get higher than top
        le a1                  a2@(AnaEval_Var v2)   = delay a1 a2                        -- rest is delayed
        le a1@(AnaEval_WHNF  ) a2@(AnaEval_WHNF  )   = return $ amsoMkOk a1 a2 (RelevCoe_Eval a1 a2)
        le a1@(AnaEval_WHNF  ) a2@(AnaEval_Lazy  )   = coe   a1 a2                        -- forget that it is alreay evaluated
        le a1@(AnaEval_Lazy  ) a2@(AnaEval_Lazy  )   = return $ amsoMkOk a1 a2 (RelevCoe_Eval a1 a2)
        le a1@(AnaEval_Lazy  ) a2@(AnaEval_WHNF  )   = coe' RelevCoe_Cast a1 a2           -- permitted, but not without marking as casting required, according to lattice
        le a1                  a2                    = return $ amsoMkFail -- Ok a1 a2 (RelevCoe_Err "anaEvalLE")

        bind = anaBind rvarmpEvalUnit (\a2 -> amsoMkOk a2 a2 $ RelevCoe_Eval a2 a2)

        -- | coerce
        coe' wrap l h = return $ amsoMkOk l h (RelevCoe_Eval l h)
        coe = coe' id

        -- | postpone by generating a qualification, to be solved later, or to end up in a signature
        delay l h
          = do { s <- get
               ; put (amsQual (RelevQual_SubEval l h) s)
               ; return $ amsoMkOk l h (RelevCoe_Eval l h)
               }



-- | Only valid for RelevTy_Ana as aggregrate of domains
instance AnaLattice RelevTy where
  top       = RelevTy_Ana top
  bot       = RelevTy_Ana bot

  isTop (RelevTy_Ana a1) = isTop a1
  isBot (RelevTy_Ana a1) = isBot a1
  isVar _                = Nothing
  -- isVar (RelevTy_Ana a1) = do { v1 <- isVar a1 ; return v1 }

  fresh u   = RelevTy_Ana (fresh u1)
            where (_,u1) = mkNewLevUID u

  meet  (RelevTy_Ana a11) (RelevTy_Ana a21) = liftM (fmap RelevTy_Ana) (meet  a11 a21)
  meet  _                 _                 = return Nothing

  join  (RelevTy_Ana a11) (RelevTy_Ana a21) = liftM (fmap RelevTy_Ana) (join  a11 a21)
  join  _                 _                 = return Nothing

  leBB  (RelevTy_Ana a11) (RelevTy_Ana a21) = do { AMSOut l h c True <- leBB a11 a21
                                                 ; return $ amsoMkOk (RelevTy_Ana l) (RelevTy_Ana h) c
                                                 }
  leBB  (RelevTy_Fun _ _ _ a1 r1) (RelevTy_Fun _ _ _ a2 r2)
                                            = do { r_amso <- leBB r1 r2
                                                 ; a_amso <- mapM (uncurry leBB) (zip a2 a1)
                                                 ; return $ amsoMk (RelevTy_Fun  RQuant_None [] [] (map amsoHi  a_amso) (amsoLo  r_amso))
                                                                   (RelevTy_Fun  RQuant_None [] [] (map amsoLo  a_amso) (amsoHi  r_amso))
                                                                   (RelevCoe_Fun                   (map amsoCoe a_amso) (amsoCoe r_amso))
                                                                   (and $ amsoIsOk r_amso : map amsoIsOk a_amso)
                                                 }
  -- others lead to a constraint between t1 and function result, with a coercion between t1 and whole function t2
  -- Note: this has to be delegated to actual AnaDomain...
  {-
  leBB  t1 t2@(RelevTy_Fun _ _ a2 r2)
                                            = do { f_amso <- leBF t1 a2 r2
                                                 ; let t1' = anaMkBotFun (length a2)
                                                 ; return $ AMSOut (RelevTy_Fun  [] [] (map amsoHi  a_amso) (amsoLo  r_amso))
                                                                   (RelevTy_Fun  [] [] (map amsoLo  a_amso) (amsoHi  r_amso))
                                                                   (RelevCoe_Fun       (map amsoCoe a_amso) (amsoCoe r_amso))
                                                 }
  -}
  leBB  _                 _                 = return $ amsoMkFail -- Ok (RelevTy_Err "leBB.lo") (RelevTy_Err "leBB.hi") (RelevCoe_Err "leBB")

  {-
  leBB  (RelevTy_Ana a11) (RelevTy_Ana a21) = do { AMSOut l h c <- leBB a11 a21
                                                 ; return $ AMSOut (RelevTy_Ana l) (RelevTy_Ana h) c
                                                 }
  leBB  _                 _                 = return $ AMSOut (RelevTy_Err "leBB.lo") (RelevTy_Err "leBB.hi") (RelevCoe_Err "leBB")
  -}



-- | bottom only for AnaEval lattice, otherwise fresh
freshStrict :: UID -> RelevTy
freshStrict u
  = case fresh u of
      RelevTy_Ana _ -> RelevTy_Ana bot

-- | top only for AnaEval lattice, otherwise fresh
freshLazy :: UID -> RelevTy
freshLazy u
  = case fresh u of
      RelevTy_Ana _ -> RelevTy_Ana top



-- | construct 'worst case' assuming function signature, where we assume function assumes nothing and we cannot assume anything about result, except for the result being bottom (i.e. strict)
anaMkBotFun :: Int -> RelevTy
anaMkBotFun arity = RelevTy_Fun RQuant_None [] [] (replicate arity top) bot



-- | decompose RelevCoe composition in top and below
relevCoeUnCompose :: RelevCoe -> (RelevCoe,[RelevCoe])
relevCoeUnCompose (RelevCoe_Comp l r)
  = (l, u r)
  where u (RelevCoe_Comp l r) = l : u r
        u c                   = [c]
relevCoeUnCompose c = (c, [])



-- | decompose RelevCoe composition in list of subsequently applied coe's
relevCoeToComposeList :: RelevCoe -> [RelevCoe]
relevCoeToComposeList c
  = u [] c
  where u a (RelevCoe_Comp l r) = let a' = u a r in u a' l
        u a c                   = [c] ++ a



data RVarMpInfo             -- for all types of Var alternatives an alternative here
  = RVMIEval    AnaEval



rvmiMbEval :: RVarMpInfo -> Maybe AnaEval
rvmiMbEval (RVMIEval x) = Just x
-- for now, until more alternatives are in it
-- rvmiMbEval _            = Nothing



type RVarMp  = VarMpStk' UID RVarMpInfo

emptyRVarMp :: RVarMp
emptyRVarMp = emptyVarMpStk



rvarmpEvalUnit :: UID -> AnaEval -> RVarMp
-- poor mens occurcheck...
-- rvarmpEvalUnit v (AnaEval_Var v2) | v == v2  = emptyRVarMp
rvarmpEvalUnit v i                           = varmpstkUnit v (RVMIEval i)  -- [mkVarMp (Map.fromList [(v,RVMIEval i)])]



-- | renaming
rvarmpKeysSet :: RVarMp -> Set.Set UID
rvarmpKeysSet = varmpstkKeysSet

rvarmpUnions :: [RVarMp] -> RVarMp
rvarmpUnions = varmpstkUnions



-- | state maintained during matching
data AnaMatchState
  = AnaMatchState
      { amsBoundS           :: !UIDS        -- vars which are already bound
      , amsOuterVarMp       :: !RVarMp      -- as known from outside matching
      , amsLocalVarMp       :: !RVarMp      -- as gathered during a match (1 invocation of runState)
      , amsGathQual         :: !RelevQualS  -- gathered constraints
      }

type AMS    a = State AnaMatchState a

emptyAnaMatchState :: AnaMatchState
emptyAnaMatchState = AnaMatchState Set.empty emptyRVarMp emptyRVarMp Set.empty

instance Show AnaMatchState where
  show x = "AMS " ++ s (amsOuterVarMp x) ++ s (amsLocalVarMp x)
    where s m = show (assocLMapElt rvmiMbEval $ varmpstkToAssocL m)



-- | direct (non state) output of matching
data AMSOut a
  = AMSOut
      { amsoLo      :: a
      , amsoHi      :: a
      , amsoCoe     :: RelevCoe
      , amsoIsOk	:: Bool
      }

emptyAMSOut :: AnaLattice a => AMSOut a
emptyAMSOut = AMSOut bot top RelevCoe_Id True

amsoMk :: a -> a -> RelevCoe -> Bool -> AMSOut a
amsoMk l h c ok = AMSOut l h c ok

amsoMkOk :: a -> a -> RelevCoe -> AMSOut a
amsoMkOk l h c = amsoMk l h c True

amsoMkFail :: AnaLattice a => AMSOut a
amsoMkFail = emptyAMSOut {amsoIsOk = False}

instance Show a => Show (AMSOut a) where
  show x = "AMSOut (" ++ show (amsoLo x) ++ " <= " ++ show (amsoHi x) ++ ")"



amsBind :: (x -> RVarMp) -> x -> AnaMatchState -> AnaMatchState
amsBind b a s = s {amsLocalVarMp = b a |+> amsLocalVarMp s}

amsQual :: RelevQual -> AnaMatchState -> AnaMatchState
amsQual q s = s {amsGathQual = Set.insert q (amsGathQual s)}



-- | run leBB
amsLE :: AnaLattice x => RVarMp -> x -> x -> (AMSOut x,AnaMatchState)
amsLE m x1 x2 = runState (leBB x1 x2) (emptyAnaMatchState {amsOuterVarMp = m})

-- | run a Maybe returning AMS
amsRunMb :: AnaMatchState -> AMS (Maybe x) -> Maybe (x,AnaMatchState)
amsRunMb s ams
  = case runState ams s of
      (Just x, s) -> Just (x,s)
      _           -> Nothing



-- | Lookup in VarMp's of AnaMatchState
amsLookup :: UID -> AnaMatchState -> Maybe RVarMpInfo
amsLookup i s = varmpLookup i (amsOuterVarMp s) <|> varmpLookup i (amsLocalVarMp s)

amsLookupEval :: UID -> AnaMatchState -> Maybe AnaEval
amsLookupEval i s = do { i <- amsLookup i s ; rvmiMbEval i }



instance Serialize RQuant where
  sput = sputEnum8
  sget = sgetEnum8

instance Serialize RelevQual where
  sput (RelevQual_SubEval a b        ) = sputWord8 0 >> sput a >> sput b
  -- sput (RelevQual_Alt     a b c d e f) = sputWord8 1 >> sput a >> sput b >> sput c >> sput d >> sput e >> sput f
  sget
    = do t <- sgetWord8
         case t of
            0 -> liftM2 RelevQual_SubEval         sget sget
            -- 1 -> liftM6 RelevQual_Alt             sget sget sget sget sget sget

instance Serialize RelevTy where
  sput (RelevTy_Ana   a        ) = sputWord8 0 >> sput a
  sput (RelevTy_Fun   a b c d e) = sputWord8 1 >> sput a >> sput b >> sput c >> sput d >> sput e
  sput (RelevTy_None           ) = sputWord8 2
  sput (RelevTy_Err   a        ) = sputWord8 3 >> sput a
  sget
    = do t <- sgetWord8
         case t of
            0 -> liftM  RelevTy_Ana         sget
            1 -> liftM5 RelevTy_Fun         sget sget sget sget sget
            2 -> return RelevTy_None
            3 -> liftM  RelevTy_Err         sget

instance Serialize RelevCoe where
  sput (RelevCoe_Id             ) = sputWord8 0
  sput (RelevCoe_Err     a      ) = sputWord8 1 >> sput a
  sput (RelevCoe_Comp    a b    ) = sputWord8 2 >> sput a >> sput b
  sput (RelevCoe_Fun     a b    ) = sputWord8 3 >> sput a >> sput b
  sput (RelevCoe_Cast    a      ) = sputWord8 4 >> sput a
  sput (RelevCoe_Eval    a b    ) = sputWord8 5 >> sput a >> sput b
  sput (RelevCoe_CastTy  a b    ) = sputWord8 6 >> sput a >> sput b
  sget
    = do t <- sgetWord8
         case t of
            0 -> return RelevCoe_Id
            1 -> liftM  RelevCoe_Err         sget
            2 -> liftM2 RelevCoe_Comp        sget sget
            3 -> liftM2 RelevCoe_Fun         sget sget
            4 -> liftM  RelevCoe_Cast        sget
            5 -> liftM2 RelevCoe_Eval        sget sget
            6 -> liftM2 RelevCoe_CastTy      sget sget

instance Serialize AnaEval where
  sput (AnaEval_Var   a      ) = sputWord8 0 >> sput a
  sput (AnaEval_WHNF         ) = sputWord8 1
  sput (AnaEval_Lazy         ) = sputWord8 2
  sput (AnaEval_Meet  a      ) = sputWord8 3 >> sput a
  sput (AnaEval_Join  a      ) = sputWord8 4 >> sput a
  sget
    = do t <- sgetWord8
         case t of
            0 -> liftM  AnaEval_Var         sget
            1 -> return AnaEval_WHNF
            2 -> return AnaEval_Lazy
            3 -> liftM  AnaEval_Meet        sget
            4 -> liftM  AnaEval_Join        sget

-- AnaEval -----------------------------------------------------
data AnaEval  = AnaEval_Join !(AnaEvalL ) 
              | AnaEval_Lazy 
              | AnaEval_Meet !(AnaEvalL ) 
              | AnaEval_Var !(UID) 
              | AnaEval_WHNF 
              deriving ( Data,Eq,Ord,Show,Typeable)
-- AnaEvalL ----------------------------------------------------
type AnaEvalL  = [AnaEval ]
-- CoeAGItf ----------------------------------------------------
data CoeAGItf  = CoeAGItf_AGItf !(RelevCoe ) 
               deriving ( Data,Eq,Ord,Show,Typeable)
-- MbRelevTy ---------------------------------------------------
type MbRelevTy  = Maybe RelevTy 
-- QualAGItf ---------------------------------------------------
data QualAGItf  = QualAGItf_AGItf !(RelevQual ) 
                deriving ( Data,Eq,Ord,Show,Typeable)
-- RelevCoe ----------------------------------------------------
data RelevCoe  = RelevCoe_Cast !(RelevCoe ) 
               | RelevCoe_CastTy !(RelevTy ) !(RelevTy ) 
               | RelevCoe_Comp !(RelevCoe ) !(RelevCoe ) 
               | RelevCoe_Err !(String) 
               | RelevCoe_Eval !(AnaEval ) !(AnaEval ) 
               | RelevCoe_Fun !(RelevCoeL ) !(RelevCoe ) 
               | RelevCoe_Id 
               deriving ( Data,Eq,Ord,Show,Typeable)
-- RelevCoeL ---------------------------------------------------
type RelevCoeL  = [RelevCoe ]
-- RelevQual ---------------------------------------------------
data RelevQual  = RelevQual_SubEval !(AnaEval ) !(AnaEval ) 
                deriving ( Data,Eq,Ord,Show,Typeable)
-- RelevQualL --------------------------------------------------
type RelevQualL  = [RelevQual ]
-- RelevTy -----------------------------------------------------
data RelevTy  = RelevTy_Ana !(AnaEval ) 
              | RelevTy_Err !(String) 
              | RelevTy_Fun !(RQuant) !(([UID])) !(RelevQualL ) !(RelevTyL ) !(RelevTy ) 
              | RelevTy_None 
              deriving ( Data,Eq,Ord,Show,Typeable)
-- RelevTyL ----------------------------------------------------
type RelevTyL  = [RelevTy ]
-- TyAGItf -----------------------------------------------------
data TyAGItf  = TyAGItf_AGItf !(RelevTy ) 
              deriving ( Data,Eq,Ord,Show,Typeable)