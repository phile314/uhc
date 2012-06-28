

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/GrinCode.ag)
module EH101.GrinCode(module EH101.Base.BasicAnnot
, GrExpr (..), GrBind (..), GrBindL, GrGlobal (..), GrGlobalL
, GrAGItf (..), GrModule (..)
, GrType (..), GrTypeBase (..), GrTypeBaseL
, GrAlt (..), GrAltL, GrPatAlt (..), GrPatLam (..), GrVal (..), GrValL, GrTag (..), GrTagL, GrVar (..), GrVarL
, mkGrBox, mkGrRecNode, mkGrConNode
, mkGrPatRecNode, mkGrPatConNode, mkGrUnbox, mkGrUnboxFFI
, grBuiltinTyNmL
, GrTagAnn (..), mkGrTagAnn, emptyGrTagAnn
, GrBindAnn (..)
, GrAltAnn (..)
, FvInfo (..), FvUse (..), FvInfoMp
, fviMpUnion, fviMpSingleton, fviMpSingleton', fviMpUnions, fviMpFromList, fviMpFromList', fviMpDifference
, NmAlias (..), NmAliasMp, mkNmAliasMp
, nmAliasRepl, nmAliasRepl'
, GrInl (..), GrInlMp
, WillUseFor (..), WillUseForMp, WillUseForS, willUseIntersection, willUseUnion, willUseFor, willUseForEval, willUseForNodeField
, GrAdapt (..), GrAdaptL, GrSplit (..), GrSplitL
, GrNodeAdapt (..)
, mkGrPatRecSplit, mkGrPatConSplit
, grModMerge
, FFIWay (..), module EH101.Foreign
, GrFFIAnnot (..)
, grFFIAnnotIsResEvaluated) where

import EH101.Base.Common
import EH101.Opts
import EH.Util.Pretty
import EH.Util.Utils
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import EH101.Base.Builtin
import EH101.Base.Builtin2
import EH101.Base.BasicAnnot
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize
import Data.Typeable (Typeable)
import Data.Generics (Data)
import EH101.Foreign
import EH101.Base.Target



































grBuiltinTyNmL :: EHCOpts -> [HsName]
grBuiltinTyNmL opts = builtinKnownGrinBoxedTyNmL opts ++ builtinKnownRecTyNmL



data GrTagAnn
  = GrTagAnn
      { gtannArity 		:: !Int
      , gtannMaxArity 	:: !Int
      }

instance Eq GrTag where
  GrTag_Con _ _ x1 == GrTag_Con _ _ x2 = x1==x2
  GrTag_Fun     x1 == GrTag_Fun     x2 = x1==x2
  GrTag_App     x1 == GrTag_App     x2 = x1==x2
  GrTag_PApp n1 x1 == GrTag_PApp n2 x2 = n1==n2 && x1==x2
  GrTag_Unboxed    == GrTag_Unboxed    = True
  GrTag_Hole       == GrTag_Hole       = True
  GrTag_Rec        == GrTag_Rec        = True
  _                == _                = False



instance Eq GrTagAnn where
  x == y = True

instance Ord GrTagAnn where
  x `compare` y = EQ

instance Show GrTagAnn where
  show (GrTagAnn a ma) = "{" ++ show a ++ "," ++ show ma ++ "}"

mkGrTagAnn :: Int -> Int -> GrTagAnn
mkGrTagAnn = GrTagAnn

emptyGrTagAnn = mkGrTagAnn 0 0



deriving instance Typeable GrTagAnn
deriving instance Data GrTagAnn




-- Annotation for Grin Bindings
--   Normal:  normal binding
--   Class:    binds a dictionary with default definitions for al class
--      The list entries correspond 1-to-1 with the class members
--   Instance: binds a dictionary with definitions form an instance declaration
--      The first element of the list encodes the name of the dictionary constructor.
--      The second element of the list encodes the name of the dictionary containing the default definitions.
--      The remaining list entries correspond 1-to-1 with the class members
--   Specialized: this is a specialization of another function
--      The HsName is the name of the original function
--      The Int is a sequence number that enumerates all specializations of an original function
--      The list holds "Just" the argument that was specialized, or "Nothing" if it was not specialized

data GrBindAnn
  = GrBindAnnNormal
  | GrBindAnnClass    [Track]
  | GrBindAnnInstance [Track]
  | GrBindAnnSpecialized HsName Int [Maybe HsName]
  deriving (Eq,Ord,Show)




deriving instance Typeable GrBindAnn
deriving instance Data GrBindAnn




-- Annotation for Grin Alternatives
--   Normal:  this alternative has classic semantics
--   Ident:   this alternative just returns the scrutinee, but has classic semantics
--   Calling: the alternative calls a function
--   Reenter: this alternative returns a value which has to be scrutinized again against the other alternatives

data GrAltAnn
  = GrAltAnnNormal
  | GrAltAnnIdent
  | GrAltAnnCalling Int HsName
  | GrAltAnnReenter
  deriving (Eq,Ord,Show)




deriving instance Typeable GrAltAnn
deriving instance Data GrAltAnn



grFFIAnnotIsResEvaluated :: GrFFIAnnot -> Bool
grFFIAnnotIsResEvaluated (GrFFIAnnot_IsResEval b) = b
-- comment away when GrFFIAnnot is extended, for this avoid ghc warnings
-- grFFIAnnotIsResEvaluated _                        = True



data FvUse
  = FvUse_Call | FvUse_Val | FvUse_Other
  deriving (Eq,Ord,Show)

data FvInfo
  = FvInfo
      { fviUseCount		:: !Int
      , fviUseAs 		:: !(Set.Set FvUse)
      }
  deriving (Show)

type FvInfoMp = Map.Map HsName FvInfo



fviMpDifference :: FvInfoMp -> FvInfoMp -> FvInfoMp
fviMpDifference = Map.difference

fviMpUnion :: FvInfoMp -> FvInfoMp -> FvInfoMp
fviMpUnion = Map.unionWith (\i1 i2 -> i1 {fviUseCount = fviUseCount i1 + fviUseCount i2, fviUseAs = Set.unions [fviUseAs i1,fviUseAs i2]})

fviMpUnions :: [FvInfoMp] -> FvInfoMp
fviMpUnions = foldr fviMpUnion Map.empty

fviMpSingleton' :: FvUse -> HsName -> FvInfoMp
fviMpSingleton' u n = Map.singleton n (FvInfo 1 (Set.singleton u))

fviMpSingleton :: HsName -> FvInfoMp
fviMpSingleton = fviMpSingleton' FvUse_Other

fviMpFromList' :: FvUse -> [HsName] -> FvInfoMp
fviMpFromList' u = fviMpUnions . map (fviMpSingleton' u)

fviMpFromList :: [HsName] -> FvInfoMp
fviMpFromList = fviMpFromList' FvUse_Other



data NmAlias
 = NmAlias_Nm       			!HsName                                                 -- name to name
 | NmAlias_NmAfterSideEffect	!HsName                                                 -- name to name, but after an eval
 | NmAlias_Eval     			!HsName                                                 -- name to evaluated name
 | NmAlias_Const    			{naliNm :: !HsName, naliVal  :: !GrVal        }         -- name to single constant (other than name)
 | NmAlias_Grp      			{naliNm :: !HsName, naliAlis :: ![NmAlias]    }         -- name to group of values, i.e. node constituents
 | NmAlias_Basic    			{naliNm :: !HsName, naliAli  :: !NmAlias, naliAnnot :: BasicAnnot  }
                                                                     		       	-- name to basic value, i.e. node element used for FFI
 | NmAlias_None

type NmAliasMp = Map.Map HsName NmAlias

mkNmAliasMp :: AssocL HsName HsName -> NmAliasMp
mkNmAliasMp = Map.fromList . assocLMapElt NmAlias_Nm



nmAliasRepl' :: NmAliasMp -> HsName -> NmAlias
nmAliasRepl' m n
  = case Map.lookup n m of
      Just a -> a
      _      -> NmAlias_Nm n

nmAliasRepl :: NmAliasMp -> HsName -> HsName
nmAliasRepl m n
  = case Map.lookup n m of
      Just (NmAlias_Nm    n'  ) -> n'
      Just (NmAlias_Grp   n' _) -> n'
      Just (NmAlias_Const n' _) -> n'
      _                         -> n



data GrInl
  = GrInl_Call
      { inlArgNmL   :: ![HsName]
      , inlGrExpr   :: !GrExpr
      }
  | GrInl_CAF
      { inlGrExpr   :: !GrExpr
      }
  deriving (Typeable, Data)

type GrInlMp = Map.Map HsName GrInl



data GrNodeAdapt = GrNodeUpd | GrNodeExt deriving (Show,Eq)




-- partial parametrizations of the GrVal_Node etc.

mkGrConNode :: GrTagAnn -> Int -> HsName -> GrValL -> GrVal
mkGrConNode ann i nm = GrVal_Node (GrTag_Con ann i nm)

mkGrRecNode ::  GrValL -> GrVal
mkGrRecNode xs
  = GrVal_Node (GrTag_Con (mkGrTagAnn a a) 0 (hsnProd a)) xs
  where a = length xs

mkGrPatConNode :: GrTagAnn -> Int -> HsName -> [HsName] -> GrPatAlt
mkGrPatConNode ann i nm = GrPatAlt_Node (GrTag_Con ann i nm)

mkGrPatRecNode :: [HsName] -> GrPatAlt
mkGrPatRecNode xs
  = GrPatAlt_Node (GrTag_Con (mkGrTagAnn a a) 0 (hsnProd a)) xs
  where a = length xs

mkGrBox :: HsName -> GrVal -> GrVal
mkGrBox tyNm v = mkGrConNode (mkGrTagAnn 1 1) 0 tyNm [v]

mkGrUnbox :: HsName -> HsName -> GrPatLam
mkGrUnbox tyNm n = let tag = GrTag_Con (mkGrTagAnn 1 1) 0 tyNm
                   in  GrPatLam_VarNode [ GrVar_KnownTag tag, GrVar_Var n ]

mkGrUnboxFFI :: HsName -> GrPatLam
mkGrUnboxFFI   n = let tag = GrTag_Unboxed
                   in  GrPatLam_VarNode [ GrVar_KnownTag tag, GrVar_Var n ]




mkGrPatConSplit :: GrTagAnn -> Int -> HsName -> HsName -> GrSplitL -> GrPatAlt
mkGrPatConSplit ann i nm = GrPatAlt_NodeSplit (GrTag_Con ann i nm)

mkGrPatRecSplit :: HsName -> GrSplitL -> GrPatAlt
mkGrPatRecSplit  = GrPatAlt_NodeSplit GrTag_Rec



grModMerge :: [GrModule] -> GrModule
grModMerge mL
  = foldr1 cmb mL
  where cmb (GrModule_Mod m1 g1 b1 t1) (GrModule_Mod m2 g2 b2 t2)
          = GrModule_Mod m2 (List.union g1 g2) (b1 ++ b2) (Map.union t1 t2)



data WillUseFor
  = WillUseFor_Eval
  | WillUseFor_NodeField
  deriving (Eq,Ord,Show)

instance PP WillUseFor where
  pp = pp . show

type WillUseForS  = Set.Set WillUseFor
type WillUseForMp = Map.Map HsName WillUseForS

willUseUnion :: WillUseForMp -> WillUseForMp -> WillUseForMp
willUseUnion = Map.unionWith Set.union

willUseIntersection :: WillUseForMp -> WillUseForMp -> WillUseForMp
willUseIntersection = Map.intersectionWith Set.intersection

willUseFor :: HsName -> WillUseForMp -> WillUseForS
willUseFor n m = maybe Set.empty id $ Map.lookup n m

willUseForEval :: HsName -> WillUseForMp -> Bool
willUseForEval n m = maybe False (WillUseFor_Eval `Set.member`) $ Map.lookup n m

willUseForNodeField :: HsName -> WillUseForMp -> Bool
willUseForNodeField n m = maybe False (WillUseFor_NodeField `Set.member`) $ Map.lookup n m



-- instance Serialize GrInl



instance Serialize GrInl where
  sput (GrInl_Call a b) = sputWord8  0 >> sput a >> sput b
  sput (GrInl_CAF  a  ) = sputWord8  1 >> sput a
  sget = do t <- sgetWord8
            case t of
              0 -> liftM2 GrInl_Call sget sget
              1 -> liftM  GrInl_CAF  sget

instance Serialize GrExpr where
  sput (GrExpr_Seq         a b c  ) = sputWord8 0  >> sput a >> sput b >> sput c
  sput (GrExpr_Unit        a b    ) = sputWord8 1  >> sput a >> sput b
  sput (GrExpr_UpdateUnit  a b    ) = sputWord8 2  >> sput a >> sput b
  sput (GrExpr_Case        a b    ) = sputWord8 3  >> sput a >> sput b
  sput (GrExpr_FetchNode   a      ) = sputWord8 4  >> sput a
  sput (GrExpr_FetchUpdate a b    ) = sputWord8 5  >> sput a >> sput b
  sput (GrExpr_FetchField  a b c  ) = sputWord8 6  >> sput a >> sput b >> sput c
  sput (GrExpr_Store       a      ) = sputWord8 7  >> sput a
  sput (GrExpr_Call        a b    ) = sputWord8 8  >> sput a >> sput b
  sput (GrExpr_FFI         a b c d) = sputWord8 9  >> sput a >> sput b >> sput c >> sput d
  sput (GrExpr_Eval        a      ) = sputWord8 10 >> sput a
  sput (GrExpr_App         a b    ) = sputWord8 11 >> sput a >> sput b
  sput _                            = panic "GrinCode.Serialize.GrExpr.sput"		-- unused are unimplemented
  sget = do t <- sgetWord8
            case t of
              0  -> liftM3 GrExpr_Seq          sget sget sget
              1  -> liftM2 GrExpr_Unit         sget sget
              2  -> liftM2 GrExpr_UpdateUnit   sget sget
              3  -> liftM2 GrExpr_Case         sget sget
              4  -> liftM  GrExpr_FetchNode    sget
              5  -> liftM2 GrExpr_FetchUpdate  sget sget
              6  -> liftM3 GrExpr_FetchField   sget sget sget
              7  -> liftM  GrExpr_Store        sget
              8  -> liftM2 GrExpr_Call         sget sget
              9  -> liftM4 GrExpr_FFI          sget sget sget sget
              10 -> liftM  GrExpr_Eval         sget
              11 -> liftM2 GrExpr_App          sget sget
              _  -> panic "GrinCode.Serialize.GrExpr.sget"		-- unused are unimplemented

instance Serialize GrType where
  sput (GrType_None     ) = sputWord8 0
  sput (GrType_Arrow a b) = sputWord8 1  >> sput a >> sput b
  sget = do t <- sgetWord8
            case t of
              0  -> return GrType_None
              1  -> liftM2 GrType_Arrow         sget sget

instance Serialize GrTypeBase where
  sput (GrTypeBase_Node     ) = sputWord8 0
  sput (GrTypeBase_Pointer  ) = sputWord8 1
  sget = do t <- sgetWord8
            case t of
              0  -> return GrTypeBase_Node
              1  -> return GrTypeBase_Pointer

instance Serialize GrAlt where
  sput (GrAlt_Alt          a b c  ) = sputWord8 0  >> sput a >> sput b >> sput c
  sget = do t <- sgetWord8
            case t of
              0  -> liftM3 GrAlt_Alt          sget sget sget

instance Serialize GrTag where
  sput (GrTag_Con         a b c  ) = sputWord8 0  >> sput a >> sput b >> sput c
  sput (GrTag_Fun         a      ) = sputWord8 1  >> sput a
  sput (GrTag_PApp        a b    ) = sputWord8 2  >> sput a >> sput b
  sput (GrTag_App         a      ) = sputWord8 3  >> sput a
  sput (GrTag_Unboxed            ) = sputWord8 4
  sput (GrTag_Hole               ) = sputWord8 5
  sput (GrTag_Rec                ) = sputWord8 6
  sget = do t <- sgetWord8
            case t of
              0  -> liftM3 GrTag_Con          sget sget sget
              1  -> liftM  GrTag_Fun          sget
              2  -> liftM2 GrTag_PApp         sget sget
              3  -> liftM  GrTag_App          sget
              4  -> return GrTag_Unboxed
              5  -> return GrTag_Hole
              6  -> return GrTag_Rec

instance Serialize GrPatLam where
  sput (GrPatLam_Empty              ) = sputWord8 0
  sput (GrPatLam_Var         a      ) = sputWord8 1  >> sput a
  sput (GrPatLam_VarNode     a      ) = sputWord8 2  >> sput a
  sput (GrPatLam_BasicNode   a b    ) = sputWord8 3  >> sput a >> sput b
  sput (GrPatLam_EnumNode    a      ) = sputWord8 4  >> sput a
  sput (GrPatLam_PtrNode     a      ) = sputWord8 5  >> sput a
  sput (GrPatLam_OpaqueNode  a      ) = sputWord8 6  >> sput a
  sput (GrPatLam_BasicAnnot  a b    ) = sputWord8 7  >> sput a >> sput b
  sput (GrPatLam_EnumAnnot   a b    ) = sputWord8 8  >> sput a >> sput b
  sput (GrPatLam_PtrAnnot    a b    ) = sputWord8 9  >> sput a >> sput b
  sput (GrPatLam_OpaqueAnnot a      ) = sputWord8 10 >> sput a
  sget = do t <- sgetWord8
            case t of
              0  -> return GrPatLam_Empty
              1  -> liftM  GrPatLam_Var          sget
              2  -> liftM  GrPatLam_VarNode      sget
              3  -> liftM2 GrPatLam_BasicNode    sget sget
              4  -> liftM  GrPatLam_EnumNode     sget
              5  -> liftM  GrPatLam_PtrNode      sget
              6  -> liftM  GrPatLam_OpaqueNode   sget
              7  -> liftM2 GrPatLam_BasicAnnot   sget sget
              8  -> liftM2 GrPatLam_EnumAnnot    sget sget
              9  -> liftM2 GrPatLam_PtrAnnot     sget sget
              10 -> liftM  GrPatLam_OpaqueAnnot  sget

instance Serialize GrVal where
  sput (GrVal_Empty              ) = sputWord8 0
  sput (GrVal_Var         a      ) = sputWord8 1  >> sput a
  sput (GrVal_VarNode     a      ) = sputWord8 2  >> sput a
  sput (GrVal_BasicNode   a b    ) = sputWord8 3  >> sput a >> sput b
  sput (GrVal_EnumNode    a      ) = sputWord8 4  >> sput a
  sput (GrVal_PtrNode     a      ) = sputWord8 5  >> sput a
  sput (GrVal_LitInt      a      ) = sputWord8 6  >> sput a
  sput (GrVal_LitStr      a      ) = sputWord8 7  >> sput a
  sput (GrVal_Tag         a      ) = sputWord8 8  >> sput a
  sput (GrVal_Node        a b    ) = sputWord8 9  >> sput a >> sput b
  sput (GrVal_OpaqueNode  a      ) = sputWord8 10 >> sput a
  sput _                           = panic "GrinCode.Serialize.GrVal.sput"		-- unused are unimplemented
  sget = do t <- sgetWord8
            case t of
              0  -> return GrVal_Empty
              1  -> liftM  GrVal_Var          sget
              2  -> liftM  GrVal_VarNode      sget
              3  -> liftM2 GrVal_BasicNode    sget sget
              4  -> liftM  GrVal_EnumNode     sget
              5  -> liftM  GrVal_PtrNode      sget
              6  -> liftM  GrVal_LitInt       sget
              7  -> liftM  GrVal_LitStr       sget
              8  -> liftM  GrVal_Tag          sget
              9  -> liftM2 GrVal_Node         sget sget
              10 -> liftM  GrVal_OpaqueNode   sget
              _  -> panic "GrinCode.Serialize.GrVal.sget"		-- unused are unimplemented

instance Serialize GrPatAlt where
  sput (GrPatAlt_LitInt      a      ) = sputWord8 0  >> sput a
  sput (GrPatAlt_Tag         a      ) = sputWord8 1  >> sput a
  sput (GrPatAlt_Node        a b    ) = sputWord8 2  >> sput a >> sput b
  sput (GrPatAlt_Otherwise          ) = sputWord8 3
  sput _                              = panic "GrinCode.Serialize.GrPatAlt.sput"		-- unused are unimplemented
  sget = do t <- sgetWord8
            case t of
              0  -> liftM  GrPatAlt_LitInt       sget
              1  -> liftM  GrPatAlt_Tag          sget
              2  -> liftM2 GrPatAlt_Node         sget sget
              3  -> return GrPatAlt_Otherwise
              _  -> panic "GrinCode.Serialize.GrPatAlt.sget"		-- unused are unimplemented

instance Serialize GrVar where
  sput (GrVar_Var           a      ) = sputWord8 0  >> sput a
  sput (GrVar_KnownTag      a      ) = sputWord8 1  >> sput a
  sput (GrVar_Ignore               ) = sputWord8 2
  sget = do t <- sgetWord8
            case t of
              0  -> liftM  GrVar_Var         sget
              1  -> liftM  GrVar_KnownTag    sget
              2  -> return GrVar_Ignore

instance Serialize GrAltAnn where
  sput (GrAltAnnNormal              ) = sputWord8 0
  sput (GrAltAnnIdent               ) = sputWord8 1
  sput (GrAltAnnCalling      a b    ) = sputWord8 2  >> sput a >> sput b
  sput (GrAltAnnReenter             ) = sputWord8 3
  sget = do t <- sgetWord8
            case t of
              0  -> return GrAltAnnNormal
              1  -> return GrAltAnnIdent
              2  -> liftM2 GrAltAnnCalling      sget sget
              3  -> return GrAltAnnReenter

instance Serialize GrTagAnn where
  sput (GrTagAnn a b) = sput a >> sput b
  sget = liftM2 GrTagAnn sget sget

instance Serialize GrBind where
  sput (GrBind_Bind  a b c d) = sputWord8 0  >> sput a >> sput b >> sput c >> sput d
  sput (GrBind_Arity a b    ) = sputWord8 1  >> sput a >> sput b
  sput (GrBind_Rec   a      ) = sputWord8 2  >> sput a
  sget
    = do t <- sgetWord8
         case t of
           0 -> liftM4 GrBind_Bind  sget sget sget sget
           1 -> liftM2 GrBind_Arity sget sget
           2 -> liftM  GrBind_Rec   sget

instance Serialize GrModule where
  sput (GrModule_Mod  a b c d) = sput a >> sput b >> sput c >> sput d
  sget = liftM4 GrModule_Mod  sget sget sget sget

instance Serialize GrGlobal where
  sput (GrGlobal_Global a b) = sputWord8 0  >> sput a >> sput b
  sget
    = do t <- sgetWord8
         case t of
           0  -> liftM2 GrGlobal_Global sget sget






instance Serialize GrFFIAnnot where
  sput (GrFFIAnnot_IsResEval a) = sputWord8 0  >> sput a
  sget = do t <- sgetWord8
            case t of
              0  -> liftM GrFFIAnnot_IsResEval sget



instance Serialize GrBindAnn where
  sput (GrBindAnnNormal           ) = sputWord8 0
  sput (GrBindAnnClass       a    ) = sputWord8 1 >> sput a
  sput (GrBindAnnInstance    a    ) = sputWord8 2 >> sput a
  sput (GrBindAnnSpecialized a b c) = sputWord8 4 >> sput a >> sput b >> sput c
  sget
    = do t <- sgetWord8
         case t of
           0 -> return GrBindAnnNormal
           1 -> liftM  GrBindAnnClass         sget
           2 -> liftM  GrBindAnnInstance      sget
           4 -> liftM3 GrBindAnnSpecialized   sget sget sget
-- GrAGItf -----------------------------------------------------
data GrAGItf  = GrAGItf_AGItf !(GrModule ) 
              deriving ( Data,Eq,Show,Typeable)
-- GrAdapt -----------------------------------------------------
data GrAdapt  = GrAdapt_Del !(GrVal ) 
              | GrAdapt_Ins !(GrVal ) !(GrVal ) 
              | GrAdapt_Upd !(GrVal ) !(GrVal ) 
              deriving ( Data,Eq,Show,Typeable)
-- GrAdaptL ----------------------------------------------------
type GrAdaptL  = [GrAdapt ]
-- GrAlt -------------------------------------------------------
data GrAlt  = GrAlt_Alt !(GrAltAnn) !(GrPatAlt ) !(GrExpr ) 
            deriving ( Data,Eq,Show,Typeable)
-- GrAltL ------------------------------------------------------
type GrAltL  = [GrAlt ]
-- GrBind ------------------------------------------------------
data GrBind  = GrBind_Arity !(HsName) !(Int) 
             | GrBind_Bind !(HsName) !(GrBindAnn) !(([HsName])) !(GrExpr ) 
             | GrBind_Rec !(GrBindL ) 
             deriving ( Data,Eq,Show,Typeable)
-- GrBindL -----------------------------------------------------
type GrBindL  = [GrBind ]
-- GrExpr ------------------------------------------------------
data GrExpr  = GrExpr_App !(HsName) !(GrValL ) 
             | GrExpr_Call !(HsName) !(GrValL ) 
             | GrExpr_Case !(GrVal ) !(GrAltL ) 
             | GrExpr_Catch !(GrExpr ) !(HsName) !(GrExpr ) 
             | GrExpr_Eval !(HsName) 
             | GrExpr_FFI !(FFIWay) !(ForeignEnt) !(GrFFIAnnot) !(GrValL ) 
             | GrExpr_FetchField !(HsName) !(Int) !((Maybe GrTag)) 
             | GrExpr_FetchNode !(HsName) 
             | GrExpr_FetchUpdate !(HsName) !(HsName) 
             | GrExpr_Seq !(GrExpr ) !(GrPatLam ) !(GrExpr ) 
             | GrExpr_Store !(GrVal ) 
             | GrExpr_Throw !(HsName) 
             | GrExpr_Unit !(GrVal ) !(GrType ) 
             | GrExpr_UpdateUnit !(HsName) !(GrVal ) 
             deriving ( Data,Eq,Show,Typeable)
-- GrFFIAnnot --------------------------------------------------
data GrFFIAnnot  = GrFFIAnnot_IsResEval !(Bool) 
                 deriving ( Data,Eq,Show,Typeable)
-- GrGlobal ----------------------------------------------------
data GrGlobal  = GrGlobal_Global !(HsName) !(GrVal ) 
               deriving ( Data,Eq,Show,Typeable)
-- GrGlobalL ---------------------------------------------------
type GrGlobalL  = [GrGlobal ]
-- GrModule ----------------------------------------------------
data GrModule  = GrModule_Mod !(HsName) !(GrGlobalL ) !(GrBindL ) !((Map.Map HsName [GrTag])) 
               deriving ( Data,Eq,Show,Typeable)
-- GrPatAlt ----------------------------------------------------
data GrPatAlt  = GrPatAlt_LitInt !(Int) 
               | GrPatAlt_Node !(GrTag ) !(([HsName])) 
               | GrPatAlt_NodeSplit !(GrTag ) !(HsName) !(GrSplitL ) 
               | GrPatAlt_Otherwise 
               | GrPatAlt_Tag !(GrTag ) 
               deriving ( Data,Eq,Show,Typeable)
-- GrPatLam ----------------------------------------------------
data GrPatLam  = GrPatLam_BasicAnnot !(BasicAnnot) !(HsName) 
               | GrPatLam_BasicNode !(BasicAnnot) !(HsName) 
               | GrPatLam_Empty 
               | GrPatLam_EnumAnnot !(HsName) !(HsName) 
               | GrPatLam_EnumNode !(HsName) 
               | GrPatLam_OpaqueAnnot !(HsName) 
               | GrPatLam_OpaqueNode !(HsName) 
               | GrPatLam_PtrAnnot !(HsName) !(HsName) 
               | GrPatLam_PtrNode !(HsName) 
               | GrPatLam_Var !(HsName) 
               | GrPatLam_VarNode !(GrVarL ) 
               deriving ( Data,Eq,Show,Typeable)
-- GrSplit -----------------------------------------------------
data GrSplit  = GrSplit_Sel !(HsName) !(GrVal ) 
              deriving ( Data,Eq,Show,Typeable)
-- GrSplitL ----------------------------------------------------
type GrSplitL  = [GrSplit ]
-- GrTag -------------------------------------------------------
data GrTag  = GrTag_App !(HsName) 
            | GrTag_Con !(GrTagAnn) !(Int) !(HsName) 
            | GrTag_Fun !(HsName) 
            | GrTag_Hole 
            | GrTag_PApp !(Int) !(HsName) 
            | GrTag_Rec 
            | GrTag_Unboxed 
            deriving ( Data,Show,Typeable)
-- GrTagL ------------------------------------------------------
type GrTagL  = [GrTag ]
-- GrType ------------------------------------------------------
data GrType  = GrType_Arrow !(GrTypeBaseL ) !(GrTypeBase ) 
             | GrType_None 
             deriving ( Data,Eq,Show,Typeable)
-- GrTypeBase --------------------------------------------------
data GrTypeBase  = GrTypeBase_Node 
                 | GrTypeBase_Pointer 
                 deriving ( Data,Eq,Show,Typeable)
-- GrTypeBaseL -------------------------------------------------
type GrTypeBaseL  = [GrTypeBase ]
-- GrVal -------------------------------------------------------
data GrVal  = GrVal_BasicNode !(GrTag ) !(HsName) 
            | GrVal_Empty 
            | GrVal_EnumNode !(HsName) 
            | GrVal_LitInt !(Int) 
            | GrVal_LitStr !(String) 
            | GrVal_Node !(GrTag ) !(GrValL ) 
            | GrVal_NodeAdapt !(HsName) !(GrAdaptL ) 
            | GrVal_OpaqueNode !(HsName) 
            | GrVal_PtrNode !(HsName) 
            | GrVal_Tag !(GrTag ) 
            | GrVal_Var !(HsName) 
            | GrVal_VarNode !(GrValL ) 
            deriving ( Data,Eq,Show,Typeable)
-- GrValL ------------------------------------------------------
type GrValL  = [GrVal ]
-- GrVar -------------------------------------------------------
data GrVar  = GrVar_Ignore 
            | GrVar_KnownTag !(GrTag ) 
            | GrVar_Var !(HsName) 
            deriving ( Data,Eq,Show,Typeable)
-- GrVarL ------------------------------------------------------
type GrVarL  = [GrVar ]