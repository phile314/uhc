%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Core Public API
%%%
%%% Intended for constructing basic Core Programs. This module is just a thin wrapper
%%% around AbstractCore.
%%% This module does not offer any way to inspect the built Core Programs (on purpose), but the
%%% EHXX.Core module does.
%%%
%%% Invariants:
%%% - Constructor applications (mkCon) always have to be fully saturated. (Should we handle this internally?)
%%% - Haskell constructor names must be unambigous per module (mkHSCTag)
%%% - TODO Tag ordering ?? What exactly are the invariants?
%%%
%%% TODO Use AbstractCore instead of directly using the Constructors in the implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) module {%{EH}Core.API}
%%]
%%[(8 codegen) hs import(Data.Ord,Data.List)
%%]
%%[(8 codegen) hs import({%{EH}Base.Common}) export(CTag)
%%]
%%[(8 codegen) hs import({%{EH}Core}) export(CModule,CImport,CDeclMeta,CDataCon,CExpr,CBind,CAlt)
%%]
%%[(8 codegen) hs import({%{EH}AbstractCore},{%{EH}Base.HsName})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(mkModule,mkImport,mkDeclMetaData,mkDataConCon,mkCon,mkCase,mkLet,mkLet1,mkBindBound,mkVar,mkLams,mkApps,mkApps',mkInteger,mkString,mkCTag,mkHSCTag)
mkModule :: String -> [CImport] -> [CDeclMeta] -> CExpr -> CModule
mkModule modNm imps meta expr = CModule_Mod (mkHNm modNm) imps meta expr

mkImport :: String -> CImport
mkImport = CImport_Import . mkHNm

mkDeclMetaData :: String -> [CDataCon] -> CDeclMeta
mkDeclMetaData nm = CDeclMeta_Data (mkHNm nm)

mkDataConCon :: String -> Int -> Int -> CDataCon
mkDataConCon nm = CDataCon_Con (mkHNm nm)

mkCon :: CTag -> [CExpr] -> CExpr
mkCon ctg es = mkApps (CExpr_Tup ctg) es

-- | Produces a case switch on an expression.
mkCase :: String    -- ^ A fresh name. Used as an internal (local) variable.
  -> CExpr          -- ^ The case scrutinee.
  -> [CAlt]         -- ^ The case alternatives.
  -> CExpr          -- ^ The default value.
  -> CExpr
mkCase x e as def = CExpr_Let CBindCateg_Strict [mkBindBound x e] (
    CExpr_Case (mkVar x) (fixCaseOrder as) def)
  where -- If case alternatives are not in alphabetical order, the wrong case get's picked.
    fixCaseOrder :: CAltL -> CAltL
    fixCaseOrder = sortBy (\x y -> f (pat_CAlt_Alt x) (pat_CAlt_Alt y))
    -- Only using the CPat_Con constructor atm, so let's just ignore the other patterns.
    f :: CPat -> CPat -> Ordering
    f (CPat_Con tg1 _ _) (CPat_Con tg2 _ _) = comparing ctagTag tg1 tg2
    f (CPat_Con _ _ _) _ = LT
    f  _ (CPat_Con _ _ _) = GT
    f _ _ = EQ

mkLet :: [CBind] -> CExpr -> CExpr
mkLet bnds e = CExpr_Let CBindCateg_Rec bnds e

mkLet1 :: String -> CExpr -> CExpr -> CExpr
mkLet1 nm e1 e2 = mkLet [mkBindBound nm e1] e2

mkBindBound :: String -> CExpr -> CBind
mkBindBound nm e = CBind_Bind (mkHNm nm) [CBound_Bind (CMetaBind_Plain, CMetaVal_Val) e]

mkVar :: String -> CExpr
mkVar = CExpr_Var . acoreMkRef . mkHNm

mkLams :: [String] -> CExpr -> CExpr
mkLams vs b = foldr mkLam b vs
  where mkLam v b = CExpr_Lam (CBind_Bind (mkHNm v) []) b

mkApps :: CExpr -> [CExpr] -> CExpr
mkApps = acoreApp

mkApps' :: String -> [CExpr] -> CExpr
mkApps' f xs = mkApps (mkVar f) xs

mkInteger :: Int -> CExpr
mkInteger i = mkApps' "UHC.Base.primIntToInteger" [CExpr_Int i] 

mkString :: String -> CExpr
mkString s = mkApps' "UHC.Base.packedStringToString" [CExpr_String s]

-- | Builds a CTag from the data type and constructor name.
-- Use the mkHSCTag function if the datatype has been declared in a Haskell file.
mkCTag :: String    -- ^ The qualified name of the datatype.
    -> String       -- ^ The unqualified name of the constructor.
    -> Int          -- ^ The tag of the constructor.
    -> CTag
mkCTag dtNm ctor tg = CTag (mkHNm dtNm) (mkHNm $ dtNm ++ "." ++ ctor) tg (-1) (-1)

-- | Builds a CTag from the data type and constructor name.
-- Only use this function when calling Haskell constructors.
mkHSCTag :: String    -- ^ The qualified name of the datatype.
    -> String       -- ^ The unqualified name of the constructor.
    -> Int          -- ^ The tag of the constructor.
    -> CTag
mkHSCTag dtNm ctor tg = CTag (mkHNm dtNm) (mkHNm $ (modNm dtNm) ++ "." ++ ctor) tg (-1) (-1)
  where modNm s = reverse $ tail $ dropWhile (/= '.') $ reverse s

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utility functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%[(8 codegen) export(mkMain)
-- | Creates the main entry point, calling the function with the given name.
mkMain :: String -> CExpr
mkMain mainNm = 
  CExpr_Let CBindCateg_Plain
    [ CBind_Bind (mkHNm "main")
      [ CBound_Bind (CMetaBind_Plain, CMetaVal_Val) (mkApps' "UHC.Run.ehcRunMain" [mkVar mainNm]) ]
    ] (mkVar "main")
%%]
