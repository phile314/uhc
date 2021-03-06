%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC common stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Used by all compiler driver code

%%[1 module {%{EH}EHC.Common}
%%]

-- general imports
%%[1 import(Data.List, Data.Char, Data.Maybe) export(module Data.Maybe, module Data.List, module Data.Char)
%%]
%%[1 import(Control.Monad.State, System.IO) export(module System.IO)
%%]
%%[1 import(UHC.Util.CompileRun2, UHC.Util.Pretty, UHC.Util.FPath, UHC.Util.Utils) export(module UHC.Util.CompileRun2, module UHC.Util.Pretty, module UHC.Util.FPath, module UHC.Util.Utils)
%%]
%%[1 import({%{EH}Base.Common}, {%{EH}Base.HsName.Builtin}, {%{EH}Opts}) export(module {%{EH}Base.Common}, module {%{EH}Base.HsName.Builtin}, module {%{EH}Opts})
%%]
%%[1 import({%{EH}Error},{%{EH}Error.Pretty}) export(module {%{EH}Error},module {%{EH}Error.Pretty})
%%]
%%[8 import(GHC.Generics)
%%]

%%[8 import({%{EH}Gam.Full}) export(module {%{EH}Gam.Full})
%%]

%%[8 import({%{EH}Opts.CommandLine})
%%]

%%[8 import (qualified UHC.Util.RelMap as Rel)
%%]

%%[5050 import(System.Time, System.Directory)
%%]

%%[50 import(UHC.Util.Time, System.Directory)
%%]

%%[1
-- dummy, so module is not empty for initial variants, and exports will take effect
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% State of compilation unit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The state HS compilation can be in

%%[8 export(HSState(..))
data HSState
  = HSStart                 -- starting from .hs
  | HSAllSem                -- done all semantics for .hs
%%[[50
  | HMOnlyMinimal           -- done minimal info only
  -- | HMStart                 -- starting from nothing, not using .hi info nor .hs file, just for linking etc
  | HSOnlyImports           -- done imports from .hs
  | HIStart                 -- starting from .hi
  | HIAllSem                -- done all semantics for .hi
  | HIOnlyImports           -- done imports from .hi
%%]]
%%[[99
  | LHSStart                -- starting from .lhs
  | LHSOnlyImports          -- done imports from .lhs
%%]]
  deriving (Show,Eq)
%%]

Is a state working on literal haskell input?

%%[50 export(hsstateIsLiteral)
hsstateIsLiteral :: HSState -> Bool
%%[[99
hsstateIsLiteral LHSStart       = True
hsstateIsLiteral LHSOnlyImports = True
%%]]
hsstateIsLiteral _              = False
%%]

%%[50 export(hsstateShowLit)
hsstateShowLit :: HSState -> String
%%[[99
hsstateShowLit LHSStart       = "Literal"
hsstateShowLit LHSOnlyImports = "Literal"
%%]]
hsstateShowLit _              = ""
%%]

The next thing to do for HSState.

%%[50 export(hsstateNext)
hsstateNext :: HSState -> HSState
hsstateNext HSStart       = HSOnlyImports
hsstateNext HIStart       = HIOnlyImports
-- hsstateNext HMStart       = HMOnlyMinimal
%%[[99
hsstateNext LHSStart      = LHSOnlyImports
%%]]
hsstateNext st            = st
%%]

The state EH compilation can be in

%%[8 export(EHState(..))
data EHState
  = EHStart
  | EHAllSem
  deriving (Show,Eq)
%%]

The state C compilation can be in, which basically is just administering it has to be compiled

%%[(90 codegen) export(CState(..), OState(..))
-- | State for .c files
data CState
  = CStart
  | CAllSem
  deriving (Show,Eq)

-- | State for .o files
data OState
  = OStart
  | OAllSem
  deriving (Show,Eq)
%%]

The state Core compilation can be in

%%[(8 corein) export(CRState(..))
data CRState
  = CRStartBinary
  | CRStartText
  | CROnlyImports
  | CRAllSem
  deriving (Show,Eq)
%%]

%%[(8 corerunin) export(CRRState(..))
data CRRState
  = CRRStartBinary
  -- | CRRStartText
  | CRROnlyImports
  | CRRAllSem
  deriving (Show,Eq)
%%]

The state any compilation can be in

%%[8 export(EHCompileUnitState(..))
data EHCompileUnitState
  = ECUS_Unknown
  | ECUS_Haskell !HSState
  | ECUS_Eh      !EHState
%%[[(90 codegen)
  | ECUS_C       !CState
  | ECUS_O       !OState
%%]]
%%[[(8 corein)
  | ECUS_Core    !CRState
%%]]
%%[[(8 corerunin)
  | ECUS_CoreRun !CRRState
%%]]
  | ECUS_Grin
  | ECUS_Fail
  deriving (Show,Eq)
%%]

%%[8 export(ecuStateFinalDestination)
-- | The final state
ecuStateFinalDestination :: (EHCompileUnitState -> EHCompileUnitState) -> EHCompileUnitState -> EHCompileUnitState
ecuStateFinalDestination postModf
  = postModf . n
  where n (ECUS_Haskell _) = ECUS_Haskell HSAllSem
        n (ECUS_Eh      _) = ECUS_Eh      EHAllSem
%%[[(90 codegen)
        n (ECUS_C       _) = ECUS_C       CAllSem
        n (ECUS_O       _) = ECUS_O       OAllSem
%%]]
%%[[(50 corein)
        n (ECUS_Core    _) = ECUS_Core    CRAllSem
%%]]
%%[[(50 corerunin)
        n (ECUS_CoreRun _) = ECUS_CoreRun CRRAllSem
%%]]
        n _                = ECUS_Fail
%%]

%%[8 export(ecuStateIsCore)
-- | Is compilation from Core source
ecuStateIsCore :: EHCompileUnitState -> Bool
ecuStateIsCore st = case st of
%%[[(8 corein)
  ECUS_Core _ -> True
%%]]
  _           -> False
%%]

%%[8 export(ecuStateIsCoreRun)
-- | Is compilation from CoreRun source
ecuStateIsCoreRun :: EHCompileUnitState -> Bool
ecuStateIsCoreRun st = case st of
%%[[(8 corerunin)
  ECUS_CoreRun _ -> True
%%]]
  _              -> False
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Kind of compilation unit, as known from source as starting point for compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(EHCompileUnitKind(..))
data EHCompileUnitKind
  = EHCUKind_HS     -- Haskell: .hs .lhs .hi
%%[[90
  | EHCUKind_C      -- C: .c
%%]]
  | EHCUKind_None   -- Nothing
  deriving Eq
%%]

%%[8 export(ecuStateToKind)
ecuStateToKind :: EHCompileUnitState -> EHCompileUnitKind
ecuStateToKind s
  = case s of
      ECUS_Haskell _ -> EHCUKind_HS
%%[[(90 codegen)
      ECUS_C       _ -> EHCUKind_C
%%]]
      _              -> EHCUKind_None
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTType & file variation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ASTType(..))
-- | An 'Enum' of all types of ast we can deal with
data ASTType
  = ASTType_HS
  | ASTType_EH
%%[[50
  | ASTType_HI
%%]]
%%[[(8 core)
  | ASTType_Core
%%]]
%%[[(8 corerun)
  | ASTType_CoreRun
%%]]
%%[[(8 grin)
  | ASTType_Grin
%%]]
%%[[(8 cmm)
  | ASTType_Cmm
%%]]
%%[[(8 javascript)
  | ASTType_JavaScript
%%]]
%%[[(8 codegen)
  | ASTType_C
  | ASTType_O
%%]]
  | ASTType_Unknown
  deriving (Eq, Ord, Enum, Typeable, Generic, Bounded, Show)

instance Hashable ASTType
%%]

%%[8 export(ASTFileContent(..))
-- | File content variations of ast we can deal with (in principle)
data ASTFileContent
  = ASTFileContent_Text
  | ASTFileContent_LitText
  | ASTFileContent_Binary
  | ASTFileContent_Unknown
  deriving (Eq, Ord, Enum, Typeable, Generic, Bounded, Show)

instance Hashable ASTFileContent
%%]

%%[8 export(ASTHandlerKey)
-- | Combination of 'ASTType' and 'ASTFileContent' as key into map of handlers
type ASTHandlerKey = (ASTType, ASTFileContent)
%%]

%%[8 export(ASTFileUse(..))
-- | File usage variations of ast
data ASTFileUse
  = ASTFileUse_Cache		-- ^ internal use cache on file
  | ASTFileUse_Dump			-- ^ output: dumped, possibly usable as src later on
  | ASTFileUse_Target		-- ^ output: as target of compilation
  | ASTFileUse_Src			-- ^ input: src file
  | ASTFileUse_Unknown		-- ^ unknown
  deriving (Eq, Ord, Enum, Typeable, Generic, Bounded, Show)

instance Hashable ASTFileUse
%%]

%%[8 export(ASTSuffixKey)
-- | Key for allowed suffixes, multiples allowed to cater for different suffixes
type ASTSuffixKey = (ASTFileContent, ASTFileUse)
%%]

%%[8 export(ASTFileTiming(..))
-- | File timing variations of ast
data ASTFileTiming
  = ASTFileTiming_Prev		-- ^ previously generated
  | ASTFileTiming_Current	-- ^ current one
  deriving (Eq, Ord, Enum, Typeable, Generic, Bounded, Show)

instance Hashable ASTFileTiming
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The initial compile unit info derived from the file suffix
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(FileSuffInitState)
-- | initial state/settings categorizing the kind of file/ast dealing with
type FileSuffInitState =
     ( EHCompileUnitState
     , ASTType
     , ASTFileContent
     , ASTFileUse
     ) 						
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Info & Adaption of search path
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(PrevSearchInfo)
type PrevSearchInfo = (HsName,(FPath,FileLoc))
%%]

%%[99 export(prevSearchInfoAdaptedSearchPath)
-- | strip tail part corresponding to module name, and use it to search as well
prevSearchInfoAdaptedSearchPath :: Maybe PrevSearchInfo -> FileLocPath -> FileLocPath
prevSearchInfoAdaptedSearchPath (Just (prevNm,(prevFp,prevLoc))) searchPath
  = case (fpathMbDir (mkFPath prevNm), fpathMbDir prevFp, prevLoc) of
	  (_, _, p) | filelocIsPkg p
		-> p : searchPath
	  (Just n, Just p, _)
		-> mkDirFileLoc (filePathUnPrefix prefix) : searchPath
		where (prefix,_) = splitAt (length p - length n) p
	  _ -> searchPath
prevSearchInfoAdaptedSearchPath _ searchPath = searchPath
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% How to compile the final step for a target
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(FinalCompileHow(..))
data FinalCompileHow
  = FinalCompile_Module
  | FinalCompile_Exec
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Shell command construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(mkShellCmd,mkShellCmd',showShellCmd)
mkShellCmd' :: [Cmd] -> FilePath -> CmdLineOpts -> (FilePath,[String])
mkShellCmd' forCmds cmdStr o = (cmdStr, showCmdLineOpts' forCmds o)

mkShellCmd :: [String] -> (FilePath,[String])
mkShellCmd (cmd:args) = (cmd,args)

showShellCmd :: (FilePath,[String]) -> String
showShellCmd (cmd,args) = concat $ intersperse " " $ [cmd] ++ args
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Name of in/output + possible dir which is preprended
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(mkInOrOutputFPathDirFor)
mkInOrOutputFPathDirFor :: FPATH nm => InOrOutputFor -> EHCOpts -> nm -> FPath -> String -> (FPath,Maybe String)
mkInOrOutputFPathDirFor inoutputfor opts modNm fp suffix
%%[[8
  = (fpathSetSuff suffix fp', Nothing)
  where fp' = fp
%%][99
  = (fpathSetSuff suffix fp', d)
  where (fp',d) = case inoutputfor of
                    OutputFor_Module   -> f ehcOptOutputDir
                    OutputFor_Pkg      -> f ehcOptOutputDir -- ehcOptOutputPkgLibDir
                    InputFrom_Loc l
                      | filelocIsPkg l -> f (const Nothing)
                      | otherwise      -> f ehcOptOutputDir
        f g     = case g opts of
                    Just d -> ( fpathPrependDir d'
                                $ fpathSetBase (fpathBase fp)	-- ensure possibly adapted name in filesys is used
                                $ mkFPath modNm					-- includes module hierarchy into filename
                              , Just d'
                              )
                           where d' = filePathUnPrefix d
                    _      -> (fp,Nothing)
%%]]
%%]

%%[8 export(mkInOrOutputFPathFor)
mkInOrOutputFPathFor :: FPATH nm => InOrOutputFor -> EHCOpts -> nm -> FPath -> String -> FPath
mkInOrOutputFPathFor inoutputfor opts modNm fp suffix
  = fst $ mkInOrOutputFPathDirFor inoutputfor opts modNm fp suffix
%%]

%%[8 export(mkOutputFPath)
mkOutputFPath :: FPATH nm => EHCOpts -> nm -> FPath -> String -> FPath
mkOutputFPath = mkInOrOutputFPathFor OutputFor_Module
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction of output names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(mkPerModuleOutputFPath)
-- | FPath for per module output
mkPerModuleOutputFPath :: EHCOpts -> Bool -> HsName -> FPath -> String -> FPath
mkPerModuleOutputFPath opts doSepBy_ modNm fp suffix
  = fpO modNm fp
%%[[8
  where fpO m f= mkOutputFPath opts m f suffix
%%][99
  where fpO m f= case ehcOptPkgOpt opts of
                   Just _        -> nm_
                   _ | doSepBy_  -> nm_
                     | otherwise -> mkOutputFPath opts m f suffix
               where nm_ = mkOutputFPath opts (hsnMapQualified (const base) m) (fpathSetBase base f) suffix
                         where base = hsnShow "_" "_" m
%%]]
%%]

%%[8 export(mkPerExecOutputFPath)
-- | FPath for final executable, with possible suffix (and forcing flag, even on given exec)
mkPerExecOutputFPath :: EHCOpts -> HsName -> FPath -> Maybe (String, Bool) -> FPath
mkPerExecOutputFPath opts modNm fp mbSuffix
  = maybe id (\(s,force) -> if force then fpathSetSuff s else id) mbSuffix fpExec
  where fpExecBasedOnSrc = maybe (mkOutputFPath opts modNm fp "") (\(s,_) -> mkOutputFPath opts modNm fp s) mbSuffix
%%[[8
        fpExec = fpExecBasedOnSrc
%%][99
        fpExec = maybe fpExecBasedOnSrc id (ehcOptMbOutputFile opts)
%%]]
%%]

