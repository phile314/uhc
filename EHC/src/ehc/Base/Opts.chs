%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options of all sorts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.Opts} import(System.Console.GetOpt,{%{EH}Base.Common}) export(EHCOpts(..), defaultEHCOpts, ehcCmdLineOpts)
%%]

%%[4 import({%{EH}Ty},UU.Pretty,EH.Util.PPUtils) export(FIOpts(..), fioSwapCoCo, fioSwapOpts, strongFIOpts, unifyFIOpts, instFIOpts, instLRFIOpts, instLFIOpts, fioMkStrong, fioMkUnify)
%%]

%%[4 export(fioIsSubsume)
%%]

%%[4_2 export(meetFIOpts,joinFIOpts,impredFIOpts)
%%]

%%[4_2 export(fioIsMeetJoin)
%%]

%%[5 export(weakFIOpts)
%%]

%%[8 import(Data.List,Data.Char,{%{EH}Base.Builtin}) export(cmdLineTrfs,trfOptOverrides)
%%]

%%[9 export(predFIOpts,implFIOpts)
%%]

%%[99 import(EH.Util.Utils)
%%]

%%[50 import({%{EH}Ty.Instantiate})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data TrfOpt = TrfYes String | TrfNo String | TrfAllYes | TrfAllNo

cmdLineTrfs :: AssocL String String
cmdLineTrfs
  = [ ("CER"    , "Core Eta Reduction")
    , ("CCP"    , "Core Constant Propagation (simple ones introduced by frontend)")
    , ("CRU"    , "Core Rename Unique (all identifiers)")
    , ("CLU"    , "Core Let Unrec (remove unnecessary recursive defs)")
    , ("CILA"   , "Core Inline Let Alias (remove unnecessary alpha renamings)")
    , ("CFL"    , "Core Full Laziness (give names to all expressions and float them outwards)")
    , ("CLL"    , "Core Lambda Lift")
    ]

trfOptOverrides :: [TrfOpt] -> String -> Maybe Bool
trfOptOverrides opts trf
  =  ovr opts
  where  ovr [] = Nothing
         ovr (TrfYes s   :os) | trf == s  = Just True
         ovr (TrfNo s    :os) | trf == s  = Just False
         ovr (TrfAllYes  :os)             = Just True
         ovr (TrfAllNo   :os)             = Just False
         ovr (_          :os)             = ovr os
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compiler options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.EHCOpts
data EHCOpts
  = EHCOpts
      {  ehcOptShowHS         ::  Bool              -- show HS pretty print on stdout
      ,  ehcOptShowEH         ::  Bool              -- show EH pretty print on stdout
      ,  ehcOptShowAst        ::  Bool              -- show decorated EH AST on stdout
      ,  ehcOptShowTopTyPP    ::  Bool              -- show EH type of expression
      ,  ehcOptHelp           ::  Bool              -- print help
      ,  ehcOptVersion        ::  Bool              -- print version info
      ,  ehcOptDebug          ::  Bool              -- debug info
      ,  ehcStopAtPoint       ::  CompilePoint      -- stop at (after) compile phase
%%[[7_2
      ,  ehcOptUniqueness     ::  Bool
%%]]
%%[[8
      ,  ehcOptDumpCallGraph  ::  Bool
      ,  ehcOptDumpTrfGrin    ::  Maybe String
      ,  ehcOptTimeCompile    ::  Bool

      ,  ehcOptGenTrace       ::  Bool
      ,  ehcOptGenCaseDefault ::  Bool
      ,  ehcOptGenTailCall    ::  Bool
      ,  ehcOptGenOwnParams   ::  Bool
      ,  ehcOptGenOwnLocals   ::  Bool
      ,  ehcOptGenOwnCalls    ::  Bool
      ,  ehcOptGenAsmSP       ::  Bool
      ,  ehcOptGenCmt         ::  Bool
      ,  ehcOptGenDebug       ::  Bool				-- generate runtime debug info

      ,  ehcOptShowGrin       ::  Bool              -- show Grin pretty print on stdout
      ,  ehcOptEmitHS         ::  Bool
      ,  ehcOptEmitEH         ::  Bool
      ,  ehcOptEmitCore       ::  Bool
      ,  ehcOptEmitJava       ::  Bool
      ,  ehcOptEmitGrin       ::  Bool
      ,  ehcOptEmitLlc        ::  Bool
      ,  ehcOptEmitLLVM       ::  Bool
      ,  ehcOptEmitGrinBC     ::  Bool
      ,  ehcOptEmitExec       ::  Bool
      ,  ehcOptEmitExecBC     ::  Bool
      ,  ehcOptSearchPath     ::  [String]
      ,  ehcOptVerbosity      ::  Verbosity			-- verbosity level
      ,  ehcOptTrf            ::  [TrfOpt]
      ,  ehcOptOptimise       ::  Optimise			-- optimisation level

      ,  ehcOptBuiltinNames   ::  EHBuiltinNames
%%]]
%%[[9
      ,  ehcCfgInstFldHaveSelf::  Bool				-- functions/fields of instance get as arg the dictionary as well
      ,  ehcOptPrfCutOffAt    ::  Int				-- cut off limit for context reduction
      ,  ehcCfgClassViaRec    ::  Bool				-- instance representation via record instead of data
%%]]
%%[[11
      ,  ehcOptTyBetaRedCutOffAt					-- cut off for type lambda expansion
                              ::  Int
%%]]
%%[[12
      ,  ehcOptCheckRecompile ::  Bool
      ,  ehcOptFullProgGRIN   ::  Bool				-- do full GRIN program analysis
%%]]
%%[[99
      ,  ehcProgName          ::  String			-- name of this program
      ,  ehcOptShowNumVersion ::  Bool				-- numerical version, for external version comparison
      ,  ehcOptCPP            ::  Bool				-- do preprocess with C preprecessor CPP
      ,  ehcOptUseAssumePrelude						-- use & assume presence of prelude
                              ::  Bool
%%]]
      }
%%]

%%[1.defaultEHCOpts
defaultEHCOpts
  = EHCOpts
      {  ehcOptShowHS         =   False
%%[[1
      ,  ehcOptShowEH         =   True
%%][99
      ,  ehcOptShowEH         =   False
%%]]
      ,  ehcOptShowAst        =   False
      ,  ehcOptShowTopTyPP    =   False
      ,  ehcOptHelp           =   False
      ,  ehcOptVersion        =   False
      ,  ehcOptDebug          =   False
      ,  ehcStopAtPoint       =   CompilePoint_All
%%[[7_2
      ,  ehcOptUniqueness     =   True
%%]]
%%[[8
      ,  ehcOptDumpCallGraph  =   False
      ,  ehcOptDumpTrfGrin    =   Nothing
      ,  ehcOptTimeCompile    =   False

      ,  ehcOptGenTrace       =   False
      ,  ehcOptGenCaseDefault =   False
      ,  ehcOptGenTailCall    =   True
      ,  ehcOptGenOwnParams   =   True
      ,  ehcOptGenOwnLocals   =   False
      ,  ehcOptGenOwnCalls    =   False
      ,  ehcOptGenAsmSP       =   False
      ,  ehcOptGenDebug       =   True

      ,  ehcOptShowGrin       =   False
      ,  ehcOptEmitHS         =   False
      ,  ehcOptEmitEH         =   False
      ,  ehcOptEmitCore       =   True
      ,  ehcOptEmitJava       =   False
      ,  ehcOptEmitGrin       =   False
      ,  ehcOptSearchPath     =   []
      ,  ehcOptVerbosity      =   VerboseNormal
      ,  ehcOptOptimise       =   OptimiseNormal
      ,  ehcOptTrf            =   []
      ,  ehcOptBuiltinNames   =   mkEHBuiltinNames (const id)
      ,  ehcOptEmitLLVM       =   False
%%]]
%%[[8
      ,  ehcOptEmitLlc        =   False
      ,  ehcOptEmitExec       =   False
      ,  ehcOptEmitExecBC     =   False
      ,  ehcOptEmitGrinBC     =   False
      ,  ehcOptGenCmt         =   True
%%][99
      ,  ehcOptEmitLlc        =   False -- True
      ,  ehcOptEmitExec       =   False -- True
      ,  ehcOptEmitExecBC     =   True
      ,  ehcOptEmitGrinBC     =   True
      ,  ehcOptGenCmt         =   False
%%]]
%%[[9
      ,  ehcCfgInstFldHaveSelf=   False
      ,  ehcOptPrfCutOffAt    =   20
      ,  ehcCfgClassViaRec    =   False -- True
%%]]
%%[[11
      ,  ehcOptTyBetaRedCutOffAt
                              =   20
%%]]
%%[[12
      ,  ehcOptCheckRecompile    =   True
      ,  ehcOptFullProgGRIN      =   False
%%]]
%%[[99
      ,  ehcProgName          =   ""
      ,  ehcOptShowNumVersion =   False
      ,  ehcOptCPP            =   False
      ,  ehcOptUseAssumePrelude  =   True
%%]]
      }
%%]

%%[1
ehcCmdLineOpts
  =  [  Option "d"  ["debug"]            (NoArg oDebug)                       "show extra debug information"
     ,  Option "p"  ["pretty"]
%%[[1
                    (OptArg oPretty "hs|eh|ast|-")
%%][8
                    (OptArg oPretty "hs|eh|grin|ast|-")
%%]]
                    "show pretty printed EH/Grin source or EH abstract syntax tree, default=eh, -=off, (downstream only)"
     ,  Option ""   ["show-top-ty"]      (OptArg oShowTopTy "yes|no")         "show top ty, default=no"
     ,  Option "h"  ["help"]             (NoArg oHelp)                        "only show this help"
     ,  Option ""   ["version"]          (NoArg oVersion)                     "only show version info"
     ,  Option ""   ["stopat"]
%%[[1
                                         (ReqArg oStopAt "0|1|2|3")           "stop at compile phase 0=imports, 1=parse, 2=hs, 3=eh"
%%][8
                                         (ReqArg oStopAt "0|1|2|3|4")         "stop at compile phase 0=imports, 1=parse, 2=hs, 3=eh, 4=core"
%%]]
%%[[7_2
     ,  Option ""   ["nounique"]         (NoArg oUnique)                      "do not compute uniqueness solution"
%%]]
%%[[8
     ,  Option "c"  ["code"]             (OptArg oCode "hs|eh|core|java|grin|c|exe[c]|llvm|bc|bexe[c]|-")  "write code to file, default=core (downstream only)"
     ,  Option ""   ["trf"]              (ReqArg oTrf ("([+|-][" ++ concat (intersperse "|" (assocLKeys cmdLineTrfs)) ++ "])*"))
                                                                              "switch on/off transformations"
     ,  Option ""   ["time-compilation"] (NoArg oTimeCompile)                 "show grin compiler CPU usage for each compilation phase (only with -v2)"
     ,  Option ""   ["dump-call-graph"]  (NoArg oDumpCallGraph)               "output grin call graph as dot file"
     ,  Option ""   ["dump-trf-grin"]    (OptArg oDumpTrfGrin "basename")     "dump intermediate grin code after transformation"
     ,  Option "v"  ["verbose"]          (OptArg oVerbose "0|1|2|3")          "be verbose, 0=quiet 1=normal 2=noisy 3=debug-noisy, default=1"
     ,  Option "O"  ["optimise"]         (OptArg oOptimise "0|1|2")           "optimise, 0=none 1=normal 2=more, default=1"

     ,  Option ""   ["gen-trace"]        (boolArg optSetGenTrace)             "trace functioncalls in C (no)"
     ,  Option ""   ["gen-casedefault"]  (boolArg optSetGenCaseDefault)       "trap wrong casedistinction in C (no)"
     ,  Option ""   ["gen-tailcall"]     (boolArg optSetGenTailCall)          "jumps for tail calls in C (yes)"
     ,  Option ""   ["gen-ownparams"]    (boolArg optSetGenOwnParams)         "explicit parameter allocation (yes)"
     ,  Option ""   ["gen-ownlocals"]    (boolArg optSetGenOwnLocals)         "explicit local allocation (no)"
     ,  Option ""   ["gen-owncalls"]     (boolArg optSetGenOwnCalls)          "simulate calling mechanism (no)"
     ,  Option ""   ["gen-asmsp"]        (boolArg optSetGenAsmSP)             "use %esp as stack pointer (no)"
     ,  Option ""   ["gen-cmt"]          (boolArg optSetGenCmt)               "include comment about code in generated code"
     ,  Option ""   ["gen-debug"]        (boolArg optSetGenDebug)             "include debug info in generated code (yes)"
%%]]
%%[[12
     ,  Option ""   ["no-recomp"]        (NoArg oNoRecomp)                    "turn off recompilation check (force recompile)"
%%]]
%%[[99
     ,  Option ""   ["numeric-version"]  (NoArg oNumVersion)                  "only show numeric version"
     ,  Option "P"  ["search-path"]      (ReqArg oSearchPath "path")          "search path for all files, path separators=';', appended to previous"
     ,  Option ""   ["no-prelude"]       (NoArg oNoPrelude)                   "do not assume presence of Prelude"
     ,  Option ""   ["cpp"]              (NoArg oCPP)                         "preprocess source with CPP"
%%]]
     ]
%%]
%%[1
  where  oPretty     ms  o =  case ms of
                                Just "-"     -> o { ehcOptShowEH       = False     }
                                Just "no"    -> o { ehcOptShowEH       = False     }
                                Just "off"   -> o { ehcOptShowEH       = False     }
                                Just "hs"    -> o { ehcOptShowHS       = True      }
                                Just "eh"    -> o { ehcOptShowEH       = True      }
                                Just "pp"    -> o { ehcOptShowEH       = True      }
                                Just "ast"   -> o { ehcOptShowAst      = True      }
%%[[8
                                Just "grin"  -> o { ehcOptShowGrin     = True      }
%%]]
                                _            -> o
         oShowTopTy  ms  o =  case ms of
                                Just "yes"  -> o { ehcOptShowTopTyPP   = True      }
                                _           -> o
         oHelp           o =  o { ehcOptHelp          = True    }
         oVersion        o =  o { ehcOptVersion       = True    }
         oDebug          o =  o { ehcOptDebug         = True
                                , ehcOptShowAst       = True
                                }
         oStopAt       s o =  o { ehcStopAtPoint       =
                                    case s of
                                      "0" -> CompilePoint_Imports
                                      "1" -> CompilePoint_Parse
                                      "2" -> CompilePoint_AnalHS
                                      "3" -> CompilePoint_AnalEH
%%[[8
                                      "4" -> CompilePoint_Core
%%]]
                                      _   -> CompilePoint_All
                                }
%%[[7_2
         oUnique         o =  o { ehcOptUniqueness    = False   }
%%]]
%%[[8
         oTimeCompile    o =  o { ehcOptTimeCompile       = True    }
         oDumpTrfGrin ms o =  o { ehcOptDumpTrfGrin       = maybe (Just "") (const ms) ms }
         oDumpCallGraph  o =  o { ehcOptDumpCallGraph     = True }

         oCode       ms  o =  case ms of
                                Just "-"     -> o { ehcOptEmitCore     = False     }
                                Just "hs"    -> o { ehcOptEmitHS       = True      }
                                Just "eh"    -> o { ehcOptEmitEH       = True      }
                                Just "core"  -> o { ehcOptEmitCore     = True      }
                                Just "java"  -> o { ehcOptEmitJava     = True      }
                                Just "grin"  -> o { ehcOptEmitGrin     = True      }
                                Just m | m `elem` ["exe","exec"]
                                             -> o { ehcOptEmitExec     = True, ehcOptEmitLlc = True
%%[[12
                                                  , ehcOptFullProgGRIN    = True
%%]]
                                                  }
                                Just m | m `elem` ["bexe","bexec"]
                                             -> o { ehcOptEmitExecBC   = True, ehcOptEmitGrinBC = True }
                                Just "llvm"  -> o { ehcOptEmitLLVM     = True      }
                                Just "bc"    -> o { ehcOptEmitGrinBC   = True      }
                                Just "c"     -> o { ehcOptEmitLlc      = True
%%[[12
                                                  , ehcOptFullProgGRIN    = True
%%]]
                                                  }
                                _            -> o
         oTrf        s   o =  o { ehcOptTrf           = opt s   }
                           where  opt "" =  []
                                  opt o  =  let  (pm,o2) = span (\c -> c == '+' || c == '-') o
                                                 (tr,o3) = span isAlpha o2
                                                 opt2    = opt o3
                                            in   case (pm,tr) of
                                                   ("+",_:_)  -> TrfYes tr : opt2
                                                   ("-",_:_)  -> TrfNo tr : opt2
                                                   ("+",_)    -> [TrfAllYes]
                                                   ("-",_)    -> [TrfAllNo]
                                                   _          -> []
         oVerbose    ms  o =  case ms of
                                Just "0"    -> o { ehcOptVerbosity     = VerboseQuiet       }
                                Just "1"    -> o { ehcOptVerbosity     = VerboseNormal      }
                                Just "2"    -> o { ehcOptVerbosity     = VerboseALot        }
                                Just "3"    -> o { ehcOptVerbosity     = VerboseDebug       }
                                Nothing     -> o { ehcOptVerbosity     = VerboseALot        }
                                _           -> o
         oOptimise   ms  o =  case ms of
                                Just "0"    -> o { ehcOptOptimise      = OptimiseNone       }
                                Just "1"    -> o { ehcOptOptimise      = OptimiseNormal     }
                                Just "2"    -> o { ehcOptOptimise      = OptimiseALot       }
                                Nothing     -> o { ehcOptOptimise      = OptimiseALot       }
                                _           -> o
%%]]
%%[[12
         oNoRecomp       o =  o { ehcOptCheckRecompile             = False   }
%%]]
%%[[99
         oNumVersion     o =  o { ehcOptShowNumVersion          = True    }
         oSearchPath  s  o =  o { ehcOptSearchPath = ehcOptSearchPath o ++ wordsBy (==';') s }
         oNoPrelude      o =  o { ehcOptUseAssumePrelude        = False   }
         oCPP            o =  o { ehcOptCPP                     = True    }
%%]]
%%]

%%[8
boolArg tr = OptArg (optBoolean tr) "0|1|no|yes|-|+"

optSetGenTrace       o b = o { ehcOptGenTrace       = b }
optSetGenCaseDefault o b = o { ehcOptGenCaseDefault = b }
optSetGenTailCall    o b = o { ehcOptGenTailCall    = b }
optSetGenOwnParams   o b = o { ehcOptGenOwnParams   = b }
optSetGenOwnLocals   o b = o { ehcOptGenOwnLocals   = b }
optSetGenOwnCalls    o b = o { ehcOptGenOwnCalls    = b }
optSetGenAsmSP       o b = o { ehcOptGenAsmSP       = b }
optSetGenCmt         o b = o { ehcOptGenCmt         = b }
optSetGenDebug       o b = o { ehcOptGenDebug       = b }

optBoolean tr ms o
 = case ms of
     Just "-"     -> tr o False
     Just "no"    -> tr o False
     Just "off"   -> tr o False
     Just "0"     -> tr o False
     Just "+"     -> tr o True
     Just "yes"   -> tr o True
     Just "on"    -> tr o True
     Just "1"     -> tr o True
     _            -> o

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Discrimination options for recompile, represent as string, difference means recompile
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12 export(optsDiscrRecompileRepr)
optsDiscrRecompileRepr :: EHCOpts -> String
optsDiscrRecompileRepr opts
  = concat
    $ intersperse " "
    $ [ o "grin"            (ehcOptEmitGrin     opts)
      , o "grinbc"          (ehcOptEmitGrinBC   opts)
      , o "exec"            (ehcOptEmitExec     opts)
      , o "fullproggrin"    (ehcOptFullProgGRIN    opts)
      , o "bexec"           (ehcOptEmitExecBC   opts)
      , o "clsrec"          (ehcCfgClassViaRec   opts)
      , show (ehcOptOptimise opts)
      ]
  where o m v = if v then m else ""
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fitting options (should be in FitsIn, but here it avoids mut rec modules)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4.FIOpts.hd
data FIOpts =  FIOpts   {  fioLeaveRInst     ::  Bool                ,  fioBindRFirst           ::  Bool
                        ,  fioBindLFirst     ::  Bool                ,  fioBindLBeforeR         ::  Bool
                        ,  fioMode           ::  FIMode              ,  fioUniq                 ::  UID
%%]
%%[4_2
                        ,  fioBindToTyAlts   ::  Bool
                        ,  fioDontBind       ::  TyVarIdL
%%]
%%[7.FIOpts
                        ,  fioNoRLabElimFor  ::  [HsName]
%%]
%%[9.FIOpts
                        ,  fioPredAsTy       ::  Bool                ,  fioAllowRPredElim       ::  Bool
                        ,  fioDontBind       ::  TyVarIdL
%%]
%%[50.FIOpts
                        ,  fioAllowEqOpen    ::  Bool                ,  fioInstCoConst          ::  HowToInst
%%]
%%[4.FIOpts.tl
                        }
%%]

%%[4.strongFIOpts.hd
strongFIOpts :: FIOpts
strongFIOpts =  FIOpts  {  fioLeaveRInst     =   False               ,  fioBindRFirst           =   True
                        ,  fioBindLFirst     =   True                ,  fioBindLBeforeR         =   True
                        ,  fioMode           =   FitSubLR            ,  fioUniq                 =   uidStart
%%]
%%[4_2
                        ,  fioBindToTyAlts   =   False
                        ,  fioDontBind       =   []
%%]
%%[7.strongFIOpts
                        ,  fioNoRLabElimFor  =   []
%%]
%%[9.strongFIOpts
                        ,  fioPredAsTy       =   False               ,  fioAllowRPredElim       =   True
                        ,  fioDontBind       =   []
%%]
%%[50.FIOpts
                        ,  fioAllowEqOpen    =   False               ,  fioInstCoConst          =   instCoConst
%%]
%%[4.strongFIOpts.tl
                        }
%%]

%%[4
instance Show FIOpts where
  show o =  "FIOpts"
%%]

%%[4
instance PP FIOpts where
  pp   o =  "FIOpts{"
            >#< "leaveRInst=" >|< pp (fioLeaveRInst o)
            >#< "bindLFirst=" >|< pp (fioBindLFirst o)
            >#< "bindRFirst=" >|< pp (fioBindRFirst o)
%%]
%%[9
            >#< "allowRPredElim=" >|< pp (fioAllowRPredElim o)
%%]
%%[4
            >#< "}"
%%]

%%[4.FIOpts.instLFIOpts
instLFIOpts :: FIOpts
instLFIOpts = strongFIOpts {fioBindRFirst = False}
%%]

%%[4.FIOpts.instLRFIOpts
instLRFIOpts :: FIOpts
instLRFIOpts = strongFIOpts {fioBindRFirst = False, fioBindLFirst = False}
%%]

%%[4.FIOpts.instFIOpts
unifyFIOpts :: FIOpts
unifyFIOpts = strongFIOpts {fioMode = FitUnify}

instFIOpts :: FIOpts
instFIOpts = instLFIOpts {fioLeaveRInst = True, fioBindLFirst = False}
%%]

%%[4_2.FIOpts.defaults
meetFIOpts :: FIOpts
meetFIOpts = unifyFIOpts {fioMode = FitMeet}

joinFIOpts :: FIOpts
joinFIOpts = unifyFIOpts {fioMode = FitJoin}

impredFIOpts :: FIOpts
impredFIOpts = strongFIOpts {fioBindToTyAlts = True}
%%]

%%[5
weakFIOpts :: FIOpts
weakFIOpts = strongFIOpts {fioLeaveRInst = True, fioBindRFirst = False}
%%]

%%[9
predFIOpts :: FIOpts
predFIOpts = strongFIOpts {fioPredAsTy = True, fioLeaveRInst = True}

implFIOpts  :: FIOpts
implFIOpts = strongFIOpts {fioAllowRPredElim = False}
%%]

%%[4
fioSwapOpts :: FIOpts -> FIOpts
fioSwapOpts fio = fio { fioBindRFirst = fioBindLFirst fio, fioBindLFirst = fioBindRFirst fio, fioBindLBeforeR = not (fioBindLBeforeR fio) }

fioSwapCoCo :: CoContraVariance -> FIOpts -> FIOpts
fioSwapCoCo coco fio = fio {fioMode = fimSwapCoCo coco (fioMode fio)}
%%]

%%[4.fioMkStrong
fioMkStrong :: FIOpts -> FIOpts
fioMkStrong fi = fi {fioLeaveRInst = False, fioBindRFirst = True, fioBindLFirst = True}
%%]

%%[4.fioMkUnify
fioMkUnify :: FIOpts -> FIOpts
fioMkUnify fi = fi {fioMode = FitUnify}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FitsIn opts related
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4
fioIsSubsume :: FIOpts -> Bool
fioIsSubsume fio =  case fioMode fio of {FitSubLR -> True ; _ -> False}
%%]

%%[4_2
fioIsMeetJoin :: FIOpts -> Bool
fioIsMeetJoin fio =  case fioMode fio of {FitMeet -> True ; FitJoin -> True ; _ -> False}
%%]
