%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

Note: everything is exported.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Scanner.Common}
%%]

%%[1 import(System.IO, UU.Parsing, UU.Parsing.Offside, UU.Scanner.Position, UU.Scanner.GenToken, UU.Scanner.GenTokenParser, UHC.Util.ScanUtils(), {%{EH}Base.HsName.Builtin}, {%{EH}Base.Common})
%%]

%%[1 import({%{EH}Opts.Base})
%%]

%%[1 import(qualified Data.Set as Set)
%%]

%%[1 import(UHC.Util.ScanUtils)
%%]

%%[1.Scanner import(UU.Scanner, {%{EH}Scanner.TokenParser}) export(module UU.Scanner)
%%]

%%[1 export(module {%{EH}Scanner.Common})
%%]

%%[5.Scanner -1.Scanner import({%{EH}Scanner.Scanner}) export(module {%{EH}Scanner.Scanner})
%%]

%%[8 import (UHC.Util.ParseUtils)
%%]
%%[8 import ({%{EH}Base.Target})
%%]

%%[97 import (Data.Ratio)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner options: keywords etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.ehScanOpts
ehScanOpts :: EHCOpts -> ScanOpts
ehScanOpts opts
  =  defaultScanOpts
%%]
%%[1
        {   scoKeywordsTxt      =
                Set.fromList $
                       tokKeywStrsEH1
                    ++ offsideTrigs
%%]
%%[4
                    ++ tokKeywStrsEH4
%%]
%%[5
                    ++ tokKeywStrsEH5
%%]
%%[6
                    ++ tokKeywStrsEH6
%%]
%%[8
                    ++ tokKeywStrsEH8
%%]
%%[9
                    ++ tokKeywStrsEH9
%%]
%%[11
                    ++ tokKeywStrsEH11
%%]
%%[50
                    ++ tokKeywStrsEH12
%%]
%%[90
                    ++ tokKeywStrsEH90
%%]
%%[91
                    ++ tokKeywStrsEH91
%%]
%%[93
                    ++ (if ehcOptFusion opts then tokKeywStrsEH93 else [])
%%]
%%[1
        ,   scoKeywordsOps      =
                Set.fromList $
                       tokOpStrsEH1
%%]
%%[2
                    ++ tokOpStrsEH2
%%]
%%[3
                    ++ tokOpStrsEH3
%%]
%%[4
                    ++ tokOpStrsEH4
%%]
%%[5
                    ++ tokOpStrsEH5
%%]
%%[6
                    ++ tokOpStrsEH6
%%]
%%[7
                    ++ (if ehcOptExtensibleRecords opts then tokOpStrsEH7 else [])
%%]
%%[9
                    ++ tokOpStrsEH9
%%]
%%[10
                    ++ tokOpStrsEH10
%%]
%%[11
                    ++ tokOpStrsEH11
%%]
%%[1
        ,   scoSpecChars        = Set.fromList $
                "();,[]{}`"
        ,   scoOpChars          = Set.fromList $
                "!#$%&*+/<=>?@\\^|-:.~"
%%]
%%[7
        ,   scoSpecPairs        = Set.fromList $
                [  show hsnORow, show hsnCRow
                ,  show hsnOSum, show hsnCSum
%%[[9
                ,  show hsnOImpl, show hsnCImpl
%%]]
%%[[18
                -- ,  show hsnOParensUnboxed, show hsnCParensUnboxed
%%]]
                ]
%%]
%%[1
        ,   scoOffsideTrigs     =   offsideTrigs
%%[[9
        ,   scoOffsideTrigsGE   =   offsideTrigsGE
%%]]
        ,   scoOffsideModule    =   "let"
        ,   scoOffsideOpen      =   "{"
        ,   scoOffsideClose     =   "}"
        }
  where offsideTrigs     =
            [  "let", "where"
%%[[5
            ,  "of"
%%]]
%%[[8
            ,  "letstrict"
%%]]
            ]
%%[[9
        offsideTrigsGE   =
            [  "do"
            ]
%%]]
%%]

%%[1
hsScanOpts :: EHCOpts -> ScanOpts
hsScanOpts opts
  = ehScanOpts'
%%]
%%[1
        {   scoKeywordsTxt      =
                scoKeywordsTxt ehScanOpts' `Set.union`
                (Set.fromList $
                       offsideTrigs
                    ++ tokKeywStrsHS1
%%[[4
                    ++ tokKeywStrsHS4
%%]]
%%[[5
                    ++ tokKeywStrsHS5
%%]]
%%[[6
                    ++ tokKeywStrsHS6
%%]]
%%[[8
                    ++ tokKeywStrsHS8
%%]]
%%[[9
                    ++ tokKeywStrsHS9
%%]]
%%[[11
                    ++ tokKeywStrsHS11
%%]]
%%[[50
                    ++ tokKeywStrsHS12
%%]]
%%[[90
                    ++ tokKeywStrsHS90
%%]]
%%[[93
                    ++ (if ehcOptFusion opts then tokKeywStrsHS93 else [])
%%]]
                )
%%]
%%[99
        ,   scoPragmasTxt      =
                (Set.fromList $
                       tokPragmaStrsHS99
                )
%%]
%%[1
        ,   scoKeywordsOps      =
                scoKeywordsOps ehScanOpts'
                `Set.union`
                (Set.fromList $
                       tokOpStrsHS1
%%[[2
                    ++ tokOpStrsHS2
%%]]
%%[[3
                    ++ tokOpStrsHS3
%%]]
%%[[4
                    ++ tokOpStrsHS4
%%]]
%%[[5
                    ++ tokOpStrsHS5
%%]]
%%[[6
                    ++ tokOpStrsHS6
%%]]
%%[[7
                    ++ tokOpStrsHS7
%%]]
%%[[9
                    ++ tokOpStrsHS9
%%]]
%%[[10
                    ++ tokOpStrsHS10
%%]]
%%[[11
                    ++ tokOpStrsHS11
%%]]
                )
%%]
%%[1
        ,   scoOffsideTrigs     =
                scoOffsideTrigs ehScanOpts'
                ++ offsideTrigs
        ,   scoOffsideTrigsGE   =
                scoOffsideTrigsGE ehScanOpts'
        ,   scoOffsideModule    =   "module"
        }
  where offsideTrigs     =
            [  "where"
            ]
        ehScanOpts' = ehScanOpts opts
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scan opts for other parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
-- | 'ScanOpts' for 'Core' tokenization before parsing
coreScanOpts :: EHCOpts -> ScanOpts
coreScanOpts opts
  =  grinScanOpts
        {   scoKeywordsTxt      =   (Set.fromList $
                                        [ "let", "in", "case", "of", "rec", "foreign", "uniq"
                                        , "Int", "Char", "String", "Tag", "Rec"
                                        , "module", "default"
                                        , "import", "export"
                                        , "BINDPLAIN", "BINDFUNCTION0", "BINDFUNCTION1", "BINDAPPLY0"
                                        , "VAL"
                                        , "FAIL"
%%[[9
                                        , "DICT", "DICTCLASS", "DICTINSTANCE", "DICTOVERLOADED", "TRACK"
%%]]
%%[[97
                                        , "Integer"
%%]]
%%[[90
                                        , "foreignexport"
%%]]
                                        ])
                                    `Set.union` scoKeywordsTxt tyScanOpts
                                    `Set.union` scoKeywordsTxt hsScanOpts'
        ,	scoKeywExtraChars	=	Set.fromList "."
        ,   scoKeywordsOps      =   scoKeywordsOps grinScanOpts `Set.union` scoKeywordsOps hsScanOpts'
        ,   scoDollarIdent      =   True
        ,   scoOpChars          =   Set.fromList "<->:=+*"
        ,   scoSpecChars        =   Set.fromList "!=();{}#\\|,`"
        ,   scoSpecPairs        =   Set.fromList [] `Set.union` scoSpecPairs ehScanOpts'
        }
  where hsScanOpts' = hsScanOpts opts
        ehScanOpts' = ehScanOpts opts
%%]

%%[8
-- | 'ScanOpts' for 'CoreRun' tokenization before parsing
corerunScanOpts :: {- EHCOpts -> -} ScanOpts
corerunScanOpts -- opts
  =  defaultScanOpts
        {   scoKeywordsTxt      =   Set.fromList $
                                        [ "alloc", "module", "tail", "eval", "case", "of", "let", "in", "app", "ffi", "dbg", "tag"
                                        , "g", "d", "l", "i", "m"
                                        , "data", "import"
                                        ]
        -- ,	scoKeywExtraChars	=	Set.fromList "."
        ,   scoKeywordsOps      =   Set.fromList [ "->", "." ]
        ,   scoSpecChars        =   Set.fromList "();,=\\"
        ,   scoOpChars          =   Set.fromList "->."
        ,   scoAllowFloat       =   False
        ,   scoDollarIdent      =   True
        }
%%]

Todo:

%%[8
tycoreScanOpts :: ScanOpts
tycoreScanOpts
  =  defaultScanOpts
        {   scoKeywordsTxt      =   (Set.fromList $
                                        [ "let", "in", "case", "of", "rec", "foreign", "uniq"
                                        , "Int", "Char", "String", "Tag", "Rec"
                                        , "module", "default"
                                        , "BINDPLAIN", "BINDFUNCTION0", "BINDFUNCTION1", "BINDAPPLY0"
                                        , "VAL"
%%[[9
                                        , "DICT", "DICTCLASS", "DICTINSTANCE", "DICTOVERLOADED"
%%]]
%%[[50
                                        , "Integer"
%%]]
%%[[90
                                        , "foreignexport"
%%]]
                                        ])
        ,   scoKeywordsOps      =   Set.fromList [ "->", "=", ":", "::", "|", "\\" ]
        ,   scoSpecChars        =   Set.fromList "();{},[]"
        ,   scoOpChars          =   Set.fromList "|\\:=-<>"
        ,   scoDollarIdent      =   True
        }
%%]

                "();,[]{}`"
        ,   scoOpChars          = Set.fromList $
                "!#$%&*+/<=>?@\\^|-:.~"

%%[8
grinScanOpts :: ScanOpts
grinScanOpts
  =  defaultScanOpts
        {   scoKeywordsTxt      =   Set.fromList $
                                        [ "eval", "apply"
                                        , "call"
                                        , "module", "update", "fetch", "store", "unit", "of", "rec", "case", "ffi", "fetchupdate"
                                        , "throw", "try", "catch", "ctags", "applymap", "evalmap"
                                        , "C", "F", "P", "A", "R", "H", "U", "W"
                                        , "basicnode", "enumnode", "opaquenode", "ptrnode", "basicannot", "enumannot", "opaqueannot", "ptrannot"
                                        , "annotfromtaggedptr", "annottotaggedptr", "annotdflt"
                                        , "word"
                                        , "DICTCLASS", "DICTINSTANCE", "DICTOVERLOADED", "SPECIALIZED"
                                        , "_"
%%[[97
                                        , "float", "double"
%%]]
%%[[99
                                        , "True", "False"  -- for FFI annotation
%%]]
                                        ]
%%[[90
                                        ++ map show allFFIWays
%%]]
        ,   scoKeywordsOps      =   Set.fromList [ "<-", "->", "=", "+=", "-=", ":=", "-", "*" ]
        ,   scoSpecChars        =   Set.fromList "();{}#/\\|,"
        ,   scoOpChars          =   Set.fromList "<->:=+*"
        ,   scoDollarIdent      =   True
        }
%%]

%%[8
tyScanOpts :: ScanOpts
tyScanOpts
  =  defaultScanOpts
        {   scoKeywordsTxt      =   Set.fromList [ "uid" ]
        }
%%]

%%[8
hsnScanOpts :: ScanOpts
hsnScanOpts
  =  defaultScanOpts
        {   scoKeywordsTxt      =   Set.fromList
        								[ "NEW"
                                        , "ERR"
                                        , "UNQ"
                                        , "EVL"
                                        , "FLD"
                                        , "CLS"
                                        , "DCT"
                                        , "SDC"
                                        , "RDC"
                                        , "SUP"
                                        , "DFL"
                                        , "INL"
                                        , "UND"
                                        , "OFF"
                                        , "CCN"
                                        , "UPD"
                                        , "FFI"
                                        , "LBL"
                                        , "ASP"
                                        , "STR"
%%[[91 
                                        , "GEN"
%%]] 
%%[[(8 javascript) 
                                        , "JSW"
%%]] 
%%[[(8 grin) 
                                        , "GRN"
%%]] 
%%[[(8 cmm) 
                                        , "CMM"
%%]] 
%%[[90
                                        , "FFE"
                                        , "FFC"
%%]]
        							 	]
        ,	scoSpecChars        =   Set.fromList ".{},`"
        ,   scoOpChars          =   Set.fromList "[]:" `Set.union` scoOpChars hsScanOpts'
        ,   scoAllowQualified   =   False
        }
  where hsScanOpts' = hsScanOpts emptyEHCOpts
%%]

%%[90
foreignEntScanOpts :: FFIWay -> ScanOpts
foreignEntScanOpts way
  =  o {   scoKeywordsTxt      =   Set.fromList [ "dynamic", "wrapper", "h", "static", "new", "js" ]
       ,   scoSpecChars        =   Set.fromList ",.&%[]()*{}"
       ,   scoDollarIdent      =   False
       ,   scoKeywExtraChars   =   Set.fromList wayKeywExtraChars
       ,   scoAllowQualified   =   False
       ,   scoStringDelims     =   scoStringDelims o ++ wayStringDelims
       }
  where o = defaultScanOpts
        (wayKeywExtraChars,wayStringDelims)
          = case way of
%%[[(90 javascript)
              FFIWay_JavaScript -> ("$", "'")
%%]]
              _                 -> ("" , "" )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scan file/handle to tokenlist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.scanHandle
scanHandle :: ScanOpts -> FilePath -> Handle -> IO [Token]
scanHandle opts fn fh
  = do  {  txt <- hGetContents fh
        ;  return $ scan (Set.toList $ scoKeywordsTxt opts)
                         (Set.toList $ scoKeywordsOps opts)
                         (Set.toList $ scoSpecChars opts)
                         (Set.toList $ scoOpChars opts)
                         (initPos fn)
                  $ txt
        }
%%]

%%[5 -1.scanHandle
%%]

%%[99
splitTokensOnModuleTrigger :: ScanOpts -> [Token] -> Maybe ([Token],[Token])
splitTokensOnModuleTrigger scanOpts ts
  = case break ismod ts of
      (ts1,ts2@[]) -> Nothing
      tss          -> Just tss
  where ismod (Reserved s _) | s == scoOffsideModule scanOpts = True
        ismod _                                               = False
%%]

%%[1.offsideScanHandle
offsideScanHandle :: ScanOpts -> FilePath -> Handle -> IO (OffsideInput [Token] Token (Maybe Token))
offsideScanHandle scanOpts fn fh
  = do  {  tokens <- scanHandle scanOpts fn fh
        -- ;  putStrLn (" tokens: " ++ show tokens)
%%[[1
        ;  return (scanOffsideWithTriggers moduleT oBrace cBrace triggers tokens)
%%][99
        ;  case splitTokensOnModuleTrigger scanOpts tokens of
             Just (ts1,ts2) -> return $ scanLiftTokensToOffside ts1
                                      $ scanOffsideWithTriggers moduleT oBrace cBrace triggers ts2
             _              -> return $ scanOffsideWithTriggers moduleT oBrace cBrace triggers tokens
%%]]
        }
  where   moduleT   = reserved (scoOffsideModule scanOpts) noPos
          oBrace    = reserved (scoOffsideOpen scanOpts) noPos
          cBrace    = reserved (scoOffsideClose scanOpts) noPos
          triggers  =  [ (Trigger_IndentGT,reserved x noPos) | x <- scoOffsideTrigs   scanOpts ]
                    ++ [ (Trigger_IndentGE,reserved x noPos) | x <- scoOffsideTrigsGE scanOpts ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Splitting up a rational into nominator/denominator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[97
floatDenot2NomDenom :: String -> (Integer,Integer)
floatDenot2NomDenom denot
  = (numerator f,denominator f)
  where (n,m,e) = getRational denot
        f :: Rational
        f = ((read n * md + mn) * en) % (ed * md)
        en, ed, mn, md :: Integer
        (en,ed) = case e of
                    Just (Just "-",e) -> (1,10 ^ read e)
                    Just (_,e)        -> (10 ^ read e,1)
                    _                 -> (1,1)
        (mn,md) = case m of
                    Just m -> (read m,10 ^ length m)
                    _      -> (1,1)

intDenot2Integer :: Int -> String -> Integer
intDenot2Integer b s = getBaseNumber (toInteger b) s
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner related parser abstractions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pKeyTk, pKeyTk'         ::  IsParser p Token
                              => String -> p Token
pKeyTk  key             =   pCostReserved' 9 key
pKeyTk' key             =   pCostReserved' 8 key

pKeyw                   ::  (IsParser p Token,Show k) => k -> p Token
pKeyw k                 =   pKeyTk (show k)
%%]

%%[1
pKeywHsNname            ::  (IsParser p Token,Show k) => k -> p HsName
pKeywHsNname k          =   tokMkQName <$> pKeyw k

%%]

%%[1
pStringTk, pCharTk,
  pInteger8Tk, pInteger10Tk, pInteger16Tk, pFractionTk,
%%]
%%[18
  pVaridUnboxedTk, pConidUnboxedTk,
  pVarsymUnboxedTk, pConsymUnboxedTk,
%%]
%%[50
  pQVaridTk, pQConidTk,
  pQVarsymTk, pQConsymTk,
%%]
%%[1
  pVaridTk , pConidTk ,
  pVaridTk', pConidTk',
  pTextnmTk, pTextlnTk, pIntegerTk, pVarsymTk, pConsymTk
    :: IsParser p Token => p Token

pStringTk     =   pHsCostValToken' 7 TkString    ""
pCharTk       =   pHsCostValToken' 7 TkChar      "\NUL"
pInteger8Tk   =   pHsCostValToken' 7 TkInteger8  "0"
pInteger10Tk  =   pHsCostValToken' 7 TkInteger10 "0"
pInteger16Tk  =   pHsCostValToken' 7 TkInteger16 "0"
pFractionTk   =   pHsCostValToken' 7 TkFraction  "0.0"
pVaridTk      =   pHsCostValToken' 7 TkVarid     "<identifier>"
pVaridTk'     =   pHsCostValToken' 6 TkVarid     "<identifier>"
pConidTk      =   pHsCostValToken' 7 TkConid     "<Identifier>"
pConidTk'     =   pHsCostValToken' 6 TkConid     "<Identifier>"
pConsymTk     =   pHsCostValToken' 7 TkConOp     "<conoperator>"
pVarsymTk     =   pHsCostValToken' 7 TkOp        "<operator>"
pTextnmTk     =   pHsCostValToken' 7 TkTextnm    "<name>"
pTextlnTk     =   pHsCostValToken' 7 TkTextln    "<line>"
pIntegerTk    =   pInteger10Tk
%%]
%%[18
pVaridUnboxedTk      =   pHsCostValToken' 7 TkVaridUnboxed     "<identifier#>"
pConidUnboxedTk      =   pHsCostValToken' 7 TkConidUnboxed     "<Identifier#>"
pConsymUnboxedTk     =   pHsCostValToken' 7 TkConOpUnboxed     "<conoperator#>"
pVarsymUnboxedTk     =   pHsCostValToken' 7 TkOpUnboxed        "<operator#>"
%%]
%%[50
pQVaridTk     =   pHsCostValToken' 7 TkQVarid     "<identifier>"
pQConidTk     =   pHsCostValToken' 7 TkQConid     "<Identifier>"
pQConsymTk    =   pHsCostValToken' 7 TkQConOp     "<conoperator>"
pQVarsymTk    =   pHsCostValToken' 7 TkQOp        "<operator>"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Name parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pCONID, pCONID', pCONSYM, pVARID, pVARID', pVARSYM :: IsParser p Token => p Token

pCONID           = pConidTk
pCONID'          = pConidTk
pCONSYM          = pConsymTk
pVARID           = pVaridTk
pVARID'          = pVaridTk'
pVARSYM          = pVarsymTk
%%]

%%[18
pCONIDUNBOXED, pCONSYMUNBOXED, pVARIDUNBOXED, pVARSYMUNBOXED :: IsParser p Token => p Token

pCONIDUNBOXED    = pConidUnboxedTk
pCONSYMUNBOXED   = pConsymUnboxedTk
pVARIDUNBOXED    = pVaridUnboxedTk
pVARSYMUNBOXED   = pVarsymUnboxedTk
%%]

%%[50
pQCONID, pQCONSYM, pQVARID, pQVARSYM :: IsParser p Token => p Token

pQCONID          = pQConidTk
pQCONSYM         = pQConsymTk
pQVARID          = pQVaridTk
pQVARSYM         = pQVarsymTk
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Extraction from Token
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
tokGetVal :: Token -> String
tokGetVal x
%%[[1
  = case x of
      ValToken _ v p -> v
      Reserved v p   -> v
%%][5
  = tokenVal x
%%]]

pV :: (IsParser p Token) => p Token -> p String
pV p = tokGetVal <$> p

pHNm :: (IsParser p Token) => p Token -> p HsName
pHNm p = (hsnFromString . tokGetVal) <$> p
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Token utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
tokConcat :: Token -> Token -> Token
tokConcat t1 t2 = Reserved (tokenVal t1 ++ tokenVal t2) (position t1)

tokEmpty :: Token
tokEmpty = Reserved "" noPos
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Scanner related parsers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
pMODULE        ,
    pWHERE     ,
    pSEMI      ,
    pDCOLON    ,
    pCOLON     ,
    pOBRACK    ,
    pCBRACK    ,
    pOCURLY    ,
    pCCURLY    ,
    pVOCURLY   ,
    pVCCURLY   ,
    pAT        ,
    pPERCENT   ,
    pDOT       ,
    pCOMMA     ,
    pOPAREN    ,
    pCPAREN    ,
    pINFIX     ,
    pINFIXL    ,
    pINFIXR    ,
    pMINUS     ,
    pSTAR      ,
    pBANG      ,
    pEQUAL     ,
    pRARROW    ,
    pBACKQUOTE ,
    pLET       ,
    pLAM       ,
    pSLASH     ,
    pUNDERSCORE,
    pIN
  :: IsParser p Token => p Token
%%]

%%[1
pMODULE          = pKeyTk "module"
pWHERE           = pKeyTk "where"
pSEMI            = pKeyTk ";"
pCOLON           = pKeyTk ":"
pDCOLON          = pKeyTk "::"
pOBRACK          = pKeyTk "["
pCBRACK          = pKeyTk "]"
pOCURLY          = pKeyTk "{"
pCCURLY          = pKeyTk "}"
pVOCURLY         = pKeyTk "{-layout"
pVCCURLY         = pKeyTk "}-layout"
pAT              = pKeyTk "@"
pPERCENT         = pKeyTk "%"
pDOT             = pKeyTk "."
pCOMMA           = pKeyTk ","
pOPAREN          = pKeyTk "("
pCPAREN          = pKeyTk ")"
pINFIX           = pKeyTk "infix"
pINFIXL          = pKeyTk "infixl"
pINFIXR          = pKeyTk "infixr"
pMINUS           = pKeyTk "-"
pSTAR            = pKeyTk "*"
pBANG            = pKeyTk "!"
pEQUAL           = pKeyTk "="
pRARROW          = pKeyw hsnArrow
pBACKQUOTE       = pKeyTk "`"
pLET             = pKeyTk "let"
pLAM             = pKeyTk "\\"
pSLASH           = pKeyTk "/"
pUNDERSCORE      = pKeyTk "_"
pIN              = pKeyTk "in"

tokKeywStrsEH1 = [ "in", "let" ]
tokKeywStrsHS1 = [ "module", "where", "infix", "infixl", "infixr" ]
tokOpStrsEH1   = [ "=", "\\", show hsnArrow, "::", "@" ]
tokOpStrsHS1   = [ "-", "*", "!", "_", "%", "." ]
%%]

%%[2
pTDOT    	,
    pQDOT
  :: IsParser p Token => p Token
%%]

%%[2
pTDOT            = pKeyTk "..."
pQDOT            = pKeyTk "...."

tokOpStrsEH2   = [ "...", "...." ]
tokOpStrsHS2   = [  ]
%%]

%%[3
tokOpStrsEH3   = [ "%" ]
tokOpStrsHS3   = [  ]
%%]

%%[4
pFORALL       ,
    pEXISTS   ,
    pTILDE
  :: IsParser p Token => p Token
%%]

%%[4
pFORALL          = pKeyTk "forall"
pEXISTS          = pKeyTk "exists"
pTILDE           = pKeyTk (show hsnEqTilde)

tokKeywStrsEH4 = [ "forall", "exists" ]
tokKeywStrsHS4 = [  ]
tokOpStrsEH4   = [ ".", show hsnEqTilde ]
tokOpStrsHS4   = [  ]
%%]

%%[5
pLARROW        ,
    pVBAR      ,
    pDATA      ,
    pNEWTYPE   ,
    pCASE      ,
    pOF        ,
    pIF        ,
    pTHEN      ,
    pELSE      ,
    pDOTDOT
  :: IsParser p Token => p Token
%%]

%%[5
pLARROW          = pKeyTk "<-"
pVBAR            = pKeyTk "|"
pDATA            = pKeyTk "data"
pNEWTYPE         = pKeyTk "newtype"
pCASE            = pKeyTk "case"
pOF              = pKeyTk "of"
pIF              = pKeyTk "if"
pTHEN            = pKeyTk "then"
pELSE            = pKeyTk "else"
pDOTDOT          = pKeyTk ".."

tokKeywStrsEH5 = [ "data", "case", "if", "then", "else", "of" ]
tokKeywStrsHS5 = [ "newtype" ]
tokOpStrsEH5   = [ "|" ]
tokOpStrsHS5   = [ "<-", "..", ":" ]
%%]

%%[6
pFFORALL      ,
    pEEXISTS
  :: IsParser p Token => p Token
%%]

%%[6
pFFORALL         = pKeyTk "Forall"
pEEXISTS         = pKeyTk "Exists"

tokKeywStrsEH6 = [  ]
tokKeywStrsHS6 = [  ]
tokKeywStrsHI6 = [ "Forall", "Exists" ]
tokOpStrsEH6   = [ "*" ]
tokOpStrsHS6   = [  ]
%%]

%%[7
pOROWREC        ,
    pCROWREC    ,
    pOROWROW    ,
    pCROWROW    ,
    pOROWSUM    ,
    pCROWSUM    ,
    pCOLEQUAL   ,
    pHASH
  :: IsParser p Token => p Token
%%]

%%[7
pOROWREC         = pKeyTk (show hsnORec)
pCROWREC         = pKeyTk (show hsnCRec)
pOROWROW         = pKeyTk (show hsnORow)
pCROWROW         = pKeyTk (show hsnCRow)
pOROWSUM         = pKeyTk (show hsnOSum)
pCROWSUM         = pKeyTk (show hsnCSum)
pCOLEQUAL        = pKeyTk ":="
pHASH            = pKeyTk "#"
%%]

%%[7
tokOpStrsEH7   = [ ":=", "#" ]
tokOpStrsHS7   = [  ]
%%]

%%[8
pLABEL          ,
    pLETSTRICT  ,
    pSAFE       ,
    pFOREIGN    ,
    pDEFAULT    ,
    pIMPORT     ,
    pEXPORT
  :: IsParser p Token => p Token
%%]

%%[8
pLABEL           = pKeyTk "label"
pLETSTRICT       = pKeyTk "letstrict"
pSAFE            = pKeyTk "safe"
pFOREIGN         = pKeyTk "foreign"
pDEFAULT         = pKeyTk "default"
pIMPORT          = pKeyTk "import"
pEXPORT          = pKeyTk "export"

tokKeywStrsEH8
  =  [ "letstrict", "foreign", "import" ]
%%[[(8 codegen)
  ++ map show allFFIWays
%%]]
tokKeywStrsHS8 = [ "default", "export", "label", "safe" ]
%%]

%%[8
pFFIWay :: IsParser p Token => p (FFIWay,Token)
pFFIWay
  =   pAnyKey (\way -> (,) way <$> pKeyTk (show way)) allFFIWays
  <?> "pFFIWay"
%%]

%%[9
%%]

%%[9
pDARROW         ,
    pLTCOLON    ,
    pOIMPL      ,
    pCIMPL      ,
    pCLASS      ,
    pINSTANCE   ,
    pDO
  :: IsParser p Token => p Token
%%]

%%[9
pDARROW          = pKeyTk (show hsnPrArrow)
pLTCOLON         = pKeyTk "<:"
pOIMPL           = pKeyTk (show hsnOImpl)
pCIMPL           = pKeyTk (show hsnCImpl)
pCLASS           = pKeyTk "class"
pINSTANCE        = pKeyTk "instance"
pDO              = pKeyTk "do"

tokKeywStrsEH9 = [ "class", "instance" ]
tokKeywStrsHS9 = [ "do" ]
tokOpStrsEH9   = [ show hsnPrArrow, "<:" ]
tokOpStrsHS9   = [  ]
%%]

%%[10
tokOpStrsEH10  = [] -- [ show hsnDynVar ]
tokOpStrsHS10  = [  ]
%%]

%%[40
%%]

%%[11
pTYPE
  :: IsParser p Token => p Token
%%]

%%[11
pTYPE            = pKeyTk "type"
%%]

%%[11
tokKeywStrsEH11 = [ "type" ]
tokKeywStrsHS11 = [  ]
tokOpStrsEH11   = [  ]
tokOpStrsHS11   = [  ]
%%]

%%[50
pQUALIFIED      ,
    pQUESTQUEST ,
    pAS         ,
    pHIDING
  :: IsParser p Token => p Token
%%]

%%[50
pQUALIFIED       = pKeyTk "qualified"
pAS              = pKeyTk "as"
pHIDING          = pKeyTk "hiding"
pQUESTQUEST      = pKeyTk "??"

tokKeywStrsEH12 = [  ]
tokKeywStrsHS12 = [ "qualified", "as", "hiding" ]
%%]

%%[91
pDERIVING
  :: IsParser p Token => p Token

pDERIVING        = pKeyTk "deriving"

tokKeywStrsEH91 = [ "deriving" ]
%%]

%%[90
pUNSAFE     ,
    pTHREADSAFE ,
    pDYNAMIC    ,
    pWRAPPER    ,
    pSTATIC     ,
    pH          ,
    pNEW        ,
    pJS         ,
    pAMPERSAND
  :: IsParser p Token => p Token

pUNSAFE          = pKeyTk "unsafe"
pTHREADSAFE      = pKeyTk "threadsafe"
pDYNAMIC         = pKeyTk "dynamic"
pWRAPPER         = pKeyTk "wrapper" -- not a HS keyword, but only for foreign function entity
pSTATIC          = pKeyTk "static" -- not a HS keyword, but only for foreign function entity
pH               = pKeyTk "h" -- not a HS keyword, but only for foreign function entity
pAMPERSAND       = pKeyTk "&" -- not a HS keyword, but only for foreign function entity
pNEW             = pKeyTk "new"
pJS              = pKeyTk "js"

tokKeywStrsEH90  = [  ]
tokKeywStrsHS90  = [ "unsafe", "threadsafe", "dynamic" ]
%%]

%%[93
pFUSE         ,
    -- pWITH     ,
    pCONVERT
  :: IsParser p Token => p Token

pFUSE    = pKeyTk "fuse"
-- pWITH    = pKeyTk "with"
pCONVERT = pKeyTk "convert"    

tokKeywStrsEH93  = [  ]
tokKeywStrsHS93  = [ "fuse", "convert" ]
%%]

%%[99
pLANGUAGE_prag  		,
	pOPTIONSUHC_prag  	,
	pDERIVABLE_prag		,
	pEXCLUDEIFTARGET_prag,
    pOPRAGMA    		,
    pCPRAGMA
  :: IsParser p Token => p Token

pLANGUAGE_prag   = pKeyTk "LANGUAGE"
pDERIVABLE_prag  = pKeyTk "DERIVABLE"
pEXCLUDEIFTARGET_prag  = pKeyTk "EXCLUDE_IF_TARGET"
pOPTIONSUHC_prag = pKeyTk "OPTIONS_UHC"
pOPRAGMA         = pKeyTk "{-#"
pCPRAGMA         = pKeyTk "#-}"

tokPragmaStrsHS99= [ "LANGUAGE", "DERIVABLE", "EXCLUDE_IF_TARGET", "OPTIONS_UHC" {- , "INLINE", "NOINLINE", "SPECIALIZE" -} ]
%%]

pDEPRECATED_prag = pKeyTk "deprecated_prag"
pCLOSE_prag      = pKeyTk "close_prag"
pSOURCE_prag     = pKeyTk "source_prag"
pRULES_prag      = pKeyTk "rules_prag"
pESCAPE_open     = pKeyTk "parenEscape"
pUNPACK_prag     = pKeyTk "unpack_prag"
pOUBXPAREN       = pKeyTk "oubxparen"
pCUBXPAREN       = pKeyTk "cubxparen"
pINLINE_prag     = pKeyTk "inline_prag"
pNOINLINE_prag   = pKeyTk "noinline_prag"
pSPECIALISE_prag = pKeyTk "specialise_prag"
pCORE_prag       = pKeyTk "core_prag"
pREC             = pKeyTk "rec"
pPARENESCAPE     = pKeyTk "parenEscape"
pOEXPQUOTE       = pKeyTk "openExpQuote"
pCQUOTE          = pKeyTk "closeQuote"
pOTYPQUOTE       = pKeyTk "openTypQuote"
pOPATQUOTE       = pKeyTk "openPatQuote"
pODECQUOTE       = pKeyTk "openDecQuote"
pOPARENBAR       = pKeyTk "oparenbar"
pCPARENBAR       = pKeyTk "cparenbar"
pLARROWTAIL      = pKeyTk "larrowtail"      -- -<
pRARROWTAIL      = pKeyTk "rarrowtail"      -- >-
pDLARROWTAIL     = pKeyTk "Larrowtail"      -- -<<
pDRARROWTAIL     = pKeyTk "Rarrowtail"      -- >>-
pREIFY_TYPE      = pKeyTk "reifyType"
pREIFY_DECL      = pKeyTk "reifyDecl"
pREIFY_FIXITY    = pKeyTk "reifyFixity"
pOCURLYBAR       = pKeyTk "ocurlybar"
pCCURLYBAR       = pKeyTk "ccurlybar"
pCPABRACK        = pKeyTk "[:"
pOPABRACK        = pKeyTk "]"
p_SCC_           = pKeyTk "scc"
pSCC_prag        = pKeyTk "scc_prag"
pMDO             = pKeyTk "mdo"
pPROC            = pKeyTk "proc"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Position
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
%%]
instance Position (Maybe Token) where
  line    =  maybe (-1)  (line.position)
  column  =  maybe (-1)  (column.position)
  file    =  maybe ""    (file.position)

