

-- UUAGC 0.9.39.1 (build/101/lib-ehc/EH101/GrinByteCode.ag)
module EH101.GrinByteCode(module EH101.Base.BasicAnnot
, AGItf (..), Module (..), Instr (..), Instrs, Meta (..)
, InsOp_LocE (..), InsOp_LocB (..), InsOp_LocODst (..), InsOp_LocOSrc (..), InsOp_TyOp (..), InsOp_DataOp (..), InsOp_ImmSz (..), InsOp_Deref (..), InsOp_DerefB (..), Imm (..)
, nrValWords
, nrNodeHdrWords
, nrCallRetWords
, CodeAbsLoc, CodeRelOff
, LabelId, newLabelId
, LabelLocMp, labelLocAdd, labelLocNew
, InsSeq
, Const (..)
, StringConst' (..), StringConst
, LocRef (..), lrefIsLabel
, StackDepth
, GCPermitMp
, gcpermitMpRestrict
, StackState (..), emptyStackState
, ststFromDep, ststFromPerm, ststFromDepPerm
, ststPatchDepPerm, ststPatchTOSGCNot
, ststInc, ststIncDep, ststIncPerm
, GCStackInfo' (..), GCStackInfo
, CCallEncInfo' (..), CCallEncInfo
, FunctionInfoFlag (..), FunctionInfo' (..), FunctionInfo
, CallInfoKind (..)
, CallInfo' (..), CallInfo, CallInfoCall
, mkCICall
, EntryInfo' (..), EntryInfo
, LinkChainKind (..), LinkChainKey (..), LinkChainId (..), linkChainId, LinkChainKeyInfo, lckeyMbLbl, lckeyMbLoc
, LinkChainKeyMp
, LinkChainResolvedInfo (..), LinkChainResolvedIndInfoSet, emptyLinkChainResolvedIndInfoSet
, linkChainResolvedInfoEncoding
, LinkChainResolvedMp
, LinkChainEntry (..)
, linkChainConst, linkChainCode, linkChainOffset, linkChainOffsets
, linkChainResolvedLookup
, linkChainMpResolve
, LoadCtxt (..), defaultLoadCtxt
, OptimEffect (..), defaultOptimEffect
, ValAccessAnnot (..)
, ValAccess (..), ValAccessGam
, NmEnv (..)
, LoadSrc (..)
, Load (..)
, nmLd
, GBState (..), emptyGBState
, gbstIncByStackState
, GrValIntro (..)
, GrValIntroAlt (..)
, gviLd, gviLd', gviLdFold
, gvCall
, nrWord2Byte
, nrByte2Word
, tagIsUnboxed
, tagAllowsUnboxedLife
, ldc, ldl, ldg, ldi, l1tr, l2ts, ldnt
, labelref, label, labelref2
, fetch, fetchupdate
, eval, apply
, funstart, call, retcall, casecall, retcase, callc
, taileval, tailcall, tailapply
, op
, untag2
, AnnKind (..)
, meta', meta
, tag
, halt
, FunctionInfoExportMp
, linkChainImpEntry
, ImpNmMp
, nmEnvLookup) where

import EH101.Base.Common
import EH101.GrinCode
import qualified EH101.Config as Cfg
import EH101.GrinCode.Pretty
import EH101.Base.BasicAnnot
import EH.Util.Utils
import EH.Util.Pretty as Pretty
import Data.Bits
import Data.Maybe
import qualified EH.Util.FastSeq as Seq
import qualified Data.Map as Map
import EH101.Base.Builtin
import EH101.Base.Builtin2
import EH101.Opts
import EH101.Base.Bits as Binary
import EH101.Core(HsName2OffsetMpMp)
import EH101.LamInfo
import Control.Monad
import Data.Typeable (Typeable)
import Data.Generics (Data)
import EH101.Base.Serialize























nrValWords :: Int
nrValWords = 1



nrNodeHdrWords :: Int
nrNodeHdrWords = 1



nrCallRetWords :: Int
nrCallRetWords = 2



type CodeAbsLoc = Int		-- absolute location
type CodeRelOff = Int		-- relative location



type LabelId = Int

newLabelId :: GBState -> (GBState,LabelId)
newLabelId st = (st {gbstLbl = l+1},l)
  where l = gbstLbl st



type LabelLocMp = Map.Map LabelId CodeAbsLoc

labelLocAdd :: LabelId -> CodeAbsLoc -> LabelLocMp -> LabelLocMp
labelLocAdd lbl loc m = Map.insert lbl loc m

labelLocNew :: GBState -> CodeAbsLoc -> LabelLocMp -> (GBState,LabelId,LabelLocMp)
labelLocNew lbl loc m
  = (next,lbl',Map.insert lbl' loc m)
  where (next,lbl') = newLabelId lbl



type InsSeq = Seq.FastSeq Instr



data Const
  = Const_CFunction     	{ constCFunNm :: !String }
  | Const_CCallEncWrapper   { constSizes  :: ![BasicSize] }
  deriving(Eq,Ord,Show)




data StringConst' a
  = StringConst a
  deriving(Eq,Ord)

type StringConst = StringConst' String



lrefIsLabel :: LocRef -> Bool
lrefIsLabel (LocRef_CodeEntry     _) = False
lrefIsLabel _                        = True

data LocRef
  = LocRef_CodeEntry    { lrefId :: !Int     }       -- resolved at initialization runtime, translates to
  | LocRef_Label        { lrefId :: !LabelId }       -- resolved before runtime, translates to offsets
  | LocRef_EndSwitch    { lrefId :: !LabelId }       -- as label, but we know it is the end of a switch
  | LocRef_CaseArm      							 -- as label, but with tag of case arm included, for optional use further down the pipeline
                        { lrefId  :: !LabelId
                        , lrefTag :: !Int
                        }
  deriving (Eq,Ord,Show)



instance PP LocRef where
  pp (LocRef_CodeEntry c) = "_FunLbl_"                     >|< c
  pp (LocRef_Label     l) = "_Lbl_"                        >|< l
  pp (LocRef_EndSwitch l) = "_EndSwitchLbl_"               >|< l
  pp (LocRef_CaseArm l c) = "_CaseLbl_" >|< l



type StackDepth = Int



-- 1 bit No(0)/Yes(1) permit GC tracing, see also ststGCEncoding
gcpermitEncoding :: GCPermit -> Int
gcpermitEncoding GCPermit_Not = 0
gcpermitEncoding _            = 1



type GCPermitMp = Map.Map StackDepth GCPermit



-- restrict to offset
gcpermitMpRestrict :: StackDepth -> GCPermitMp -> GCPermitMp
gcpermitMpRestrict maxOffset = Map.filterWithKey (\o _ -> o <= maxOffset)



-- distinguish only between GCPermit_Not and the rest
gcpermitMpCanonicalize :: GCPermitMp -> GCPermitMp
gcpermitMpCanonicalize
  = Map.map m
  where m x@GCPermit_Not = x
        m _              = GCPermit_Must



data StackState
  = StackState
      { ststDepth		:: !StackDepth
      , ststGCPermitMp	:: !GCPermitMp
      }
  deriving Show

instance PP StackState where
  pp  = pp . show

emptyStackState :: StackState
emptyStackState = StackState 0 Map.empty



ststFromDep :: StackDepth -> StackState
ststFromDep d = emptyStackState { ststDepth = d }

ststFromPerm :: [StackDepth] -> GCPermit -> StackState
ststFromPerm offs perm = emptyStackState { ststGCPermitMp = Map.fromList [ (o,perm) | o<-offs ] }

ststFromDepPerm :: StackDepth -> GCPermit -> StackState
ststFromDepPerm d perm = ststFromPerm [0..d-1] perm `ststInc` ststFromDep d



-- patch top elements
ststPatchDepPerm :: StackDepth -> GCPermit -> StackState
ststPatchDepPerm d perm = ststFromPerm [-d .. -1] perm

ststPatchTOSGCNot :: StackState
ststPatchTOSGCNot = ststPatchDepPerm 1 GCPermit_Not



infixl 3 `ststInc`, `ststIncDep`

-- right operand increments left
ststInc :: StackState -> StackState -> StackState
ststInc st stBy
  = st { ststDepth      = dep + ststDepth stBy
       , ststGCPermitMp = Map.mapKeys (+dep) (ststGCPermitMp stBy) `Map.union` gmp
       }
  where dep = ststDepth      st
        gmp = ststGCPermitMp st

ststIncDep :: StackState -> StackDepth -> StackState
ststIncDep st d = st `ststInc` ststFromDep d

ststIncPerm :: StackState -> ([StackDepth],GCPermit) -> StackState
ststIncPerm st (o,p) = st `ststInc` ststFromPerm o p



data GCStackInfo' x
  = GCStackInfo
      { gcstinfoSz		:: !Int			-- the size of the stack covered
      , gcstinfoPerms	:: !x			-- descriptors for GC permission, encoded as 7 bits size, 1 bit No(0)/Yes(1) permit GC tracing
      }
  | GCStackInfo_None
  deriving (Eq,Ord,Show)

type GCStackInfo = GCStackInfo' [Int]



ststGCEncoding :: StackState -> GCStackInfo
ststGCEncoding stState
  = GCStackInfo sz encs
  where sz = ststDepth stState
        fillGaps off sz []
             | sz > 0      = [(off,sz,GCPermit_Must)]
             | otherwise   = []
        fillGaps off sz (p@(poff,psz,_):ps)
             | poff >  off = [(off,poff-off,GCPermit_Must)] ++ [p] ++ ps'
             | poff == off = [p] ++ ps'
             | otherwise   = panic $ "GrinByteCode.ststGCEncoding: sz/poff/off " ++ show sz ++ "/" ++ show poff ++ "/" ++ show off
             where off' = poff + psz
                   ps' = fillGaps off' (sz - off') ps
        encs
           = concatMap
               (\(_,psz,perm)
                  -> let n = entierUpShrBy max psz
                         enc  = gcpermitEncoding perm
                         mk x = x `shiftL` 1 .|. enc
                     in  replicate (n-1) (mk max) ++ [mk (psz - (n-1) * max)]
               )
           $ map (\l@((o,_,p) : _) -> (o,sum $ map (\(_,len,_) -> len) l,p))
           $ groupOn (\(_,_,p) -> p)
           $ fillGaps 0 sz
           $ map (\l@((o,p) : _) -> (o,length l,p))
           $ consecutiveBy (\(o1,p1) (o2,p2) -> o1 == o2-1 && p1 == p2)
           $ sortOn fst
           $ Map.toList
           $ gcpermitMpCanonicalize
           $ ststGCPermitMp stState
        logmax = 7
        max = pow2 logmax - 1



data CCallEncInfo' x
  = CCallEncInfo
      { ccencinfoSizes	:: !x			-- wrapping/encoding of C call, between BC & C stacks
      }
  deriving (Eq,Ord,Show)

type CCallEncInfo = CCallEncInfo' [BasicSize]



data FunctionInfoFlag
  = FunctionInfoFlag_None
  | FunctionInfoFlag_1stArgIsStackTrace					-- 1st arg is explicit stacktrace
  deriving Show

instance PP FunctionInfoFlag where
  pp = pp . show

data FunctionInfo' x
  = FunctionInfo
      { funinfoKey				:: !x					-- name of function, later the index
      , funinfoNm				:: !HsName				-- name of function, used to build exported/propagated mapping to above index
      , funinfoMaxStackSz		:: !StackDepth			-- max stack size used by function, in words (in final bytecode in bytes)
      , funinfoFlags			:: [FunctionInfoFlag]	-- optional info, config, etc
      }

instance Eq (FunctionInfo' x) where
  i1 == i2 = funinfoNm i1 == funinfoNm i2

instance Ord (FunctionInfo' x) where
  i1 `compare` i2 = funinfoNm i1 `compare` funinfoNm i2

type FunctionInfo = FunctionInfo' String

emptyFunctionInfo :: FunctionInfo
emptyFunctionInfo = FunctionInfo "" hsnUnknown 1 []



type FunctionInfoExportMp = Map.Map HsName GrinByteCodeLamInfo			-- to index into table of FunctionInfos, to be included in HIInfo



-- the order must correspond to the first defines of CallInfo_Kind_XXX in rts/base/types.h
data CallInfoKind
  = CallInfoKind_Call             -- normal call
  | CallInfoKind_Tail             -- tail call
  | CallInfoKind_Eval             -- eval call
  | CallInfoKind_EvalWrap         -- eval call internal wrapper
  | CallInfoKind_TailEv           -- tail eval call
  | CallInfoKind_Apply            -- apply call
  | CallInfoKind_CCall            -- C call
  | CallInfoKind_EvCont           -- eval update continuation
  | CallInfoKind_ApCont           -- apply continuation
  | CallInfoKind_PApCont          -- partial apply continuation
  | CallInfoKind_Hdlr             -- exception handler installment
  | CallInfoKind_TailEval         -- tail eval call
  deriving (Enum,Eq,Ord,Show)



data CallInfoExtra
  = CallInfoExtra_CCall [BasicSize]		-- type: (res : args)
  | CallInfoExtra_None
  deriving (Eq,Ord,Show)



instance PP CallInfoExtra where
  pp (CallInfoExtra_CCall t) = pp $ show $ concatMap basicGrinSizeCharEncoding t
  pp CallInfoExtra_None      = pp "NULL"



type MbStrInx  = Maybe Int
type MbCIStr   = Maybe String
type MbCIStrNm = Maybe (Maybe String,HsName)

data CallInfo' info str
  = CallInfo
      { ciKind			:: !CallInfoKind
      , ciMbKey	        :: !(Maybe str)					-- string (or index), if any
      , ciGCStackInfo 	:: !info
      , ciExtra			:: !CallInfoExtra
      }
  deriving (Eq,Ord,Show)

type CallInfo     = CallInfo' GCStackInfo StringConst
type CallInfoCall = CallInfo' GCStackInfo (Maybe StringConst,HsName)




mkCIExtra :: CallInfoExtra -> CallInfoKind -> MbCIStr -> GCStackInfo -> CallInfo
mkCIExtra e k i gc = CallInfo k (fmap StringConst i) gc e

mkCI :: CallInfoKind -> MbCIStr -> CallInfo
mkCI k i = mkCIExtra CallInfoExtra_None k i GCStackInfo_None

mkCIEval, mkCITailEval, mkCIApply :: MbCIStr -> CallInfo
mkCIEval       = mkCI                              CallInfoKind_Eval
mkCITailEval   = mkCI                              CallInfoKind_TailEval
mkCIApply      = mkCI                              CallInfoKind_Apply

mkCICall :: MbCIStrNm -> CallInfoCall
mkCICall i = CallInfo CallInfoKind_Call (fmap (\(i,n) -> (fmap StringConst i,n)) i) GCStackInfo_None CallInfoExtra_None

mkCICCall :: [BasicSize] -> MbCIStr -> GCStackInfo -> CallInfo
mkCICCall e    = mkCIExtra (CallInfoExtra_CCall e) CallInfoKind_CCall



data EntryInfo' str
  = EntryInfo
      { eiNm			:: !HsName
      , eiEntryNr		:: !Int
      , eiIsCAF			:: !Bool
      -- , eiNmStrConst	:: !Const
      , eiMbNmStr		:: !(Maybe str)
      }

type EntryInfo = EntryInfo' StringConst



data LinkChainEncoding
  = LinkChainEncoding_Ind			-- to a separate table, the payload now is the index into that table
  | LinkChainEncoding_16_10			-- inline, 16(32) bits for inx, 10(26) for offset
  deriving (Enum)

data LinkChainKind
  = LinkChainKind_None
  | LinkChainKind_GCInfo			-- ref to GC Info
  | LinkChainKind_Const				-- ref to constant
  | LinkChainKind_Code				-- ref to global code entrypoint
  | LinkChainKind_Offset			-- ref to label
  | LinkChainKind_Offsets			-- ref to labels
  | LinkChainKind_CallInfo			-- ref to callinfo
  | LinkChainKind_StringConst		-- ref to C string start address
  | LinkChainKind_FunctionInfo		-- ref to function info
  | LinkChainKind_ImpEntry			-- ref to imported code entrypoint
  deriving (Eq,Ord,Enum,Show)

data LinkChainId
  = LinkChainId_Lbl !LabelId
  | LinkChainId_Loc !CodeAbsLoc
  deriving (Eq,Ord,Show)

linkChainId :: (LabelId -> x) -> (CodeAbsLoc -> x) -> LinkChainId -> x
linkChainId lbl _   (LinkChainId_Lbl l) = lbl l
linkChainId _   loc (LinkChainId_Loc l) = loc l

data LinkChainKey
  = LinkChainKey
      { lckeyKind		:: !LinkChainKind
      , lckeyId			:: !LinkChainId
      }
  deriving (Eq,Ord,Show)

lckeyMbLbl :: LinkChainKey -> Maybe LabelId
lckeyMbLbl k = case lckeyId k of {LinkChainId_Lbl l -> Just l ; _ -> Nothing}

lckeyMbLoc :: LinkChainKey -> Maybe LabelId
lckeyMbLoc k = case lckeyId k of {LinkChainId_Loc l -> Just l ; _ -> Nothing}

type LinkChainKeyInfo = Int													-- info must be representable by Int



type LinkChainKeyMp      = Map.Map LinkChainKey LinkChainKeyInfo





-- when resolved we have the original info & relative offset to next entry
data LinkChainResolvedInfo = LinkChainResolvedInfo LinkChainKind LinkChainKeyInfo CodeRelOff

-- the 'escape' entries when it does not fit in a word, as reversed list + its (cached) size
type LinkChainResolvedIndInfoSet = (Int,[LinkChainResolvedInfo])

emptyLinkChainResolvedIndInfoSet :: LinkChainResolvedIndInfoSet
emptyLinkChainResolvedIndInfoSet = (0,[])



linkChainResolvedInfoEncoding :: LinkChainResolvedIndInfoSet -> LinkChainResolvedInfo -> (Integer,LinkChainResolvedIndInfoSet)
linkChainResolvedInfoEncoding
  = \indset i
       -> case i of
            LinkChainResolvedInfo kind info off
              | info' >= 0 && info' < p1 && off' < p2
                          -> (info' `shiftL` b1 .|. off' `shiftL` b2 .|. k kind `shiftL` b3 .|. e16_10, indset)
              | otherwise -> let (sz,inds) = indset
                             in  (toInteger sz `shiftL` b3 .|. eInd, (sz+1, i : inds))
              where info' = toInteger info
                    off'  = toInteger off
  where k :: Enum a => a -> Integer
  	k e  = toInteger $ fromEnum e
        b1 = Cfg.sizeofWordInBits `div` 2
        b3 = 2
        b2 = b3 + 4
        p1 = pow2 (Cfg.sizeofWordInBits - b1)
        p2 = pow2 (b1 - b2)
        e16_10 = k LinkChainEncoding_16_10
        eInd   = k LinkChainEncoding_Ind



type LinkChainResolvedMp = Map.Map LinkChainKey LinkChainResolvedInfo



-- as it appears in GB code
data LinkChainEntry
  = LinkChainEntry
       { lcentryKey		:: !LinkChainKey
       , lcentryInfo	:: !LinkChainKeyInfo
       -- , lcentryInfo2	:: !LinkChainKeyInfo	-- extra info only for certain LinkChainKinds
       }
  deriving Show



linkChainEntry' :: LinkChainKind -> LabelId -> Int -> Int -> LinkChainEntry
linkChainEntry' kind lbl inx inx2 = LinkChainEntry (LinkChainKey kind (LinkChainId_Lbl lbl)) inx -- inx2

linkChainEntry :: LinkChainKind -> LabelId -> Int -> LinkChainEntry
linkChainEntry kind lbl inx = linkChainEntry' kind lbl inx 0

linkChainConst :: LabelId -> Int -> LinkChainEntry
linkChainConst = linkChainEntry LinkChainKind_Const

linkChainCode :: LabelId -> Int -> LinkChainEntry
linkChainCode = linkChainEntry LinkChainKind_Code

linkChainOffset :: LabelId -> Int -> LinkChainEntry
linkChainOffset = linkChainEntry LinkChainKind_Offset

linkChainOffsets :: LabelId -> Int -> LinkChainEntry
linkChainOffsets = linkChainEntry LinkChainKind_Offsets



linkChainImpEntry :: LabelId -> Int -> LinkChainEntry
linkChainImpEntry = linkChainEntry LinkChainKind_ImpEntry



linkChainResolvedLookup :: LinkChainKey -> LinkChainResolvedMp -> Maybe LinkChainResolvedInfo
linkChainResolvedLookup = Map.lookup



linkChainMpResolve :: LabelLocMp -> LinkChainKeyMp -> (LinkChainResolvedMp,Maybe CodeRelOff)
linkChainMpResolve labelLocMp keyMp
  = foldr (\(k,(info,mbabsloc)) (m,mbnextabsloc) ->
              let diff  = case (mbnextabsloc,mbabsloc) of
                            (Just n, Just l) -> n - l
                            _                -> 0
                  info' = case (lckeyKind k,mbabsloc) of
                            (LinkChainKind_Offset, Just l) | isJust mblblloc
                               -> fromJust mblblloc - (l + Cfg.gbLabelOffsetSize)
                               where mblblloc = Map.lookup info labelLocMp
                            _  -> info
              in  (Map.insert k (LinkChainResolvedInfo (lckeyKind k) info' diff) m, mbabsloc)
          )
          (Map.empty,Nothing)
  $ sortOn (snd.snd)
  $ [ (k, (i, linkChainId (\l -> Map.lookup l labelLocMp) Just (lckeyId k)))
    | (k,i) <- Map.toList keyMp
    ]



data LoadCtxt
  = LoadCtxt
      { lcxDoLdTOS    	:: !Bool			-- for 0 offset sp relative emit relative load (when True) or delay by emitting LoadSrc_TOS
      , lcxOmitTOSLd	:: !Bool			-- if LoadSrc_TOS is emitted allow its corresponding optimization by not loading a ref to TOS on TOS
      }

defaultLoadCtxt :: LoadCtxt
defaultLoadCtxt = LoadCtxt False False



data OptimEffect
  = OptimEffect
{- -- moved to separate transformation to eliminate redundant eval's
      { oefIsEvaluated         :: Bool
      }
-}

defaultOptimEffect :: OptimEffect
defaultOptimEffect = OptimEffect {- False -}



-- type ValAccessAnnot = Either BasicAnnot BasicSize			-- either we have to deal with the annotation or it has already been done partly and we need to propagate the size

data ValAccessAnnot			-- either we have to deal with the annotation or it has already been done partly and we need to propagate the size
  = ValAccessAnnot_Annot !BasicAnnot
  | ValAccessAnnot_Basic !BasicSize  !GCPermit
  deriving Show

valAccessAnnot :: (BasicAnnot -> x) -> (BasicSize -> GCPermit -> x) -> ValAccessAnnot -> x
valAccessAnnot f _ (ValAccessAnnot_Annot a  ) = f a
valAccessAnnot _ f (ValAccessAnnot_Basic s p) = f s p

vaAnnotBasicSize :: ValAccessAnnot -> BasicSize
vaAnnotBasicSize = valAccessAnnot grinBasicAnnotSize const

vaAnnotGCPermit :: ValAccessAnnot -> GCPermit
vaAnnotGCPermit = valAccessAnnot grinBasicAnnotGCPermit (flip const)



data ValAccess
  = Val_Local           { vaStackDepth :: !StackDepth, vaAnnot :: !ValAccessAnnot }		-- value on the stack
  | Val_NodeFldLocal    { vaStackDepth :: !StackDepth, vaAnnot :: !ValAccessAnnot }		-- field 0 of node on the stack
  | Val_Int             Integer
  | Val_GlobEntry       { vaEntryInx :: !Int }
  | Val_ImpEntry        { vaModOff, vaEntryOff :: !Int }
  deriving Show

instance PP ValAccess where
  pp = pp . show

type ValAccessGam = Map.Map HsName ValAccess



vaHasAnnot :: ValAccess -> Bool
vaHasAnnot (Val_Local        _ _) = True
vaHasAnnot (Val_NodeFldLocal _ _) = True
vaHasAnnot _                      = False



type ImpNmMp = Map.Map HsName Int



data NmEnv
  = NmEnv
      { neVAGam     :: ValAccessGam
      , neImpNmMp   :: HsName2OffsetMpMp
      , neLamMp     :: LamMp
      }



nmEnvLookup :: HsName -> NmEnv -> Maybe ValAccess
nmEnvLookup nm env
  = case Map.lookup nm $ neVAGam env of
      Nothing
        -> do { q <- hsnQualifier nm
              ; (mo,entryMp) <- Map.lookup q $ neImpNmMp env
              ; eo <- Map.lookup nm entryMp
              ; return (Val_ImpEntry mo eo
                                     {- (maybe (-1) id
                                      $ do { li <- Map.lookup nm $ neLamMp env
                                           ; fi <- laminfoGrinByteCode li
                                           ; return (gblaminfoFuninfoKey fi)
                                           }
                       ) -}            )
              }
      v -> v



data LoadSrc
  = LoadSrc_TOS
  | LoadSrc_Imm
      { ldsrcImm      	:: !Integer
      }
  | LoadSrc_Imm_Int
      { ldsrcImm      	:: !Integer
      }
  | LoadSrc_TOS_Rel
      { ldsrcOff      	:: !StackDepth
      , ldsrcNrWords 	:: Int
      }
  | LoadSrc_Reg_Rel
      { ldsrcOff      	:: !StackDepth
      , ldsrcNrWords 	:: Int
      }

instance Show LoadSrc where
  show LoadSrc_TOS = "sp[0]"
  show (LoadSrc_Imm x) = "#" ++ show x
  show (LoadSrc_Imm_Int x) = "#" ++ show x
  show (LoadSrc_TOS_Rel o s) = "sp[" ++ show o ++ ".." ++ show (o+s-1) ++ "]"
  show (LoadSrc_Reg_Rel o s) = "rr[" ++ show o ++ ".." ++ show (o+s-1) ++ "]"



ldsrc2ins :: Bool -> (Instr -> Instr) -> GCPermit -> LoadSrc -> (InsSeq,StackState)
ldsrc2ins doLdTOS instrann gcPermit s
  = case s of
      {-
      LoadSrc_TOS | doLdTOS   -> ( Seq.fromList  $ ann      $ ldlm nrValWords 0, nrwd )
                  | otherwise -> ( Seq.empty                          , zero )
      -}
      LoadSrc_TOS             -> ( Seq.empty                          , zero )
      LoadSrc_TOS_Rel  o n    -> ( Seq.fromList  $ ann      $ ldlm n o, d n  )
      LoadSrc_Reg_Rel  o n    -> ( Seq.fromList  $ ann      [ l1tr (o+o') | o' <- reverse rng ]
                                 , d n
                                 )
                              where rng = [0 .. n-1]
      LoadSrc_Imm      c      -> ( Seq.singleton $ instrann $ ldc  c  , nrwd )
      LoadSrc_Imm_Int  c      -> ( Seq.singleton $ instrann $ ldi  c  , nrwd )
  where ldlm n o = replicate n (ldl o)
        -- ldrm n o = [ l1tr o' | o' <- reverse [o..o+n-1] ]	-- hardcoded 'stack grows down' assumption
        ann (h:t) = instrann h : t
        d i  = ststFromDepPerm i gcPermit
        zero = d 0
        nrwd = d nrValWords



instance PP LoadSrc where
  pp = pp . show



data Load
  = Load
      { ldPreIns        :: !InsSeq
      , ldPreStackInc   :: !StackDepth
      , ldPreLoc        :: !LoadSrc
      , ldPostIns       :: !InsSeq
      }

loadWithPrePostFrom :: [Instr] -> StackDepth -> LoadSrc -> [Instr] -> Load
loadWithPrePostFrom pre presz s post = Load (Seq.fromList pre) presz s (Seq.fromList post)

loadWithPreFrom :: [Instr] -> StackDepth -> LoadSrc -> Load
loadWithPreFrom pre presz s = loadWithPrePostFrom pre presz s []

loadWithPre0From :: [Instr] -> LoadSrc -> Load
loadWithPre0From pre s = loadWithPreFrom pre 0 s

loadWithPost0From :: LoadSrc -> [Instr] -> Load
loadWithPost0From s post = loadWithPrePostFrom [] 0 s post

loadFrom :: LoadSrc -> Load
loadFrom s = loadWithPreFrom [] 0 s



type NmLdInfo = (Maybe ValAccess,Load,BasicSize,GCPermit)



nmLd' :: LoadCtxt -> NmEnv -> StackState -> GBState -> HsName -> (NmLdInfo,GBState)
nmLd' ldcxt env stState gbState nm
  = case nmEnvLookup nm env of
      mva@(Just va)
        -> case va of
             Val_Local o annot
               -> ( ( mva
                    , loadWithPrePostFrom [meta' AnnIdUse (nm >#< valAccessAnnot pp (>#<) annot >#< src)]
                                          0 src post
                    , vaAnnotBasicSize annot, vaAnnotGCPermit annot
                    )
                  , gbState
                  )
               where (src,post)
                         = case annot of
                             ValAccessAnnot_Annot (BasicAnnot_Size bsz _    BasicAnnotTagging_FromPtr   sgn)
                                                                                   -> (srcDflt Cfg.sizeofGrWord ,[untag2 sgn {-(basicSizeIsSigned bsz)-}])
                             ValAccessAnnot_Annot (BasicAnnot_Size bsz _    BasicAnnotTagging_ToPtr     _)
                                                                                   -> (srcDflt Cfg.sizeofGrWord ,[])
                             ValAccessAnnot_Annot (BasicAnnot_Size bsz _    _   _) -> (srcDflt (basicSizeInBytes bsz),[])
                             ValAccessAnnot_Annot (BasicAnnot_Dflt               ) -> (srcDflt Cfg.sizeofGrWord ,[])
                             ValAccessAnnot_Basic _ _                              -> (srcDflt Cfg.sizeofGrWord ,[])
                         where srcDflt n = offSrc (stkDepth - o) (nrByte2Word n)
             {-
             Val_NodeTagLocal o
               -> (mva,loadFrom (offSrc (stkDepth - o) nrValWords),dfltBasicSize,gbState)
             -}
             Val_NodeFldLocal o annot
               -> ( (mva,l,vaAnnotBasicSize annot,vaAnnotGCPermit annot)
                  , gbState
                  )
               where a = meta' AnnIdUse (nm >#< valAccessAnnot pp (>#<) annot >#< vaAnnotGCPermit annot)
                     l = case annot of
                           ValAccessAnnot_Annot (BasicAnnot_Size bsz _    BasicAnnotTagging_FromPtr   sgn)
                                                                                 -> loadWithPost0From (offSrc (stkDepth - o) nrValWords) [a, untag2 sgn {-(basicSizeIsSigned bsz)-}]
                           ValAccessAnnot_Annot (BasicAnnot_Size bsz _    BasicAnnotTagging_ToPtr     sgn)
                                                                                 -> loadWithPost0From (offSrc (stkDepth - o) nrValWords) [a, tag2   sgn {-(basicSizeIsSigned bsz)-}]
                           ValAccessAnnot_Annot (BasicAnnot_Size bsz _    _   _) -> loadWithPre0From [a, ldlr (stkDepth - o)] (LoadSrc_Reg_Rel nrNodeHdrWords (nrByte2Word $ basicSizeInBytes bsz))
                           _                                                     -> panic "GrinByteCode.nmLd'.Val_NodeFldLocal"
             Val_Int i
               -> ( (mva,loadFrom (LoadSrc_Imm_Int i),dfltBasicSize,GCPermit_May)
                  , gbState
                  )
             Val_GlobEntry o
               -> ( ( mva
                    , loadWithPreFrom [instrann' AnnIdUse nm $ ldg InsOp_LocB_TOS (linkChainCode codeLbl o)]
                                      nrValWords LoadSrc_TOS -- (offSrc 0 nrValWords)
                    , dfltBasicSize, GCPermit_Must
                    )
                  , gbState2
                  )
               where (gbState2,codeLbl) = newLabelId gbState
             Val_ImpEntry mo eo
               -> ( ( mva
                    , loadWithPre0From [instrann' AnnIdUse nm $ ldg InsOp_LocB_Reg (linkChainImpEntry codeLbl mo)]
                                       (LoadSrc_Reg_Rel (eo + nrNodeHdrWords) nrValWords)
                    , dfltBasicSize
                    , GCPermit_Must
                    )
                  , gbState2
                  )
               where (gbState2,codeLbl) = newLabelId gbState
      _ -> ( (Nothing,loadWithPreFrom [instrann' AnnIdUse ("dummy ld for" >#< nm) $ ldl stkDepth] nrValWords LoadSrc_TOS {-(offSrc 0 nrValWords)-},dfltBasicSize,GCPermit_Not)
           , gbState
           )
  where offSrc off sz | off == 0 && not (lcxDoLdTOS ldcxt) = LoadSrc_TOS
                      | otherwise                          = LoadSrc_TOS_Rel off sz
        dfltBasicSize = grinBasicAnnotSize BasicAnnot_Dflt
        stkDepth = ststDepth stState



vaAndLd2Ins :: LoadCtxt -> HsName -> (NmLdInfo,GBState) -> (InsSeq,StackState,GBState)
vaAndLd2Ins ldcxt nm vald
  = case vald of
      ((Just (Val_Local _ (ValAccessAnnot_Annot annot)),Load pins pdep LoadSrc_TOS postins,_,gcPermit),gbState)
        | not (lcxOmitTOSLd ldcxt)
          -> (pins Seq.:++: ins Seq.:++: postins, ststFromDep pdep `ststInc` incld, gbState)
          where (ins,incld) = ldsrc2ins False (instrann' AnnIdUse (nm >#< annot)) gcPermit (LoadSrc_TOS_Rel 0 nrValWords)
      ((_,Load pins pdep ls postins,_,gcPermit),gbState)
          -> (pins Seq.:++: ins Seq.:++: postins, ststFromDep pdep `ststInc` incld, gbState)
          where (ins,incld) = ldsrc2ins (lcxDoLdTOS ldcxt) (instrann $ show ls) gcPermit ls



nmLd :: LoadCtxt -> NmEnv -> StackState -> GBState -> HsName -> (InsSeq,StackState,GBState)
nmLd ldcxt env stState gbState nm
  = vaAndLd2Ins ldcxt nm $ nmLd' ldcxt env stState gbState nm



data GBState
  = GBState
      { gbstLbl			:: !LabelId
      , gbstMaxStkDepth	:: !StackDepth
      }

emptyGBState :: GBState
emptyGBState = GBState 0 0



-- Increment gbstate by info from stackstate.
-- The function may be called redundantly, i.e. its implementation must be idempotent
gbstIncByStackState :: GBState -> StackState -> GBState
gbstIncByStackState g s
  = g { gbstMaxStkDepth = gbstMaxStkDepth g `max` ststDepth s
      }



data GrValIntro
  = GrValIntro_Nm    !HsName
  | GrValIntro_Int   !Integer
  | GrValIntro_Str   !String -- !Int
  | GrValIntro_Grp   !GrTag ![GrValIntro]
  | GrValIntro_Basic !GrTag !HsName
  | GrValIntro_Enum  !HsName
  | GrValIntro_None



data GrValIntroAlt
  = GrValIntroAlt_OnTOS     InsSeq StackState OptimEffect [BasicSize]
  | GrValIntroAlt_Delay     InsSeq StackState OptimEffect NmLdInfo



gviLdFold' :: EHCOpts -> GrValIntroAlt -> LoadCtxt -> NmEnv -> StackState -> GBState -> [GrValIntro] -> (GrValIntroAlt,GBState)
gviLdFold' opts dflt ldcxt env stState gbState introL
  = foldl ld (dflt, gbState) $ reverse $ introL
  where ld (GrValIntroAlt_OnTOS ins inc _ basicsz, gbState) intro
          = (GrValIntroAlt_OnTOS (ins Seq.:++: ins') (inc `ststInc` inc') defaultOptimEffect (basicsz' ++ basicsz), gbState2)
          where (GrValIntroAlt_OnTOS ins' inc' _ basicsz', gbState2) = gviLd opts ldcxt env (stState `ststInc` inc) gbState intro

gviLdFold :: EHCOpts -> LoadCtxt -> NmEnv -> StackState -> GBState -> [GrValIntro] -> (GrValIntroAlt,GBState)
gviLdFold opts = gviLdFold' opts (GrValIntroAlt_OnTOS Seq.empty emptyStackState defaultOptimEffect [])

gviLd' :: EHCOpts -> LoadCtxt -> NmEnv -> StackState -> GBState -> GrValIntro -> (GrValIntroAlt,GBState)
gviLd' opts ldcxt env stState gbState intro
  = ld ldcxt (GrValIntroAlt_OnTOS Seq.empty emptyStackState defaultOptimEffect []) intro
  where ld ldcxt dflt@(GrValIntroAlt_OnTOS ins dfltInc _ _) intro
          = case intro of
              GrValIntro_Nm nm
                -> (GrValIntroAlt_Delay ins dfltInc defaultOptimEffect nmLdInfo, gbState3)
                where (nmLdInfo,gbState3) = nmLd' ldcxt env stState gbState2 nm
              GrValIntro_Str s -- constInx
                -> (GrValIntroAlt_OnTOS (ins Seq.::+: str) (dfltInc `ststIncDep` nrValWords) defaultOptimEffect [dfltBasicSize], gbState3)
                where (gbState3,constLbl) = newLabelId gbState2
                      -- lce = linkChainConst constLbl constInx
                      -- str = ldg InsOp_LocB_TOS lce
                      str = ldstr InsOp_LocB_TOS (StringConst s)
              GrValIntro_Int i
                -> (GrValIntroAlt_Delay ins dfltInc defaultOptimEffect (Nothing,loadFrom $ LoadSrc_Imm_Int i,dfltBasicSize,GCPermit_Not), gbState)
              GrValIntro_Enum nm
                -> case mbva of
                     Just va | vaHasAnnot va
                       -> case vaAnnot va of
                            ValAccessAnnot_Annot BasicAnnot_Dflt                -> basicAnnotDflt ml
                            ValAccessAnnot_Annot (BasicAnnot_Size bsz _ BasicAnnotTagging_ToPtr sgn)
                                                                                -> basicAnnotTaggedPtr sgn {-(basicSizeIsSigned bsz)-} nm ml dfltBasicSize
                            _                                   -> panic "GrinByteCode.gviLd'.GrValIntro_Enum.Just"
                     _ -> panic "GrinByteCode.gviLd'.GrValIntro_Enum.Nothing"
                where ml@((mbva,_,_,_),_) = nmLd' ldcxt env stState gbState nm
              GrValIntro_Basic gtag nm
                -> case mbva of
                     Just va | vaHasAnnot va
                       -> case vaAnnot va of
                            ValAccessAnnot_Annot BasicAnnot_Dflt                    -> basicAnnotDflt ml
                            ValAccessAnnot_Annot (a@(BasicAnnot_Size bsz _ BasicAnnotTagging_ToPtr sgn))
                                                                                    -> basicAnnotTaggedPtr sgn {-(basicSizeIsSigned bsz)-} nm ml (grinBasicAnnotSize a)
                            ValAccessAnnot_Annot (BasicAnnot_Size bsz _ _ _)
                              -> ( GrValIntroAlt_OnTOS (ins Seq.:++: Seq.fromList [meta' AnnTag (ppGrTag gtag)] Seq.:++: tins Seq.:++: sins)
                                                       sinc defaultOptimEffect [basicsz]
                                 , gbState6 `gbstIncByStackState` stState4
                                 )
                              where (ins,inc,gbState4) = vaAndLd2Ins ldcxt nm ml
                                    stState3 = stState2 `ststInc` inc
                                    (tins,tinc,isEvaluated,gbState5) = tag env stState3 gtag (Just bsz) wsz gbState4
                                    (gbState6,allocLbl) = newLabelId gbState5
                                    stState4 = stState3 `ststInc` tinc
                                    (sins,sinc) = ndStore stState4 (wsz + ststDepth tinc) allocLbl
                                    wsz  = nrByte2Word $ basicSizeInBytes bsz
                            ValAccessAnnot_Basic bSz _ -> basicSize nm ml bSz
                            _ -> panic "GrinByteCode.gviLd'.GrValIntro_Basic.Just"
                     _ -> panic "GrinByteCode.gviLd'.GrValIntro_Basic.Nothing"
                where ml@((mbva,_,basicsz,_),gbState3) = nmLd' ldcxt env stState gbState2 nm
              GrValIntro_Grp gtag is
                -> ( GrValIntroAlt_OnTOS (prepins Seq.:++: ins Seq.:++: Seq.fromList [meta' AnnTag (ppGrTag gtag)] Seq.:++: tins Seq.:++: sins)
                                         sinc defaultOptimEffect [dfltBasicSize]
                   , gbState5 `gbstIncByStackState` stState5
                   )
                where (prepins,prepinc) = case (is, tag env stState2 gtag Nothing 0 gbState2) of
                                            ([],(_,st,_,_)) | Cfg.nodeNeedsForwarding opts && ststDepth st == 1
                                              -> (Seq.fromList [ldc 0],ststFromDepPerm 1 GCPermit_Not)
                                            _ -> (Seq.empty           ,ststFromDep 0)
                      stState3 = stState2 `ststInc` prepinc
                      i@(GrValIntroAlt_OnTOS ins inc _ _, gbState3) = gviLdFold' opts dflt (ldcxt {lcxOmitTOSLd = False}) env stState3 gbState2 is
                      inc2 = prepinc `ststInc` inc
                      stState4 = stState3 `ststInc` inc
                      (tins,tinc,isEvaluated,gbState4) = tag env stState4 gtag Nothing (ststDepth inc2) gbState3
                      stState5 = stState4 `ststInc` tinc
                      inc3 = inc2 `ststInc` tinc
                      (gbState5,allocLbl) = newLabelId gbState4
                      (sins,sinc) = ndStore stState5 (ststDepth inc3) allocLbl
              _ -> (dflt, gbState2)
          where stState2 = stState `ststInc` dfltInc
                gbState2 = gbState `gbstIncByStackState` stState2
                basicAnnotDflt (nmLdInfo,gbState)
                  = (GrValIntroAlt_Delay ins dfltInc defaultOptimEffect nmLdInfo, gbState)
                basicAnnotTaggedPtr sgn nm ml bSz
                  = (GrValIntroAlt_OnTOS (ins Seq.::+: tag2 sgn) inc defaultOptimEffect [bSz], gbState)
                  where (ins,inc,gbState) = vaAndLd2Ins ldcxt nm ml
                basicSize nm ml bSz
                  = (GrValIntroAlt_OnTOS (ins) inc defaultOptimEffect [bSz], gbState)
                  where (ins,inc,gbState) = vaAndLd2Ins ldcxt nm ml
                dfltBasicSize = grinBasicAnnotSize BasicAnnot_Dflt

gviLd :: EHCOpts -> LoadCtxt -> NmEnv -> StackState -> GBState -> GrValIntro -> (GrValIntroAlt,GBState)
gviLd opts ldcxt env stState gbState intro
  = case gviLd' opts ldcxt env stState gbState intro of
      (GrValIntroAlt_Delay ins inc optimEffect nmLdInfo@(mbva,l,basicsz,gcPermit), gbState2)
        -> (GrValIntroAlt_OnTOS (ins Seq.:++: sins) (inc `ststInc` sinc) optimEffect [basicsz], gbState3)
        where (sins,sinc,gbState3) = vaAndLd2Ins ldcxt hsnUnknown (nmLdInfo,gbState2)
      a -> a



gvCall :: MbCIStrNm -> NmEnv -> InsSeq -> StackState -> StackState -> GBState -> HsName -> (InsSeq,StackState,GBState)
gvCall i nmEnv ins inc stState gbState nm
  = (ins Seq.:++: fins Seq.:++: Seq.fromList [call i InsOp_LocB_TOS], ststFromDep nrValWords, gbState2)
  where (fins,_,gbState2) = nmLd defaultLoadCtxt nmEnv (stState `ststInc` inc) gbState nm



nrWord2Byte :: Integral c => c -> c
nrWord2Byte sz
  = sz * wSz
  where wSz = fromInteger Cfg.sizeofGrWordAsInteger



nrByte2Word :: Integral c => c -> c
nrByte2Word sz
  = (sz-1) `div` wSz + 1
  where wSz = fromInteger Cfg.sizeofGrWordAsInteger



tagIsUnboxed :: EHCOpts -> GrTag -> Bool
tagIsUnboxed opts
  = \gt -> case gt of
              GrTag_Con _ _ nm | check nm
                -> True
              GrTag_Unboxed
                -> True
              _ -> False
  where check = maybe False biGbcMayLiveUnboxed . builtinGrinMayLiveUnboxed opts



tagAllowsUnboxedLife :: EHCOpts -> GrTag -> Unbox
tagAllowsUnboxedLife opts gt
  = if tagIsUnboxed opts gt
    then Unbox_FirstField
    else case gt of
           GrTag_Con ann t nm | gtannMaxArity ann == 0
             -> Unbox_Tag t
           _ -> Unbox_None



imm :: Integral c => c -> (InsOp_ImmSz,Imm)
imm c
  = (s,Imm_Int i)
  where (s,i)
          = case toInteger c of
              i | i <  0 -> (s, i .&. m)
                         where (s,m) = case i of
                                         i | i >= pow2_7neg  -> (InsOp_ImmSz_Bits08,mask2_8 )
                                           | i >= pow2_15neg -> (InsOp_ImmSz_Bits16,mask2_16)
                                           | i >= pow2_31neg -> (InsOp_ImmSz_Bits32,mask2_32)
                                           | otherwise       -> (InsOp_ImmSz_Bits64,mask2_64)
              i | i >= 0 -> (s, i .&. mask2_64)
                         where s = case i of
                                     i | i < pow2_7sub1  -> InsOp_ImmSz_Bits08
                                       | i < pow2_15sub1 -> InsOp_ImmSz_Bits16
                                       | i < pow2_31sub1 -> InsOp_ImmSz_Bits32
                                       | otherwise       -> InsOp_ImmSz_Bits64



ld :: Integral c => InsOp_Deref -> InsOp_LocB -> InsOp_LocE -> c -> Instr
ld ind locB locE c
  = Instr_Ld ind locB locE s i
  where (s,i) = imm c

-- load constant on TOS
ldc :: Integral c => c -> Instr
ldc = ld InsOp_Deref_Zero InsOp_LocB_TOS InsOp_LocE_Imm

-- load integer constant on TOS
ldi :: Integral c => c -> Instr
ldi = ld InsOp_Deref_Int InsOp_LocB_TOS InsOp_LocE_Imm

-- load local relative to SP on TOS
ldl :: Integral c => c -> Instr
ldl c = ld InsOp_Deref_One InsOp_LocB_TOS InsOp_LocE_SP (nrWord2Byte c)

-- load local relative to SP in reg
ldlr :: Integral c => c -> Instr
ldlr c = ld InsOp_Deref_One InsOp_LocB_Reg InsOp_LocE_SP (nrWord2Byte c)

-- load relative to reg on TOS
l1tr :: Integral c => c -> Instr
l1tr c = ld InsOp_Deref_One InsOp_LocB_TOS InsOp_LocE_Reg (nrWord2Byte c)

l2ts :: Integral c => c -> Instr
l2ts c = ld InsOp_Deref_Two InsOp_LocB_TOS InsOp_LocE_SP (nrWord2Byte c)

ldg :: InsOp_LocB -> LinkChainEntry -> Instr
ldg locB l = Instr_LdGlobal locB l

ldstr :: InsOp_LocB -> StringConst -> Instr
ldstr locB s = Instr_LdString locB s

ldnt :: Instr
ldnt = Instr_LdNodeTag





st :: Integral c => InsOp_DerefB -> InsOp_LocE -> InsOp_LocB -> c -> Instr
st ind locE locB c
  = Instr_St ind locE locB s i
  where (s,i) = imm c



labelref :: LocRef -> Instr
labelref l = Instr_LabelRef l

labelref2 :: [LabelId] -> GBState -> ([Instr],GBState)
labelref2 lbls st
  = foldr (\l (is,st) -> let (st',llbl) = newLabelId st
                         in  (Instr_LinkChain (linkChainOffset llbl l) : is, st')
          )
          ([],st) lbls

label :: LocRef -> Instr
label l = Instr_Label l



fetch :: InsOp_LocB -> Instr
fetch locB = Instr_Fetch locB

fetchupdate :: Instr
fetchupdate = Instr_FetchUpdate



allocstore :: InsOp_LocB -> LabelId -> GCStackInfo -> Instr
allocstore locB lbl gcStackInfo = Instr_AllocStore locB gcStackInfo

-- prepare node allocation by providing padding upto minimum size
{-
ndStorePrep :: EHCOpts -> Int -> (InsSeq,StackDepth)
ndStorePrep opts sz
  = (Seq.fromList $ replicate szExtra $ ldc 0, szExtra)
  where szPadded = Cfg.minwordsofNode opts `max` sz
        szExtra  = szPadded - sz
-}

ndStore :: StackState -> Int -> LabelId -> (InsSeq,StackState)
ndStore stState sz lbl
  = ( Seq.fromList [ ldc (nrWord2Byte sz)
                   , meta' AnnAllocStore sz, meta' AnnStackDepth stState2
                   , allocstore InsOp_LocB_TOS lbl (ststGCEncoding stState2)
                   ]
    , ststFromDep nrValWords
    )
  where stState2 = stState `ststInc` ststFromDepPerm 1 GCPermit_Not



eval :: MbCIStr -> InsOp_LocB -> Instr
eval i locB = Instr_Eval locB (mkCIEval i)

apply :: MbCIStr -> InsOp_LocB -> Instr
apply i locB = Instr_Apply locB (mkCIApply i)



funstart :: FunctionInfo -> Instr
funstart i = Instr_FunStart i

call :: MbCIStrNm -> InsOp_LocB -> Instr
call i locB = Instr_Call locB (mkCICall i)

rettailcall :: Integral c => (InsOp_ImmSz -> InsOp_ImmSz -> Imm -> Imm -> Instr) -> c -> c -> Instr
rettailcall mk nArgMine nArgSurr -- retOffSurr
  = mk s1 s2 i1 i2
  where (s1,i1) = imm $ nrWord2Byte nArgMine
        (s2,i2) = imm $ nrWord2Byte nArgSurr
        -- (s3,i3) = imm $ nrWord2Byte retOffSurr

retcall :: Integral c => c -> c -> Instr
retcall = rettailcall Instr_RetCall

casecall :: LinkChainEntry -> Instr
casecall l
  = Instr_CaseCall l

callc :: Integral c => MbCIStr -> (LabelId,Int,[BasicSize]) -> StackState -> c -> Instr
callc i (lbl,inx,ty) stState nArg
  = Instr_CallC s1 i1 {- ce -} l (mkCICCall ty i (ststGCEncoding stState))
  where (s1,i1) = imm nArg
        -- (_ ,ce) = imm $ basicGrinSizeLEncoding ty
        l       = linkChainConst lbl inx

retcase :: Integral c => c -> c -> LinkChainEntry -> Instr
retcase nRes retOffSurr l
  = Instr_RetCase s1 s2 i1 i2 l
  where (s1,i1) = imm $ nrWord2Byte nRes
        (s2,i2) = imm $ nrWord2Byte retOffSurr



taileval :: Integral c => MbCIStr -> InsOp_LocB -> c -> Instr
taileval i locB nArgSurr
  = Instr_TailEval locB s1 i1 (mkCITailEval i)
  where (s1,i1) = imm $ nrWord2Byte nArgSurr
        -- (s2,i2) = imm $ nrWord2Byte retOffSurr

tailcall :: Integral c => InsOp_LocB -> c -> c -> Instr
tailcall locB = rettailcall (Instr_TailCall locB)

tailapply :: Integral c => InsOp_LocB -> c -> c -> Instr
tailapply locB = rettailcall (Instr_TailApply locB)




op :: Integral c => InsOp_TyOp -> InsOp_DataOp -> InsOp_LocODst -> InsOp_Deref -> InsOp_LocOSrc -> c -> Instr
op opTy opndTy locDst ind locSrc c
  = Instr_Op opTy opndTy locDst ind locSrc s1 i1
  where (s1,i1) = imm c



tagi2w :: Instr
tagi2w = Instr_TagInt2Word

untagw2i :: Instr
untagw2i = Instr_UntagWord2Int

tagw2w :: Instr
tagw2w = Instr_TagWord2Word

untagw2w :: Instr
untagw2w = Instr_UntagWord2Word



tag2 :: Bool -> Instr
tag2 sgn = if sgn then tagi2w else tagw2w

untag2 :: Bool -> Instr
untag2 sgn = if sgn then untagw2i else untagw2w



data AnnKind
  = AnnComment
  | AnnFunStart
  | AnnStackDepth
  | AnnIdUse
  | AnnTag
  | AnnAllocStore
  | AnnLabel
  | AnnLabelRef
  deriving (Eq,Show)

instance PP AnnKind where
  pp AnnComment      = Pretty.empty
  pp AnnFunStart     = pp "funstart"
  pp AnnStackDepth   = pp "stackoff"
  pp AnnIdUse        = pp "iduse"
  pp AnnTag          = pp "tag"
  pp AnnAllocStore   = pp "allocstore"
  pp AnnLabel        = pp "lbldef"
  pp AnnLabelRef     = pp "lblref"



meta' :: PP a => AnnKind -> a -> Instr
meta' k a = Instr_Meta $ Meta_CmtHeader k $ pp a

meta :: PP a => a -> Instr
meta = meta' AnnComment



instrann' :: PP a => AnnKind -> a -> Instr -> Instr
instrann' k a = Instr_Ann k $ showPP a

instrann :: PP a => a -> Instr -> Instr
instrann = instrann' AnnComment





[tgShSize,tgShCateg,tagShGC,tagShTag,tgShNdEv]
  = if Cfg.use32Bits then [16,14,12,2,0::Int] else [32,30,28,2,0]

tgNeedEval_No, tgNeedEval_Yes, tgNeedEval_BlH :: Integer
tgNeedEval_No  = 0
tgNeedEval_Yes = 1
tgNeedEval_BlH = 2

tgCat_GB_NodeTagCat_Con
  , tgCat_GB_NodeTagCat_PAp
  , tgCat_GB_NodeTagCat_Fun
  , tgCat_GB_NodeTagCat_App
  , tgCat_GB_NodeTagCat_Intl
      :: Integer

-- not requiring evaluation
tgCat_GB_NodeTagCat_Con  = 0
tgCat_GB_NodeTagCat_PAp  = 1
tgCat_GB_NodeTagCat_Intl = 3
-- requiring evaluation
tgCat_GB_NodeTagCat_Fun  = 0
tgCat_GB_NodeTagCat_App  = 1

tgIntl_GB_NodeTag_Intl_Float
  , tgIntl_GB_NodeTag_Intl_Double
      :: Int
tgIntl_GB_NodeTag_Intl_Float  = 2
tgIntl_GB_NodeTag_Intl_Double = 3



tag :: NmEnv -> StackState -> GrTag -> Maybe BasicSize -> StackDepth -> GBState -> (InsSeq,StackState,Bool,GBState)
tag env stState gt bsz sz gbState
  = case gt of
      GrTag_Unboxed
        -> (Seq.empty,zero,False,gbState)
      _ -> (tins Seq.:++: Seq.fromList [ldc tword],tinc `ststInc` one,isEvaluated,gbState2)
        where (needEval,categ',tg',tins,tinc,gbState2)
                = case bsz of
                    Just BasicSize_Double       -> (tgNeedEval_No ,tgCat_GB_NodeTagCat_Intl,tgIntl_GB_NodeTag_Intl_Double,Seq.empty           ,zero ,gbState )
                    Just BasicSize_Float        -> (tgNeedEval_No ,tgCat_GB_NodeTagCat_Intl,tgIntl_GB_NodeTag_Intl_Float ,Seq.empty           ,zero ,gbState )
                    _ -> case gt of
                           GrTag_Con _ tg nm    -> (tgNeedEval_No ,tgCat_GB_NodeTagCat_Con ,tg                           ,Seq.empty           ,zero ,gbState )
                           GrTag_Hole           -> (tgNeedEval_BlH,0                       ,0                            ,Seq.fromList [ldc 0],one  ,gbState )
                           GrTag_Rec            -> (tgNeedEval_No ,tgCat_GB_NodeTagCat_Con ,0                            ,Seq.empty           ,zero ,gbState )
                           GrTag_App      nm    -> (tgNeedEval_Yes,tgCat_GB_NodeTagCat_App ,0                            ,Seq.empty           ,zero ,gbState )
                           GrTag_Fun      nm    -> (tgNeedEval_Yes,tgCat_GB_NodeTagCat_Fun ,0                            ,ins                 ,inc  ,gbState2)
                                                where (ins,inc,gbState2) = nmLd defaultLoadCtxt env stState gbState nm
                           GrTag_PApp m   nm    -> (tgNeedEval_No ,tgCat_GB_NodeTagCat_PAp ,m                            ,ins                 ,inc  ,gbState2)
                                                where (ins,inc,gbState2) = nmLd defaultLoadCtxt env stState gbState nm
              tword = (toInteger (sz + ststDepth tinc + 1) `shiftL` tgShSize) .|. needEval `shiftL` tgShNdEv .|. categ' `shiftL` tgShCateg .|. toInteger tg' `shiftL` tagShTag
              isEvaluated = needEval == tgNeedEval_No
  where zero     = ststFromDep 0
        one      = ststFromDepPerm 1 GCPermit_Not



halt :: Instr
halt = Instr_Halt

-- AGItf -------------------------------------------------------
data AGItf  = AGItf_AGItf !(Module ) 
-- Imm ---------------------------------------------------------
data Imm  = Imm_Int !(Integer) 
-- InsOp_DataOp ------------------------------------------------
data InsOp_DataOp  = InsOp_DataOp_FloatWord 
                   | InsOp_DataOp_IntInf 
                   | InsOp_DataOp_IntWord 
-- InsOp_Deref -------------------------------------------------
data InsOp_Deref  = InsOp_Deref_Int 
                  | InsOp_Deref_One 
                  | InsOp_Deref_Two 
                  | InsOp_Deref_Zero 
-- InsOp_DerefB ------------------------------------------------
data InsOp_DerefB  = InsOp_DerefB_One 
                   | InsOp_DerefB_Two 
-- InsOp_ImmSz -------------------------------------------------
data InsOp_ImmSz  = InsOp_ImmSz_Bits08 
                  | InsOp_ImmSz_Bits16 
                  | InsOp_ImmSz_Bits32 
                  | InsOp_ImmSz_Bits64 
-- InsOp_LocB --------------------------------------------------
data InsOp_LocB  = InsOp_LocB_Reg 
                 | InsOp_LocB_TOS 
-- InsOp_LocE --------------------------------------------------
data InsOp_LocE  = InsOp_LocE_Imm 
                 | InsOp_LocE_PC 
                 | InsOp_LocE_Reg 
                 | InsOp_LocE_SP 
-- InsOp_LocODst -----------------------------------------------
data InsOp_LocODst  = InsOp_LocODst_Reg 
                    | InsOp_LocODst_TOS 
-- InsOp_LocOSrc -----------------------------------------------
data InsOp_LocOSrc  = InsOp_LocOSrc_Imm 
                    | InsOp_LocOSrc_Reg 
                    | InsOp_LocOSrc_SP 
                    | InsOp_LocOSrc_TOS 
-- InsOp_TyOp --------------------------------------------------
data InsOp_TyOp  = InsOp_TyOp_Add 
                 | InsOp_TyOp_Mul 
                 | InsOp_TyOp_Quot 
                 | InsOp_TyOp_Sub 
-- Instr -------------------------------------------------------
data Instr  = Instr_AllocStore !(InsOp_LocB ) !(GCStackInfo) 
            | Instr_Ann !(AnnKind) !(String) !(Instr ) 
            | Instr_Apply !(InsOp_LocB ) !(CallInfo) 
            | Instr_Call !(InsOp_LocB ) !(CallInfoCall) 
            | Instr_CallC !(InsOp_ImmSz ) !(Imm ) !(LinkChainEntry) !(CallInfo) 
            | Instr_CaseCall !(LinkChainEntry) 
            | Instr_Eval !(InsOp_LocB ) !(CallInfo) 
            | Instr_Fetch !(InsOp_LocB ) 
            | Instr_FetchUpdate 
            | Instr_FunStart !(FunctionInfo) 
            | Instr_Halt 
            | Instr_Label !(LocRef) 
            | Instr_LabelRef !(LocRef) 
            | Instr_Ld !(InsOp_Deref ) !(InsOp_LocB ) !(InsOp_LocE ) !(InsOp_ImmSz ) !(Imm ) 
            | Instr_LdGlobal !(InsOp_LocB ) !(LinkChainEntry) 
            | Instr_LdNodeTag 
            | Instr_LdString !(InsOp_LocB ) !(StringConst) 
            | Instr_LinkChain !(LinkChainEntry) 
            | Instr_Meta !(Meta ) 
            | Instr_Op !(InsOp_TyOp ) !(InsOp_DataOp ) !(InsOp_LocODst ) !(InsOp_Deref ) !(InsOp_LocOSrc ) !(InsOp_ImmSz ) !(Imm ) 
            | Instr_RetCall !(InsOp_ImmSz ) !(InsOp_ImmSz ) !(Imm ) !(Imm ) 
            | Instr_RetCase !(InsOp_ImmSz ) !(InsOp_ImmSz ) !(Imm ) !(Imm ) !(LinkChainEntry) 
            | Instr_St !(InsOp_DerefB ) !(InsOp_LocE ) !(InsOp_LocB ) !(InsOp_ImmSz ) !(Imm ) 
            | Instr_TagInt2Word 
            | Instr_TagWord2Word 
            | Instr_TailApply !(InsOp_LocB ) !(InsOp_ImmSz ) !(InsOp_ImmSz ) !(Imm ) !(Imm ) 
            | Instr_TailCall !(InsOp_LocB ) !(InsOp_ImmSz ) !(InsOp_ImmSz ) !(Imm ) !(Imm ) 
            | Instr_TailEval !(InsOp_LocB ) !(InsOp_ImmSz ) !(Imm ) !(CallInfo) 
            | Instr_UntagWord2Int 
            | Instr_UntagWord2Word 
-- Instrs ------------------------------------------------------
type Instrs  = [Instr ]
-- Meta --------------------------------------------------------
data Meta  = Meta_CmtHeader !(AnnKind) !(PP_Doc) 
-- Module ------------------------------------------------------
data Module  = Module_Mod !(String) !((AssocL HsName String)) !((AssocL HsName Int)) !(([EntryInfo])) !(([EntryInfo])) !(Instrs ) !(([Const])) !(([Int])) !(Int) !(([String])) 