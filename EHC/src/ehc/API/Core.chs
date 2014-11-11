%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Core Public API
%%%
%%% Intended for constructing basic Core Programs.
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

%%[(8 codegen) export(acoreApp)
%%]
