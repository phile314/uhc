/* src/rts/mm/config.ch.  Generated from config.ch.in by configure.  */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Memory management: configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Pages configuration

%%[8
// alternate implementations
#define MM_Cfg_Pages_Buddy 		0

// default implementation
#define MM_Cfg_Pages			MM_Cfg_Pages_Buddy
%%]

Plan configuration

%%[8
// alternate implementations
#define MM_Cfg_Plan_SS 			0

// default implementation
#define MM_Cfg_Plan				MM_Cfg_Plan_SS
%%]

