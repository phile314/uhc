# location of library src
SRC_RTS_PREFIX				:= $(SRC_PREFIX)rts/

# build location
RTS_BLD_RTS_PREFIX			:= $(EHC_BLD_VARIANT_PREFIX)rts/

# this file + other mk files
RTS_MKF						:= $(patsubst %,$(SRC_RTS_PREFIX)%.mk,files)

# lib/cabal config
#RTS_PKG_NAME				:= EH-RTS # via mk/config.mk.in
RTS_INS_FLAG				:= $(INSABS_FLAG_PREFIX)$(RTS_PKG_NAME)

# install location
INSABS_RTS_PREFIX			:= $(INSABS_PREFIX)$(EHC_VARIANT_PREFIX)
INSABS_RTS_LIB_PREFIX		:= $(INSABS_RTS_PREFIX)lib/
INSABS_RTS_INC_PREFIX		:= $(INSABS_RTS_PREFIX)include/

# inplace install
INSABS_LIB_RTS				:= $(INSABS_RTS_LIB_PREFIX)lib$(RTS_PKG_NAME)$(LIB_SUFFIX)

# main + sources + dpds, for .c/.h
RTS_C_RTS_SRC_CC			:= $(patsubst %,$(SRC_RTS_PREFIX)%.cc,\
									rts prim utils llvm-gc timing \
									grinbc/grinbc \
									mm/mm mm/common \
									mm/basic/flexarray mm/basic/dll mm/basic/deque mm/basic/rangemap \
									mm/pages mm/allocator mm/trace mm/tracesupply mm/collector mm/space mm/mutator mm/roots mm/plan \
									mm/pages/buddy \
									mm/tracesupply/group mm/tracesupply/buffer mm/tracesupply/bumpsupply mm/tracesupply/supplyroots \
									mm/space/fragment mm/space/copyspace \
									mm/semispace/ss mm/semispace/sscollector mm/semispace/gbssmutator \
									mm/gbm/gbtrace mm/gbm/gbtracesupregs mm/gbm/gbtracesupstack \
									mm/allocator/listoffree mm/allocator/bump \
								)
RTS_C_RTS_SRC_CC_OPTIM_O2	:= $(patsubst %,$(SRC_RTS_PREFIX)%.cc,grinbc/gbprim)
RTS_H_RTS_SRC_CH			:= $(patsubst %,$(SRC_RTS_PREFIX)%.ch,\
									rts config sizes bits utils timing \
									grinbc/grinbc \
									mm/mm mm/config mm/common \
									mm/basic/flexarray mm/basic/dll mm/basic/deque mm/basic/rangemap \
									mm/pages mm/allocator mm/trace mm/tracesupply mm/collector mm/space mm/mutator mm/roots mm/plan \
									mm/pages/buddy \
									mm/tracesupply/group mm/tracesupply/buffer mm/tracesupply/bumpsupply mm/tracesupply/supplyroots \
									mm/space/fragment mm/space/copyspace \
									mm/semispace/ss mm/semispace/sscollector mm/semispace/gbssmutator \
									mm/gbm/gbtrace mm/gbm/gbtracesupregs mm/gbm/gbtracesupstack \
									mm/allocator/listoffree mm/allocator/bump \
								)
MAIN_C_MAIN_SRC_CC			:= $(patsubst %,$(SRC_RTS_PREFIX)%.cc,mainSil)

RTS_C_RTS_GBCCALL_DRV_C		:= $(addprefix $(RTS_BLD_RTS_PREFIX),grinbc/gbccall.c)
RTS_H_RTS_GBCCALL_DRV_H		:= $(addprefix $(RTS_BLD_RTS_PREFIX),grinbc/gbccall.h)

RTS_C_RTS_DRV_C				:= $(patsubst $(SRC_RTS_PREFIX)%.cc,$(RTS_BLD_RTS_PREFIX)%.c,$(RTS_C_RTS_SRC_CC))
RTS_C_RTS_DRV_C_OPTIM_O2	:= $(patsubst $(SRC_RTS_PREFIX)%.cc,$(RTS_BLD_RTS_PREFIX)%.c,$(RTS_C_RTS_SRC_CC_OPTIM_O2))
RTS_C_RTS_DRV_C_OTHER		:= $(RTS_C_RTS_GBCCALL_DRV_C)
RTS_H_RTS_DRV_H				:= $(patsubst $(SRC_RTS_PREFIX)%.ch,$(RTS_BLD_RTS_PREFIX)%.h,$(RTS_H_RTS_SRC_CH))
RTS_H_RTS_DRV_H_OTHER		:= $(RTS_H_RTS_GBCCALL_DRV_H)
MAIN_C_MAIN_DRV_C			:= $(patsubst $(SRC_RTS_PREFIX)%.cc,$(RTS_BLD_RTS_PREFIX)%.c,$(MAIN_C_MAIN_SRC_CC))
RTS_H_RTS_PRIM_DRV_H		:= $(addprefix $(RTS_BLD_RTS_PREFIX),prim.h grinbc/gbprim.h)

RTS_H_RTS_ALL_DRV_H			:= $(RTS_H_RTS_DRV_H) $(RTS_H_RTS_PRIM_DRV_H) $(RTS_H_RTS_DRV_H_OTHER)
MAIN_C_MAIN_ALL_DRV_C		:= $(MAIN_C_MAIN_DRV_C)

RTS_C_RTS_DRV_O				:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.c,$(RTS_BLD_RTS_PREFIX)%.o,$(RTS_C_RTS_DRV_C))
RTS_C_RTS_DRV_O_OPTIM_O2	:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.c,$(RTS_BLD_RTS_PREFIX)%.o,$(RTS_C_RTS_DRV_C_OPTIM_O2))
RTS_C_RTS_DRV_O_OTHER		:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.c,$(RTS_BLD_RTS_PREFIX)%.o,$(RTS_C_RTS_DRV_C_OTHER))
RTS_O_RTS_ALL_DRV_O			:= $(RTS_C_RTS_DRV_O) $(RTS_C_RTS_DRV_O_OPTIM_O2) $(RTS_C_RTS_DRV_O_OTHER)
RTS_O_RTS_INS_O				:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.o,$(INSABS_RTS_LIB_PREFIX)%.o,$(RTS_O_RTS_ALL_DRV_O))
RTS_H_RTS_INS_H				:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.h,$(INSABS_RTS_INC_PREFIX)%.h,$(RTS_H_RTS_ALL_DRV_H))
MAIN_C_MAIN_INS_C			:= $(patsubst $(RTS_BLD_RTS_PREFIX)%.c,$(INSABS_RTS_INC_PREFIX)%.c,$(MAIN_C_MAIN_ALL_DRV_C))

RTS_ALL_SRC					:= $(RTS_H_RTS_SRC_CH) $(RTS_C_RTS_SRC_CC) $(RTS_C_RTS_SRC_CC_OPTIM_O2) $(MAIN_C_MAIN_SRC_CC)

# target
rts: $(INSABS_LIB_RTS)

# build rules
$(RTS_C_RTS_GBCCALL_DRV_C): $(GEN_RTSGBCCALL_BLD_EXEC) $(RTS_MKF)
	mkdir -p $(@D)
	$(GEN_RTSGBCCALL_BLD_EXEC) c 3 > $@

$(RTS_H_RTS_GBCCALL_DRV_H): $(GEN_RTSGBCCALL_BLD_EXEC) $(RTS_MKF)
	mkdir -p $(@D)
	$(GEN_RTSGBCCALL_BLD_EXEC) h 3 > $@

$(RTS_H_RTS_DRV_H): $(RTS_BLD_RTS_PREFIX)%.h: $(SRC_RTS_PREFIX)%.ch
	mkdir -p $(@D)
	$(SHUFFLE_C) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F) --order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
	touch $@

$(MAIN_C_MAIN_DRV_C): $(RTS_BLD_RTS_PREFIX)%.c: $(SRC_RTS_PREFIX)%.cc
	mkdir -p $(@D)
	$(SHUFFLE_C) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F) --order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
	touch $@

$(RTS_C_RTS_DRV_C) $(RTS_C_RTS_DRV_C_OPTIM_O2): $(RTS_BLD_RTS_PREFIX)%.c: $(SRC_RTS_PREFIX)%.cc
	mkdir -p $(@D)
	$(SHUFFLE_C) $(LIB_EHC_SHUFFLE_DEFS) --gen=$(EHC_VARIANT) --base=$(*F) --order="$(EHC_SHUFFLE_ORDER)" $< > $@&& \
	touch $@

$(RTS_C_RTS_DRV_O) $(RTS_C_RTS_DRV_O_OTHER): $(RTS_BLD_RTS_PREFIX)%.o: $(RTS_BLD_RTS_PREFIX)%.c $(RTS_H_RTS_ALL_DRV_H)
	$(GCC) $(RTS_GCC_CC_OPTS_OPTIM) $(EHC_GCC_CC_OPTS) -o $@ -c $<

$(RTS_C_RTS_DRV_O_OPTIM_O2): $(RTS_BLD_RTS_PREFIX)%.o: $(RTS_BLD_RTS_PREFIX)%.c $(RTS_H_RTS_ALL_DRV_H)
	$(GCC) $(EHC_GCC_CC_OPTS) $(RTS_GCC_CC_OPTS) -O2 -o $@ -c $<

$(INSABS_LIB_RTS): $(EHC_RTS_DPDS_EXTLIBS) $(RTS_C_RTS_DRV_O) $(RTS_C_RTS_DRV_O_OPTIM_O2) $(RTS_C_RTS_DRV_O_OTHER) $(RTS_H_RTS_INS_H) $(RTS_O_RTS_INS_O) $(MAIN_C_MAIN_INS_C) $(RTS_MKF)
	mkdir -p $(@D)
	$(call LIB_MK_STATIC,$@,$(RTS_C_RTS_DRV_O) $(RTS_C_RTS_DRV_O_OPTIM_O2) $(RTS_C_RTS_DRV_O_OTHER))
	touch $@

$(RTS_H_RTS_PRIM_DRV_H): %.h: %.c $(RTS_MKF)
	( echo "/* Generated from $< */" ; \
	  sed -n -e 's/^PRIM \(.*\)$$/extern \1 ;/p' < $< ; \
	) > $@

# inplace install rules
$(RTS_H_RTS_INS_H): $(INSABS_RTS_INC_PREFIX)%: $(RTS_BLD_RTS_PREFIX)%
	mkdir -p $(@D)
	install $< $@

$(RTS_O_RTS_INS_O): $(INSABS_RTS_LIB_PREFIX)%: $(RTS_BLD_RTS_PREFIX)%
	mkdir -p $(@D)
	install $< $@

$(MAIN_C_MAIN_INS_C): $(INSABS_RTS_INC_PREFIX)%: $(RTS_BLD_RTS_PREFIX)%
	mkdir -p $(@D)
	install $< $@

