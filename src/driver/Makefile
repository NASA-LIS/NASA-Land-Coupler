# GNU Makefile template for user ESMF application

################################################################################
################################################################################
## This Makefile must be able to find the "esmf.mk" Makefile fragment in the  ##
## 'include' line below. Following the ESMF User's Guide, a complete ESMF     ##
## installation should ensure that a single environment variable "ESMFMKFILE" ##
## is made available on the system. This variable should point to the         ##
## "esmf.mk" file.                                                            ##
##                                                                            ##
## This example Makefile uses the "ESMFMKFILE" environment variable.          ##
##                                                                            ##
## If you notice that this Makefile cannot find variable ESMFMKFILE then      ##
## please contact the person responsible for the ESMF installation on your    ##
## system.                                                                    ##
## As a work-around you can simply hardcode the path to "esmf.mk" in the      ##
## include line below. However, doing so will render this Makefile a lot less ##
## flexible and non-portable.                                                 ##
################################################################################

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif

include $(ESMFMKFILE)

COMMA := ,

### NASA Land Coupler Build Directives ###
NLC_OPTLEVEL = 1
NLC_PRCLEVEL = 2
NLC_DBGBUILD = 0

ifdef NLC_OPTLEVEL
 ESMF_F90COMPILECPPFLAGS += -DNLC_OPTLEVEL=$(NLC_OPTLEVEL)
 ESMF_CXXCOMPILECPPFLAGS += -DNLC_OPTLEVEL=$(NLC_OPTLEVEL)
endif

ifdef NLC_PRCLEVEL
 ESMF_F90COMPILECPPFLAGS += -DNLC_PRCLEVEL=$(NLC_PRCLEVEL)
 ESMF_CXXCOMPILECPPFLAGS += -DNLC_PRCLEVEL=$(NLC_PRCLEVEL)
endif

ifdef NLC_DBGBUILD
 ESMF_F90COMPILEOPTS += -g -traceback
 ESMF_CXXCOMPILEOPTS += -g -traceback
endif

### LIS dependencies ###
include $(NLC_LIS_INST)/lis.mk
DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_LND=$(ESMF_DEP_FRONT)
DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) \
  $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))

### WRF-Hydro dependencies ###
include $(NLC_WRFHYDRO_INST)/wrfhydro.mk
DEP_FRONTS    := $(DEP_FRONTS) -DFRONT_HYD=$(ESMF_DEP_FRONT)
DEP_INCS      := $(DEP_INCS) $(addprefix -I, $(ESMF_DEP_INCPATH))
DEP_CMPL_OBJS := $(DEP_CMPL_OBJS) $(ESMF_DEP_CMPL_OBJS)
DEP_LINK_OBJS := $(DEP_LINK_OBJS) $(ESMF_DEP_LINK_OBJS)
DEP_SHRD_PATH := $(DEP_SHRD_PATH) $(addprefix -L, $(ESMF_DEP_SHRD_PATH)) \
  $(addprefix -Wl$(COMMA)-rpath$(COMMA), $(ESMF_DEP_SHRD_PATH))
DEP_SHRD_LIBS := $(DEP_SHRD_LIBS) $(addprefix -l, $(ESMF_DEP_SHRD_LIBS))


################################################################################
################################################################################

.SUFFIXES: .f90 .F90 .c .C

%.o : %.f90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $(DEP_INCS) $<

%.o : %.F90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) $(DEP_INCS) $<

%.o : %.c
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

%.o : %.C
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<


# -----------------------------------------------------------------------------
NLC.exe: app.o driver.o mediator.o fields.o flags.o
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS) $(DEP_LINK_OBJS) $(DEP_SHRD_PATH) $(DEP_SHRD_LIBS)

# module dependencies:
app.o: driver.o
driver.o: flags.o fields.o mediator.o
mediator.o: flags.o fields.o
fields.o: flags.o

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean info edit
dust:
	rm -f PET*.ESMF_LogFile fort.*
clean:
	rm -f NLC.exe *.o *.mod
distclean: dust clean

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================
	@echo DEP_FRONTS=$(DEP_FRONTS)
	@echo DEP_INCS=$(DEP_INCS)
	@echo DEP_CMPL_OBJS=$(DEP_CMPL_OBJS)
	@echo DEP_LINK_OBJS=$(DEP_LINK_OBJS)
	@echo DEP_SHRD_PATH=$(DEP_SHRD_PATH)
	@echo DEP_SHRD_LIB:=$(DEP_SHRD_LIBS)
