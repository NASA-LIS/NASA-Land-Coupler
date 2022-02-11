#ifndef NLC_OPTLEVEL
#define NLC_OPTLEVEL 1
#endif

#ifndef NLC_PRCLEVEL
#define NLC_PRCLEVEL 2
#endif

#ifndef NLC_LOGKIND
#if NLC_OPTLEVEL >= 3
#define NLC_LOGKIND      ESMF_LOGKIND_NONE
#elif NLC_OPTLEVEL >= 2
#define NLC_LOGKIND      ESMF_LOGKIND_MULTI_ON_ERROR
#else
#define NLC_LOGKIND      ESMF_LOGKIND_MULTI
#endif
#endif

#ifndef NLC_TYPEKIND
#if NLC_PRCLEVEL >= 2
#define NLC_TYPEKIND     ESMF_TYPEKIND_R8
#else
#define NLC_TYPEKIND     ESMF_TYPEKIND_R4
#endif
#endif

#ifndef NLC_KIND
#if NLC_PRCLEVEL >= 2
#define NLC_KIND         ESMF_KIND_R8
#else
#define NLC_KIND         ESMF_KIND_R4
#endif
#endif

#ifndef NLC_CONFIG
#define NLC_CONFIG       "nlc.runconfig"
#endif

#ifndef NLC_INITVAL
#define NLC_INITVAL      999999999.0D0
#endif
