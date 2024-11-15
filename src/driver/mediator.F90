!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------------
! NASA Goddard Space Flight Center
! NASA Land Coupler (NLC)
! Version 0.5
!
! Copyright (c) 2022 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
! Licensed under Apache License 2.0.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------------
#include "settings.h"

module Mediator

  !-----------------------------------------------------------------------------
  ! Mediator Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Mediator, only: &
    mediator_routine_SS             => SetServices, &
    mediator_routine_Run            => routine_Run, &
    mediator_label_Advertise        => label_Advertise, &
    mediator_label_ModifyAdvertised => label_ModifyAdvertised, &
    mediator_label_AcceptTransfer   => label_AcceptTransfer, &
    mediator_label_RealizeProvided  => label_RealizeProvided, &
    mediator_label_RealizeAccepted  => label_RealizeAccepted, &
    mediator_label_DataInitialize   => label_DataInitialize, &
    mediator_label_Advance          => label_Advance, &
    mediator_label_CheckImport      => label_CheckImport, &
    mediator_label_TimestampExport  => label_TimestampExport, &
    mediator_label_SetRunClock      => label_SetRunClock, &
    mediator_label_Finalize         => label_Finalize, &
    NUOPC_MediatorGet
  use Fields
  use Flags

  implicit none

  private

  type dataIniType
    sequence
    private
      integer :: initype
  end type

  type(dataIniType), parameter ::      &
    INIT_ERROR    = dataIniType(-1), &
    INIT_DEFAULTS = dataIniType(0),  &
    INIT_MODELS   = dataIniType(1)

  type ensMapType
    sequence
    private
      integer :: maptype
  end type

  type(ensMapType), parameter ::   &
    EMAP_ERR     = ensMapType(-1), & ! Error code
    EMAP_NOENSM  = ensMapType(0),  & ! No ensemble remapping
    EMAP_UNDIST  = ensMapType(1),  & ! Ensemble dimension 1
    EMAP_MINSTS  = ensMapType(2)     ! Multi component scatter

  type med_ext_conn_type
    type(ESMF_State)                :: extFrState
    type(ESMF_State)                :: extToState
    type(ESMF_State), allocatable   :: intFrState(:)
    type(ESMF_State), allocatable   :: intToState(:)
    type(med_fld_type), pointer     :: allFrFlds(:) => null()
    type(med_fld_type), pointer     :: allToFlds(:) => null()
    type(med_fld_type), allocatable :: connFrFlds(:)
    type(med_fld_type), allocatable :: connToFlds(:)
    type(ESMF_FieldBundle)          :: connFrFB
    type(ESMF_FieldBundle)          :: connToFB
  end type med_ext_conn_type

  type med_int_conn_type
    character(len=16)                :: cname
    type(ESMF_State)                 :: srcState
    type(ESMF_State)                 :: dstState
    type(fieldRemapFlag)             :: mapping = FLD_REMAP_UNKOWN
    type(fieldMaskFlag)              :: srcMask = FLD_MASK_UNK
    type(fieldMaskFlag)              :: dstMask = FLD_MASK_UNK
    type(ESMF_FieldBundle)           :: srcFB
    type(ESMF_FieldBundle)           :: dstFB
    type(ESMF_RouteHandle)           :: rh
  end type med_int_conn_type

  character(*), parameter :: &
    label_InternalState = "Med_InternalState"

  type med_comp_type
    character(len=3)                     :: cname
    logical                              :: enabled   = .false.
    type(ensMapType)                     :: ensMap    = EMAP_NOENSM
    logical                              :: multiInst = .false.
    integer                              :: cntInst   = 1
    integer                              :: cntMbrs   = 1
    character(4)                         :: strFmt    = "I0.0"
    type(dataIniType)                    :: iniType   = INIT_DEFAULTS
    type(fieldRemapFlag)                 :: remap     = FLD_REMAP_UNKOWN
    type(fieldMaskFlag)                  :: maskFr    = FLD_MASK_UNK
    type(fieldMaskFlag)                  :: maskTo    = FLD_MASK_UNK
    type(med_ext_conn_type), allocatable :: econn(:)  ! members
  end type med_comp_type

  type type_InternalStateStruct
    type(med_comp_type) :: LND = med_comp_type(cname="LND")
    type(med_comp_type) :: HYD = med_comp_type(cname="HYD")
    type(med_comp_type) :: GWR = med_comp_type(cname="GWR")
    type(med_int_conn_type), allocatable :: LND2HYD(:)
    type(med_int_conn_type), allocatable :: LND2GWR(:)
    type(med_int_conn_type), allocatable :: HYD2LND(:)
    type(med_int_conn_type), allocatable :: HYD2GWR(:)
    type(med_int_conn_type), allocatable :: GWR2LND(:)
    type(med_int_conn_type), allocatable :: GWR2HYD(:)
    character(len=40)           :: dirOutput    = "."
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  integer, target :: maskLND(1) = (/1/)
  integer, target :: maskWTR(1) = (/0/)
  integer, parameter :: srcTermProcessing_Value = 0
  real(ESMF_KIND_R8) :: dfltVal = 9.99e20_ESMF_KIND_R8
  type(ESMF_PoleMethod_Flag), parameter :: polemethod=ESMF_POLEMETHOD_ALLAVG
  type(ESMF_UnmappedAction_Flag), parameter :: unmappedaction=ESMF_UNMAPPEDACTION_IGNORE
  type(ESMF_Time)    :: time_invalidTimeStamp
  type(ESMF_Clock)   :: clock_invalidTimeStamp
  public SetServices

  interface operator (==)
    module procedure dataIniType_eq
    module procedure ensMapType_eq
  end interface

  interface assignment (=)
    module procedure dataIniType_tostring
    module procedure dataIniType_frstring
    module procedure ensMapType_tostring
    module procedure ensMapType_frstring
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    ! local variables
    type(type_InternalState)  :: is
    integer                   :: stat

    rc = ESMF_SUCCESS

    ! the NUOPC mediator component will register the generic methods
    call NUOPC_CompDerive(mediator, mediator_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! --- Initialization phases --------------------------------------

    ! Advertise fields
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_Advertise, &
      specRoutine=Advertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! Modify advertised fields
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_ModifyAdvertised, &
      specRoutine=ModifyAdvertised, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! Realize all fields with transfer action "provide"
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_RealizeProvided, &
      specRoutine=RealizeProvided, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! Optionally modify the decomp/distr of transferred Grid/Mesh
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_AcceptTransfer, &
      specRoutine=AcceptTransfer, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! Realize all fields with transfer action "accept"
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_RealizeAccepted, &
      specRoutine=RealizeAccepted, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_Advance, &
      specRoutine=MediatorAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set entry point for Run( phase = prepLND ) and specialize
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_RUN, &
      phaseLabelList=(/"prepLND"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_Advance, &
      specPhaseLabel="prepLND", specRoutine=MediatorRemapToLnd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_TimestampExport, &
      specPhaseLabel="prepLND", &
      specRoutine=TimestampExport_remapLnd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set entry point for Run( phase = prepHyd ) and specialize
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_RUN, &
      phaseLabelList=(/"prepHYD"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_Advance, &
      specPhaseLabel="prepHYD", specRoutine=MediatorRemapToHyd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_TimestampExport, &
      specPhaseLabel="prepHYD", &
      specRoutine=TimestampExport_remapHyd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set entry point for Run( phase = prepGwr ) and specialize
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_RUN, &
      phaseLabelList=(/"prepGWR"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_Advance, &
      specPhaseLabel="prepGWR", specRoutine=MediatorRemapToGwr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_TimestampExport, &
      specPhaseLabel="prepGWR", &
      specRoutine=TimestampExport_remapGwr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_MethodRemove(mediator, mediator_label_CheckImport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_CheckImport, &
      specRoutine=NUOPC_NoOp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_MethodRemove(mediator, mediator_label_SetRunClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_SetRunClock, &
      specRoutine=SetRunClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_Finalize, &
      specRoutine=MediatorFinalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! Set up the internal state
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
      file=__FILE__, rcToReturn=rc)) return ! bail out
    call ESMF_GridCompSetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end subroutine SetServices

  !-----------------------------------------------------------------------------

  subroutine Advertise(mediator, rc)
    ! Advertise Fields
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="Advertise"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_State)          :: importState, exportState
    type(ESMF_Clock)          :: clock
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString
    integer                   :: i
    integer                   :: stat
    character(6)              :: maxStr
    character(10)             :: instStr

    rc = ESMF_SUCCESS

    ! get component information
    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! query for importState, exportState, and clock
    call NUOPC_MediatorGet(mediator, importState=importState, &
      exportState=exportState, driverClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! prepare diagnostic values
    if (verbosity>0 .OR. diagnostic>0) then
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_TimeGet(currTime, timeStringISOFrac=currTimeString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! query component for the internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! get component output directory
    call ESMF_AttributeGet(mediator, name="output_directory", &
      value=is%wrap%dirOutput, defaultValue=trim(name)//"_OUTPUT", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! get LND settings
    call ESMF_AttributeGet(mediator, name="instance_count_lnd", &
      value=is%wrap%LND%cntInst, defaultValue=1, &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (is%wrap%LND%cntInst.lt.1) then
      is%wrap%LND%enabled = .false.
      is%wrap%LND%multiInst = .false.
    elseif (is%wrap%LND%cntInst.eq.1) then
      is%wrap%LND%enabled = .true.
      is%wrap%LND%multiInst = .false.
    else
      is%wrap%LND%enabled = .true.
      is%wrap%LND%multiInst = .true.
    endif
    call ESMF_AttributeGet(mediator, name="multi_instance_lnd", &
      value=is%wrap%LND%multiInst, defaultValue=is%wrap%LND%multiInst, &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if ((.NOT.is%wrap%LND%multiInst) .AND. (is%wrap%LND%cntInst.gt.1)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="multi_instance_lnd must be true for instance count gt 1", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    elseif (is%wrap%LND%cntInst.gt.999999) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="instance_count_lnd must be less than 999999", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif
    call ESMF_AttributeGet(mediator, name="DataInitLnd", &
      value=value, defaultValue="INIT_DEFAULTS", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%LND%iniType = value
    call ESMF_AttributeGet(mediator, name="EnsembleLnd", &
      value=value, defaultValue="EMAP_NOENSM", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%LND%ensMap = value
    call ESMF_AttributeGet(mediator, name="RemapLND", &
      value=value, defaultValue="FLD_REMAP_REDIST", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%LND%remap = value
    call ESMF_AttributeGet(mediator, name="MaskFrLND", &
      value=value, defaultValue="FLD_MASK_NNE", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%LND%maskFr = value
    call ESMF_AttributeGet(mediator, name="MaskToLND", &
      value=value, defaultValue="FLD_MASK_NNE", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%LND%maskTo = value
    if (verbosity>0) then
      call med_comp_logwrite(is%wrap%LND, name=name, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! get HYD settings
    call ESMF_AttributeGet(mediator, name="instance_count_hyd", &
      value=is%wrap%HYD%cntInst, defaultValue=1, &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (is%wrap%HYD%cntInst.lt.1) then
      is%wrap%HYD%enabled = .false.
      is%wrap%HYD%multiInst = .false.
    elseif (is%wrap%HYD%cntInst.eq.1) then
      is%wrap%HYD%enabled = .true.
      is%wrap%HYD%multiInst = .false.
    else
      is%wrap%HYD%enabled = .true.
      is%wrap%HYD%multiInst = .true.
    endif
    call ESMF_AttributeGet(mediator, name="multi_instance_hyd", &
      value=is%wrap%HYD%multiInst, defaultValue=is%wrap%HYD%multiInst, &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if ((.NOT.is%wrap%HYD%multiInst) .AND. (is%wrap%HYD%cntInst.gt.1)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="multi_instance_hyd must be true for instance count gt 1", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    elseif (is%wrap%HYD%cntInst.gt.999999) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="instance_count_hyd must be less than 999999", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif
    call ESMF_AttributeGet(mediator, name="DataInitHyd", &
      value=value, defaultValue="INIT_DEFAULTS", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%HYD%iniType = value
    call ESMF_AttributeGet(mediator, name="EnsembleHyd", &
      value=value, defaultValue="EMAP_NOENSM", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%HYD%ensMap = value
    call ESMF_AttributeGet(mediator, name="RemapHYD", &
      value=value, defaultValue="FLD_REMAP_REDIST", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%HYD%remap = value
    call ESMF_AttributeGet(mediator, name="MaskFrHYD", &
      value=value, defaultValue="FLD_MASK_NNE", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%HYD%maskFr = value
    call ESMF_AttributeGet(mediator, name="MaskToHYD", &
      value=value, defaultValue="FLD_MASK_NNE", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%HYD%maskTo = value
    if (verbosity>0) then
      call med_comp_logwrite(is%wrap%HYD, name=name, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! get GWR settings
    call ESMF_AttributeGet(mediator, name="instance_count_gwr", &
      value=is%wrap%GWR%cntInst, defaultValue=1, &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (is%wrap%GWR%cntInst.lt.1) then
      is%wrap%GWR%enabled = .false.
      is%wrap%GWR%multiInst = .false.
    elseif (is%wrap%GWR%cntInst.eq.1) then
      is%wrap%GWR%enabled = .true.
      is%wrap%GWR%multiInst = .false.
    else
      is%wrap%GWR%enabled = .true.
      is%wrap%GWR%multiInst = .true.
    endif
    call ESMF_AttributeGet(mediator, name="multi_instance_gwr", &
      value=is%wrap%GWR%multiInst, defaultValue=is%wrap%GWR%multiInst, &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if ((.NOT.is%wrap%GWR%multiInst) .AND. (is%wrap%GWR%cntInst.gt.1)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="multi_instance_gwr must be true for instance count gt 1", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    elseif (is%wrap%GWR%cntInst.gt.999999) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="instance_count_gwr must be less than 999999", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif
    call ESMF_AttributeGet(mediator, name="DataInitGwr", &
      value=value, defaultValue="INIT_DEFAULTS", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%GWR%iniType = value
    call ESMF_AttributeGet(mediator, name="EnsembleGwr", &
      value=value, defaultValue="EMAP_NOENSM", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%GWR%ensMap = value
    call ESMF_AttributeGet(mediator, name="RemapGWR", &
      value=value, defaultValue="FLD_REMAP_REDIST", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%GWR%remap = value
    call ESMF_AttributeGet(mediator, name="MaskFrGWR", &
      value=value, defaultValue="FLD_MASK_NNE", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%GWR%maskFr = value
    call ESMF_AttributeGet(mediator, name="MaskToGWR", &
      value=value, defaultValue="FLD_MASK_NNE", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%GWR%maskTo = value
    if (verbosity>0) then
      call med_comp_logwrite(is%wrap%GWR, name=name, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! prepare diagnostics folder
    if (btest(diagnostic,16)) then
      call ESMF_UtilIOMkDir(pathName=trim(is%wrap%dirOutput), &
        relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    if (is%wrap%LND%enabled) then
      allocate(is%wrap%LND%econn(is%wrap%LND%cntInst), stat=stat)
      if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
      call med_comp_advertise(is%wrap%LND, fldsFrLnd, fldsToLnd, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (verbosity>0) then
        call med_comp_logwrite(is%wrap%LND, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
    endif
    if (is%wrap%HYD%enabled) then
      allocate(is%wrap%HYD%econn(is%wrap%HYD%cntInst), stat=stat )
      if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
      call med_comp_advertise(is%wrap%HYD, fldsFrHyd, fldsToHyd, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (verbosity>0) then
        call med_comp_logwrite(is%wrap%HYD, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
    endif
    if (is%wrap%GWR%enabled) then
      allocate(is%wrap%GWR%econn(is%wrap%GWR%cntInst), stat=stat)
      if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
      call med_comp_advertise(is%wrap%GWR, fldsFrGwr, fldsToGwr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (verbosity>0) then
        call med_comp_logwrite(is%wrap%GWR, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
    endif

    call ESMF_TimeSet(time_invalidTimeStamp, yy=99999999, mm=1, dd=1, h=0, m=0, s=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    clock_invalidTimeStamp = ESMF_ClockCreate(clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_ClockSet(clock_invalidTimeStamp, currTime=time_invalidTimeStamp, &
      stopTime=time_invalidTimeStamp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine med_comp_advertise(comp, fldsFr, fldsTo, rc)
      type(med_comp_type), intent(inout)     :: comp
      type(med_fld_type), target, intent(in) :: fldsFr(:)
      type(med_fld_type), target, intent(in) :: fldsTo(:)
      integer, intent(out) :: rc

      character(6)  :: maxStr
      character(10) :: instStr

      ! add to fields dictionary
      call field_dictionary_add(fldsFr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call field_dictionary_add(fldsTo, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      ! generate the instance string format descriptors
      write(maxStr,"(I0)") comp%cntInst
      write(comp%strFmt,"(A1,I0,A1,I0)") "I", len_trim(maxStr), &
                                         ".", len_trim(maxStr)

      do i=1, comp%cntInst
        ! generate comp name for instance
        if (comp%multiInst) then
          write(instStr,"(A4,"//trim(comp%strFmt)//")") trim(comp%cname)//"-",i
        else
          instStr = trim(comp%cname)
        endif
        ! advertise from fields to nested import state
        comp%econn(i)%allFrFlds => fldsFr
        call NUOPC_AddNamespace(importState, namespace=trim(instStr), &
          nestedState=comp%econn(i)%extFrState, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call field_advertise(comp%econn(i)%allFrFlds, &
          comp%econn(i)%extFrState, "cannot provide", rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        ! advertise to fields to nested export state
        comp%econn(i)%allToFlds => fldsTo
        call NUOPC_AddNamespace(exportState, namespace=trim(instStr), &
          nestedState=comp%econn(i)%extToState, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call field_advertise(comp%econn(i)%allToFlds, &
          comp%econn(i)%extToState, "cannot provide", rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
    end subroutine med_comp_advertise

  end subroutine Advertise

  !-----------------------------------------------------------------------------

  subroutine ModifyAdvertised(mediator, rc)
    ! Modify Advertised Fields
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="ModifyAdvertised"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_State)          :: importState, exportState
    type(ESMF_Clock)          :: clock
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    rc = ESMF_SUCCESS

    ! get component information
    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! query for importState, exportState, and clock
    call NUOPC_MediatorGet(mediator, importState=importState, &
      exportState=exportState, driverClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! prepare diagnostic values
    if (verbosity>0 .OR. diagnostic>0) then
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_TimeGet(currTime, timeStringISOFrac=currTimeString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! query component for the internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (is%wrap%LND%enabled) then
      call med_comp_rm(is%wrap%LND, &
        chkCmpLst=(/is%wrap%HYD, is%wrap%GWR/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (verbosity>0) then
        call med_comp_logwrite(is%wrap%LND, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
    endif
    if (is%wrap%HYD%enabled) then
      call med_comp_rm(is%wrap%HYD, &
        chkCmpLst=(/is%wrap%LND, is%wrap%GWR/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (verbosity>0) then
        call med_comp_logwrite(is%wrap%HYD, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
    endif
    if (is%wrap%GWR%enabled) then
      call med_comp_rm(is%wrap%GWR, &
        chkCmpLst=(/is%wrap%LND, is%wrap%HYD/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (verbosity>0) then
        call med_comp_logwrite(is%wrap%GWR, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
    endif

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine med_comp_rm(comp, chkCmpLst, rc)
      type(med_comp_type), intent(inout) :: comp
      type(med_comp_type), intent(in)    :: chkCmpLst(:)
      integer                            :: rc
      ! local variables
      integer                                 :: i, j, k
      integer                                 :: jj
      integer                                 :: itemCount
      character(len=80), allocatable          :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
      logical                                 :: hasConnection

      rc = ESMF_SUCCESS

      do j=1, comp%cntInst

        call ESMF_StateGet(comp%econn(j)%extFrState, nestedFlag=.true., &
          itemCount=itemCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        allocate(itemNameList(itemCount), itemTypeList(itemCount))
        call ESMF_StateGet(comp%econn(j)%extFrState, nestedFlag=.true., &
          itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        do i=1, itemCount
          if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then
            hasConnection=med_comp_searchextto(chkCmpLst, &
              itemNameList(i), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            if (.not. hasConnection) then
              call ESMF_StateRemove(comp%econn(j)%extFrState, &
                (/itemNameList(i)/), relaxedflag=.true., rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            endif
          endif
        enddo
        deallocate(itemNameList, itemTypeList)

        call ESMF_StateGet(comp%econn(j)%extToState, nestedFlag=.true., &
          itemCount=itemCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        allocate(itemNameList(itemCount), itemTypeList(itemCount))
        call ESMF_StateGet(comp%econn(j)%extToState, nestedFlag=.true., &
          itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        do i=1, itemCount
          if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then
            hasConnection=med_comp_searchextfr(chkCmpLst, &
              itemNameList(i), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            if (.not. hasConnection) then
              call ESMF_StateRemove(comp%econn(j)%extToState, &
                (/itemNameList(i)/), relaxedflag=.true., rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            endif
          endif
        enddo
        deallocate(itemNameList, itemTypeList)

      enddo
    end subroutine med_comp_rm

    function med_comp_searchextto(comp, itemName, rc) result(hasConnection)
      type(med_comp_type), intent(in) :: comp(:)
      character(*), intent(in)        :: itemName
      integer, intent(out)            :: rc
      ! result
      logical :: hasConnection
      ! local variables
      integer                   :: i, j
      type(ESMF_StateItem_Flag) :: itemType
      logical                   :: isConnected

      rc = ESMF_SUCCESS

      hasConnection=.false.
      search: do j=1, size(comp)
        if (comp(j)%enabled) then
          do i=1, comp(j)%cntInst
            call ESMF_StateGet(comp(j)%econn(i)%extToState, itemName=itemName, &
              itemType=itemType, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            if (itemType==ESMF_STATEITEM_FIELD) then
!              isConnected=NUOPC_IsConnected(comp(j)%econn(i)%extToState, &
!                fieldName=itemName, rc=rc)
!              if (isConnected) then
                hasConnection=.true.
                exit search
!              endif
            endif
          enddo
        endif
      enddo search

    end function med_comp_searchextto

    function med_comp_searchextfr(comp, itemName, rc) result(hasConnection)
      type(med_comp_type), intent(in) :: comp(:)
      character(*), intent(in)        :: itemName
      integer, intent(out)            :: rc
      ! result
      logical :: hasConnection
      ! local variables
      integer                   :: i, j
      type(ESMF_StateItem_Flag) :: itemType
      logical                   :: isConnected

      rc = ESMF_SUCCESS

      hasConnection=.false.
      search: do j=1, size(comp)
        if (comp(j)%enabled) then
          do i=1, comp(j)%cntInst
            call ESMF_StateGet(comp(j)%econn(i)%extFrState, itemName=itemName, &
              itemType=itemType, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            if (itemType==ESMF_STATEITEM_FIELD) then
!              isConnected=NUOPC_IsConnected(comp(j)%econn(i)%extFrState, &
!                fieldName=itemName, rc=rc)
!              if (isConnected) then
                hasConnection=.true.
                exit search
!              endif
            endif
          enddo
        endif
      enddo search

    end function med_comp_searchextfr

  end subroutine ModifyAdvertised

  !-----------------------------------------------------------------------------

  subroutine RealizeProvided(mediator, rc)
    ! Realize connected Fields with transfer action "provide"
    ! and remove Fields that are not connected
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="RealizeProvided"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_State)          :: importState, exportState
    type(ESMF_Clock)          :: clock
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    rc = ESMF_SUCCESS

    ! get component information
    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! query for importState, exportState, and clock
    call NUOPC_MediatorGet(mediator, importState=importState, &
      exportState=exportState, driverClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! prepare diagnostic values
    if (verbosity>0 .OR. diagnostic>0) then
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_TimeGet(currTime, timeStringISOFrac=currTimeString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! query component for the internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call checkConnectedFlagProvide(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call checkConnectedFlagProvide(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine checkConnectedFlagProvide(state, rc)
      ! Look at all of the fields in state, including in nested states. Error
      ! out if a connected field is found for which geom object must be
      ! provided here. Remove all not connected fields.
      type(ESMF_State)  :: state
      integer, optional :: rc
      ! local variables
      integer                                 :: itemCount, item
      character(len=80)                       :: stateName
      type(ESMF_Field)                        :: field
      character(len=80)                       :: connectedValue
      character(len=20)                       :: transferAction
      character(len=80), allocatable          :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
      type(ESMF_StateIntent_Flag)             :: stateIntent
      character(len=80)                       :: transferActionAttr

      if (present(rc)) rc = ESMF_SUCCESS

      call ESMF_StateGet(state, stateIntent=stateIntent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      if (stateIntent==ESMF_STATEINTENT_EXPORT) then
        transferActionAttr="ProducerTransferAction"
      elseif (stateIntent==ESMF_STATEINTENT_IMPORT) then
        transferActionAttr="ConsumerTransferAction"
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="The stateIntent must either be IMPORT or EXPORT here.", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return  ! bail out
      endif

      call ESMF_StateGet(state, name=stateName, nestedFlag=.true., &
        itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      allocate(itemNameList(itemCount), itemTypeList(itemCount))

      call ESMF_StateGet(state, nestedFlag=.true., &
        itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      do item=1, itemCount
        if (itemTypeList(item)==ESMF_STATEITEM_FIELD) then
          ! this is a field -> get more info
          call ESMF_StateGet(state, field=field, itemName=itemNameList(item), &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call NUOPC_GetAttribute(field, name="Connected", &
            value=connectedValue, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (connectedValue=="false") then
            ! remove the field from the state
            call ESMF_StateRemove(state, (/itemNameList(item)/), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          else
            call NUOPC_GetAttribute(field, name=transferActionAttr, &
              value=transferAction, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            if (trim(transferAction)=="provide") then
              ! the Connector instructed the Mediator to provide geom object
              call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
                msg="Cannot fulfill request to provide geom object for "// &
                trim(itemNameList(item))//" in State "//trim(stateName), &
                line=__LINE__, file=__FILE__, rcToReturn=rc)
              return ! bail out
            endif
          endif
        endif
      enddo

      deallocate(itemNameList, itemTypeList)

    end subroutine checkConnectedFlagProvide

  end subroutine RealizeProvided

  !-----------------------------------------------------------------------------

  subroutine AcceptTransfer(mediator, rc)
    ! Optionally modify the decomp/distr of transferred Grid/Mesh
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="AcceptTransfer"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_State)          :: importState, exportState
    type(ESMF_Clock)          :: clock
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString
    type(ESMF_VM)             :: vm
    integer                   :: medPetCount

    rc = ESMF_SUCCESS

    ! get component information
    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! query for importState, exportState, and clock
    call NUOPC_MediatorGet(mediator, importState=importState, &
      exportState=exportState, driverClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! prepare diagnostic values
    if (verbosity>0 .OR. diagnostic>0) then
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_TimeGet(currTime, timeStringISOFrac=currTimeString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! query component for the internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_GridCompGet(mediator, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_VMGet(vm, petCount=medPetCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call adjustAcceptedGeom(importState, medPetCount=medPetCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call adjustAcceptedGeom(exportState, medPetCount=medPetCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine adjustAcceptedGeom(state, medPetCount, rc)
      ! Look at all of the fields in state, including in nested states. Adjust
      ! the distribution of the accepted geom object to a 1 DE/PET distribution.
      type(ESMF_State)              :: state
      integer,intent(in)            :: medPetCount
      integer,intent(out),optional  :: rc
      ! local variables
      integer                                 :: itemCount, item
      type(ESMF_Field)                        :: field
      character(len=20)                       :: transferAction
      character(len=80), allocatable          :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
      type(ESMF_GeomType_Flag)                :: geomtype
      type(ESMF_Grid)                         :: grid
      type(ESMF_Mesh)                         :: mesh
      type(ESMF_DistGrid)                     :: distgrid
      type(ESMF_DELayout)                     :: delayout
      integer                                 :: deCount
      integer                                 :: petCount
      integer, allocatable                    :: petMap(:)
      integer                                 :: i
      integer                                 :: dimCount, tileCount
      integer, allocatable                    :: minIndexPTile(:,:), maxIndexPTile(:,:)
      type(ESMF_StateIntent_Flag)             :: stateIntent
      character(len=80)                       :: transferActionAttr

      if (present(rc)) rc = ESMF_SUCCESS

      call ESMF_StateGet(state, stateIntent=stateIntent, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      if (stateIntent==ESMF_STATEINTENT_EXPORT) then
        transferActionAttr="ProducerTransferAction"
      elseif (stateIntent==ESMF_STATEINTENT_IMPORT) then
        transferActionAttr="ConsumerTransferAction"
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="The stateIntent must either be IMPORT or EXPORT here.", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return  ! bail out
      endif

      call ESMF_StateGet(state, nestedFlag=.true., itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      allocate(itemNameList(itemCount), itemTypeList(itemCount))

      call ESMF_StateGet(state, nestedFlag=.true., &
        itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      do item=1, itemCount
        if (itemTypeList(item)==ESMF_STATEITEM_FIELD) then
          ! this is a field -> get more info
          call ESMF_StateGet(state, field=field, itemName=itemNameList(item), &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call NUOPC_GetAttribute(field, name=transferActionAttr, &
            value=transferAction, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (trim(transferAction)=="accept") then
            ! the Connector instructed the Mediator to accept geom object
            ! -> find out which type geom object the field holds
            call ESMF_FieldGet(field, geomtype=geomtype, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            if (geomtype==ESMF_GEOMTYPE_GRID) then
              ! empty field holds a Grid with DistGrid
              call ESMF_FieldGet(field, grid=grid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              ! access the DistGrid
              call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              ! access the DELayout
              call ESMF_DistGridGet(distgrid, delayout=delayout, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              ! access petMap
              call ESMF_DELayoutGet(delayout, deCount=deCount, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              if (deCount .gt. 0) then
                petCount = 1
                allocate(petMap(deCount))
                call ESMF_DELayoutGet(delayout, petMap=petMap, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__)) return  ! bail out
                do i=2,deCount
                  if(ALL(petMap(i) .ne. petMap(1:i-1))) petCount = petCount + 1
                enddo
                deallocate(petMap)
              else
                petCount = 0
              endif
              if (petCount .eq. medPetCount) then
                distgrid = ESMF_DistGridCreate(distgrid=distgrid, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__)) return  ! bail out
              else
                ! Create a custom DistGrid, based on the minIndex, maxIndex of the
                ! accepted DistGrid, but with a default regDecomp for the current VM
                ! that leads to 1DE/PET.
                ! get dimCount and tileCount
                call ESMF_DistGridGet(distgrid, dimCount=dimCount, &
                  tileCount=tileCount, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__)) return  ! bail out
                ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
                allocate(minIndexPTile(dimCount, tileCount), &
                  maxIndexPTile(dimCount, tileCount))
                ! get minIndex and maxIndex arrays
                call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
                  maxIndexPTile=maxIndexPTile, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__)) return  ! bail out
                ! create the new DistGrid with the same minIndexPTile and maxIndexPTile,
                ! but with a default regDecompPTile
                distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
                  maxIndexPTile=maxIndexPTile, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__)) return  ! bail out
                ! local clean-up
                deallocate(minIndexPTile, maxIndexPTile)
              endif
              ! Create a new Grid on the new DistGrid and swap it in the Field
              grid = ESMF_GridCreate(distgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              call ESMF_FieldEmptySet(field, grid=grid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            elseif (geomtype==ESMF_GEOMTYPE_MESH) then
              ! empty field holds a Mesh with DistGrid
              call ESMF_FieldGet(field, mesh=mesh, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              ! access the DistGrid
              call ESMF_MeshGet(mesh, elementDistgrid=distgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              ! access the DELayout
              call ESMF_DistGridGet(distgrid, delayout=delayout, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              ! access petMap
              call ESMF_DELayoutGet(delayout, deCount=deCount, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              if (deCount .gt. 0) then
                petCount = 1
                allocate(petMap(deCount))
                call ESMF_DELayoutGet(delayout, petMap=petMap, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__)) return  ! bail out
                do i=2,deCount
                  if(ALL(petMap(i) .ne. petMap(1:i-1))) petCount = petCount + 1
                enddo
                deallocate(petMap)
              else
                petCount = 0
              endif
              if (petCount .eq. medPetCount) then
                distgrid = ESMF_DistGridCreate(distgrid=distgrid, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__)) return  ! bail out
              else
                ! Create a custom DistGrid, based on the minIndex, maxIndex of the
                ! accepted DistGrid, but with a default regDecomp for the current VM
                ! that leads to 1DE/PET.
                ! get dimCount and tileCount
                call ESMF_DistGridGet(distgrid, dimCount=dimCount, &
                  tileCount=tileCount, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__)) return  ! bail out
                ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
                allocate(minIndexPTile(dimCount, tileCount), &
                  maxIndexPTile(dimCount, tileCount))
                ! get minIndex and maxIndex arrays
                call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
                  maxIndexPTile=maxIndexPTile, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__)) return  ! bail out
                ! create the new DistGrid with the same minIndexPTile and maxIndexPTile,
                ! but with a default regDecompPTile
                distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
                  maxIndexPTile=maxIndexPTile, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__)) return  ! bail out
                ! local clean-up
                deallocate(minIndexPTile, maxIndexPTile)
              endif
              ! Create a new Grid on the new DistGrid and swap it in the Field
              mesh = ESMF_MeshCreate(distgrid, distgrid, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              call ESMF_FieldEmptySet(field, mesh=mesh, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            else
              call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
                msg="Unsupported geom object found in "// &
                trim(itemNameList(item)), &
                line=__LINE__, file=__FILE__, rcToReturn=rc)
              return ! bail out
            endif
          endif
        endif
      enddo

      deallocate(itemNameList, itemTypeList)

    end subroutine adjustAcceptedGeom

  end subroutine AcceptTransfer

  !-----------------------------------------------------------------------------

  subroutine RealizeAccepted(mediator, rc)
    ! Realize all Fields with transfer action "accept"
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="RealizeAccepted"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_State)          :: importState, exportState
    type(ESMF_Clock)          :: clock
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString
    type(ESMF_Field)          :: field
    character(len=64)         :: attValue
    integer                   :: i
    character(10)             :: instStr
    character(ESMF_MAXSTR)    :: msg
    integer                   :: stat

    rc = ESMF_SUCCESS

    ! get component information
    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! query for importState, exportState, and clock
    call NUOPC_MediatorGet(mediator, importState=importState, &
      exportState=exportState, driverClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! prepare diagnostic values
    if (verbosity>0 .OR. diagnostic>0) then
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_TimeGet(currTime, timeStringISOFrac=currTimeString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! query component for the internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call realizeWithAcceptedGeom(is%wrap%LND, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call realizeWithAcceptedGeom(is%wrap%HYD, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call realizeWithAcceptedGeom(is%wrap%GWR, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! realize component connections
    ! count ensemble members and create ensemble state
    if (is%wrap%LND%enabled) then
      call med_ext_conn_realize(is%wrap%LND, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call med_count_ensMembers(is%wrap%LND%econn(1)%extToState, &
        cntMbrs=is%wrap%LND%cntMbrs, label="MED: toLnd", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call create_ensemble_state(is%wrap%LND, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (verbosity>0) then
        call med_comp_logwrite(is%wrap%LND, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
    endif
    if (is%wrap%HYD%enabled) then
      call med_ext_conn_realize(is%wrap%HYD, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call med_count_ensMembers(is%wrap%HYD%econn(1)%extToState, &
        cntMbrs=is%wrap%HYD%cntMbrs, label="MED: toHyd", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call create_ensemble_state(is%wrap%HYD, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (verbosity>0) then
        call med_comp_logwrite(is%wrap%HYD, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
    endif
    if (is%wrap%GWR%enabled) then
      call med_ext_conn_realize(is%wrap%GWR, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call med_count_ensMembers(is%wrap%GWR%econn(1)%extToState, &
        cntMbrs=is%wrap%GWR%cntMbrs, label="MED: toGwr", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call create_ensemble_state(is%wrap%GWR, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (verbosity>0) then
        call med_comp_logwrite(is%wrap%GWR, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
    endif

    ! LND <-> HYD internal connections
    if (is%wrap%LND%enabled .and. is%wrap%HYD%enabled) then
      call create_iconn_state(srccmp=is%wrap%LND, dstcmp=is%wrap%HYD, &
        iconn=is%wrap%LND2HYD, cname="LND2HYD", verbosity=verbosity, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call create_iconn_state(srccmp=is%wrap%HYD, dstcmp=is%wrap%LND, &
        iconn=is%wrap%HYD2LND, cname="HYD2LND", verbosity=verbosity, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (verbosity>0) then
        call med_iconn_logwrite(is%wrap%LND2HYD, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call med_iconn_logwrite(is%wrap%HYD2LND, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
    endif
    ! LND <-> GWR internal connections
    if (is%wrap%LND%enabled .and. is%wrap%GWR%enabled) then
      call create_iconn_state(srccmp=is%wrap%LND, dstcmp=is%wrap%GWR, &
        iconn=is%wrap%LND2GWR, cname="LND2GWR", verbosity=verbosity, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call create_iconn_state(srccmp=is%wrap%GWR, dstcmp=is%wrap%LND, &
        iconn=is%wrap%GWR2LND, cname="GWR2LND", verbosity=verbosity, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (verbosity>0) then
        call med_iconn_logwrite(is%wrap%LND2GWR, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call med_iconn_logwrite(is%wrap%GWR2LND, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
    endif
    ! HYD <-> GWR internal connections
    if (is%wrap%HYD%enabled .and. is%wrap%GWR%enabled) then
      call create_iconn_state(srccmp=is%wrap%HYD, dstcmp=is%wrap%GWR, &
        iconn=is%wrap%HYD2GWR, cname="HYD2GWR", verbosity=verbosity, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call create_iconn_state(srccmp=is%wrap%GWR, dstcmp=is%wrap%HYD, &
        iconn=is%wrap%GWR2HYD, cname="GWR2HYD", verbosity=verbosity, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (verbosity>0) then
        call med_iconn_logwrite(is%wrap%HYD2GWR, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call med_iconn_logwrite(is%wrap%GWR2HYD, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
    endif

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine realizeWithAcceptedGeom(comp, rc)
      ! Look at all of the fields in state, including in nested states. Realize
      ! with the accepted and adjusted geom object.
      type(med_comp_type), intent(inout) :: comp
      integer                            :: rc
      ! local variables
      integer                                 :: i, j
      integer                                 :: itemCount
      character(len=80), allocatable          :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
      type(ESMF_Field)                        :: field
      logical                                 :: isConnected

      rc = ESMF_SUCCESS

      do j=1, comp%cntInst

        call ESMF_StateGet(comp%econn(j)%extFrState, nestedFlag=.true., &
          itemCount=itemCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        allocate(itemNameList(itemCount), itemTypeList(itemCount))
        call ESMF_StateGet(comp%econn(j)%extFrState, nestedFlag=.true., &
          itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        do i=1, itemCount
          if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then
            isConnected=NUOPC_IsConnected(comp%econn(j)%extFrState, &
              fieldName=itemNameList(i), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            if (isConnected) then
              call NUOPC_Realize(comp%econn(j)%extFrState, &
                fieldName=itemNameList(i), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              call ESMF_StateGet(comp%econn(j)%extFrState, &
                itemName=itemNameList(i), field=field, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              call ESMF_FieldFill(field, dataFillScheme="const", &
                const1=dfltVal, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            else
              call ESMF_StateRemove(comp%econn(j)%extFrState, &
                (/itemNameList(i)/), relaxedflag=.true., rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            endif
          endif
        enddo
        deallocate(itemNameList, itemTypeList)

        call ESMF_StateGet(comp%econn(j)%extToState, nestedFlag=.true., &
          itemCount=itemCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        allocate(itemNameList(itemCount), itemTypeList(itemCount))
        call ESMF_StateGet(comp%econn(j)%extToState, nestedFlag=.true., &
          itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        do i=1, itemCount
          if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then
            isConnected=NUOPC_IsConnected(comp%econn(j)%extToState, &
              fieldName=itemNameList(i), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            if (isConnected) then
              call NUOPC_Realize(comp%econn(j)%extToState, &
                fieldName=itemNameList(i), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              call ESMF_StateGet(comp%econn(j)%extToState, &
                itemName=itemNameList(i), field=field, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              call ESMF_FieldFill(field, dataFillScheme="const", &
                const1=dfltVal, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            else
              call ESMF_StateRemove(comp%econn(j)%extToState, &
                (/itemNameList(i)/), relaxedflag=.true., rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            endif
          endif
        enddo
        deallocate(itemNameList, itemTypeList)

      enddo
    end subroutine realizeWithAcceptedGeom

  end subroutine RealizeAccepted

  !---------------------------------------------------------------------------

  subroutine med_ext_conn_realize(comp, rc)
    type(med_comp_type), intent(inout) :: comp
    integer, intent(out)               :: rc
    ! local variables
    integer :: itemCount
    type(ESMF_StateItem_Flag) :: itemType
    type(med_fld_type), allocatable :: tmpFlds(:)
    integer :: i, j
    integer :: connCount
    type(ESMF_Field) :: field
    integer :: stat
    character(10) :: instStr
    logical :: isProvided

    rc = ESMF_SUCCESS

    ! Generate connect field lists
    do j=1, comp%cntInst
      ! Create from Field Bundle
      comp%econn(j)%connFrFB = ESMF_FieldBundleCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      ! Set the max size of from tmpFields
      call ESMF_StateGet(comp%econn(j)%extFrState, nestedFlag=.true., &
        itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(tmpFlds(itemCount), stat=stat)
      if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
      ! Find connected from fields
      connCount = 0
      do i=1, size(comp%econn(j)%allFrFlds)
        call ESMF_StateGet(comp%econn(j)%extFrState, &
          itemName=comp%econn(j)%allFrFlds(i)%stateName, &
          itemType=itemType, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (itemType .eq. ESMF_STATEITEM_FIELD) then
          connCount = connCount + 1
          tmpFlds(connCount) = comp%econn(j)%allFrFlds(i)
          call ESMF_StateGet(comp%econn(j)%extFrState, &
            itemName=comp%econn(j)%allFrFlds(i)%stateName, &
            field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldBundleAdd(comp%econn(j)%connFrFB, &
            fieldList=(/field/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        else
          ! FROM FIELD NOT FOUND IN FROM STATE
        endif
      enddo
      ! Build connected from fields list
      if (connCount .gt. 0) then
        allocate(comp%econn(j)%connFrFlds(connCount),stat=stat)
        if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out
        comp%econn(j)%connFrFlds(1:connCount) = tmpFlds(1:connCount)
      else
        allocate(comp%econn(j)%connFrFlds(0),stat=stat)
        if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out
      endif
      deallocate(tmpFlds,stat=stat)
      if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out

      ! Create to Field Bundle
      comp%econn(j)%connToFB = ESMF_FieldBundleCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      ! Set the max size of to tmpFields
      call ESMF_StateGet(comp%econn(j)%extToState, nestedFlag=.true., &
        itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(tmpFlds(itemCount), stat=stat)
      if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
      ! Find connected to fields
      connCount = 0
      do i=1, size(comp%econn(j)%allToFlds)
        call ESMF_StateGet(comp%econn(j)%extToState, &
          itemName=comp%econn(j)%allToFlds(i)%stateName, &
          itemType=itemType, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (itemType .eq. ESMF_STATEITEM_FIELD) then
          connCount = connCount + 1
          tmpFlds(connCount) = comp%econn(j)%allToFlds(i)
          call ESMF_StateGet(comp%econn(j)%extToState, &
            itemName=comp%econn(j)%allToFlds(i)%stateName, &
            field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldBundleAdd(comp%econn(j)%connToFB, &
            fieldList=(/field/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        else
          ! TO FIELD NOT FOUND IN TO STATE
        endif
      enddo
      ! Build connected to fields list
      if (connCount .gt. 0) then
        allocate(comp%econn(j)%connToFlds(connCount),stat=stat)
        if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out
        comp%econn(j)%connToFlds(1:connCount) = tmpFlds(1:connCount)
      else
        allocate(comp%econn(j)%connToFlds(0),stat=stat)
        if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out
      endif
      deallocate(tmpFlds,stat=stat)
      if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out

    enddo

  end subroutine med_ext_conn_realize

  !---------------------------------------------------------------------------

  subroutine med_count_ensMembers(ensState, cntMbrs, label, rc)
    type(ESMF_State),intent(in) :: ensState
    integer,intent(out)         :: cntMbrs
    character(*),intent(in)     :: label
    integer,intent(out)         :: rc
    ! local variables
    integer                                 :: i
    character(len=80), allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    integer                                 :: itemCount
    type(ESMF_Field)                        :: field
    type(ESMF_Array)                        :: array
    integer                                 :: rank
    integer                                 :: dimCount
    integer                                 :: undistDimCount
    integer, allocatable                    :: undistLBound(:)
    integer, allocatable                    :: undistUBound(:)

    call ESMF_StateGet(ensState, itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(itemNameList(itemCount), itemTypeList(itemCount))
    call ESMF_StateGet(ensState, &
      itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    do i=1, itemCount
      if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(ensState, field=field, itemName=itemNameList(i), &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_FieldGet(field, array=array, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_ArrayGet(array, rank=rank, dimCount=dimCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        undistDimCount = rank - dimCount
        if (undistDimCount .eq. 1) then
          allocate(undistLBound(undistDimCount), undistUBound(undistDimCount))
          call ESMF_ArrayGet(array, undistLBound=undistLBound, &
            undistUBound=undistUBound, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          cntMbrs = undistUBound(1) - undistLBound(1) + 1
          deallocate(undistLBound, undistUBound)
        elseif (undistDimCount .lt. 1) then
          cntMbrs = 1
        else
          cntMbrs = -1
          call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
            msg=trim(label)//" - Cannot determine ensemble count for "// &
                "greater than 1 undistributed dimension.", &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return ! bail out
        endif
        exit
      endif
    enddo
    deallocate(itemNameList, itemTypeList)

  end subroutine med_count_ensMembers

  !---------------------------------------------------------------------------

  subroutine create_ensemble_state(comp, rc)
    ! Look at all of the fields in state, including in nested states. Create
    ! new field without undistributed dimension and add to new state

    type(med_comp_type), intent(inout) :: comp
    integer, intent(out) :: rc

    ! local variables
    integer                                 :: i, j
    integer                                 :: itemCount
    integer                                 :: fieldCount
    character(len=80), allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    type(ESMF_Field)                        :: field
    type(ESMF_Grid)                         :: grid
    type(ESMF_Field)                        :: newfield
    type(ESMF_TypeKind_Flag)                :: typekind

    rc = ESMF_SUCCESS

    ! Create ensemble state for LND
    if (comp%ensMap .eq. EMAP_NOENSM) then
      comp%cntMbrs = 1
      allocate(comp%econn(1)%intFrState(1))
      allocate(comp%econn(1)%intToState(1))
      comp%econn(1)%intToState(1) = comp%econn(1)%extToState
      comp%econn(1)%intFrState(1) = comp%econn(1)%extFrState
    elseif (comp%ensMap .eq. EMAP_UNDIST) then
      ! create internal from state
      if (allocated(comp%econn(1)%intFrState)) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="MED: scatter state already allocated Fr"//trim(comp%cname), &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return ! bail out
      else
        allocate(comp%econn(1)%intFrState(comp%cntMbrs))
      endif
      ! realize all the fields in the state (geoms have been transferred)
      do i=1, comp%cntMbrs
        ! create empty state
        comp%econn(1)%intFrState(i) = ESMF_StateCreate(rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
      ! query info about the items in the state
      call ESMF_StateGet(comp%econn(1)%extFrState, nestedFlag=.true., &
        itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(itemNameList(itemCount), itemTypeList(itemCount))
      call ESMF_StateGet(comp%econn(1)%extFrState, nestedFlag=.true., &
        itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      do j=1, itemCount
        if (itemTypeList(j)==ESMF_STATEITEM_FIELD) then
          ! add field to field bundle
          call ESMF_StateGet(comp%econn(1)%extFrState, field=field, &
            itemName=itemNameList(j), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldGet(field, grid=grid, typekind=typekind, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          do i=1, comp%cntMbrs
            newfield = ESMF_FieldCreate(name=trim(itemNameList(j)), &
              grid=grid, typekind=typekind, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_FieldFill(newfield, dataFillScheme="const", &
              const1=dfltVal, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_StateAdd(comp%econn(1)%intFrState(i), &
              fieldList=(/newfield/), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          enddo
        endif
      enddo
      deallocate(itemNameList, itemTypeList)

      ! create internal to state
      if (allocated(comp%econn(1)%intToState)) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="MED: scatter state already allocated To"//trim(comp%cname), &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return ! bail out
      else
        allocate(comp%econn(1)%intToState(comp%cntMbrs))
      endif
      ! realize all the fields in the state (geoms have been transferred)
      do i=1, comp%cntMbrs
        ! create empty state
        comp%econn(1)%intToState(i) = ESMF_StateCreate(rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
      ! query info about the items in the state
      call ESMF_StateGet(comp%econn(1)%extToState, nestedFlag=.true., &
        itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(itemNameList(itemCount), itemTypeList(itemCount))
      call ESMF_StateGet(comp%econn(1)%extToState, nestedFlag=.true., &
        itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      do j=1, itemCount
        if (itemTypeList(j)==ESMF_STATEITEM_FIELD) then
          ! add field to field bundle
          call ESMF_StateGet(comp%econn(1)%extToState, field=field, &
            itemName=itemNameList(j), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldGet(field, grid=grid, typekind=typekind, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          do i=1, comp%cntMbrs
            newfield = ESMF_FieldCreate(name=trim(itemNameList(j)), &
              grid=grid, typekind=typekind, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_FieldFill(newfield, dataFillScheme="const", &
              const1=dfltVal, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_StateAdd(comp%econn(1)%intToState(i), &
              fieldList=(/newfield/), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          enddo
        endif
      enddo
      deallocate(itemNameList, itemTypeList)
    elseif (comp%ensMap .eq. EMAP_MINSTS) then
      do i=1, comp%cntInst
        allocate(comp%econn(i)%intToState(1))
        allocate(comp%econn(i)%intFrState(1))
        comp%econn(i)%intToState(1) = comp%econn(i)%extToState
        comp%econn(i)%intFrState(1) = comp%econn(i)%extFrState
      enddo
    else
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="MED: Undefined ensemble mapping "//trim(comp%cname), &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return ! bail out
    endif ! comp%ensMap

  end subroutine create_ensemble_state

  !---------------------------------------------------------------------------

  subroutine create_iconn_state(srccmp, dstcmp, iconn, cname, verbosity, rc)
    type(med_comp_type), intent(in)                     :: srccmp
    type(med_comp_type), intent(in)                     :: dstcmp
    type(med_int_conn_type), intent(inout), allocatable :: iconn(:)
    character(*),intent(in)                             :: cname
    integer, intent(in)                                 :: verbosity
    integer,intent(out)                                 :: rc
    ! local variables
    integer :: i, j, k
    logical :: fieldMatch
    type(ESMF_Field) :: srcFld, dstFld
    integer,pointer :: srcMaskValues(:)
    integer,pointer :: dstMaskValues(:)
    character(len=24) :: remapStr
    character(len=24) :: srcMaskStr
    character(len=24) :: dstMaskStr
    character(ESMF_MAXSTR) :: msg
    integer :: stat
    character(6)  :: maxStr
    character(4)  :: strFmt

    rc = ESMF_SUCCESS

    if (allocated(iconn)) then
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="MED: connection already allocated "//trim(cname), &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return ! bail out
    endif

    if ((srccmp%ensMap .eq. EMAP_NOENSM) .and. &
        (dstcmp%ensMap .eq. EMAP_NOENSM)) then
      allocate(iconn(1), stat=stat)
      if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
      iconn(1)%cname=cname
      iconn(1)%srcState=srccmp%econn(1)%intFrState(1)
      iconn(1)%dstState=dstcmp%econn(1)%intToState(1)
      iconn(1)%mapping=dstcmp%remap
      iconn(1)%srcMask=srccmp%maskFr
      iconn(1)%dstMask=dstcmp%maskTo
      call med_compute_rh(iconn=iconn(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    elseif ((srccmp%ensMap .eq. EMAP_UNDIST) .and. &
            (dstcmp%ensMap .eq. EMAP_UNDIST)) then
      if (srccmp%cntMbrs .ne. dstcmp%cntMbrs) then
        call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
          msg="MED: ensemble members do not match "//trim(cname), &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return ! bail out
      endif
      allocate(iconn(1), stat=stat)
      if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
      iconn(1)%cname=cname
      iconn(1)%srcState=srccmp%econn(1)%intFrState(1)
      iconn(1)%dstState=dstcmp%econn(1)%intToState(1)
      iconn(1)%mapping=dstcmp%remap
      iconn(1)%srcMask=srccmp%maskFr
      iconn(1)%dstMask=dstcmp%maskTo
      call med_compute_rh(iconn=iconn(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    elseif ((srccmp%ensMap .eq. EMAP_MINSTS) .and. &
            (dstcmp%ensMap .eq. EMAP_MINSTS)) then
      if (srccmp%cntInst .ne. dstcmp%cntInst) then
        call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
          msg="MED: ensemble members do not match "//trim(cname), &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return ! bail out
      endif
      allocate(iconn(srccmp%cntInst), stat=stat)
      if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
      write(maxStr,"(I0)") srccmp%cntInst
      write(strFmt,"(A1,I0,A1,I0)") "I", len_trim(maxStr), &
                                    ".", len_trim(maxStr)
      do i=1, srccmp%cntInst
        write(iconn(i)%cname,"(A7,"//trim(strFmt)//")") trim(cname)//"-",i
        iconn(i)%srcState=srccmp%econn(i)%intFrState(1)
        iconn(i)%dstState=dstcmp%econn(i)%intToState(1)
        iconn(i)%mapping=dstcmp%remap
        iconn(i)%srcMask=srccmp%maskFr
        iconn(i)%dstMask=dstcmp%maskTo
        call med_compute_rh(iconn=iconn(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
    elseif ((srccmp%ensMap .eq. EMAP_MINSTS) .and. &
            (dstcmp%ensMap .eq. EMAP_UNDIST)) then
      if (srccmp%cntInst .ne. dstcmp%cntMbrs) then
        call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
          msg="MED: ensemble members do not match "//trim(cname), &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return ! bail out
      endif
      allocate(iconn(srccmp%cntInst), stat=stat)
      if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
      write(maxStr,"(I0)") srccmp%cntInst
      write(strFmt,"(A1,I0,A1,I0)") "I", len_trim(maxStr), &
                                    ".", len_trim(maxStr)
      do i=1, srccmp%cntInst
        write(iconn(i)%cname,"(A7,"//trim(strFmt)//")") trim(cname)//"-",i
        iconn(i)%srcState=srccmp%econn(i)%intFrState(1)
        iconn(i)%dstState=dstcmp%econn(1)%intToState(i)
        iconn(i)%mapping=dstcmp%remap
        iconn(i)%srcMask=srccmp%maskFr
        iconn(i)%dstMask=dstcmp%maskTo
        call med_compute_rh(iconn=iconn(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
    elseif ((srccmp%ensMap .eq. EMAP_UNDIST) .and. &
            (dstcmp%ensMap .eq. EMAP_MINSTS)) then
      if (srccmp%cntMbrs .ne. dstcmp%cntInst) then
        call ESMF_LogSetError(ESMF_RC_ARG_VALUE, &
          msg="MED: ensemble members do not match "//trim(cname), &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return ! bail out
      endif
      allocate(iconn(srccmp%cntMbrs), stat=stat)
      if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
      write(maxStr,"(I0)") srccmp%cntMbrs
      write(strFmt,"(A1,I0,A1,I0)") "I", len_trim(maxStr), &
                                    ".", len_trim(maxStr)
      do i=1, srccmp%cntMbrs
        write(iconn(i)%cname,"(A7,"//trim(strFmt)//")") trim(cname)//"-",i
        iconn(i)%srcState=srccmp%econn(1)%intFrState(i)
        iconn(i)%dstState=dstcmp%econn(i)%intToState(1)
        iconn(i)%mapping=dstcmp%remap
        iconn(i)%srcMask=srccmp%maskFr
        iconn(i)%dstMask=dstcmp%maskTo
        call med_compute_rh(iconn(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
    else
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="MED: ensemble connection not supported "//trim(cname), &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return ! bail out
    endif

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine med_compute_rh(iconn, rc)
      type(med_int_conn_type),intent(inout) :: iconn
      integer,intent(out)                   :: rc
      ! local variables
      integer                                :: i
      character(len=80), allocatable         :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
      type(ESMF_StateItem_Flag)              :: srcItemType
      integer                                :: itemCount
      type(ESMF_Field)                       :: srcFld, dstFld
      integer,pointer                        :: srcMaskValues(:)
      integer,pointer                        :: dstMaskValues(:)
      integer                                :: stat

      rc = ESMF_SUCCESS

      iconn%srcFB = ESMF_FieldBundleCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      iconn%dstFB = ESMF_FieldBundleCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      call ESMF_StateGet(iconn%dstState, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(itemNameList(itemCount), itemTypeList(itemCount))
      call ESMF_StateGet(iconn%dstState, itemNameList=itemNameList, &
        itemTypeList=itemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      do i=1, itemCount
        if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(iconn%srcState, itemName=itemNameList(i), &
            itemType=srcItemType, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (srcItemType==ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(iconn%srcState, itemName=itemNameList(i), &
              field=srcFld, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_FieldBundleAdd(iconn%srcFB, fieldList=(/srcFld/), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_StateGet(iconn%dstState, itemName=itemNameList(i), &
              field=dstFld, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_FieldBundleAdd(iconn%dstFB, fieldList=(/dstFld/), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          else
            call ESMF_LogSetError(ESMF_RC_NOT_FOUND, &
              msg="Source field not found for "// &
              trim(itemNameList(i)), &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return ! bail out
          endif
        endif
      enddo
      deallocate(itemNameList, itemTypeList)

      if (iconn%mapping .eq. FLD_REMAP_BILINR) then
        if (iconn%srcMask .eq. FLD_MASK_NNE) then
          nullify(srcMaskValues)
        elseif (iconn%srcMask .eq. FLD_MASK_LND) then
          srcMaskValues=>maskLND
        elseif (iconn%srcMask .eq. FLD_MASK_WTR) then
          srcMaskValues=>maskWTR
        else
          call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
            msg="Source mask is not implemented.", &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return
        endif
        if (iconn%dstMask .eq. FLD_MASK_NNE) then
          nullify(dstMaskValues)
        elseif (iconn%dstMask .eq. FLD_MASK_LND) then
          dstMaskValues=>maskLND
        elseif (iconn%dstMask .eq. FLD_MASK_WTR) then
          dstMaskValues=>maskWTR
        else
          call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
            msg="Destintation mask is not implemented.", &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return
        endif
        if (associated(srcMaskValues) .AND. associated(dstMaskValues)) then
          call ESMF_FieldBundleRegridStore(iconn%srcFB, iconn%dstFB, &
            routehandle=iconn%rh, &
            srcMaskValues=srcMaskValues, &
            dstMaskValues=dstMaskValues, &
            regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
            polemethod=polemethod, &
            unmappedaction=unmappedaction, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        elseif (associated(srcMaskValues)) then
          call ESMF_FieldBundleRegridStore(iconn%srcFB, iconn%dstFB, &
            routehandle=iconn%rh, &
            srcMaskValues=srcMaskValues, &
            regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
            polemethod=polemethod, &
            unmappedaction=unmappedaction, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        elseif (associated(dstMaskValues)) then
          call ESMF_FieldBundleRegridStore(iconn%srcFB, iconn%dstFB, &
            routehandle=iconn%rh, &
            dstMaskValues=dstMaskValues, &
            regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
            polemethod=polemethod, &
            unmappedaction=unmappedaction, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        else
          call ESMF_FieldBundleRegridStore(iconn%srcFB, iconn%dstFB, &
            routehandle=iconn%rh, &
            regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
            polemethod=polemethod, &
            unmappedaction=unmappedaction, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
      elseif (iconn%mapping .eq. FLD_REMAP_REDIST) then
        call ESMF_FieldBundleRedistStore(iconn%srcFB, iconn%dstFB, &
          routehandle=iconn%rh, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      else
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="Remapping method has not been implemented.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif

    end subroutine med_compute_rh

  end subroutine create_iconn_state

  !-----------------------------------------------------------------------------

  subroutine DataInitialize(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    ! local variables
    character(len=40)            :: name
    character(*), parameter      :: rName="DataInitialize"
    type(type_InternalState)     :: is
    character(len=40)            :: value
    integer                      :: verbosity, diagnostic
    logical                      :: allSatisfied
    logical                      :: cmpSatisfied
    logical                      :: srcSatisfied
    type(ESMF_Clock)             :: mediatorClock
    type(ESMF_Time)              :: currTime
    character(len=40)            :: currTimeString
    character(ESMF_MAXSTR)       :: msg
    integer                      :: stat

    rc = ESMF_SUCCESS

    ! get component information
    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! query the Mediator for clocks
    call NUOPC_MediatorGet(mediator, mediatorClock=mediatorClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! prepare diagnostic values
    if (verbosity>0 .OR. diagnostic>0) then
      call ESMF_ClockGet(mediatorClock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_TimeGet(currTime, timeStringISOFrac=currTimeString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! query component for the internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    allSatisfied=.TRUE.

    ! Initialize LND export
    if (is%wrap%LND%enabled) then
      cmpSatisfied=.TRUE.
      if (is%wrap%LND%iniType == INIT_MODELS) then
        if (is%wrap%HYD%enabled) then
          call med_comp_checkdep(is%wrap%HYD2LND, checkTime=currTime, &
            satisfied=srcSatisfied, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (srcSatisfied) cmpSatisfied=.FALSE.
        endif
        if (is%wrap%GWR%enabled) then
          call med_comp_checkdep(is%wrap%GWR2LND, checkTime=currTime, &
            satisfied=srcSatisfied, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (.NOT. srcSatisfied) cmpSatisfied=.FALSE.
        endif
        if (cmpSatisfied) then
          if (verbosity>0) then
            write (msg,"(A,A)")  trim(name), &
              ": LND data dependency SATISFIED!!!"
            call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          endif
          call MediatorRemapToLnd(mediator, rc=rc) ! Remap toLND
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call med_comp_updated(is%wrap%LND, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        else
          if (verbosity>0) then
            write (msg,"(A,A)")  trim(name), &
              ": LND data dependency NOT YET SATISFIED!!!"
            call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          endif
          allSatisfied=.FALSE.
        endif
      elseif (is%wrap%LND%iniType == INIT_DEFAULTS) then
        if (verbosity>0) then
          write (msg,"(A,A)")  trim(name), &
            ": LND data initialization using coldstart values!!!"
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
        call med_comp_datareset(is%wrap%LND, src=.FALSE., dst=.TRUE., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call med_comp_updated(is%wrap%LND, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      else
        value = is%wrap%LND%iniType
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg=trim(name)//": Unsupported DataInitLnd - "//trim(value), &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    endif

    ! Initialize HYD export
    if (is%wrap%HYD%enabled) then
      cmpSatisfied=.TRUE.
      if (is%wrap%HYD%iniType == INIT_MODELS) then
        if (is%wrap%LND%enabled) then
          call med_comp_checkdep(is%wrap%LND2HYD, checkTime=currTime, &
            satisfied=srcSatisfied, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (srcSatisfied) cmpSatisfied=.FALSE.
        endif
        if (is%wrap%GWR%enabled) then
          call med_comp_checkdep(is%wrap%GWR2HYD, checkTime=currTime, &
            satisfied=srcSatisfied, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (.NOT. srcSatisfied) cmpSatisfied=.FALSE.
        endif
        if (cmpSatisfied) then
          if (verbosity>0) then
            write (msg,"(A,A)")  trim(name), &
              ": HYD data dependency SATISFIED!!!"
            call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          endif
          call MediatorRemapToHyd(mediator, rc=rc) ! Remap toHYD
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call med_comp_updated(is%wrap%HYD, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        else
          if (verbosity>0) then
            write (msg,"(A,A)")  trim(name), &
              ": HYD data dependency NOT YET SATISFIED!!!"
            call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          endif
          allSatisfied=.FALSE.
        endif
      elseif (is%wrap%HYD%iniType == INIT_DEFAULTS) then
        if (verbosity>0) then
          write (msg,"(A,A)")  trim(name), &
            ": HYD data initialization using coldstart values!!!"
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
        call med_comp_datareset(is%wrap%HYD, src=.FALSE., dst=.TRUE., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call med_comp_updated(is%wrap%HYD, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      else
        value = is%wrap%HYD%iniType
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg=trim(name)//": Unsupported DataInitHyd - "//trim(value), &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    endif

    ! Initialize GWR export
    if (is%wrap%GWR%enabled) then
      cmpSatisfied=.TRUE.
      if (is%wrap%GWR%iniType == INIT_MODELS) then
        if (is%wrap%HYD%enabled) then
          call med_comp_checkdep(is%wrap%HYD2GWR, checkTime=currTime, &
            satisfied=srcSatisfied, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (srcSatisfied) cmpSatisfied=.FALSE.
        endif
        if (is%wrap%LND%enabled) then
          call med_comp_checkdep(is%wrap%LND2GWR, checkTime=currTime, &
            satisfied=srcSatisfied, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (.NOT. srcSatisfied) cmpSatisfied=.FALSE.
        endif
        if (cmpSatisfied) then
          if (verbosity>0) then
            write (msg,"(A,A)")  trim(name), &
              ": GWR data dependency SATISFIED!!!"
            call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          endif
          call MediatorRemapToGwr(mediator, rc=rc) ! Remap toGWR
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call med_comp_updated(is%wrap%GWR, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        else
          if (verbosity>0) then
            write (msg,"(A,A)")  trim(name), &
              ": GWR data dependency NOT YET SATISFIED!!!"
            call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          endif
          allSatisfied=.FALSE.
        endif
      elseif (is%wrap%GWR%iniType == INIT_DEFAULTS) then
        if (verbosity>0) then
          write (msg,"(A,A)")  trim(name), &
            ": GWR data initialization using coldstart values!!!"
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
        call med_comp_datareset(is%wrap%GWR, src=.FALSE., dst=.TRUE., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call med_comp_updated(is%wrap%GWR, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      else
        value = is%wrap%GWR%iniType
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg=trim(name)//": Unsupported DataInitGwr - "//trim(value), &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    endif

    if (allSatisfied) then
      if (btest(diagnostic,16)) then
        if (is%wrap%LND%enabled) then
          call med_comp_write(is%wrap%LND, dirOutput=is%wrap%dirOutput, &
            timeStr=currTimeString, phaseStr="InitializeData", &
            src=.TRUE., dst=.TRUE., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
        if (is%wrap%HYD%enabled) then
          call med_comp_write(is%wrap%HYD, dirOutput=is%wrap%dirOutput, &
            timeStr=currTimeString, phaseStr="InitializeData", &
            src=.TRUE., dst=.TRUE., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
        if (is%wrap%GWR%enabled) then
          call med_comp_write(is%wrap%GWR, dirOutput=is%wrap%dirOutput, &
            timeStr=currTimeString, phaseStr="InitializeData", &
            src=.TRUE., dst=.TRUE., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
      endif
      ! indicate that data initialization is complete (breaking out of init-loop)
      call NUOPC_CompAttributeSet(mediator, &
        name="InitializeDataComplete", value="true", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif ! endif allSatisfied

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine med_comp_checkdep(iconn, checkTime, satisfied, rc)
      type(med_int_conn_type), intent(in) :: iconn(:)
      type(ESMF_Time), intent(in)         :: checkTime
      logical, intent(out)                :: satisfied
      integer, intent(out)                :: rc
      ! local variables
      integer                      :: i, j
      integer                      :: fieldCount
      type(ESMF_Field),allocatable :: fieldList(:)
      logical                      :: check

      rc = ESMF_SUCCESS

      satisfied = .TRUE.
      do j=1, size(iconn)
        call ESMF_FieldBundleGet(iconn(j)%srcFB, &
          fieldCount=fieldCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        allocate(fieldList(fieldCount), stat=stat)
        if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out
        call ESMF_FieldBundleGet(iconn(j)%srcFB, &
          fieldList=fieldList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        do i=1, fieldCount
          check = NUOPC_IsAtTime(fieldList(i), checkTime, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (.NOT.check) then
            satisfied = .FALSE.
          endif
        enddo
        deallocate(fieldList, stat=stat)
        if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out
      enddo
    end subroutine med_comp_checkdep

    !---------------------------------------------------------------------------

    subroutine med_comp_updated(comp, rc)
      type(med_comp_type), intent(in) :: comp
      integer, intent(out)            :: rc
      ! local variables
      integer                      :: i, j
      integer                      :: fieldCount
      type(ESMF_Field),allocatable :: fieldList(:)

      rc = ESMF_SUCCESS

      do j=1, size(comp%econn)
        call ESMF_FieldBundleGet(comp%econn(j)%connToFB, &
          fieldCount=fieldCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        allocate(fieldList(fieldCount), stat=stat)
        if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out
        call ESMF_FieldBundleGet(comp%econn(j)%connToFB, &
          fieldList=fieldList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        do i=1, fieldCount
          call NUOPC_SetAttribute(fieldList(i), name="Updated", &
            value="true", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo
        deallocate(fieldList, stat=stat)
        if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out
      enddo
    end subroutine med_comp_updated

  end subroutine DataInitialize

  !-----------------------------------------------------------------------------

  subroutine SetRunClock(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)     :: name
    character(*), parameter    :: rName="SetRunClock"
    type(type_InternalState)   :: is
    character(len=40)          :: value
    integer                    :: verbosity, diagnostic
    type(ESMF_Time)            :: currTime
    character(len=40)          :: currTimeString
    type(ESMF_Clock)           :: mediatorClock, driverClock
    type(ESMF_TimeInterval)    :: timeStep

    rc = ESMF_SUCCESS

    ! get component information
    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! query the Mediator for clocks
    call NUOPC_MediatorGet(mediator, mediatorClock=mediatorClock, &
      driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! prepare diagnostic values
    if (verbosity>0 .OR. diagnostic>0) then
      call ESMF_ClockGet(mediatorClock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_TimeGet(currTime, timeStringISOFrac=currTimeString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! query component for the internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set the mediatorClock to have the current start time as the driverClock
    call ESMF_ClockGet(driverClock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_ClockSet(mediatorClock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! check and set the component clock against the driver clock
    call NUOPC_CompCheckSetClock(mediator, driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end subroutine SetRunClock

  !-----------------------------------------------------------------------------

  subroutine MediatorAdvance(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="MediatorAdvance"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_Clock)          :: mediatorClock
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    rc = ESMF_SUCCESS

    ! get component information
    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! query the Mediator for clocks
    call NUOPC_MediatorGet(mediator, mediatorClock=mediatorClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! prepare diagnostic values
    if (verbosity>0 .OR. diagnostic>0) then
      call ESMF_ClockGet(mediatorClock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_TimeGet(currTime, timeStringISOFrac=currTimeString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! query component for the internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (is%wrap%LND%enabled) then
      call MediatorRemapToLnd(mediator, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif
    if (is%wrap%HYD%enabled) then
      call MediatorRemapToHyd(mediator, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif
    if (is%wrap%GWR%enabled) then
      call MediatorRemapToGwr(mediator, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

  end subroutine MediatorAdvance

  !-----------------------------------------------------------------------------

  subroutine MediatorRemapToLnd(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="MediatorRemapToLnd"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_Clock)          :: mediatorClock
    character(10)             :: instStr
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString
    integer                   :: i

    rc = ESMF_SUCCESS

    ! get component information
    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(mediator, clock=mediatorClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! prepare diagnostic values
    if (verbosity>0 .OR. diagnostic>0) then
      call ESMF_ClockGet(mediatorClock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_TimeGet(currTime, timeStringISOFrac=currTimeString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! query component for the internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (.NOT. is%wrap%LND%enabled) then
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="LND component is not enabled.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! HYD2LND
    if (is%wrap%HYD%enabled) then
      if (btest(diagnostic,16)) then
        call med_comp_write(is%wrap%HYD, dirOutput=is%wrap%dirOutput, &
          timeStr=currTimeString, src=.TRUE., dst=.FALSE., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      if (is%wrap%HYD%ensMap .eq. EMAP_UNDIST) then
        call MedConn_ScatterEnsemble(is%wrap%HYD%econn(1), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      if (is%wrap%LND%remap .eq. FLD_REMAP_BILINR) then
        do i=1, size(is%wrap%HYD2LND)
          call ESMF_FieldBundleRegrid(is%wrap%HYD2LND(i)%srcFB, &
            is%wrap%HYD2LND(i)%dstFB, &
            routehandle=is%wrap%HYD2LND(i)%rh, &
            zeroregion=ESMF_REGION_SELECT, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo
      elseif (is%wrap%LND%remap .eq. FLD_REMAP_REDIST) then
        do i=1, size(is%wrap%HYD2LND)
          call ESMF_FieldBundleRedist(is%wrap%HYD2LND(i)%srcFB, &
            is%wrap%HYD2LND(i)%dstFB, &
            routehandle=is%wrap%HYD2LND(i)%rh, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo
      else
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="LND remapping method has not been implemented.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    endif
    ! GWR2LND
    if (is%wrap%GWR%enabled) then
      if (btest(diagnostic,16)) then
        call med_comp_write(is%wrap%GWR, dirOutput=is%wrap%dirOutput, &
          timeStr=currTimeString, src=.TRUE., dst=.FALSE., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      if (is%wrap%GWR%ensMap .eq. EMAP_UNDIST) then
        call MedConn_ScatterEnsemble(is%wrap%GWR%econn(1), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      if (is%wrap%LND%remap .eq. FLD_REMAP_BILINR) then
        do i=1, size(is%wrap%GWR2LND)
          call ESMF_FieldBundleRegrid(is%wrap%GWR2LND(i)%srcFB, &
            is%wrap%GWR2LND(i)%dstFB, &
            routehandle=is%wrap%GWR2LND(i)%rh, &
            zeroregion=ESMF_REGION_SELECT, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo
      elseif (is%wrap%LND%remap .eq. FLD_REMAP_REDIST) then
        do i=1, size(is%wrap%GWR2LND)
          call ESMF_FieldBundleRedist(is%wrap%GWR2LND(i)%srcFB, &
            is%wrap%GWR2LND(i)%dstFB, &
            routehandle=is%wrap%GWR2LND(i)%rh, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo
      else
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="LND remapping method has not been implemented.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    endif

    if (is%wrap%LND%ensMap .eq. EMAP_UNDIST) then
      call MedConn_GatherEnsemble(is%wrap%LND%econn(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! write dst field bundle
    if (btest(diagnostic,16)) then
      call med_comp_write(is%wrap%LND, dirOutput=is%wrap%dirOutput, &
        timeStr=currTimeString, src=.FALSE., dst=.TRUE., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

  end subroutine MediatorRemapToLnd

  !-----------------------------------------------------------------------------

  subroutine TimestampExport_remapLnd(mediator, rc)
    type(ESMF_GridComp)   :: mediator
    integer, intent(out)  :: rc

    ! This attaches an invalid timestamp on fields sometimes.
    ! Otherwise, it just sets the timestamp to the current clock.

    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="TimestampExport_remapLnd"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_Clock)          :: mediatorClock, driverClock
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString
    integer                   :: n, fieldcount
    integer                   :: i

    rc = ESMF_SUCCESS

    ! get component information
    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! query the Mediator for clocks
    call NUOPC_MediatorGet(mediator, mediatorClock=mediatorClock, &
      driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! prepare diagnostic values
    if (verbosity>0 .OR. diagnostic>0) then
      call ESMF_ClockGet(mediatorClock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_TimeGet(currTime, timeStringISOFrac=currTimeString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! query component for the internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (.NOT. is%wrap%LND%enabled) then
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="LND component is not enabled.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! set the Clock to have the current time as the driverClock
    call ESMF_ClockGet(driverClock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_ClockSet(mediatorClock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !---------------------------
    ! validate all data by default
    !---------------------------

    do i=1, is%wrap%LND%cntInst
#if ESMF_VERSION_MAJOR >= 8
      call NUOPC_SetTimestamp(is%wrap%LND%econn(i)%extToState, mediatorClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
#else
      call NUOPC_UpdateTimestamp(is%wrap%LND%econn(i)%extToState, mediatorClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
#endif
    enddo

  end subroutine TimestampExport_remapLnd

  !-----------------------------------------------------------------------------

  subroutine MediatorRemapToHyd(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="MediatorRemapToHyd"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_Clock)          :: mediatorClock
    character(10)             :: instStr
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString
    integer                   :: i

    rc = ESMF_SUCCESS

    ! get component information
    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! query the Component for its clock
    call ESMF_GridCompGet(mediator, clock=mediatorClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! prepare diagnostic values
    if (verbosity>0 .OR. diagnostic>0) then
      call ESMF_ClockGet(mediatorClock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_TimeGet(currTime, timeStringISOFrac=currTimeString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! query component for the internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (.NOT. is%wrap%HYD%enabled) then
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="HYD component is not enabled.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! LND2HYD
    if (is%wrap%LND%enabled) then
      if (btest(diagnostic,16)) then
        call med_comp_write(is%wrap%LND, dirOutput=is%wrap%dirOutput, &
          timeStr=currTimeString, src=.TRUE., dst=.FALSE., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      if (is%wrap%LND%ensMap .eq. EMAP_UNDIST) then
        call MedConn_ScatterEnsemble(is%wrap%LND%econn(1), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      if (is%wrap%HYD%remap .eq. FLD_REMAP_BILINR) then
        do i=1, size(is%wrap%LND2HYD)
          call ESMF_FieldBundleRegrid(is%wrap%LND2HYD(i)%srcFB, &
            is%wrap%LND2HYD(i)%dstFB, &
            routehandle=is%wrap%LND2HYD(i)%rh, &
            zeroregion=ESMF_REGION_SELECT, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo
      elseif (is%wrap%HYD%remap .eq. FLD_REMAP_REDIST) then
        do i=1, size(is%wrap%LND2HYD)
          call ESMF_FieldBundleRedist(is%wrap%LND2HYD(i)%srcFB, &
            is%wrap%LND2HYD(i)%dstFB, &
            routehandle=is%wrap%LND2HYD(i)%rh, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo
      else
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="HYD remapping method has not been implemented.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    endif
    ! GWR2HYD
    if (is%wrap%GWR%enabled) then
      if (btest(diagnostic,16)) then
        call med_comp_write(is%wrap%GWR, dirOutput=is%wrap%dirOutput, &
          timeStr=currTimeString, src=.TRUE., dst=.FALSE., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      if (is%wrap%GWR%ensMap .eq. EMAP_UNDIST) then
        call MedConn_ScatterEnsemble(is%wrap%GWR%econn(1), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      if (is%wrap%HYD%remap .eq. FLD_REMAP_BILINR) then
        do i=1, size(is%wrap%GWR2HYD)
          call ESMF_FieldBundleRegrid(is%wrap%GWR2HYD(i)%srcFB, &
            is%wrap%GWR2HYD(i)%dstFB, &
            routehandle=is%wrap%GWR2HYD(i)%rh, &
            zeroregion=ESMF_REGION_SELECT, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo
      elseif (is%wrap%HYD%remap .eq. FLD_REMAP_REDIST) then
        do i=1, size(is%wrap%GWR2HYD)
          call ESMF_FieldBundleRedist(is%wrap%GWR2HYD(i)%srcFB, &
            is%wrap%GWR2HYD(i)%dstFB, &
            routehandle=is%wrap%GWR2HYD(i)%rh, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo
      else
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="HYD remapping method has not been implemented.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    endif

    if (is%wrap%HYD%ensMap .eq. EMAP_UNDIST) then
      call MedConn_GatherEnsemble(is%wrap%HYD%econn(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! write dst field bundle
    if (btest(diagnostic,16)) then
      call med_comp_write(is%wrap%HYD, dirOutput=is%wrap%dirOutput, &
        timeStr=currTimeString, src=.FALSE., dst=.TRUE., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

  end subroutine MediatorRemapToHyd

  !-----------------------------------------------------------------------------

  subroutine TimestampExport_remapHyd(mediator, rc)
    type(ESMF_GridComp)   :: mediator
    integer, intent(out)  :: rc

    ! This attaches an invalid timestamp on fields sometimes.
    ! Otherwise, it just sets the timestamp to the current clock.

    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="TimestampExport_remapHyd"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_Clock)          :: mediatorClock, driverClock
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString
    integer                   :: n, fieldcount
    integer                   :: i

    rc = ESMF_SUCCESS

    ! get component information
    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! query the Mediator for clocks
    call NUOPC_MediatorGet(mediator, mediatorClock=mediatorClock, &
      driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! prepare diagnostic values
    if (verbosity>0 .OR. diagnostic>0) then
      call ESMF_ClockGet(mediatorClock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_TimeGet(currTime, timeStringISOFrac=currTimeString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! query component for the internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (.NOT. is%wrap%HYD%enabled) then
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="HYD component is not enabled.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! set the Clock to have the current time as the driverClock
    call ESMF_ClockGet(driverClock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_ClockSet(mediatorClock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !---------------------------
    ! validate all data by default
    !---------------------------

    do i=1, is%wrap%HYD%cntInst
#if ESMF_VERSION_MAJOR >= 8
      call NUOPC_SetTimestamp(is%wrap%HYD%econn(i)%extToState, mediatorClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
#else
      call NUOPC_UpdateTimestamp(is%wrap%HYD%econn(i)%extToState, mediatorClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
#endif
    enddo

  end subroutine TimestampExport_remapHyd

  !-----------------------------------------------------------------------------

  subroutine MediatorRemapToGwr(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="MediatorRemapToGWR"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_Clock)          :: mediatorClock
    character(10)             :: instStr
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString
    integer                   :: i

    rc = ESMF_SUCCESS

    ! get component information
    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! query the Component for its clock
    call ESMF_GridCompGet(mediator, clock=mediatorClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! prepare diagnostic values
    if (verbosity>0 .OR. diagnostic>0) then
      call ESMF_ClockGet(mediatorClock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_TimeGet(currTime, timeStringISOFrac=currTimeString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! query component for the internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (.NOT. is%wrap%GWR%enabled) then
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="GWR component is not enabled.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! LND2GWR
    if (is%wrap%LND%enabled) then
      if (btest(diagnostic,16)) then
        call med_comp_write(is%wrap%LND, dirOutput=is%wrap%dirOutput, &
          timeStr=currTimeString, src=.TRUE., dst=.FALSE., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      if (is%wrap%LND%ensMap .eq. EMAP_UNDIST) then
        call MedConn_ScatterEnsemble(is%wrap%LND%econn(1), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      if (is%wrap%GWR%remap .eq. FLD_REMAP_BILINR) then
        do i=1, size(is%wrap%LND2GWR)
          call ESMF_FieldBundleRegrid(is%wrap%LND2GWR(i)%srcFB, &
            is%wrap%LND2GWR(i)%dstFB, &
            routehandle=is%wrap%LND2GWR(i)%rh, &
            zeroregion=ESMF_REGION_SELECT, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo
      elseif (is%wrap%GWR%remap .eq. FLD_REMAP_REDIST) then
        do i=1, size(is%wrap%LND2GWR)
          call ESMF_FieldBundleRedist(is%wrap%LND2GWR(i)%srcFB, &
            is%wrap%LND2GWR(i)%dstFB, &
            routehandle=is%wrap%LND2GWR(i)%rh, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo
      else
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="GWR remapping method has not been implemented.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    endif
    ! HYD2GWR
    if (is%wrap%HYD%enabled) then
      if (btest(diagnostic,16)) then
        call med_comp_write(is%wrap%HYD, dirOutput=is%wrap%dirOutput, &
          timeStr=currTimeString, src=.TRUE., dst=.FALSE., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      if (is%wrap%GWR%ensMap .eq. EMAP_UNDIST) then
        call MedConn_ScatterEnsemble(is%wrap%GWR%econn(1), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      if (is%wrap%GWR%remap .eq. FLD_REMAP_BILINR) then
        do i=1, size(is%wrap%HYD2GWR)
          call ESMF_FieldBundleRegrid(is%wrap%HYD2GWR(i)%srcFB, &
            is%wrap%HYD2GWR(i)%dstFB, &
            routehandle=is%wrap%HYD2GWR(i)%rh, &
            zeroregion=ESMF_REGION_SELECT, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo
      elseif (is%wrap%GWR%remap .eq. FLD_REMAP_REDIST) then
        do i=1, size(is%wrap%HYD2GWR)
          call ESMF_FieldBundleRedist(is%wrap%HYD2GWR(i)%srcFB, &
            is%wrap%HYD2GWR(i)%dstFB, &
            routehandle=is%wrap%HYD2GWR(i)%rh, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo
      else
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="GWR remapping method has not been implemented.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    endif

    if (is%wrap%GWR%ensMap .eq. EMAP_UNDIST) then
      call MedConn_GatherEnsemble(is%wrap%GWR%econn(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! write dst field bundle
    if (btest(diagnostic,16)) then
      call med_comp_write(is%wrap%GWR, dirOutput=is%wrap%dirOutput, &
        timeStr=currTimeString, src=.FALSE., dst=.TRUE., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

  end subroutine MediatorRemapToGwr

  !-----------------------------------------------------------------------------

  subroutine TimestampExport_remapGwr(mediator, rc)
    type(ESMF_GridComp)   :: mediator
    integer, intent(out)  :: rc

    ! This attaches an invalid timestamp on fields sometimes.
    ! Otherwise, it just sets the timestamp to the current clock.

    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="TimestampExport_remapGWR"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_Clock)          :: mediatorClock, driverClock
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString
    integer                   :: n, fieldcount
    integer                   :: i

    rc = ESMF_SUCCESS

    ! get component information
    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! query the Mediator for clocks
    call NUOPC_MediatorGet(mediator, mediatorClock=mediatorClock, &
      driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! prepare diagnostic values
    if (verbosity>0 .OR. diagnostic>0) then
      call ESMF_ClockGet(mediatorClock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_TimeGet(currTime, timeStringISOFrac=currTimeString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! query component for the internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (.NOT. is%wrap%GWR%enabled) then
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="GWR component is not enabled.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! set the Clock to have the current time as the driverClock
    call ESMF_ClockGet(driverClock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_ClockSet(mediatorClock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !---------------------------
    ! validate all data by default
    !---------------------------

    do i=1, is%wrap%GWR%cntInst
#if ESMF_VERSION_MAJOR >= 8
      call NUOPC_SetTimestamp(is%wrap%GWR%econn(i)%extToState, mediatorClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
#else
      call NUOPC_UpdateTimestamp(is%wrap%GWR%econn(i)%extToState, mediatorClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
#endif
    enddo

  end subroutine TimestampExport_remapGwr

  !-----------------------------------------------------------------------------

  subroutine MediatorFinalize(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="MediatorFinalize"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_Clock)          :: mediatorClock
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString
    integer                   :: i
    integer                   :: stat

    rc = ESMF_SUCCESS

    ! get component information
    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
      diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! query the Mediator for clocks
    call NUOPC_MediatorGet(mediator, mediatorClock=mediatorClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! prepare diagnostic values
    if (verbosity>0 .OR. diagnostic>0) then
      call ESMF_ClockGet(mediatorClock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_TimeGet(currTime, timeStringISOFrac=currTimeString, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! query component for the internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (is%wrap%LND%enabled .and. is%wrap%HYD%enabled) then
      deallocate(is%wrap%LND2HYD, is%wrap%HYD2LND, stat=stat)
      if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
    endif

    if (is%wrap%LND%enabled .and. is%wrap%GWR%enabled) then
      deallocate(is%wrap%LND2GWR, is%wrap%GWR2LND, stat=stat)
      if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
    endif

    if (is%wrap%HYD%enabled .and. is%wrap%GWR%enabled) then
      deallocate(is%wrap%HYD2GWR, is%wrap%GWR2HYD, stat=stat)
      if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
    endif

    if (is%wrap%LND%enabled) then
      do i=1, size(is%wrap%LND%econn)
        deallocate(is%wrap%LND%econn(i)%intToState, is%wrap%LND%econn(i)%intFrState, &
          stat=stat)
        if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out
        deallocate(is%wrap%LND%econn(i)%connFrFlds, is%wrap%LND%econn(i)%connToFlds, &
          stat=stat)
        if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out
      enddo
      deallocate(is%wrap%LND%econn, stat=stat)
      if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
    endif
    if (is%wrap%HYD%enabled) then
      do i=1, size(is%wrap%HYD%econn)
        deallocate(is%wrap%HYD%econn(i)%intToState, is%wrap%HYD%econn(i)%intFrState, &
          stat=stat)
        if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out
        deallocate(is%wrap%HYD%econn(i)%connFrFlds, is%wrap%HYD%econn(i)%connToFlds, &
          stat=stat)
        if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out
      enddo
      deallocate(is%wrap%HYD%econn, stat=stat)
      if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
    endif
    if (is%wrap%GWR%enabled) then
      do i=1, size(is%wrap%GWR%econn)
        deallocate(is%wrap%GWR%econn(i)%intToState, is%wrap%GWR%econn(i)%intFrState, &
          stat=stat)
        if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out
        deallocate(is%wrap%GWR%econn(i)%connFrFlds, is%wrap%GWR%econn(i)%connToFlds, &
          stat=stat)
        if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out
      enddo
      deallocate(is%wrap%GWR%econn, stat=stat)
      if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
    endif
    ! deallocate internal state
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
      file=__FILE__, rcToReturn=rc)) return ! bail out

  end subroutine MediatorFinalize

  !-----------------------------------------------------------------------------

  subroutine MedConn_Scatter(connList, rc)
    type(med_ext_conn_type), intent(inout) :: connList(:)
    integer, intent(out) :: rc
    ! local variables
    integer                       :: i, j
    type(ESMF_Field), allocatable :: srcFields(:)
    integer                       :: fieldCount
    character(len=64)             :: fieldName
    type(ESMF_Field)              :: dstField

    call ESMF_FieldBundleGet(connList(1)%connToFB, fieldCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(srcFields(fieldCount))
    call ESMF_FieldBundleGet(connList(1)%connToFB, fieldList=srcFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    do i=1, fieldCount
      call ESMF_FieldGet(srcFields(i), name=fieldName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      do j=2, size(connList)
        call ESMF_FieldBundleGet(connList(j)%connToFB,fieldName=fieldName, &
          field=dstField, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_FieldCopy(fieldOut=dstField, fieldIn=srcFields(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
    enddo

    deallocate(srcFields)

  end subroutine MedConn_Scatter

  !-----------------------------------------------------------------------------

  subroutine MedConn_ScatterEnsemble(extConn, rc)
    type(med_ext_conn_type), intent(inout) :: extConn
    integer, intent(out) :: rc
    ! local variables
    integer                                 :: i, j
    integer                                 :: itemCount
    integer                                 :: fieldCount
    character(len=80), allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    type(ESMF_Field)                        :: ensField
    type(ESMF_Field)                        :: field
    type(ESMF_TypeKind_Flag)                :: typekind
    real(ESMF_KIND_R4), pointer             :: efdp_R4(:,:,:)
    real(ESMF_KIND_R4), pointer             :: fdp_R4(:,:)
    real(ESMF_KIND_R8), pointer             :: efdp_R8(:,:,:)
    real(ESMF_KIND_R8), pointer             :: fdp_R8(:,:)

    rc = ESMF_SUCCESS

    ! query info about the items in the state
    call ESMF_StateGet(extConn%extFrState, nestedFlag=.true., &
      itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(itemNameList(itemCount), itemTypeList(itemCount))
    call ESMF_StateGet(extConn%extFrState, nestedFlag=.true., &
      itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    ! copy slice of ensemble field to another field
    do i=1, itemCount
      if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(extConn%extFrState, field=ensField, &
          itemName=itemNameList(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_FieldGet(ensField, typekind=typekind, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (typekind==ESMF_TYPEKIND_R4) then
          call ESMF_FieldGet(ensField, farrayPtr=efdp_R4, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          do j=1, size(extConn%intFrState)
            call ESMF_StateGet(extConn%intFrState(j), field=field, &
              itemName=itemNameList(i), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_FieldGet(field, farrayPtr=fdp_R4, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            fdp_R4(:,:) = efdp_R4(:,:,j)
          enddo
        elseif (typekind==ESMF_TYPEKIND_R8) then
          call ESMF_FieldGet(ensField, farrayPtr=efdp_R8, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          do j=1, size(extConn%intFrState)
            call ESMF_StateGet(extConn%intFrState(j), field=field, &
              itemName=itemNameList(i), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_FieldGet(field, farrayPtr=fdp_R8, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            fdp_R8(:,:) = efdp_R8(:,:,j)
          enddo
        else
          call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
            msg="MedConn_ScatterEnsemble: typekind not supported", &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return
        endif
      endif
    enddo

    deallocate(itemNameList, itemTypeList)

  end subroutine MedConn_ScatterEnsemble

  !-----------------------------------------------------------------------------

  subroutine MedConn_GatherEnsemble(extComp, rc)
    type(med_ext_conn_type), intent(inout) :: extComp
    integer, intent(out) :: rc
    ! local variables
    integer                                 :: i, j
    integer                                 :: itemCount
    integer                                 :: fieldCount
    character(len=80), allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
    type(ESMF_Field)                        :: ensField
    type(ESMF_Field)                        :: field
    type(ESMF_TypeKind_Flag)                :: typekind
    real(ESMF_KIND_R4), pointer             :: efdp_R4(:,:,:)
    real(ESMF_KIND_R4), pointer             :: fdp_R4(:,:)
    real(ESMF_KIND_R8), pointer             :: efdp_R8(:,:,:)
    real(ESMF_KIND_R8), pointer             :: fdp_R8(:,:)


    rc = ESMF_SUCCESS

    ! query info about the items in the state
    call ESMF_StateGet(extComp%extToState, itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    allocate(itemNameList(itemCount), itemTypeList(itemCount))
    call ESMF_StateGet(extComp%extToState, itemNameList=itemNameList, &
      itemTypeList=itemTypeList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    ! copy field to every slice in ensemble field
    do i=1, itemCount
      if (itemTypeList(i)==ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(extComp%extToState, field=ensField, &
          itemName=itemNameList(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_FieldGet(ensField, typekind=typekind, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (typekind==ESMF_TYPEKIND_R4) then
          call ESMF_FieldGet(ensField, farrayPtr=efdp_R4, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          do j=1, size(extComp%intToState)
            call ESMF_StateGet(extComp%intToState(j), field=field, &
              itemName=itemNameList(i), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_FieldGet(field, farrayPtr=fdp_R4, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            efdp_R4(:,:,j) = fdp_R4(:,:)
          enddo
        elseif (typekind==ESMF_TYPEKIND_R8) then
          call ESMF_FieldGet(ensField, farrayPtr=efdp_R8, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          do j=1, size(extComp%intToState)
            call ESMF_StateGet(extComp%intToState(j), field=field, &
              itemName=itemNameList(i), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_FieldGet(field, farrayPtr=fdp_R8, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            efdp_R8(:,:,j) = fdp_R8(:,:)
          enddo
        else
          call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
            msg="MedConn_ScatterEnsemble: typekind not supported", &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return
        endif
      endif
    enddo

    deallocate(itemNameList, itemTypeList)

  end subroutine MedConn_GatherEnsemble

  !-----------------------------------------------------------------------------

  subroutine med_comp_datareset(comp, resetValue, src, dst, rc)
    type(med_comp_type), intent(inout)       :: comp
    real(ESMF_KIND_R8), intent(in), optional :: resetValue
    logical, intent(in)                      :: src
    logical, intent(in)                      :: dst
    integer, intent(out) :: rc
    ! local variables
    integer :: i, j
    type(ESMF_Field) :: field

    rc = ESMF_SUCCESS

    if (present(resetValue)) then
      if (src) then
        do j=1, size(comp%econn)
          do i=1, size(comp%econn(j)%connFrFlds)
            call ESMF_FieldBundleGet(comp%econn(j)%connFrFB, &
              comp%econn(j)%connFrFlds(i)%stateName, field=field, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_FieldFill(field, dataFillScheme="const", &
              const1=resetValue, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          enddo
        enddo
      endif
      if (dst) then
        do j=1, size(comp%econn)
          do i=1, size(comp%econn(j)%connToFlds)
            call ESMF_FieldBundleGet(comp%econn(j)%connToFB, &
              comp%econn(j)%connToFlds(i)%stateName, field=field, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_FieldFill(field, dataFillScheme="const", &
              const1=resetValue, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          enddo
        enddo
      endif
    else ! no resetValue
      if (src) then
        do j=1, size(comp%econn)
          do i=1, size(comp%econn(j)%connFrFlds)
            call ESMF_FieldBundleGet(comp%econn(j)%connFrFB, &
              comp%econn(j)%connFrFlds(i)%stateName, field=field, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_FieldFill(field, dataFillScheme="const", &
              const1=REAL(comp%econn(j)%connFrFlds(i)%fillValue,ESMF_KIND_R8), &
              rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          enddo
        enddo
      endif
      if (dst) then
        do j=1, size(comp%econn)
          do i=1, size(comp%econn(j)%connToFlds)
            call ESMF_FieldBundleGet(comp%econn(j)%connToFB, &
              comp%econn(j)%connToFlds(i)%stateName, field=field, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_FieldFill(field, dataFillScheme="const", &
              const1=REAL(comp%econn(j)%connToFlds(i)%fillValue,ESMF_KIND_R8), &
              rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          enddo
        enddo
      endif
    endif
  end subroutine med_comp_datareset

  !-----------------------------------------------------------------------------

  subroutine med_comp_logwrite(comp, name, rc)
    type(med_comp_type), intent(in) :: comp
    character(*), intent(in)        :: name
    integer, intent(out)            :: rc
    ! local variables
    character(ESMF_MAXSTR) :: msg
    character(len=40)      :: value
    integer                :: i
    character(10)          :: instStr

    rc = ESMF_SUCCESS

    call ESMF_LogWrite(  trim(name)//": "//trim(comp%cname)//" Settings", &
      ESMF_LOGMSG_INFO)
    write (msg,"(A,L1)") trim(name)//":   enabled=", comp%enabled
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    value = comp%ensMap
    write (msg,"(A,A)")  trim(name)//":   ensemble_type=", trim(value)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
    write (msg,"(A,L1)") trim(name)//":   multi_instance=", comp%multiInst
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    write (msg,"(A,I0)") trim(name)//":   instance_count=", comp%cntInst
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    write (msg,"(A,I0)") trim(name)//":   member_count=", comp%cntMbrs
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
    value = comp%iniType
    write (msg,"(A,A)")  trim(name)//":   init_type=", trim(value)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
    value = comp%remap
    write (msg,"(A,A)")  trim(name)//":   remap_type=", trim(value)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
    value = comp%maskFr
    write (msg,"(A,A)")  trim(name)//":   mask_from=", trim(value)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
    value = comp%maskTo
    write (msg,"(A,A)")  trim(name)//":   mask_to=", trim(value)
    call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)

    if (allocated(comp%econn)) then
      do i=1, size(comp%econn)
        if (comp%multiInst) then
          write(instStr,"(A4,"//trim(comp%strFmt)//")") trim(comp%cname)//"-",i
        else
          instStr = trim(comp%cname)
        endif
        call med_econn_logwrite(comp%econn(i), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
    endif

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine med_econn_logwrite(econn, rc)
      type(med_ext_conn_type), intent(in) :: econn
      integer,intent(out)                 :: rc
      ! local variables
      integer                               :: stat
      integer                               :: itemCount
      character(len=64), allocatable        :: itemNameList(:)
      integer                               :: iIndex
      character(ESMF_MAXSTR)                :: msg
      character(len=40)                     :: value

      rc = ESMF_SUCCESS

      call ESMF_LogWrite(  trim(name)//": "//trim(instStr)//" ExtFrFields", &
        ESMF_LOGMSG_INFO)
      call ESMF_StateGet(econn%extFrState, nestedFlag=.true., &
        itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(itemNameList(itemCount),stat=stat)
      if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
      call ESMF_StateGet(econn%extFrState, nestedFlag=.true., &
        itemNameList=itemNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (itemCount .gt. 0) then
        do iIndex=1, itemCount
          write (msg,"(A,A)") trim(name)//":   ", itemNameList(iIndex)
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
        enddo
      else
        write (msg,"(A,A)") trim(name)//":   (none)"
        call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
      endif
      deallocate(itemNameList, stat=stat)
      if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out

      call ESMF_LogWrite(  trim(name)//": "//trim(instStr)//" ExtToFields", &
        ESMF_LOGMSG_INFO)
      call ESMF_StateGet(econn%extToState, nestedFlag=.true., &
        itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(itemNameList(itemCount),stat=stat)
      if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out
      call ESMF_StateGet(econn%extToState, nestedFlag=.true., &
        itemNameList=itemNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (itemCount .gt. 0) then
        do iIndex=1, itemCount
          write (msg,"(A,A)") trim(name)//":   ", itemNameList(iIndex)
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
        enddo
      else
        write (msg,"(A,A)") trim(name)//":   (none)"
        call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
      endif
      deallocate(itemNameList, stat=stat)
      if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
        file=__FILE__, rcToReturn=rc)) return ! bail out

    end subroutine med_econn_logwrite

  end subroutine med_comp_logwrite

  !-----------------------------------------------------------------------------

  subroutine med_iconn_logwrite(iconn, name, rc)
    type(med_int_conn_type), allocatable, intent(in) :: iconn(:)
    character(*), intent(in)                         :: name
    integer, intent(out)                             :: rc
    ! local variables
    character(ESMF_MAXSTR) :: msg
    character(len=40)      :: value
    integer                :: i, j
    integer                               :: stat
    integer                               :: fieldCount
    character(len=64), allocatable        :: fieldNameList(:)

    rc = ESMF_SUCCESS

    if (allocated(iconn)) then
      do j=1, size(iconn)

        call ESMF_LogWrite(trim(name)//": "//trim(iconn(j)%cname)// &
          " iconnSrcFB", ESMF_LOGMSG_INFO)
        call ESMF_FieldBundleGet(iconn(j)%srcFB, fieldCount=fieldCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        allocate(fieldNameList(fieldCount),stat=stat)
        if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out
        call ESMF_FieldBundleGet(iconn(j)%srcFB, &
          fieldNameList=fieldNameList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (fieldCount .gt. 0) then
          do i=1, fieldCount
            write (msg,"(A,A)") trim(name)//":   ", fieldNameList(i)
            call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
          enddo
        else
          write (msg,"(A,A)") trim(name)//":   (none)"
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
        endif
        deallocate(fieldNameList, stat=stat)
        if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out

        call ESMF_LogWrite(trim(name)//": "//trim(iconn(j)%cname)// &
          " iconnDstFB", ESMF_LOGMSG_INFO)
        call ESMF_FieldBundleGet(iconn(j)%dstFB, fieldCount=fieldCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        allocate(fieldNameList(fieldCount),stat=stat)
        if (ESMF_LogFoundAllocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out
        call ESMF_FieldBundleGet(iconn(j)%dstFB, &
          fieldNameList=fieldNameList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (fieldCount .gt. 0) then
          do i=1, fieldCount
            write (msg,"(A,A)") trim(name)//":   ", fieldNameList(i)
            call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
          enddo
        else
          write (msg,"(A,A)") trim(name)//":   (none)"
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
        endif
        deallocate(fieldNameList, stat=stat)
        if (ESMF_LogFoundDeallocError(stat, line=__LINE__, &
          file=__FILE__, rcToReturn=rc)) return ! bail out

      enddo
    endif

  end subroutine med_iconn_logwrite

  !-----------------------------------------------------------------------------

  subroutine med_comp_write(comp, dirOutput, timeStr, phaseStr, src, dst, rc)
    type(med_comp_type), intent(in)    :: comp
    character(*), intent(in), optional :: dirOutput
    character(*), intent(in), optional :: timeStr
    character(*), intent(in), optional :: phaseStr
    logical, intent(in)                :: src
    logical, intent(in)                :: dst
    integer, intent(out)               :: rc
    ! local variables
    integer                :: i
    character(ESMF_MAXSTR) :: filePrefix
    character(10)          :: instStr

    rc = ESMF_SUCCESS

    if (present(dirOutput)) then
      filePrefix=trim(dirOutput)//"/diag_"
    else
      filePrefix="diag_"
    endif
    if (present(phaseStr)) filePrefix=trim(filePrefix)//trim(phaseStr)//"_"
    if (present(timeStr))  filePrefix=trim(filePrefix)//trim(timeStr)//"_"

    ! remapping source
    if (src) then
      if (comp%multiInst) then
        do i=1, comp%cntInst
          write(instStr,"(A4,"//trim(comp%strFmt)//")") trim(comp%cname)//"-",i
          call ESMF_FieldBundleWrite(comp%econn(i)%connFrFB, &
            fileName=trim(filePrefix)//trim(instStr)//"_src.nc", &
            singleFile=.true., overwrite=.true., &
            status=ESMF_FILESTATUS_REPLACE, timeslice=1, &
            iofmt=ESMF_IOFMT_NETCDF, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo
      else
        call ESMF_FieldBundleWrite(comp%econn(1)%connFrFB, &
          fileName=trim(filePrefix)//trim(comp%cname)//"_src.nc", &
          singleFile=.true., overwrite=.true., &
          status=ESMF_FILESTATUS_REPLACE, timeslice=1, &
          iofmt=ESMF_IOFMT_NETCDF, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
    endif
    ! remapping destination
    if (dst) then
      if (comp%multiInst) then
        do i=1, comp%cntInst
          write(instStr,"(A4,"//trim(comp%strFmt)//")") trim(comp%cname)//"-",i
          call ESMF_FieldBundleWrite(comp%econn(i)%connToFB, &
            fileName=trim(filePrefix)//trim(instStr)//"_dst.nc", &
            singleFile=.true., overwrite=.true., &
            status=ESMF_FILESTATUS_REPLACE, timeslice=1, &
            iofmt=ESMF_IOFMT_NETCDF, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo
      else
        call ESMF_FieldBundleWrite(comp%econn(1)%connToFB, &
          fileName=trim(filePrefix)//trim(comp%cname)//"_dst.nc", &
          singleFile=.true., overwrite=.true., &
          status=ESMF_FILESTATUS_REPLACE, timeslice=1, &
          iofmt=ESMF_IOFMT_NETCDF, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
    endif
  end subroutine med_comp_write

  !-----------------------------------------------------------------------------

  function dataIniType_eq(type1, type2)
    logical dataIniType_eq
    type(dataIniType), intent(in) :: type1, type2
    dataIniType_eq = (type1%initype == type2%initype)
  end function dataIniType_eq

  !-----------------------------------------------------------------------------

  subroutine dataIniType_tostring(string, tval)
    character(len=*), intent(out) :: string
    type(dataIniType), intent(in) :: tval
    if (tval == INIT_DEFAULTS) then
      string = 'INIT_DEFAULTS'
    elseif (tval == INIT_MODELS) then
      string = 'INIT_MODELS'
    else
      string = 'INIT_ERROR'
    endif
  end subroutine dataIniType_tostring

  !-----------------------------------------------------------------------------

  subroutine dataIniType_frstring(tval, string)
    type(dataIniType), intent(out) :: tval
    character(len=*), intent(in)   :: string
    if (string .eq. 'INIT_DEFAULTS') then
      tval = INIT_DEFAULTS
    elseif (string .eq.'INIT_MODELS') then
      tval = INIT_MODELS
    else
      tval = INIT_ERROR
    endif
  end subroutine dataIniType_frstring

  !-----------------------------------------------------------------------------

  function ensMapType_eq(type1, type2)
    logical ensMapType_eq
    type(ensMapType), intent(in) :: type1, type2
    ensMapType_eq = (type1%maptype == type2%maptype)
  end function ensMapType_eq

  !-----------------------------------------------------------------------------

  subroutine ensMapType_tostring(string, tval)
    character(len=*), intent(out) :: string
    type(ensMapType), intent(in) :: tval
    if (tval == EMAP_NOENSM) then
      string = 'EMAP_NOENSM'
    elseif (tval == EMAP_UNDIST) then
      string = 'EMAP_UNDIST'
    elseif (tval == EMAP_MINSTS) then
      string = 'EMAP_MINSTS'
    else
      string = 'EMAP_ERR'
    endif
  end subroutine ensMapType_tostring

  !-----------------------------------------------------------------------------

  subroutine ensMapType_frstring(tval, string)
    type(ensMapType), intent(out) :: tval
    character(len=*), intent(in)   :: string
    if (string .eq. 'EMAP_NOENSM') then
      tval = EMAP_NOENSM
    elseif (string .eq.'EMAP_UNDIST') then
      tval = EMAP_UNDIST
    elseif (string .eq.'EMAP_MINSTS') then
      tval = EMAP_MINSTS
    else
      tval = EMAP_ERR
    endif
  end subroutine ensMapType_frstring

  !-----------------------------------------------------------------------------

end module Mediator
