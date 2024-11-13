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
    mediator_routine_SS            => SetServices, &
    mediator_routine_Run           => routine_Run, &
    mediator_label_DataInitialize  => label_DataInitialize, &
    mediator_label_Advance         => label_Advance, &
    mediator_label_CheckImport     => label_CheckImport, &
    mediator_label_TimestampExport => label_TimestampExport, &
    mediator_label_SetRunClock     => label_SetRunClock, &
    mediator_label_Finalize        => label_Finalize, &
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

  type(ensMapType), parameter ::  &
    EMAP_ERR    = ensMapType(-1), & ! Error code
    EMAP_NOENSM = ensMapType(0),  & ! No ensemble remapping
    EMAP_ENSLND = ensMapType(1),  & ! Scatter LND ensemble to HYD instances
    EMAP_ENSHYD = ensMapType(2)     ! Scatter HYD ensemble to LND instances

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
    type(med_fld_type), allocatable  :: dstFlds(:)
    type(med_fld_type), allocatable  :: srcFlds(:)
    type(ESMF_FieldBundle)           :: srcFB
    type(ESMF_FieldBundle)           :: dstFB
    type(ESMF_RouteHandle)           :: rhBilnr
    type(ESMF_RouteHandle)           :: rhFcopy
  end type med_int_conn_type

  character(*), parameter :: &
    label_InternalState = "Med_InternalState"

  type type_InternalStateStruct
    type(med_ext_conn_type), allocatable :: LND(:)
    type(med_ext_conn_type), allocatable :: HYD(:)
    type(med_int_conn_type), allocatable :: toLND(:)
    type(med_int_conn_type), allocatable :: toHYD(:)
    type(ensMapType)                     :: ensMap       = EMAP_NOENSM
    logical                              :: multiInstLnd = .false.
    integer                              :: instCntLnd   = 1
    integer                              :: mbrCntLnd    = 1
    character(3)                         :: lndStrFmt    = "0"
    logical                              :: multiInstHyd = .false.
    integer                              :: instCntHyd   = 1
    integer                              :: mbrCntHyd    = 1
    character(3)                         :: hydStrFmt    = "0"
    type(dataIniType)                    :: iniType      = INIT_DEFAULTS
    type(fieldRemapFlag)                 :: remapDstLND  = FLD_REMAP_UNKOWN
    type(fieldRemapFlag)                 :: remapDstHYD  = FLD_REMAP_UNKOWN
    type(fieldMaskFlag)                  :: maskFrLND    = FLD_MASK_UNK
    type(fieldMaskFlag)                  :: maskToLND    = FLD_MASK_UNK
    type(fieldMaskFlag)                  :: maskFrHYD    = FLD_MASK_UNK
    type(fieldMaskFlag)                  :: maskToHYD    = FLD_MASK_UNK
    character(len=40)                    :: dirOutput    = "."
  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  integer, parameter :: srcTermProcessing_Value = 0
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

    ! Provide InitializeP0 to switch from default IPDv00 to IPDv03
    call ESMF_GridCompSetEntryPoint(mediator, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! IPDv03p1: advertise Fields
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! IPDv03p3: realize connected Fields with transfer action "provide"
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeP3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! IPDv03p4: optionally modify the decomp/distr of transferred Grid/Mesh
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p4"/), userRoutine=InitializeP4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! IPDv03p5: realize all Fields with transfer action "accept"
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p5"/), userRoutine=InitializeP5, rc=rc)
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
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of internal state memory failed.", &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_GridCompSetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(mediator, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: mediator
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="InitializeP1"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString
    character(ESMF_MAXSTR)    :: msg

    rc = ESMF_SUCCESS

    ! get the component information
!    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_GridCompGet(mediator, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Diagnostic", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Verbosity", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
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

    ! get lnd instance count from attributes
    call ESMF_AttributeGet(mediator, name="instance_count_lnd", &
      value=is%wrap%instCntLnd, defaultValue=1, &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (is%wrap%instCntLnd.gt.1) then
      is%wrap%multiInstLnd = .true.  ! default multi instance .true.
    else
      is%wrap%multiInstLnd = .false. ! default multi instance .false.
    endif
    call ESMF_AttributeGet(mediator, name="multi_instance_lnd", &
      value=is%wrap%multiInstLnd, defaultValue=is%wrap%multiInstLnd, &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if ((.NOT.is%wrap%multiInstLnd) .AND. (is%wrap%instCntLnd.gt.1)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="multi_instance_lnd must be true for instance count gt 1", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    elseif (is%wrap%instCntLnd.gt.999999) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="instance_count_lnd must be less than 999999", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    ! get hyd instance count from attributes
    call ESMF_AttributeGet(mediator, name="instance_count_hyd", &
      value=is%wrap%instCntHyd, defaultValue=1, &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (is%wrap%instCntHyd.gt.1) then
      is%wrap%multiInstHyd = .true.  ! default multi instance .true.
    else
      is%wrap%multiInstHyd = .false. ! default multi instance .false.
    endif
    call ESMF_AttributeGet(mediator, name="multi_instance_hyd", &
      value=is%wrap%multiInstHyd, defaultValue=is%wrap%multiInstHyd, &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if ((.NOT.is%wrap%multiInstHyd) .AND. (is%wrap%instCntHyd.gt.1)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="multi_instance_hyd must be true for instance count gt 1", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    elseif (is%wrap%instCntHyd.gt.999999) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="instance_count_hyd must be less than 999999", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    call ESMF_AttributeGet(mediator, name="DataInitialization", &
      value=value, defaultValue="INIT_DEFAULTS", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%iniType = value

    call ESMF_AttributeGet(mediator, name="EnsembleMapping", &
      value=value, defaultValue="EMAP_NOENSM", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%ensMap = value

    ! get component output directory
    call ESMF_AttributeGet(mediator, name="OutputDirectory", &
      value=is%wrap%dirOutput, defaultValue=trim(name)//"_OUTPUT", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! prepare diagnostics folder
    if (btest(diagnostic,16)) then
      call ESMF_UtilIOMkDir(pathName=trim(is%wrap%dirOutput), &
        relaxedFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! log settings
    if (verbosity>0) then
      call ESMF_LogWrite(trim(name)//": Settings", ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      write (msg,"(A,I0)") trim(name)//":   Verbosity=", verbosity
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      write (msg,"(A,I0)") trim(name)//":   Diagnostic=", diagnostic
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      write (msg,"(A,L1)")  trim(name)//":   multi_instance_hyd=", &
        is%wrap%multiInstHyd
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      write (msg,"(A,L1)")  trim(name)//":   multi_instance_lnd=", &
        is%wrap%multiInstLnd
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      value = is%wrap%iniType
      write (msg,"(A,A)")  trim(name)//":   DataInitialization=", &
        trim(value)
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      value = is%wrap%ensMap
      write (msg,"(A,A)")  trim(name)//":   EnsembleMapping=", &
        trim(value)
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      write (msg,"(A,A)")  trim(name)//":   OutputDirectory=", &
        is%wrap%dirOutput
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(mediator, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP1(mediator, importState, exportState, clock, rc)
    ! IPDv03p1: advertise Fields
    type(ESMF_GridComp)  :: mediator
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="InitializeP1"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString
    integer                   :: i
    integer                   :: stat
    character(6)              :: maxStr
    character(10)             :: instStr

    rc = ESMF_SUCCESS

    ! get the component information
!    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_GridCompGet(mediator, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Diagnostic", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Verbosity", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
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

    ! allocate component instances
    allocate( is%wrap%LND(is%wrap%instCntLnd), &
              is%wrap%HYD(is%wrap%instCntHyd), &
              stat=stat )
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of component instances memory failed.", &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) &
      return  ! bail out

    ! add to fields dictionary
    call field_dictionary_add(fldsFrLnd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call field_dictionary_add(fldsToLnd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call field_dictionary_add(fldsFrHyd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call field_dictionary_add(fldsToHyd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! ##########################
    ! # Fields to and from LND #
    ! ##########################

    ! generate the instance string format descriptors
    write(maxStr,"(I0)") is%wrap%instCntLnd
    write(is%wrap%lndStrFmt,"(I0,A1,I0)") len_trim(maxStr),".",len_trim(maxStr)
    write(maxStr,"(I0)") is%wrap%instCntHyd
    write(is%wrap%hydStrFmt,"(I0,A1,I0)") len_trim(maxStr),".",len_trim(maxStr)

    do i=1, is%wrap%instCntLnd

      if (is%wrap%multiInstLnd) then
        write(instStr,"(A4,I"//trim(is%wrap%lndStrFmt)//")") "LND-",i
      else
        instStr = "LND"
      endif

      is%wrap%LND(i)%allFrFlds => fldsFrLnd
      !  use namespace in the importState
      call NUOPC_AddNamespace(importState, namespace=trim(instStr), &
        nestedState=is%wrap%LND(i)%extFrState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      !  advertise fields in the nested state
      call field_advertise(is%wrap%LND(i)%allFrFlds, is%wrap%LND(i)%extFrState, &
        "cannot provide", rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      is%wrap%LND(i)%allToFlds => fldsToLnd
      !  use namespace in the exportState
      call NUOPC_AddNamespace(exportState, namespace=trim(instStr), &
        nestedState=is%wrap%LND(i)%extToState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      !  advertise fields in the nested state
      call field_advertise(is%wrap%LND(i)%allToFlds, is%wrap%LND(i)%extToState, &
        "cannot provide", rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

    enddo ! end instCntLnd

    ! ##########################
    ! # Fields to and from HYD #
    ! ##########################

    do i=1, is%wrap%instCntHyd

      if (is%wrap%multiInstHyd) then
        write(instStr,"(A4,I"//trim(is%wrap%hydStrFmt)//")") "HYD-",i
      else
        instStr = "HYD"
      endif

      is%wrap%HYD(i)%allFrFlds => fldsFrHyd
      !  use namespace in the importState
      call NUOPC_AddNamespace(importState, namespace=trim(instStr), &
        nestedState=is%wrap%HYD(i)%extFrState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      !  advertise fields in the nested state
      call field_advertise(is%wrap%HYD(i)%allFrFlds, is%wrap%HYD(i)%extFrState, &
        "cannot provide", rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      is%wrap%HYD(i)%allToFlds => fldsToHyd
      !  use namespace in the exportState
      call NUOPC_AddNamespace(exportState, namespace=trim(instStr), &
        nestedState=is%wrap%HYD(i)%extToState, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      !  advertise fields in the nested state
      call field_advertise(is%wrap%HYD(i)%allToFlds, is%wrap%HYD(i)%extToState, &
        "cannot provide", rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

    enddo ! end instCntHyd

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

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP3(mediator, importState, exportState, clock, rc)
    ! IPDv03p3: realize connected Fields with transfer action "provide"
    ! and remove Fields that are not connected
    type(ESMF_GridComp)  :: mediator
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="InitializeP3"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString

    rc = ESMF_SUCCESS

    ! get the component information
!    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_GridCompGet(mediator, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Diagnostic", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Verbosity", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
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

    end subroutine

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP4(mediator, importState, exportState, clock, rc)
    ! IPDv03p4: optionally modify the decomp/distr of transferred Grid/Mesh
    type(ESMF_GridComp)  :: mediator
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="InitializeP4"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString
    type(ESMF_VM)             :: vm
    integer                   :: medPetCount

    rc = ESMF_SUCCESS

    ! get the component information
!    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_GridCompGet(mediator, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Diagnostic", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Verbosity", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
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

    end subroutine

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP5(mediator, importState, exportState, clock, rc)
    ! IPDv03p5: realize all Fields with transfer action "accept"
    type(ESMF_GridComp)  :: mediator
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    ! local variables
    character(len=40)         :: name
    character(*), parameter   :: rName="InitializeP5"
    type(type_InternalState)  :: is
    character(len=40)         :: value
    integer                   :: verbosity, diagnostic
    type(ESMF_Time)           :: currTime
    character(len=40)         :: currTimeString
    type(ESMF_Field)          :: field
    character(len=64)         :: attValue
    integer                   :: i
    character(10)             :: instStr
    character(ESMF_MAXSTR)    :: msg
    integer                   :: stat

    rc = ESMF_SUCCESS

    ! get the component information
!    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_GridCompGet(mediator, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Diagnostic", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Verbosity", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
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

    call realizeWithAcceptedGeom(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call realizeWithAcceptedGeom(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! Generate connect field lists
    do i=1, is%wrap%instCntLnd
      call med_ext_conn_realize(is%wrap%LND(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    enddo
    do i=1, is%wrap%instCntHyd
      call med_ext_conn_realize(is%wrap%HYD(i), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    enddo

    ! Log Realized States and Dist Grids
    if (btest(verbosity,16)) then
      do i=1, is%wrap%instCntLnd
        if (is%wrap%multiInstLnd) then
          write(instStr,"(A4,I"//trim(is%wrap%lndStrFmt)//")") "LND-",i
        else
          instStr = "LND"
        endif
        call MedState_LogWrite(is%wrap%LND(i)%extFrState, &
          "MED: fr"//trim(instStr),rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call MedState_LogWrite(is%wrap%LND(i)%extToState, &
          "MED: to"//trim(instStr),rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call MedDistGrid_LogWrite(is%wrap%LND(i)%extFrState, &
          "MED: fr"//trim(instStr),rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo ! end instCntLnd
      do i=1, is%wrap%instCntHyd
        if (is%wrap%multiInstHyd) then
          write(instStr,"(A4,I"//trim(is%wrap%hydStrFmt)//")") "HYD-",i
        else
          instStr = "HYD"
        endif
        call MedState_LogWrite(is%wrap%HYD(i)%extFrState, &
          "MED: fr"//trim(instStr),rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call MedState_LogWrite(is%wrap%HYD(i)%extToState, &
          "MED: to"//trim(instStr),rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call MedDistGrid_LogWrite(is%wrap%HYD(i)%extFrState, &
          "MED: fr"//trim(instStr),rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
    endif

    ! Read Remapping Settings
    call ESMF_AttributeGet(mediator, name="RemapLND", value=attValue, &
      defaultValue="FLD_REMAP_REDIST", convention="NUOPC", purpose="Instance", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%remapDstLND = attValue
    call ESMF_AttributeGet(mediator, name="RemapHYD", value=attValue, &
      defaultValue="FLD_REMAP_REDIST", convention="NUOPC", purpose="Instance", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%remapDstHYD = attValue

    ! Read Mask Settings
    call ESMF_AttributeGet(mediator, name="MaskFrLND", value=attValue, &
      defaultValue="FLD_MASK_NNE", convention="NUOPC", purpose="Instance", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%maskFrLND = attValue
    call ESMF_AttributeGet(mediator, name="MaskToLND", value=attValue, &
      defaultValue="FLD_MASK_NNE", convention="NUOPC", purpose="Instance", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%maskToLND = attValue
    call ESMF_AttributeGet(mediator, name="MaskFrHYD", value=attValue, &
      defaultValue="FLD_MASK_NNE", convention="NUOPC", purpose="Instance", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%maskFrHYD = attValue
    call ESMF_AttributeGet(mediator, name="MaskToHYD", value=attValue, &
      defaultValue="FLD_MASK_NNE", convention="NUOPC", purpose="Instance", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    is%wrap%maskToHYD = attValue

    ! Count Ensemble Members
    call med_count_ensMembers(is%wrap%HYD(1)%extToState, &
      mbrCount=is%wrap%mbrCntHyd, label="MED: toHYD", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call med_count_ensMembers(is%wrap%LND(1)%extToState, &
      mbrCount=is%wrap%mbrCntLnd, label="MED: toLND", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! Log ensemble and instance counts
    if (btest(verbosity,16)) then
      write (msg,"(A,I0)") trim(name)//": Instance count Hyd   = ", &
        is%wrap%instCntHyd
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      write (msg,"(A,I0)") trim(name)//": Ensemble members Hyd = ", &
        is%wrap%mbrCntHyd
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      write (msg,"(A,I0)") trim(name)//": Instance count Lnd   = ", &
        is%wrap%instCntLnd
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      write (msg,"(A,I0)") trim(name)//": Ensemble members Lnd = ", &
        is%wrap%mbrCntLnd
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! Check ensemble mapping type
    if (is%wrap%ensMap .eq. EMAP_NOENSM) then
      if ((is%wrap%instCntLnd .ne. 1) .OR. (is%wrap%instCntHyd .ne. 1) .OR. &
          (is%wrap%mbrCntLnd .ne. 0) .OR. (is%wrap%mbrCntHyd .ne. 0)) then
            call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
              msg=trim(name)//" - Ensemble member counts and "// &
                  "instance counts must equal 1 for EMAP_NOENSM", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return ! bail out
      endif
      is%wrap%mbrCntLnd = 1
      is%wrap%mbrCntHyd = 1
    elseif (is%wrap%ensMap .eq. EMAP_ENSLND) then
      if ((is%wrap%instCntLnd .ne. 1) .OR. (is%wrap%mbrCntHyd .ne. 0) .OR. &
          (is%wrap%mbrCntLnd .ne. is%wrap%instCntHyd)) then
            call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
              msg=trim(name)//" - LND ensemble member count must "// &
                  "must match HYD instance count for EMAP_ENSLND", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return ! bail out
      endif
      is%wrap%mbrCntHyd = 1
    elseif (is%wrap%ensMap .eq. EMAP_ENSHYD) then
      if ((is%wrap%instCntHyd .ne. 1) .OR. (is%wrap%mbrCntLnd .ne. 0) .OR. &
          (is%wrap%mbrCntHyd .ne. is%wrap%instCntLnd)) then
            call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
              msg=trim(name)//" - HYD ensemble member count must "// &
                  "must match LND instance count for EMAP_ENSHYD", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return ! bail out
      endif
      is%wrap%mbrCntLnd = 1
    else
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg=trim(name)//" - LND-HYD ensemble mapping type unknown. ", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
      return ! bail out
    endif

    if (is%wrap%ensMap .eq. EMAP_ENSLND) then
      allocate(is%wrap%toLnd(is%wrap%mbrCntLnd), &
               is%wrap%toHyd(is%wrap%instCntHyd), &
               stat=stat )
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of component member connection memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) &
        return  ! bail out
      call createScatterState(is%wrap%LND(1)%extFrState,&
        is%wrap%LND(1)%intFrState, is%wrap%mbrCntLnd, &
        label="MED: scatterFrLND", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call createScatterState(is%wrap%LND(1)%extToState,&
        is%wrap%LND(1)%intToState, is%wrap%mbrCntLnd, &
        label="MED: scatterToLND", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      ! Compute RHs
      do i=1, is%wrap%instCntHyd
        allocate(is%wrap%HYD(i)%intToState(1))
        allocate(is%wrap%HYD(i)%intFrState(1))
        is%wrap%HYD(i)%intToState(1) = is%wrap%HYD(i)%extToState
        is%wrap%HYD(i)%intFrState(1) = is%wrap%HYD(i)%extFrState
        call med_compute_rh(srcCmpList=(/is%wrap%LND(1)/), &
          dstCmp=is%wrap%HYD(i), srcMember=i, dstMember=1, &
          conn=is%wrap%toHyd(i), label="MED: toHYD", &
          mapping=is%wrap%remapDstHYD, srcMask=is%wrap%maskFrLND, &
          dstMask=is%wrap%maskToHYD, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call med_compute_rh(srcCmpList=(/is%wrap%HYD(i)/), &
          dstCmp=is%wrap%LND(1), srcMember=1, dstMember=i, &
          conn=is%wrap%toLND(i), label="MED: toLND", mapping=is%wrap%remapDstLND, &
          srcMask=is%wrap%maskFrHYD, dstMask=is%wrap%maskToLND, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        ! Initialize realized fields
        call MedConn_DataReset(is%wrap%toLND(i), resetValue=NLC_INITVAL, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call MedConn_DataReset(is%wrap%toHYD(i), resetValue=NLC_INITVAL, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
    elseif (is%wrap%ensMap .eq. EMAP_ENSHYD) then
      allocate(is%wrap%toLnd(is%wrap%instCntLnd), &
               is%wrap%toHyd(is%wrap%mbrCntHyd), &
               stat=stat )
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of component member connection memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) &
        return  ! bail out
      call createScatterState(is%wrap%HYD(1)%extFrState,&
        is%wrap%HYD(1)%intFrState, is%wrap%mbrCntHYD, &
        label="MED: scatterFrHYD", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call createScatterState(is%wrap%HYD(1)%extToState, &
        is%wrap%HYD(1)%intToState, is%wrap%mbrCntHYD, &
        label="MED: scatterToHYD", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      ! Compute RHs
      do i=1, is%wrap%instCntLnd
        allocate(is%wrap%LND(i)%intToState(1))
        allocate(is%wrap%LND(i)%intFrState(1))
        is%wrap%LND(i)%intToState(1) = is%wrap%LND(i)%extToState
        is%wrap%LND(i)%intFrState(1) = is%wrap%LND(i)%extFrState
        call med_compute_rh(srcCmpList=(/is%wrap%LND(i)/), &
          dstCmp=is%wrap%HYD(1), srcMember=1, dstMember=i, &
          conn=is%wrap%toHyd(i), label="MED: toHYD", &
          mapping=is%wrap%remapDstHYD, srcMask=is%wrap%maskFrLND, &
          dstMask=is%wrap%maskToHYD, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call med_compute_rh(srcCmpList=(/is%wrap%HYD(1)/), &
          dstCmp=is%wrap%LND(i), srcMember=i, dstMember=1, &
          conn=is%wrap%toLND(i), label="MED: toLND", mapping=is%wrap%remapDstLND, &
          srcMask=is%wrap%maskFrHYD, dstMask=is%wrap%maskToLND, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        ! Initialize realized fields
        call MedConn_DataReset(is%wrap%toLND(i), resetValue=NLC_INITVAL, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call MedConn_DataReset(is%wrap%toHYD(i), resetValue=NLC_INITVAL, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
    elseif (is%wrap%ensMap .eq. EMAP_NOENSM) then
      allocate(is%wrap%toLnd(1), &
               is%wrap%toHyd(1), &
               stat=stat )
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of component member connection memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) &
        return  ! bail out
      allocate(is%wrap%HYD(1)%intToState(1))
      allocate(is%wrap%HYD(1)%intFrState(1))
      allocate(is%wrap%LND(1)%intToState(1))
      allocate(is%wrap%LND(1)%intFrState(1))
      is%wrap%HYD(1)%intToState = is%wrap%HYD(1)%extToState
      is%wrap%HYD(1)%intFrState = is%wrap%HYD(1)%extFrState
      is%wrap%LND(1)%intToState = is%wrap%LND(1)%extToState
      is%wrap%LND(1)%intFrState = is%wrap%LND(1)%extFrState
      ! Compute RHs
      call med_compute_rh(srcCmpList=(/is%wrap%LND(1)/), &
        dstCmp=is%wrap%HYD(1), srcMember=1, dstMember=1, &
        conn=is%wrap%toHyd(1), label="MED: toHYD", &
        mapping=is%wrap%remapDstHYD, srcMask=is%wrap%maskFrLND, &
        dstMask=is%wrap%maskToHYD, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call med_compute_rh(srcCmpList=(/is%wrap%HYD(1)/), &
        dstCmp=is%wrap%LND(1), srcMember=1, dstMember=1, &
        conn=is%wrap%toLND(1), label="MED: toLND", mapping=is%wrap%remapDstLND, &
        srcMask=is%wrap%maskFrHYD, dstMask=is%wrap%maskToLND, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      ! Initialize realized fields
      call MedConn_DataReset(is%wrap%toLND(1), resetValue=NLC_INITVAL, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call MedConn_DataReset(is%wrap%toHYD(1), resetValue=NLC_INITVAL, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif ! is%wrap%ensMap

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine realizeWithAcceptedGeom(state, rc)
      ! Look at all of the fields in state, including in nested states. Realize
      ! with the accepted and adjusted geom object.
      type(ESMF_State)  :: state
      integer, optional :: rc
      ! local variables
      logical                                 :: isPresent
      integer                                 :: itemCount, item, stat
      type(ESMF_Field)                        :: field
      type(ESMF_FieldStatus_Flag)             :: fieldStatus
      character(len=80), allocatable          :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
      integer, pointer                        :: ugLBound(:), ugUBound(:)
      integer, pointer                        :: gridToFieldMap(:)

      if (present(rc)) rc = ESMF_SUCCESS

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
          call ESMF_FieldGet(field, status=fieldStatus, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          if (fieldStatus==ESMF_FIELDSTATUS_GRIDSET) then
            ! the Connector instructed the Mediator to accept geom object
            ! the transferred geom object is already set, allocate memory
            ! for data by complete
            nullify(ugLBound, ugUBound, gridToFieldMap)
            ! deal with gridToFieldMap
            call ESMF_AttributeGet(field, name="GridToFieldMap", &
              convention="NUOPC", purpose="Instance", &
              isPresent=isPresent, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            if (isPresent) then
              call ESMF_AttributeGet(field, name="GridToFieldMap", &
                convention="NUOPC", purpose="Instance", &
                itemCount=itemCount, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            else
              itemCount = 0
            endif
            if (itemCount > 0) then
              allocate(gridToFieldMap(itemCount))
              call ESMF_AttributeGet(field, name="GridToFieldMap", &
                convention="NUOPC", purpose="Instance", &
                valueList=gridToFieldMap, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            endif
            ! deal with ungriddedLBound
            call ESMF_AttributeGet(field, name="UngriddedLBound", &
              convention="NUOPC", purpose="Instance", &
              isPresent=isPresent, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            if (isPresent) then
              call ESMF_AttributeGet(field, name="UngriddedLBound", &
                convention="NUOPC", purpose="Instance", &
                itemCount=itemCount, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            else
              itemCount = 0
            endif
            if (itemCount > 0) then
              allocate(ugLBound(itemCount))
              call ESMF_AttributeGet(field, name="UngriddedLBound", &
                convention="NUOPC", purpose="Instance", &
                valueList=ugLBound, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            endif
            ! deal with ungriddedUBound
            call ESMF_AttributeGet(field, name="UngriddedUBound", &
              convention="NUOPC", purpose="Instance", &
              isPresent=isPresent, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            if (isPresent) then
              call ESMF_AttributeGet(field, name="UngriddedUBound", &
                convention="NUOPC", purpose="Instance", &
                itemCount=itemCount, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            else
              itemCount = 0
            endif
            if (itemCount > 0) then
              allocate(ugUBound(itemCount))
              call ESMF_AttributeGet(field, name="UngriddedUBound", &
                convention="NUOPC", purpose="Instance", &
                valueList=ugUBound, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            endif

            if (associated(ugLBound).and.associated(ugUBound)) then
              call ESMF_FieldEmptyComplete(field, typekind=NLC_TYPEKIND, &
                ungriddedLBound=ugLBound, ungriddedUBound=ugUBound, &
                gridToFieldMap=gridToFieldMap, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              deallocate(ugLBound, ugUBound)
            else
              call ESMF_FieldEmptyComplete(field, typekind=NLC_TYPEKIND, &
                gridToFieldMap=gridToFieldMap, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            endif
            deallocate(gridToFieldMap)
          endif
        endif
      enddo

      deallocate(itemNameList, itemTypeList)

    end subroutine

    !---------------------------------------------------------------------------

    subroutine med_ext_conn_realize(cmp,rc)
      type(med_ext_conn_type),intent(inout) :: cmp
      integer,intent(out)                   :: rc
      ! local variables
      integer :: itemCount
      type(ESMF_StateItem_Flag) :: itemType
      type(med_fld_type), allocatable :: tmpFlds(:)
      integer :: i
      integer :: connCount
      type(ESMF_Field) :: field
      integer :: stat

      rc = ESMF_SUCCESS

      ! Create from Field Bundle
      cmp%connFrFB = ESMF_FieldBundleCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      ! Set the max size of from tmpFields
      call ESMF_StateGet(cmp%extFrState, nestedFlag=.true., &
        itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(tmpFlds(itemCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of field list memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return ! bail out
      ! Find connected from fields
      connCount = 0
      do i=1, size(cmp%allFrFlds)
        call ESMF_StateGet(cmp%extFrState, itemName=cmp%allFrFlds(i)%stateName, &
          itemType=itemType, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (itemType .eq. ESMF_STATEITEM_FIELD) then
          connCount = connCount + 1
          tmpFlds(connCount) = cmp%allFrFlds(i)
          call ESMF_StateGet(cmp%extFrState, itemName=cmp%allFrFlds(i)%stateName, &
            field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldBundleAdd(cmp%connFrFB, fieldList=(/field/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        else
          ! FROM FIELD NOT FOUND IN FROM STATE
        endif
      enddo
     ! Build connected from fields list
     if (connCount .gt. 0) then
        allocate(cmp%connFrFlds(connCount),stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of field list memory failed.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)) return ! bail out
        cmp%connFrFlds(1:connCount) = tmpFlds(1:connCount)
      else
        allocate(cmp%connFrFlds(0),stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of field list memory failed.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)) return ! bail out
      endif
      deallocate(tmpFlds,stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of field list memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return ! bail out

      ! Create to Field Bundle
      cmp%connToFB = ESMF_FieldBundleCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      ! Set the max size of to tmpFields
      call ESMF_StateGet(cmp%extToState, nestedFlag=.true., &
        itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(tmpFlds(itemCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of field list memory failed.", &
        file=__FILE__, &
        rcToReturn=rc)) &
        return ! bail out
      ! Find connected to fields
      connCount = 0
      do i=1, size(cmp%allToFlds)
        call ESMF_StateGet(cmp%extToState, itemName=cmp%allToFlds(i)%stateName, &
          itemType=itemType, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (itemType .eq. ESMF_STATEITEM_FIELD) then
          connCount = connCount + 1
          tmpFlds(connCount) = cmp%allToFlds(i)
          call ESMF_StateGet(cmp%extToState, itemName=cmp%allToFlds(i)%stateName, &
            field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldBundleAdd(cmp%connToFB, fieldList=(/field/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        else
          ! TO FIELD NOT FOUND IN TO STATE
        endif
      enddo
      ! Build connected to fields list
      if (connCount .gt. 0) then
        allocate(cmp%connToFlds(connCount),stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of field list memory failed.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)) return ! bail out
        cmp%connToFlds(1:connCount) = tmpFlds(1:connCount)
      else
        allocate(cmp%connToFlds(0),stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of field list memory failed.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)) return ! bail out
      endif
      deallocate(tmpFlds,stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of field list memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return ! bail out

    end subroutine

    !---------------------------------------------------------------------------

    subroutine med_count_ensMembers(ensState, mbrCount, label, rc)
      type(ESMF_State),intent(in) :: ensState
      integer,intent(out)         :: mbrCount
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
      integer                                 :: undistCount
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
          undistCount = rank - dimCount
          if (undistCount .eq. 1) then
            allocate(undistLBound(undistCount), undistUBound(undistCount))
            call ESMF_ArrayGet(array, undistLBound=undistLBound, &
              undistUBound=undistUBound, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            mbrCount = undistUBound(1) - undistLBound(1) + 1
            deallocate(undistLBound, undistUBound)
          elseif (undistCount .lt. 1) then
            mbrCount = 0
!            call ESMF_LogWrite(trim(label)//" - No ensemble dimension found.", &
!              ESMF_LOGMSG_WARNING, rc=rc)
!            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!              line=__LINE__, file=__FILE__)) return  ! bail out
          else
            mbrCount = -1
            call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
              msg=trim(label)//" - Cannot determine ensemble count for "// &
                  "greater than 1 distributed dimension.", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return ! bail out
          endif
          exit
        endif
      enddo

    end subroutine

    !---------------------------------------------------------------------------

    subroutine createScatterState(ensState, scEnsState, ensCount, label, rc)
      ! Look at all of the fields in state, including in nested states. Create
      ! new field without undistributed dimension and add to new state
      type(ESMF_State), intent(in)                 :: ensState
      type(ESMF_State), allocatable, intent(inout) :: scEnsState(:)
      integer, intent(in)                          :: ensCount
      character(*),intent(in)                      :: label
      integer                                      :: rc
      ! local variables
      integer                                 :: i, j
      integer                                 :: itemCount
      integer                                 :: fieldCount
      character(len=80), allocatable          :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
      type(ESMF_Field)                        :: field
      type(ESMF_Grid)                         :: grid
      type(ESMF_Field)                        :: newfield

      rc = ESMF_SUCCESS

      if (allocated(scEnsState)) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg=trim(label)//" - Scatter ensemble state already allocated.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return ! bail out
      else
        allocate(scEnsState(ensCount))
      endif

      ! realize all the fields in the state (geoms have been transferred)
      do i=1, ensCount
        ! create empty state
        scEnsState(i) = ESMF_StateCreate(rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo

      ! query info about the items in the state
      call ESMF_StateGet(ensState, nestedFlag=.true., itemCount=itemCount, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(itemNameList(itemCount), itemTypeList(itemCount))
      call ESMF_StateGet(ensState, nestedFlag=.true., &
        itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      do j=1, itemCount
        if (itemTypeList(j)==ESMF_STATEITEM_FIELD) then
          ! add field to field bundle
          call ESMF_StateGet(ensState, field=field, &
            itemName=itemNameList(j), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldGet(field, grid=grid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          do i=1, ensCount
            newfield = ESMF_FieldCreate(name=trim(itemNameList(j)), &
              grid=grid, typekind=NLC_TYPEKIND, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            call ESMF_StateAdd(scEnsState(i), fieldList=(/newfield/), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          enddo
        endif
      enddo

      deallocate(itemNameList, itemTypeList)

    end subroutine

    !---------------------------------------------------------------------------

    subroutine med_compute_rh(srcCmpList, dstCmp, srcMember, dstMember, label, &
      conn, mapping, srcMask, dstMask, rc)
      type(med_ext_conn_type),intent(in)    :: srcCmpList(:)
      type(med_ext_conn_type),intent(in)    :: dstCmp
      integer,intent(in)                    :: srcMember
      integer,intent(in)                    :: dstMember
      character(*),intent(in)               :: label
      type(med_int_conn_type),intent(inout) :: conn
      type(fieldRemapFlag),intent(in)       :: mapping
      type(fieldMaskFlag),intent(in)        :: srcMask
      type(fieldMaskFlag),intent(in)        :: dstMask
      integer,intent(out)                   :: rc
      ! local variables
      integer :: l_member
      integer :: i, j, k
      logical :: fieldMatch
      type(ESMF_Field) :: srcFld, dstFld
      integer,target :: maskLND(1) = (/1/)
      integer,target :: maskWTR(1) = (/0/)
      integer,pointer :: srcMaskValues(:)
      integer,pointer :: dstMaskValues(:)
      character(len=24) :: remapStr
      character(len=24) :: srcMaskStr
      character(len=24) :: dstMaskStr
      character(ESMF_MAXSTR) :: msg
      integer :: stat

      rc = ESMF_SUCCESS

      allocate( &
        conn%srcFlds(size(dstCmp%connToFlds)), &
        conn%dstFlds(size(dstCmp%connToFlds)), &
        stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of field list memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return ! bail out
      conn%srcFB = ESMF_FieldBundleCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      conn%dstFB = ESMF_FieldBundleCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      do k=1, size(dstCmp%connToFlds)
        fieldMatch = .false.
        search: do j=1, size(srcCmpList)
                do i=1, size(srcCmpList(j)%connFrFlds)
                  if (NUOPC_FieldDictionaryMatchSyno( &
                    srcCmpList(j)%connFrFlds(i)%standardName, &
                    dstCmp%connToFlds(k)%standardName)) then
                      fieldMatch = .true.
                      exit search
                  endif
                enddo
                enddo search
        if (fieldMatch) then
          conn%srcFlds(k) = srcCmpList(j)%connFrFlds(i)
          conn%dstFlds(k) = dstCmp%connToFlds(k)
          call ESMF_StateGet(srcCmpList(j)%intFrState(srcMember), &
            itemName=srcCmpList(j)%connFrFlds(i)%stateName, &
            field=srcFld, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldBundleAdd(conn%srcFB, fieldList=(/srcFld/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_StateGet(dstCmp%intToState(dstMember), &
            itemName=dstCmp%connToFlds(k)%stateName, &
            field=dstFld, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldBundleAdd(conn%dstFB, fieldList=(/dstFld/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        else
          call ESMF_LogSetError(ESMF_RC_NOT_FOUND, &
            msg="Source field not found for "// &
            trim(dstCmp%connToFlds(k)%standardName), &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return ! bail out
        endif
      enddo

      if (btest(verbosity,16)) then
        remapStr = mapping
        srcMaskStr = srcMask
        dstMaskStr = dstMask
        write (msg,"(A10,3(A,A))") label, " remapping= ",trim(remapStr), &
          " srcMask=", trim(srcMaskStr), " dstMask=", trim(dstMaskStr)
        call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif

      if (mapping .eq. FLD_REMAP_BILINR) then
        if (srcMask .eq. FLD_MASK_NNE) then
          nullify(srcMaskValues)
        elseif (srcMask .eq. FLD_MASK_LND) then
          srcMaskValues=>maskLND
        elseif (srcMask .eq. FLD_MASK_WTR) then
          srcMaskValues=>maskWTR
        else
          call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
            msg="Source mask is not implemented.", &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return
        endif
        if (dstMask .eq. FLD_MASK_NNE) then
          nullify(dstMaskValues)
        elseif (dstMask .eq. FLD_MASK_LND) then
          dstMaskValues=>maskLND
        elseif (dstMask .eq. FLD_MASK_WTR) then
          dstMaskValues=>maskWTR
        else
          call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
            msg="Destintation mask is not implemented.", &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return
        endif
        if (associated(srcMaskValues) .AND. associated(dstMaskValues)) then
          call ESMF_FieldBundleRegridStore(conn%srcFB, conn%dstFB, &
            routehandle=conn%rhBilnr, &
            srcMaskValues=srcMaskValues, &
            dstMaskValues=dstMaskValues, &
            regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
            polemethod=polemethod, &
            unmappedaction=unmappedaction, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        elseif (associated(srcMaskValues)) then
          call ESMF_FieldBundleRegridStore(conn%srcFB, conn%dstFB, &
            routehandle=conn%rhBilnr, &
            srcMaskValues=srcMaskValues, &
            regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
            polemethod=polemethod, &
            unmappedaction=unmappedaction, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        elseif (associated(dstMaskValues)) then
          call ESMF_FieldBundleRegridStore(conn%srcFB, conn%dstFB, &
            routehandle=conn%rhBilnr, &
            dstMaskValues=dstMaskValues, &
            regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
            polemethod=polemethod, &
            unmappedaction=unmappedaction, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        else
          call ESMF_FieldBundleRegridStore(conn%srcFB, conn%dstFB, &
            routehandle=conn%rhBilnr, &
            regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
            polemethod=polemethod, &
            unmappedaction=unmappedaction, &
            rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
      elseif (mapping .eq. FLD_REMAP_REDIST) then
        call ESMF_FieldBundleRedistStore(conn%srcFB, conn%dstFB, &
          routehandle=conn%rhFcopy, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      else
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="Remapping method has not been implemented.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif

    end subroutine

  end subroutine

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
    logical                      :: isCurrent
    logical                      :: allSatisfied
    logical                      :: checkSatisfied
    integer                      :: i
    integer                      :: fieldCount
    type(ESMF_Field),allocatable :: fieldList(:)
    type(ESMF_Clock)             :: mediatorClock
    character(10)                :: instStr
    type(ESMF_Time)              :: currTime
    character(len=40)            :: currTimeString
    character(ESMF_MAXSTR)       :: msg
    integer                      :: stat

    rc = ESMF_SUCCESS

    ! get the component information
!    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_GridCompGet(mediator, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
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

    if (is%wrap%iniType == INIT_MODELS) then
      allSatisfied=.TRUE.

      ! Check toLnd Data
      checkSatisfied = .TRUE.
      call ESMF_FieldBundleGet(is%wrap%toLND(1)%srcFB, &
        fieldCount=fieldCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(fieldList(fieldCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of field list memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return ! bail out
      call ESMF_FieldBundleGet(is%wrap%toLND(1)%srcFB, &
        fieldList=fieldList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      do i=1, fieldCount
        isCurrent = NUOPC_IsAtTime(fieldList(i), currTime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (.NOT.isCurrent) then
          if (verbosity>0) then
            write (msg,"(A,A)")  trim(name), &
              ": toLND DataInitialization-Dependency NOT YET SATISFIED!!!"
            call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          endif
          checkSatisfied = .FALSE.
          exit
        endif
      enddo
      deallocate(fieldList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of field list memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return ! bail out
      if (checkSatisfied) then
        if (verbosity>0) then
          write (msg,"(A,A)")  trim(name), &
            ": toLND DataInitialization-Dependency SATISFIED!!!"
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
        call MediatorRemapToLnd(mediator, rc=rc) ! Remap toLND
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_FieldBundleGet(is%wrap%toLND(1)%dstFB, &
          fieldCount=fieldCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        allocate(fieldList(fieldCount), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of field list memory failed.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)) return ! bail out
        call ESMF_FieldBundleGet(is%wrap%toLND(1)%dstFB, &
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
        if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
          msg="Deallocation of field list memory failed.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)) return ! bail out
      else ! if .NOT. checkSatisfied
        allSatisfied = .FALSE.
      endif

      ! Check toHyd Data
      checkSatisfied = .TRUE.
      call ESMF_FieldBundleGet(is%wrap%toHYD(1)%srcFB, &
        fieldCount=fieldCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(fieldList(fieldCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of field list memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return ! bail out
      call ESMF_FieldBundleGet(is%wrap%toHYD(1)%srcFB, &
        fieldList=fieldList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      do i=1, fieldCount
        isCurrent = NUOPC_IsAtTime(fieldList(i), currTime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (.NOT.isCurrent) then
          if (verbosity>0) then
            write (msg,"(A,A)")  trim(name), &
              ": toHYD DataInitialization-Dependency NOT YET SATISFIED!!!"
            call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          endif
          checkSatisfied = .FALSE.
          exit
        endif
      enddo
      deallocate(fieldList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of field list memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return ! bail out
      if (checkSatisfied) then
        if (verbosity>0) then
          write (msg,"(A,A)")  trim(name), &
            ": toHYD DataInitialization-Dependency SATISFIED!!!"
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
        call MediatorRemapToHyd(mediator, rc=rc) ! Remap toHYD
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_FieldBundleGet(is%wrap%toHYD(1)%dstFB, &
          fieldCount=fieldCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        allocate(fieldList(fieldCount), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of field list memory failed.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)) return ! bail out
        call ESMF_FieldBundleGet(is%wrap%toHYD(1)%dstFB, &
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
        if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
          msg="Deallocation of field list memory failed.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)) return ! bail out
      else ! if .NOT. checkSatisfied
        allSatisfied = .FALSE.
      endif
    elseif (is%wrap%iniType == INIT_DEFAULTS) then
      allSatisfied = .TRUE.
      call MedConn_DataReset(is%wrap%toLND(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call MedConn_DataReset(is%wrap%toHYD(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (verbosity>0) then
        write (msg,"(A,A)")  trim(name), &
          ": Default DataInitialization. Using coldstart values!!!"
        call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
    else
      allSatisfied = .FALSE.
      write (msg,"(A)") is%wrap%iniType
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="Unsupported DataInitialization "//trim(msg), &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return ! bail out
    endif

    if (allSatisfied) then
      ! write src field bundle
      if (btest(diagnostic,16)) then
        do i=1, is%wrap%instCntHyd
          if (is%wrap%multiInstHyd) then
            write(instStr,"(A4,I"//trim(is%wrap%hydStrFmt)//")") "HYD-",i
          else
            instStr = "HYD"
          endif
          call ESMF_FieldBundleWrite(is%wrap%HYD(i)%connFrFB, &
            fileName=trim(is%wrap%dirOutput)//"/diag_"//trim(name)//"_"// &
                     trim(rName)//"_"//trim(instStr)//"_Src_"// &
                     trim(currTimeString)//".nc", &
            singleFile=.true., overwrite=.true., status=ESMF_FILESTATUS_REPLACE, &
            timeslice=1, iofmt=ESMF_IOFMT_NETCDF, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldBundleWrite(is%wrap%HYD(i)%connToFB, &
            fileName=trim(is%wrap%dirOutput)//"/diag_"//trim(name)//"_"// &
                     trim(rName)//"_"//trim(instStr)//"_Dst_"// &
                     trim(currTimeString)//".nc", &
            singleFile=.true., overwrite=.true., status=ESMF_FILESTATUS_REPLACE, &
            timeslice=1, iofmt=ESMF_IOFMT_NETCDF, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo
        do i=1, is%wrap%instCntLnd
          if (is%wrap%multiInstLnd) then
            write(instStr,"(A4,I"//trim(is%wrap%lndStrFmt)//")") "LND-",i
          else
            instStr = "LND"
          endif
          call ESMF_FieldBundleWrite(is%wrap%LND(i)%connFrFB, &
            fileName=trim(is%wrap%dirOutput)//"/diag_"//trim(name)//"_"// &
                     trim(rName)//"_"//trim(instStr)//"_Src_"// &
                     trim(currTimeString)//".nc", &
            singleFile=.true., overwrite=.true., status=ESMF_FILESTATUS_REPLACE, &
            timeslice=1, iofmt=ESMF_IOFMT_NETCDF, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldBundleWrite(is%wrap%LND(i)%connToFB, &
            fileName=trim(is%wrap%dirOutput)//"/diag_"//trim(name)//"_"// &
                     trim(rName)//"_"//trim(instStr)//"_Dst_"// &
                     trim(currTimeString)//".nc", &
            singleFile=.true., overwrite=.true., status=ESMF_FILESTATUS_REPLACE, &
            timeslice=1, iofmt=ESMF_IOFMT_NETCDF, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo
      endif

      ! indicate that data initialization is complete (breaking out of init-loop)
      call NUOPC_CompAttributeSet(mediator, &
        name="InitializeDataComplete", value="true", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif ! endif allSatisfied

  end subroutine

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

    ! get the component information
!    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_GridCompGet(mediator, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Diagnostic", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Verbosity", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
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

    ! get the component information
!    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_GridCompGet(mediator, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Diagnostic", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Verbosity", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
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

    call MediatorRemapToLnd(mediator, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call MediatorRemapToHyd(mediator, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end subroutine

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

    ! get the component information
!    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_GridCompGet(mediator, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Diagnostic", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Verbosity", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
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

    ! write src field bundle
    if (btest(diagnostic,16)) then
      do i=1, is%wrap%instCntLnd
        if (is%wrap%multiInstLnd) then
          write(instStr,"(A4,I"//trim(is%wrap%lndStrFmt)//")") "LND-",i
        else
          instStr = "LND"
        endif
        call ESMF_FieldBundleWrite(is%wrap%LND(i)%connFrFB, &
          fileName=trim(is%wrap%dirOutput)//"/diag_"//trim(name)//"_"// &
                   trim(rName)//"_"//trim(instStr)//"_Src_"// &
                   trim(currTimeString)//".nc", &
          singleFile=.true., overwrite=.true., status=ESMF_FILESTATUS_REPLACE, &
          timeslice=1, iofmt=ESMF_IOFMT_NETCDF, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
    endif

    if (is%wrap%ensMap .eq. EMAP_ENSHYD) then
      call MedConn_ScatterEnsemble(is%wrap%HYD(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! HERE THE MEDIATOR ADVANCES: currTime -> currTime + timeStep
    do i=1, size(is%wrap%toLND)
      if (is%wrap%remapDstLND .eq. FLD_REMAP_BILINR) then
        call ESMF_FieldBundleRegrid(is%wrap%toLND(i)%srcFB, is%wrap%toLND(i)%dstFB, &
          routehandle=is%wrap%toLND(i)%rhBilnr, &
          zeroregion=ESMF_REGION_SELECT, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      elseif (is%wrap%remapDstLND .eq. FLD_REMAP_REDIST) then
        call ESMF_FieldBundleRedist(is%wrap%toLND(i)%srcFB,is%wrap%toLND(i)%dstFB, &
          routehandle=is%wrap%toLND(i)%rhFcopy, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      else
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="Remapping method has not been implemented.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    enddo

!    ! scatter to other instances
!    if (is%wrap%multiInstLnd) then
!      call MedConn_Scatter(is%wrap%LND, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!        line=__LINE__, file=__FILE__)) return  ! bail out
!    endif

    if (is%wrap%ensMap .eq. EMAP_ENSLND) then
      call MedConn_GatherEnsemble(is%wrap%LND(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! write dst field bundle
    if (btest(diagnostic,16)) then
      do i=1, is%wrap%instCntLnd
        if (is%wrap%multiInstLnd) then
          write(instStr,"(A4,I"//trim(is%wrap%lndStrFmt)//")") "LND-",i
        else
          instStr = "LND"
        endif
        call ESMF_FieldBundleWrite(is%wrap%LND(i)%connToFB, &
          fileName=trim(is%wrap%dirOutput)//"/diag_"//trim(name)//"_"// &
                   trim(rName)//"_"//trim(instStr)//"_Dst_"// &
                   trim(currTimeString)//".nc", &
          singleFile=.true., overwrite=.true., status=ESMF_FILESTATUS_REPLACE, &
          timeslice=1, iofmt=ESMF_IOFMT_NETCDF, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
    endif

  end subroutine

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

    ! get the component information
!    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_GridCompGet(mediator, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Diagnostic", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Verbosity", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
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

    do i=1, is%wrap%instCntLnd
#if ESMF_VERSION_MAJOR >= 8
      call NUOPC_SetTimestamp(is%wrap%LND(i)%extToState, mediatorClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
#else
      call NUOPC_UpdateTimestamp(is%wrap%LND(i)%extToState, mediatorClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
#endif
    enddo

  end subroutine

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

    ! get the component information
!    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_GridCompGet(mediator, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Diagnostic", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Verbosity", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
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

    ! write src field bundle
    if (btest(diagnostic,16)) then
      do i=1, is%wrap%instCntHyd
        if (is%wrap%multiInstHyd) then
          write(instStr,"(A4,I"//trim(is%wrap%hydStrFmt)//")") "HYD-",i
        else
          instStr = "HYD"
        endif
        call ESMF_FieldBundleWrite(is%wrap%HYD(i)%connFrFB, &
          fileName=trim(is%wrap%dirOutput)//"/diag_"//trim(name)//"_"// &
                   trim(rName)//"_"//trim(instStr)//"_Src_"// &
                   trim(currTimeString)//".nc", &
          singleFile=.true., overwrite=.true., status=ESMF_FILESTATUS_REPLACE, &
          timeslice=1, iofmt=ESMF_IOFMT_NETCDF, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
    endif

    if (is%wrap%ensMap .eq. EMAP_ENSLND) then
      call MedConn_ScatterEnsemble(is%wrap%LND(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! HERE THE MEDIATOR ADVANCES: currTime -> currTime + timeStep
    do i=1, size(is%wrap%toHYD)
      if (is%wrap%remapDstHYD .eq. FLD_REMAP_BILINR) then
        call ESMF_FieldBundleRegrid(is%wrap%toHYD(i)%srcFB, is%wrap%toHYD(i)%dstFB, &
          routehandle=is%wrap%toHYD(i)%rhBilnr, &
          zeroregion=ESMF_REGION_SELECT, &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      elseif (is%wrap%remapDstHYD .eq. FLD_REMAP_REDIST) then
        call ESMF_FieldBundleRedist(is%wrap%toHYD(i)%srcFB, is%wrap%toHYD(i)%dstFB, &
          routehandle=is%wrap%toHYD(i)%rhFcopy, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      else
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="Remapping method has not been implemented.", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    enddo

!    ! scatter to other instances
!    if (is%wrap%multiInstHyd) then
!      call MedConn_Scatter(is%wrap%HYD, rc=rc)
!      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!        line=__LINE__, file=__FILE__)) return  ! bail out
!    endif

    if (is%wrap%ensMap .eq. EMAP_ENSHYD) then
      call MedConn_GatherEnsemble(is%wrap%HYD(1), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    endif

    ! write src field bundle
    if (btest(diagnostic,16)) then
      do i=1, is%wrap%instCntHyd
        if (is%wrap%multiInstHyd) then
          write(instStr,"(A4,I"//trim(is%wrap%hydStrFmt)//")") "HYD-",i
        else
          instStr = "HYD"
        endif
        call ESMF_FieldBundleWrite(is%wrap%HYD(i)%connToFB, &
          fileName=trim(is%wrap%dirOutput)//"/diag_"//trim(name)//"_"// &
                   trim(rName)//"_"//trim(instStr)//"_Dst_"// &
                   trim(currTimeString)//".nc", &
          singleFile=.true., overwrite=.true., status=ESMF_FILESTATUS_REPLACE, &
          timeslice=1, iofmt=ESMF_IOFMT_NETCDF, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
    endif

  end subroutine

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

    ! get the component information
!    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_GridCompGet(mediator, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Diagnostic", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Verbosity", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
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

    do i=1, is%wrap%instCntHyd
#if ESMF_VERSION_MAJOR >= 8
      call NUOPC_SetTimestamp(is%wrap%HYD(i)%extToState, mediatorClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
#else
      call NUOPC_UpdateTimestamp(is%wrap%HYD(i)%extToState, mediatorClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
#endif
    enddo

  end subroutine

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

    ! get the component information
!    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_GridCompGet(mediator, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Diagnostic", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_AttributeGet(mediator, name="Verbosity", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/0,65535,65536/), rc=rc)
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

    do i=1, size(is%wrap%toHYD)
      deallocate(is%wrap%toHYD(i)%srcFlds, is%wrap%toHYD(i)%dstFlds, &
        stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of mediator internal fields memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) &
        return  ! bail out
    enddo
    do i=1, size(is%wrap%toLND)
      deallocate(is%wrap%toLND(i)%srcFlds, is%wrap%toLND(i)%dstFlds, &
        stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of mediator internal fields memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) &
        return  ! bail out
    enddo

    deallocate(is%wrap%toHyd, is%wrap%toLnd, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of mediator internal connection memory failed.", &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) &
      return  ! bail out

    do i=1, size(is%wrap%HYD)
      deallocate(is%wrap%HYD(i)%intToState, is%wrap%HYD(i)%intFrState, &
        stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of mediator internal state memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) &
        return  ! bail out
      deallocate(is%wrap%HYD(i)%connFrFlds, is%wrap%HYD(i)%connToFlds, &
        stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of mediator fields memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) &
        return  ! bail out
    enddo
    do i=1, size(is%wrap%LND)
      deallocate(is%wrap%LND(i)%intToState, is%wrap%LND(i)%intFrState, &
        stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of mediator internal state memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) &
        return  ! bail out
      deallocate(is%wrap%LND(i)%connFrFlds, is%wrap%LND(i)%connToFlds, &
        stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg="Deallocation of mediator fields memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) &
        return  ! bail out
    enddo

    ! deallocate component instances
    deallocate(is%wrap%LND, is%wrap%HYD, stat=stat )
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of component instances memory failed.", &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) &
      return  ! bail out

    ! deallocate internal state
    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) &
      return  ! bail out

  end subroutine

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

  end subroutine

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
    real(NLC_KIND), pointer            :: ensFarrayPtr(:,:,:)
    real(NLC_KIND), pointer            :: farrayPtr(:,:)

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
        call ESMF_FieldGet(ensField, farrayPtr=ensFarrayPtr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        do j=1, size(extConn%intFrState)
          call ESMF_StateGet(extConn%intFrState(j), field=field, &
            itemName=itemNameList(i), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldGet(field, farrayPtr=farrayPtr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          farrayPtr(:,:) = ensFarrayPtr(:,:,j)
        enddo
      endif
    enddo

    deallocate(itemNameList, itemTypeList)

  end subroutine

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
    real(NLC_KIND), pointer            :: ensFarrayPtr(:,:,:)
    real(NLC_KIND), pointer            :: farrayPtr(:,:)

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
        call ESMF_FieldGet(ensField, farrayPtr=ensFarrayPtr, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        do j=1, size(extComp%intToState)
          call ESMF_StateGet(extComp%intToState(j), field=field, &
            itemName=itemNameList(i), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldGet(field, farrayPtr=farrayPtr, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          ensFarrayPtr(:,:,j) = farrayPtr(:,:)
        enddo
      endif
    enddo

    deallocate(itemNameList, itemTypeList)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine MedConn_DataReset(toComp, resetValue, rc)
    type(med_int_conn_type) :: toComp
    real(ESMF_KIND_R8), intent(in), optional :: resetValue
    integer, intent(out) :: rc
    ! local variables
    integer :: i
    type(ESMF_Field) :: field

    rc = ESMF_SUCCESS

    if (present(resetValue)) then
      do i=1, size(toComp%srcFlds)
        call ESMF_FieldBundleGet(toComp%srcFB, toComp%srcFlds(i)%stateName, &
          field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_FieldFill(field, dataFillScheme="const", &
          const1=resetValue, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo

      do i=1, size(toComp%dstFlds)
        call ESMF_FieldBundleGet(toComp%dstFB, toComp%dstFlds(i)%stateName, &
          field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_FieldFill(field, dataFillScheme="const", &
          const1=resetValue, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
    else
      do i=1, size(toComp%srcFlds)
        call ESMF_FieldBundleGet(toComp%srcFB, toComp%srcFlds(i)%stateName, &
          field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_FieldFill(field, dataFillScheme="const", &
          const1=REAL(toComp%srcFlds(i)%fillValue,ESMF_KIND_R8), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
      do i=1, size(toComp%dstFlds)
        call ESMF_FieldBundleGet(toComp%dstFB, toComp%dstFlds(i)%stateName, &
          field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_FieldFill(field, dataFillScheme="const", &
          const1=REAL(toComp%dstFlds(i)%fillValue,ESMF_KIND_R8), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      enddo
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine MedState_LogWrite(state,label,rc)
    type(ESMF_State),intent(in)  :: state
    character(len=*),intent(in)  :: label
    integer,intent(out)          :: rc
    ! local variables
    integer                               :: stat
    integer                               :: itemCount
    character(len=64), allocatable        :: itemNameList(:)
    integer                               :: iIndex
    character(ESMF_MAXSTR)                :: msg

    rc = ESMF_SUCCESS

    call ESMF_StateGet(state, nestedFlag=.true., &
      itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    allocate(itemNameList(itemCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=trim(label)//" Allocation of item list memory failed.", &
      file=__FILE__, &
      rcToReturn=rc)) &
      return ! bail out

    call ESMF_StateGet(state, nestedFlag=.true., &
      itemNameList=itemNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (itemCount .gt. 0) then
      do iIndex=1, itemCount
        write (msg,"(A10,A1,I2.2,A2,A20)") label, &
          "(",iIndex,") ", itemNameList(iIndex)
          call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
      enddo
      deallocate(itemNameList,stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=trim(label)//" Deallocation of item list memory failed.", &
        file=__FILE__, &
        rcToReturn=rc)) &
        return ! bail out
    else
      write (msg,"(A10,A16)") label," state is empty."
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine MedDistGrid_LogWrite(state,label,rc)
    type(ESMF_State),intent(in)  :: state
    character(len=*),intent(in)  :: label
    integer,intent(out)          :: rc
    ! local variables
    integer                               :: stat
    integer                               :: itemCount
    character(len=64),allocatable         :: itemNameList(:)
    type(ESMF_StateItem_Flag),allocatable :: itemTypeList(:)
    integer                               :: iIndex
    character(len=40)                     :: indexStr
    character(ESMF_MAXSTR)                :: msg
    logical                               :: fieldFound
    type(ESMF_Field)                      :: field
    type(ESMF_Grid)                       :: grid
    type(ESMF_DistGrid)                   :: distGrid
    integer                               :: deCount
    integer                               :: dimCount
    integer                               :: maxIndex
    character(len=16)                     :: lenDe
    character(len=16)                     :: lenDim
    character(len=16)                     :: lenIndex
    integer,allocatable                   :: minIndexPDe(:,:)
    integer,allocatable                   :: maxIndexPDe(:,:)
    integer                               :: deIndex
    integer                               :: dimIndex

    rc = ESMF_SUCCESS
    fieldFound = .false.

    call ESMF_StateGet(state, nestedFlag=.true., &
      itemCount=itemCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    allocate(itemNameList(itemCount),itemTypeList(itemCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=trim(label)//" Allocation of item list memory failed.", &
      file=__FILE__, &
      rcToReturn=rc)) &
      return ! bail out

    call ESMF_StateGet(state, nestedFlag=.true., &
      itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (itemCount .gt. 0) then
      do iIndex=1, itemCount
        if (itemTypeList(iIndex) .eq. ESMF_STATEITEM_FIELD) then
          fieldFound=.true.
          call ESMF_StateGet(state, itemName=itemNameList(iIndex), &
            field=field, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldGet(field, grid=grid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_GridGet(grid, distgrid=distGrid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
      enddo
      deallocate(itemNameList,itemTypeList,stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=trim(label)//" Deallocation of item list memory failed.", &
        file=__FILE__, &
        rcToReturn=rc)) &
        return ! bail out
    else
      write (msg,"(A10,A16)") label," state is empty."
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
    endif

    if (fieldFound) then
      call ESMF_DistGridGet(distgrid, dimCount=dimCount, &
        deCount=deCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(minIndexPDe(dimCount, deCount), &
        maxIndexPDe(dimCount, deCount),stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg=trim(label)//" Allocation of index list memory failed.", &
        file=__FILE__, &
        rcToReturn=rc)) &
        return ! bail out
      call ESMF_DistGridGet(distGrid, minIndexPDe=minIndexPDe, &
         maxIndexPDe=maxIndexPDe, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      maxIndex = MAXVAL(maxIndexPDe)
      write (lenDe,"(I16)") deCount
      write (lenDim,"(I16)") dimCount
      write (lenIndex,"(I16)") maxIndex
      write (lenDe,"(A1,I0,A1,I0)") "I",LEN_TRIM(ADJUSTL(lenDe)),".", &
        LEN_TRIM(ADJUSTL(lenDe))
      write (lenDim,"(A1,I0,A1,I0)") "I",LEN_TRIM(ADJUSTL(lenDim)),".", &
        LEN_TRIM(ADJUSTL(lenDim))
      write (lenIndex,"(A1,I0,A1,I0)") "I",LEN_TRIM(ADJUSTL(lenIndex)),".", &
        LEN_TRIM(ADJUSTL(lenIndex))
      write (msg,"(A10,A21,"//trim(lenDim)//")") label, &
        " distgrid dimensions ",dimCount
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
      write (msg,"(A10,A18,"//trim(lenDe)//")") label, &
        " distgrid decount ",deCount
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
      do deIndex=1,deCount
        write (indexStr,"(2(A1,"//trim(lenIndex)//"))") "(", &
          minIndexPDe(1,deIndex), ":", maxIndexPDe(1,deIndex)
        do dimIndex=2,dimCount
          if (LEN_TRIM(indexStr) .le. 30) then
            write (indexStr,"(A,2(A1,"//trim(lenIndex)//"))") trim(indexStr), &
              ",", minIndexPDe(dimIndex,deIndex), &
              ":", maxIndexPDe(dimIndex,deIndex)
          else
            write (indexStr,"(A,A3)") trim(indexStr),"..."
            exit
          endif
        enddo
        write (indexStr,"(A,A1)") trim(indexStr),")"
        write (msg,"(A10,A15,"//trim(lenDe)//",A2,A)") label, &
          " distgrid DE(",deIndex,") = ",trim(indexStr)
        call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
      enddo
      deallocate(minIndexPDe, maxIndexPDe,stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=trim(label)//" Deallocation of index list memory failed.", &
        file=__FILE__, &
        rcToReturn=rc)) &
        return ! bail out
    else
      write (msg,"(A10,A20)") label," no fields in state."
      call ESMF_LogWrite(msg, ESMF_LOGMSG_INFO)
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  function dataIniType_eq(type1, type2)
    logical dataIniType_eq
    type(dataIniType), intent(in) :: type1, type2
    dataIniType_eq = (type1%initype == type2%initype)
  end function

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
  end subroutine

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
  end subroutine

  !-----------------------------------------------------------------------------

  function ensMapType_eq(type1, type2)
    logical ensMapType_eq
    type(ensMapType), intent(in) :: type1, type2
    ensMapType_eq = (type1%maptype == type2%maptype)
  end function

  !-----------------------------------------------------------------------------

  subroutine ensMapType_tostring(string, tval)
    character(len=*), intent(out) :: string
    type(ensMapType), intent(in) :: tval
    if (tval == EMAP_ERR) then
      string = 'EMAP_ERR'
    elseif (tval == EMAP_NOENSM) then
      string = 'EMAP_NOENSM'
    elseif (tval == EMAP_ENSLND) then
      string = 'EMAP_ENSLND'
    elseif (tval == EMAP_ENSHYD) then
      string = 'EMAP_ENSHYD'
    else
      string = 'EMAP_ERR'
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ensMapType_frstring(tval, string)
    type(ensMapType), intent(out) :: tval
    character(len=*), intent(in)   :: string
    if (string .eq. 'EMAP_NOENSM') then
      tval = EMAP_NOENSM
    elseif (string .eq.'EMAP_ENSLND') then
      tval = EMAP_ENSLND
    elseif (string .eq.'EMAP_ENSHYD') then
      tval = EMAP_ENSHYD
    else
      tval = EMAP_ERR
    endif
  end subroutine

  !-----------------------------------------------------------------------------

end module
