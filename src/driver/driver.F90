#include "settings.h"

module ESM

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driver_routine_SS             => SetServices, &
    driver_label_SetModelServices => label_SetModelServices, &
    driver_label_SetRunSequence   => label_SetRunSequence, &
    driver_label_ModifyCplLists   => label_ModifyCplLists

  use LIS_NUOPC, only: lndSS => SetServices
  use WRFHydro_NUOPC, only: hydSS => SetServices
  use Mediator, only: medSS => SetServices
  use Fields

  use NUOPC_Connector, only: cplSS => SetServices

  implicit none

  private

  public SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! locals
    type(ESMF_Config) :: config

    rc = ESMF_SUCCESS

    ! NUOPC_Driver registers the generic methods
    call NUOPC_CompDerive(driver, driver_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetModelServices,&
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetRunSequence, &
      specRoutine=SetRunSequence, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call NUOPC_CompSpecialize(driver, specLabel=driver_label_ModifyCplLists, &
      specRoutine=ModifyCplLists, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! create, open and set the config
    config = ESMF_ConfigCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_ConfigLoadFile(config, filename=LISHYDRO_CONFIG, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_GridCompSet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    integer                       :: localrc
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: connector
    type(ESMF_Config)             :: config
    type(NUOPC_FreeFormat)        :: attrFF
    logical                       :: enabledLnd, enabledHyd, enabledMed
    integer, allocatable          :: petList(:)
    integer                       :: dt

    rc = ESMF_SUCCESS

    ! read free format driver attributes
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    attrFF = NUOPC_FreeFormatCreate(config, label="driverAttributes::", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompAttributeIngest(driver, attrFF, addFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call isComponentEnabled(config, "lnd", isEnabled=enabledLnd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (enabledLnd) then

      ! get PET lists from config
      call getPetListFromConfig(config, "pets_lnd:", petList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      ! SetServices for LND
      if (allocated(petList)) then
        call NUOPC_DriverAddComp(driver, "LND", lndSS, petList=petList, comp=child, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        deallocate(petList)
      else
        call NUOPC_DriverAddComp(driver, "LND", lndSS, comp=child, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      call NUOPC_CompAttributeSet(child, name="Verbosity", value="0", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      ! read LND attributes from config file into FreeFormat
      attrFF = NUOPC_FreeFormatCreate(config, label="lndAttributes::", &
        relaxedflag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call NUOPC_CompAttributeIngest(child, attrFF, addFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

    endif ! enabledLnd

    call isComponentEnabled(config, "hyd", isEnabled=enabledHyd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (enabledHyd) then

      ! get PET lists from config
      call getPetListFromConfig(config, "pets_hyd:", petList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      ! SetServices for HYD
      if (allocated(petList)) then
        call NUOPC_DriverAddComp(driver, "HYD", hydSS, petList=petList, comp=child, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        deallocate(petList)
      else
        call NUOPC_DriverAddComp(driver, "HYD", hydSS, comp=child, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      call NUOPC_CompAttributeSet(child, name="Verbosity", value="0", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      ! read HYD attributes from config file into FreeFormat
      attrFF = NUOPC_FreeFormatCreate(config, label="hydAttributes::", &
        relaxedflag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call NUOPC_CompAttributeIngest(child, attrFF, addFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

    endif !enabledHyd

    call isComponentEnabled(config, "med", isEnabled=enabledMed, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (enabledMed) then

      ! get PET lists from config
      call getPetListFromConfig(config, "pets_med:", petList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      ! SetServices for MED
      if (allocated(petList)) then
        call NUOPC_DriverAddComp(driver, "MED", medSS, petList=petList, comp=child, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        deallocate(petList)
      else
        call NUOPC_DriverAddComp(driver, "MED", medSS, comp=child, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif
      call NUOPC_CompAttributeSet(child, name="Verbosity", value="0", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      ! read MED attributes from config file into FreeFormat
      attrFF = NUOPC_FreeFormatCreate(config, label="medAttributes::", &
        relaxedflag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call NUOPC_CompAttributeIngest(child, attrFF, addFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

    endif !enabledMed

    ! read clock set up from config
    call getTimeFromConfig(config, "start_time:", startTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call getTimeFromConfig(config, "stop_time:", stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_ConfigGetAttribute(config, dt, label="time_step:", &
      default=-1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (dt == -1) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="time_step not set in run config", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

    call ESMF_TimeIntervalSet(timeStep, s=dt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    internalClock = ESMF_ClockCreate(name="Application Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetRunSequence(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    integer                       :: localrc
    character(ESMF_MAXSTR)        :: name
    type(ESMF_Config)             :: config
    type(NUOPC_FreeFormat)        :: runSeqFF
    type(ESMF_CplComp), pointer   :: connectorList(:)
    integer                       :: i
    type(NUOPC_FreeFormat)        :: attrFF

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! read free format run sequence from config
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    runSeqFF = NUOPC_FreeFormatCreate(config, label="runSeq::", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

#if 0
    call NUOPC_FreeFormatPrint(runSeqFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#endif

    ! ingest FreeFormat run sequence
    call NUOPC_DriverIngestRunSequence(driver, runSeqFF, &
      autoAddConnectors=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

#if 0
    ! Diagnostic output
    call NUOPC_DriverPrint(driver, orderflag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#endif

    ! clean-up
    call NUOPC_FreeFormatDestroy(runSeqFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    nullify(connectorList)
    call NUOPC_DriverGetComp(driver, compList=connectorList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    do i=1, size(connectorList)
      call NUOPC_CompAttributeSet(connectorList(i), name="Verbosity", &
        value="0", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      ! read CON attributes from config file into FreeFormat
      attrFF = NUOPC_FreeFormatCreate(config, label="connectorAttributes::", &
        relaxedflag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call NUOPC_CompAttributeIngest(connectorList(i), attrFF, &
        addFlag=.true., rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    enddo

    deallocate(connectorList)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine isComponentEnabled(config, label, isEnabled, rc)
    type(ESMF_Config), intent(inout) :: config
    character(len=*), intent(in)     :: label
    logical, intent(out)             :: isEnabled
    integer, intent(out)             :: rc

    ! local
    logical :: isPresent
    character(len=10) :: value

    isEnabled = .true.

    call ESMF_ConfigGetAttribute(config, value, label=label//":", &
      default="no", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    isEnabled = (trim(value) == "yes")

  end subroutine isComponentEnabled

  subroutine getPetListFromConfig(config, label, petList, rc)
    type(ESMF_Config), intent(inout) :: config
    character(len=*), intent(in)  :: label
    integer, allocatable          :: petList(:)
    integer, intent(out)          :: rc

    ! local
    logical :: isPresent
    integer :: i, minPet, maxPet

    call ESMF_ConfigFindLabel(config, trim(label), &
      isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isPresent) then
      call ESMF_ConfigGetAttribute(config, minPet, default=-1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_ConfigGetAttribute(config, maxPet, default=-2, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (minPet <= maxPet) then
        allocate(petList(maxPet-minPet+1), stat=rc)
        if (ESMF_LogFoundAllocError(statusToCheck=rc, &
          msg="Could not allocate petList", &
          line=__LINE__, file=__FILE__)) return  ! bail out
        do i=1, maxPet-minPet+1
          petList(i) = minPet+i-1
        enddo
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg=trim(label)//" min must be <= max", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    endif

  end subroutine getPetListFromConfig

  subroutine getTimeFromConfig(config, label, tm, rc)
    type(ESMF_Config), intent(inout) :: config
    character(len=*), intent(in)     :: label
    type(ESMF_Time), intent(inout)   :: tm
    integer, intent(out)             :: rc

    ! local
    logical :: isPresent
    integer :: yy, mm, dd, h, m

    call ESMF_ConfigFindLabel(config, trim(label), &
      isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (.not. isPresent) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=trim(label)//" is not present in run config file", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif
    call ESMF_ConfigGetAttribute(config, yy, default=-1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_ConfigGetAttribute(config, mm, default=-1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_ConfigGetAttribute(config, dd, default=-1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_ConfigGetAttribute(config, h, default=-1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_ConfigGetAttribute(config, m, default=-1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_TimeSet(tm, yy=yy, mm=mm, dd=dd, h=h, m=m, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end subroutine getTimeFromConfig

  !-----------------------------------------------------------------------------

  subroutine ModifyCplLists(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_CplComp), pointer     :: connectorList(:)
    integer                         :: i, j, cplListSize
    character(len=256)              :: msg
    character(len=32)               :: connectorName
    character(len=160), allocatable :: cplList(:)
    integer                         :: splitPos
    type(med_fld_type), pointer     :: srcFlds(:) => null()
    type(med_fld_type), pointer     :: dstFlds(:) => null()
    type(fieldRemapFlag)            :: fieldDstRemap = FLD_REMAP_REDIST
    type(fieldMaskFlag)             :: fieldSrcMask  = FLD_MASK_NNE
    type(fieldMaskFlag)             :: fieldDstMask  = FLD_MASK_NNE

    rc = ESMF_SUCCESS

    nullify(connectorList)
    call NUOPC_DriverGetComp(driver, compList=connectorList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    do i=1, size(connectorList)
      nullify(dstFlds)
      nullify(srcFlds)
      ! query the name for connector i
      call ESMF_CplCompGet(connectorList(i), name=connectorName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (connectorName .eq. "LND-TO-HYD") then
        srcFlds=>fldsFrLnd
        dstFlds=>fldsToHyd
      elseif (connectorName .eq. "HYD-TO-LND") then
        srcFlds=>fldsFrHyd
        dstFlds=>fldsToLnd
      elseif (connectorName .eq. "LND-TO-MED") then
        srcFlds=>fldsFrLnd
      elseif (connectorName .eq. "MED-TO-LND") then
        dstFlds=>fldsToLnd
      elseif (connectorName .eq. "HYD-TO-MED") then
        srcFlds=>fldsFrHyd
      elseif (connectorName .eq. "MED-TO-HYD") then
        dstFlds=>fldsToHyd
      else
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="ModifyCplList for "//trim(connectorName)//" not implemented", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
      ! query the cplList for connector i
      call NUOPC_CompAttributeGet(connectorList(i), name="CplList", &
        itemCount=cplListSize, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (cplListSize>0) then
        allocate(cplList(cplListSize))
        call NUOPC_CompAttributeGet(connectorList(i), name="CplList", &
          valueList=cplList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        ! go through all of the entries in the cplList and add options
        do j=1, cplListSize
          splitPos = index(cplList(j),":")
          if (splitPos .eq. 0) splitPos = LEN(cplList(j))
          if (associated(srcFlds)) then
            call field_find_standardname(fieldList=srcFlds, &
              standardName=cplList(j)(1:splitPos), &
              mask=fieldSrcMask, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          else
            fieldSrcMask=FLD_MASK_NNE
          endif
          if (associated(dstFlds)) then
            call field_find_standardname(fieldList=dstFlds, &
              standardName=cplList(j)(1:splitPos), &
              mapping=fieldDstRemap, mask=fieldDstMask, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          else
            fieldDstRemap=FLD_REMAP_BILINR
            fieldDstMask=FLD_MASK_NNE
          endif
          if (fieldDstRemap .eq. FLD_REMAP_REDIST) then
            cplList(j) = trim(cplList(j))//":remapmethod=redist"
          elseif (fieldDstRemap .eq. FLD_REMAP_BILINR) then
            cplList(j) = trim(cplList(j))//":remapmethod=bilinear"
          elseif (fieldDstRemap .eq. FLD_REMAP_CONSRV) then
            cplList(j) = trim(cplList(j))//":remapmethod=conserve"
          else
            call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
              msg="Field remap type is unknown.", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
          endif
          if (fieldSrcMask == FLD_MASK_NNE) then
            ! No Mask
          elseif (fieldSrcMask == FLD_MASK_LND) then
            cplList(j) = trim(cplList(j))//":srcmaskvalues=1"
          elseif (fieldSrcMask == FLD_MASK_WTR) then
            cplList(j) = trim(cplList(j))//":srcmaskvalues=0"
          else
            call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
              msg="Field source mask value is unknown.", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
          endif
          if (fieldDstMask == FLD_MASK_NNE) then
            ! No Mask
          elseif (fieldDstMask == FLD_MASK_LND) then
            cplList(j) = trim(cplList(j))//":dstmaskvalues=1"
          elseif (fieldDstMask == FLD_MASK_WTR) then
            cplList(j) = trim(cplList(j))//":dstmaskvalues=0"
          else
            call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
              msg="Field destination mask value is unknown.", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
          endif
#if 1
          write (msg,"(A,A,A,I0,A,A)") trim(connectorName),": ", &
            "CplList(",j,")=",trim(cplList(j))
          call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
#endif
        enddo

        ! store the modified cplList in CplList attribute of connector i
        call NUOPC_CompAttributeSet(connectorList(i), &
          name="CplList", valueList=cplList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        deallocate(cplList)
      endif
    enddo

    deallocate(connectorList)

  end subroutine

end module
