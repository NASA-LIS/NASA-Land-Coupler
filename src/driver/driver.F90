module ESM

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driver_routine_SS             => SetServices, &
    driver_label_SetModelServices => label_SetModelServices, &
    driver_label_SetRunSequence => label_SetRunSequence
  
  use LIS_NUOPC, only: lndSS => SetServices
  use WRFHydro_NUOPC, only: hydSS => SetServices
  use Mediator, only: medSS => SetServices
  
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
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetModelServices,&
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
 
    call NUOPC_CompSpecialize(driver, specLabel=driver_label_SetRunSequence, &
      specRoutine=SetRunSequence, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
       
   ! create, open and set the config
     config = ESMF_ConfigCreate(rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     	 line=__LINE__, &
      	 file=__FILE__)) &
      	 return  ! bail out
     call ESMF_ConfigLoadFile(config, "lishydro.runconfig", rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      	line=__LINE__, &
      	file=__FILE__)) &
      	return  ! bail out
     call ESMF_GridCompSet(driver, config=config, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      	line=__LINE__, &
      	file=__FILE__)) &
      	return  ! bail out
    
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
    logical                       :: enabledLnd, enabledHyd
    integer, allocatable          :: petList(:)
    integer                       :: dt 
    
    rc = ESMF_SUCCESS
    
    ! read free format driver attributes
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    attrFF = NUOPC_FreeFormatCreate(config, label="driverAttributes::", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeIngest(driver, attrFF, addFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call isComponentEnabled(config, "lnd", isEnabled=enabledLnd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out  

    if (enabledLnd) then
#if 1
       print *, "LND enabled"
#endif

       ! get PET lists from config
       call getPetListFromConfig(config, "pets_lnd:", petList, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out  
       
       ! SetServices for LND
       if (allocated(petList)) then
#if 1
          print *, "LND petList = ", petList
#endif
          call NUOPC_DriverAddComp(driver, "LND", lndSS, petList=petList, comp=child, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, &
               file=__FILE__)) &
               return              ! bail out  
          deallocate(petList)
       else
          call NUOPC_DriverAddComp(driver, "LND", lndSS, comp=child, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, &
               file=__FILE__)) &
               return                ! bail out  
       endif
       call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
       
       ! read LND attributes from config file into FreeFormat
       attrFF = NUOPC_FreeFormatCreate(config, label="lndAttributes::", &
            relaxedflag=.true., rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
       call NUOPC_CompAttributeIngest(child, attrFF, addFlag=.true., rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
       call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
    endif ! enabledLnd

    call isComponentEnabled(config, "hyd", isEnabled=enabledHyd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out  

    if (enabledHyd) then
#if 1
       print *, "HYD enabled"
#endif
    
       ! get PET lists from config
       call getPetListFromConfig(config, "pets_hyd:", petList, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out  
       ! SetServices for HYD
       if (allocated(petList)) then
#if 1
          print *, "HYD petList = ", petList
#endif
          call NUOPC_DriverAddComp(driver, "HYD", hydSS, petList=petList, comp=child, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, &
               file=__FILE__)) &
               return  ! bail out  
          deallocate(petList)
       else
          call NUOPC_DriverAddComp(driver, "HYD", hydSS, comp=child, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
               line=__LINE__, &
               file=__FILE__)) &
               return  ! bail out  
       endif
       call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
       ! read HYD attributes from config file into FreeFormat
       attrFF = NUOPC_FreeFormatCreate(config, label="hydAttributes::", &
            relaxedflag=.true., rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
       call NUOPC_CompAttributeIngest(child, attrFF, addFlag=.true., rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
       call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
       
    endif !enabledHyd

    ! add Mediator component
    call NUOPC_DriverAddComp(driver, "MED", medSS, comp=child, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out  
    

    ! read clock set up from config
    call getTimeFromConfig(config, "start_time:", startTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    
    call getTimeFromConfig(config, "stop_time:", stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call ESMF_ConfigGetAttribute(config, dt, label="time_step:", & 
         default=-1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    if (dt == -1) then
       call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="time_step not set in run config", &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)
       return    
    endif
    
    call ESMF_TimeIntervalSet(timeStep, s=dt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    
    internalClock = ESMF_ClockCreate(name="Application Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    
    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    
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
      
#if 1
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
    	 default="yes", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    
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
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (isPresent) then
       call ESMF_ConfigGetAttribute(config, minPet, default=-1, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
       call ESMF_ConfigGetAttribute(config, maxPet, default=-2, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
       if (minPet <= maxPet) then
          allocate(petList(maxPet-minPet+1), stat=rc)
          if (ESMF_LogFoundAllocError(statusToCheck=rc, &
               msg="Could not allocate petList", &
               line=__LINE__, &
               file=__FILE__)) &
               return  ! bail out
          do i=1, maxPet-minPet+1
             petList(i) = minPet+i-1
          enddo
       else
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
               msg=trim(label)//" min must be <= max", &
               line=__LINE__, &
               file=__FILE__, &
               rcToReturn=rc)
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
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    if (.not. isPresent) then
       call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg=trim(label)//" is not present in run config file", &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)
       return
    endif
    call ESMF_ConfigGetAttribute(config, yy, default=-1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    call ESMF_ConfigGetAttribute(config, mm, default=-1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    call ESMF_ConfigGetAttribute(config, dd, default=-1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    call ESMF_ConfigGetAttribute(config, h, default=-1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    call ESMF_ConfigGetAttribute(config, m, default=-1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

    call ESMF_TimeSet(tm, yy=yy, mm=mm, dd=dd, h=h, m=m, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out

  end subroutine getTimeFromConfig


end module
