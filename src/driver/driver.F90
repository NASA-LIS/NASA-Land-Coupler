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

#ifdef NUOPCCAP_LIS
  use NUOPCCAP_LIS, only: lis_ss => SetServices
#endif
#ifdef NUOPCCAP_WRFHYDRO
  use NUOPCCAP_WRFHYDRO, only: wrfhydro_ss => SetServices
#endif
  use Mediator, only: medSS => SetServices
  use Fields
  use Flags

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
    call ESMF_ConfigLoadFile(config, filename=NLC_CONFIG, rc=rc)
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
    integer                       :: i
    character(6)                  :: maxStr
    character(3)                  :: instStrFmt
    character(10)                 :: compName
    character(40)                 :: model
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: connector
    type(ESMF_Config)             :: config
    type(NUOPC_FreeFormat)        :: attrFF
    logical                       :: enabledLnd, enabledHyd, enabledGwr
    logical                       :: enabledMed
    integer, allocatable          :: petList(:)
    logical                       :: multiInst
    integer                       :: instCnt
    integer                       :: dt
    logical                       :: isPresent

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

    ! ###########
    ! Land Models
    ! ###########

    call isComponentEnabled(config, "lnd", isEnabled=enabledLnd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (enabledLnd) then

      ! get model from config
      call getModelFromConfig(config, "lnd_model:", model=model, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      ! get PET lists from config
      call getPetListFromConfig(config, "pets_lnd:", petList=petList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      ! SetServices for LND
      select case (model)
        case ('lis','default')
#ifdef NUOPCCAP_LIS
          if (allocated(petList)) then
            call NUOPC_DriverAddComp(driver, "LND", lis_ss, petList=petList, &
              comp=child, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            deallocate(petList)
          else
            call NUOPC_DriverAddComp(driver, "LND", lis_ss, comp=child, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
          endif
#else
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="LIS model missing from build", &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return
#endif
        case default
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="invalid lnd_model: "//trim(model), &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return
      endselect

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

    ! ################
    ! Hydrology Models
    ! ################

    call isComponentEnabled(config, "hyd", isEnabled=enabledHyd, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (enabledHyd) then

      ! get model from config
      call getModelFromConfig(config, "hyd_model:", model=model, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      ! get instance count from config
      call ESMF_ConfigGetAttribute(config, instCnt, &
        label="instance_count_hyd:", default=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (instCnt.gt.1) then
        multiInst = .true.  ! default multi instance .true.
      else
        multiInst = .false. ! default multi instance .false.
      endif
      call ESMF_ConfigGetAttribute(config, multiInst, &
        label="multi_instance_hyd:", default=multiInst, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if ((.NOT.multiInst) .AND. (instCnt.gt.1)) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="multi_instance_hyd must be true for instance count gt 1", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      elseif (instCnt.gt.999999) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="instance_count_hyd must be less than 999999", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif

      if (multiInst) then

        ! generate the instance string format descriptor
        write(maxStr,"(I0)") instCnt
        write(instStrFmt,"(I0,A1,I0)") len_trim(maxStr),".",len_trim(maxStr)

        ! check for overlapping PETs
        call checkPetListFromConfig(config, "pets_hyd:", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        do i=1, instCnt

          write(compName,"(A4,I"//trim(instStrFmt)//")") "HYD-",i

          ! get instance PET lists from config
          call getPetListFromConfig(config, "pets_hyd:", instance=i, &
            petList=petList, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

          ! SetServices for HYD
          select case (model)
            case ('wrfhydro','default')
#ifdef NUOPCCAP_WRFHYDRO
              if (allocated(petList)) then
                call NUOPC_DriverAddComp(driver, compName, wrfhydro_ss, &
                  petList=petList, comp=child, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__)) return  ! bail out
                deallocate(petList)
              else
                call NUOPC_DriverAddComp(driver, compName, wrfhydro_ss, &
                  comp=child, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__)) return  ! bail out
              endif
#else
              call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
                msg="WRFHYDRO model missing from build", &
                line=__LINE__, file=__FILE__, rcToReturn=rc)
              return
#endif
            case default
              call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
                msg="invalid hyd_model: "//trim(model), &
                line=__LINE__, file=__FILE__, rcToReturn=rc)
              return
          endselect

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
          call NUOPC_CompAttributeAdd(child, &
            attrList=(/"multi_instance_hyd"/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_AttributeSet(child, name="multi_instance_hyd", &
            value="true", convention="NUOPC", purpose="Instance", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo

      else

        ! get PET lists from config
        call getPetListFromConfig(config, "pets_hyd:", petList=petList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        ! SetServices for HYD
        select case (model)
          case ('wrfhydro','default')
#ifdef NUOPCCAP_WRFHYDRO
            if (allocated(petList)) then
              call NUOPC_DriverAddComp(driver, "HYD", wrfhydro_ss, &
                petList=petList, comp=child, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              deallocate(petList)
            else
              call NUOPC_DriverAddComp(driver, "HYD", wrfhydro_ss, &
                comp=child, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            endif
#else
            call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
              msg="WRFHYDRO model missing from build", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          case default
            call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
              msg="invalid hyd_model: "//trim(model), &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
        endselect

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

      endif ! multiple instances

    endif !enabledHyd

    ! ##################
    ! Groundwater Models
    ! ##################

    call isComponentEnabled(config, "gwr", isEnabled=enabledGwr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (enabledGwr) then

      ! get model from config
      call getModelFromConfig(config, "gwr_model:", model=model, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      ! get instance count from config
      call ESMF_ConfigGetAttribute(config, instCnt, &
        label="instance_count_gwr:", default=1, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (instCnt.gt.1) then
        multiInst = .true.  ! default multi instance .true.
      else
        multiInst = .false. ! default multi instance .false.
      endif
      call ESMF_ConfigGetAttribute(config, multiInst, &
        label="multi_instance_gwr:", default=multiInst, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if ((.NOT.multiInst) .AND. (instCnt.gt.1)) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="multi_instance_gwr must be true for instance count gt 1", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      elseif (instCnt.gt.999999) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="instance_count_gwr must be less than 999999", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif

      if (multiInst) then

        ! generate the instance string format descriptor
        write(maxStr,"(I0)") instCnt
        write(instStrFmt,"(I0,A1,I0)") len_trim(maxStr),".",len_trim(maxStr)

        ! check for overlapping PETs
        call checkPetListFromConfig(config, "pets_gwr:", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        do i=1, instCnt

          write(compName,"(A4,I"//trim(instStrFmt)//")") "GWR-",i

          ! get instance PET lists from config
          call getPetListFromConfig(config, "pets_gwr:", instance=i, &
            petList=petList, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

          ! SetServices for GWR
          select case (model)
            case ('default')
              call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
                msg="GWR model missing from build", &
                line=__LINE__, file=__FILE__, rcToReturn=rc)
              return
            case default
              call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
                msg="invalid gwr_model: "//trim(model), &
                line=__LINE__, file=__FILE__, rcToReturn=rc)
              return
          endselect

          call NUOPC_CompAttributeSet(child, name="Verbosity", value="0", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out

          ! read GWR attributes from config file into FreeFormat
          attrFF = NUOPC_FreeFormatCreate(config, label="gwrAttributes::", &
            relaxedflag=.true., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call NUOPC_CompAttributeIngest(child, attrFF, addFlag=.true., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call NUOPC_CompAttributeAdd(child, &
            attrList=(/"multi_instance_gwr"/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_AttributeSet(child, name="multi_instance_gwr", &
            value=.true., convention="NUOPC", purpose="Instance", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        enddo

      else

        ! get PET lists from config
        call getPetListFromConfig(config, "pets_gwr:", petList=petList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        ! SetServices for GWR
        select case (model)
          case ('default')
            call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
              msg="GWR model missing from build", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
          case default
            call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
              msg="invalid gwr_model: "//trim(model), &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
        endselect

        call NUOPC_CompAttributeSet(child, name="Verbosity", value="0", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

        ! read GWR attributes from config file into FreeFormat
        attrFF = NUOPC_FreeFormatCreate(config, label="gwrAttributes::", &
          relaxedflag=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call NUOPC_CompAttributeIngest(child, attrFF, addFlag=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out

      endif ! multiple instances

    endif !enabledGwr

    ! #########
    ! Mediators
    ! #########

    call isComponentEnabled(config, "med", isEnabled=enabledMed, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    if (enabledMed) then

      ! get PET lists from config
      call getPetListFromConfig(config, "pets_med:", petList=petList, rc=rc)
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

      ! Pass multi instance setting to mediator
      if (enabledLnd) then
        call ESMF_ConfigFindLabel(config, label="instance_count_lnd:", &
          isPresent=isPresent, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (isPresent) then
          call ESMF_ConfigGetAttribute(config, instCnt, &
            label="instance_count_lnd:", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call NUOPC_CompAttributeAdd(child, &
            attrList=(/"instance_count_lnd"/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_AttributeSet(child, name="instance_count_lnd", &
            value=instCnt, convention="NUOPC", purpose="Instance", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
        call ESMF_ConfigFindLabel(config, label="multi_instance_lnd:", &
          isPresent=isPresent, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (isPresent) then
          call ESMF_ConfigGetAttribute(config, multiInst, &
            label="multi_instance_lnd:", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call NUOPC_CompAttributeAdd(child, &
            attrList=(/"multi_instance_lnd"/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_AttributeSet(child, name="multi_instance_lnd", &
            value=multiInst, convention="NUOPC", purpose="Instance", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
      else
        call NUOPC_CompAttributeAdd(child, &
          attrList=(/"instance_count_lnd"/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_AttributeSet(child, name="instance_count_lnd", &
          value=0, convention="NUOPC", purpose="Instance", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif !enabledLnd
      if (enabledHyd) then
        call ESMF_ConfigFindLabel(config, label="instance_count_hyd:", &
          isPresent=isPresent, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (isPresent) then
          call ESMF_ConfigGetAttribute(config, instCnt, &
            label="instance_count_hyd:", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call NUOPC_CompAttributeAdd(child, &
            attrList=(/"instance_count_hyd"/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_AttributeSet(child, name="instance_count_hyd", &
            value=instCnt, convention="NUOPC", purpose="Instance", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
        call ESMF_ConfigFindLabel(config, label="multi_instance_hyd:", &
          isPresent=isPresent, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (isPresent) then
          call ESMF_ConfigGetAttribute(config, multiInst, &
            label="multi_instance_hyd:", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call NUOPC_CompAttributeAdd(child, &
            attrList=(/"multi_instance_hyd"/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_AttributeSet(child, name="multi_instance_hyd", &
            value=multiInst, convention="NUOPC", purpose="Instance", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
      else
        call NUOPC_CompAttributeAdd(child, &
          attrList=(/"instance_count_hyd"/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_AttributeSet(child, name="instance_count_hyd", &
          value=0, convention="NUOPC", purpose="Instance", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif !enabledHyd
      if (enabledGwr) then
        call ESMF_ConfigFindLabel(config, label="instance_count_gwr:", &
          isPresent=isPresent, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (isPresent) then
          call ESMF_ConfigGetAttribute(config, instCnt, &
            label="instance_count_gwr:", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call NUOPC_CompAttributeAdd(child, &
            attrList=(/"instance_count_gwr"/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_AttributeSet(child, name="instance_count_gwr", &
            value=instCnt, convention="NUOPC", purpose="Instance", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
        call ESMF_ConfigFindLabel(config, label="multi_instance_gwr:", &
          isPresent=isPresent, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (isPresent) then
          call ESMF_ConfigGetAttribute(config, multiInst, &
            label="multi_instance_gwr:", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call NUOPC_CompAttributeAdd(child, &
            attrList=(/"multi_instance_gwr"/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_AttributeSet(child, name="multi_instance_gwr", &
            value=multiInst, convention="NUOPC", purpose="Instance", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif
      else
        call NUOPC_CompAttributeAdd(child, &
          attrList=(/"instance_count_gwr"/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_AttributeSet(child, name="instance_count_gwr", &
          value=0, convention="NUOPC", purpose="Instance", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
      endif !enabledGwr

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

  subroutine getPetListFromConfig(config, label, instance, petList, rc)
    type(ESMF_Config), intent(inout)    :: config
    character(len=*), intent(in)        :: label
    integer, intent(in), optional       :: instance
    integer, allocatable, intent(inout) :: petList(:)
    integer, intent(out)                :: rc

    ! local
    integer              :: l_instance
    logical              :: isPresent
    integer              :: attrCnt
    integer, allocatable :: petBoundsList(:)
    integer              :: i, minPet, maxPet

    if (present(instance)) then
      l_instance = instance
    else
      l_instance = 1
    endif

    call ESMF_ConfigFindLabel(config, trim(label), &
      isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isPresent) then
      attrCnt = ESMF_ConfigGetLen(config, label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (attrCnt .ge. (l_instance*2)) then
        allocate (petBoundsList(attrCnt), stat=rc)
        if (ESMF_LogFoundAllocError(statusToCheck=rc, &
          msg="Could not allocate petBoundsList", &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_ConfigGetAttribute(config, petBoundsList, &
          label=trim(label), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        minPet = petBoundsList((2*l_instance)-1)
        maxPet = petBoundsList(2*l_instance)
        deallocate(petBoundsList, stat=rc)
        if (ESMF_LogFoundDeallocError(statusToCheck=rc, &
          msg="Could not deallocate petBoundsList", &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (minPet <= maxPet) then
          allocate(petList(maxPet-minPet+1), stat=rc)
          do i=1, maxPet-minPet+1
            petList(i) = minPet+i-1
          enddo
        else
          deallocate (petList, stat=rc)
          if (ESMF_LogFoundDeallocError(statusToCheck=rc, &
            msg="Could not deallocate petList", &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg=trim(label)//" min must be <= max", &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return
        endif
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg=trim(label)//" PET bounds missing", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    endif

  end subroutine getPetListFromConfig

  subroutine checkPetListFromConfig(config, label, rc)
    type(ESMF_Config), intent(inout)    :: config
    character(len=*), intent(in)        :: label
    integer, intent(out)                :: rc

    ! local
    logical              :: isPresent
    integer              :: attrCnt
    integer, allocatable :: petBoundsList(:)
    integer              :: i, j

    call ESMF_ConfigFindLabel(config, trim(label), &
      isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isPresent) then
      attrCnt = ESMF_ConfigGetLen(config, label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      if (MOD(attrCnt,2) .eq. 0) then
        allocate (petBoundsList(attrCnt), stat=rc)
        if (ESMF_LogFoundAllocError(statusToCheck=rc, &
          msg="Could not allocate petBoundsList", &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_ConfigGetAttribute(config, petBoundsList, &
          label=trim(label), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        do i=1, attrCnt, 2
          if (petBoundsList(i).gt.petBoundsList(i)) then
            call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
              msg=trim(label)//" min must be <= max", &
              line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
          endif
          do j=i+2, attrCnt, 2
            if ((petBoundsList(j).le.petBoundsList(i+1)) .AND. &
                (petBoundsList(j).ge.petBoundsList(i))) then
              call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
                msg=trim(label)//" instances have overlapping PETs", &
                line=__LINE__, file=__FILE__, rcToReturn=rc)
              return
            endif
          enddo
        enddo
        deallocate(petBoundsList, stat=rc)
        if (ESMF_LogFoundDeallocError(statusToCheck=rc, &
          msg="Could not deallocate petBoundsList", &
          line=__LINE__, file=__FILE__)) return  ! bail out
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg=trim(label)//" PET bounds missing", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return
      endif
    else
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg=trim(label)//" PET bounds missing", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

  end subroutine checkPetListFromConfig

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

  subroutine getModelFromConfig(config, label, model, rc)
    type(ESMF_Config), intent(inout)    :: config
    character(len=*), intent(in)        :: label
    character(len=*), intent(out)       :: model
    integer, intent(out)                :: rc

    ! local
    logical           :: isPresent
    character(len=40) :: value

    call ESMF_ConfigFindLabel(config, trim(label), &
      isPresent=isPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    if (isPresent) then
      call ESMF_ConfigGetAttribute(config, value, label=trim(label), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      model = ESMF_UtilStringLowerCase(value, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else
      model="default"
    endif

  end subroutine getModelFromConfig

  !-----------------------------------------------------------------------------

  subroutine ModifyCplLists(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_CplComp), pointer     :: connectorList(:)
    integer                         :: i, j, cplListSize
    character(len=256)              :: msg
    character(len=40)               :: value
    integer                         :: verbosity, diagnostic
    character(len=32)               :: connectorName
    integer                         :: split
    character(len=16)               :: srcName
    character(len=16)               :: dstName
    character(len=160), allocatable :: cplList(:)
    character(len=160)              :: standardName
    character(len=160)              :: defaultOpts
    integer                         :: findPos
    type(med_fld_type), pointer     :: srcFlds(:) => null()
    type(med_fld_type), pointer     :: dstFlds(:) => null()
    type(cplListFlag)               :: cplListOpt = CPL_LIST_INT
    type(fieldRemapFlag)            :: fieldDstRemap = FLD_REMAP_REDIST
    type(fieldMaskFlag)             :: fieldSrcMask  = FLD_MASK_NNE
    type(fieldMaskFlag)             :: fieldDstMask  = FLD_MASK_NNE

    rc = ESMF_SUCCESS

    call ESMF_AttributeGet(driver, name="cpl_list", &
      value=value, defaultValue="CPL_LIST_INT", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    cplListOpt = value

    if (cplListOpt .eq. CPL_LIST_NNE) then
      ! use config file cplList options
    elseif (cplListOpt .eq. CPL_LIST_INT) then
      ! use code specified cplList options
      nullify(connectorList)
      call NUOPC_DriverGetComp(driver, compList=connectorList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out

      do i=1, size(connectorList)
        ! get connector information
  !      call NUOPC_CompGet(connectorList(i), name=connectorName, verbosity=verbosity, &
  !        diagnostic=diagnostic, rc=rc)
  !      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
  !        line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_CplCompGet(connectorList(i), name=connectorName, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_AttributeGet(connectorList(i), name="Diagnostic", value=value, &
          defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        diagnostic = ESMF_UtilString2Int(value, &
          specialStringList=(/"min","max","debug"/), &
          specialValueList=(/0,65535,65536/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        call ESMF_AttributeGet(connectorList(i), name="Verbosity", value=value, &
          defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        verbosity = ESMF_UtilString2Int(value, &
          specialStringList=(/"min","max","debug"/), &
          specialValueList=(/0,65535,65536/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (verbosity>0) then
          write (msg,"(A,A,A)") trim(connectorName),": ", &
            "Modifying CplList"
          call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
        endif

        split = index(connectorName,"-TO-")
        if ((split .lt. 2) .OR. (split .ge. (len_trim(connectorName)-3))) then
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg="Invalid connector name: "//trim(connectorName), &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return
        endif

        srcName = connectorName(1:split-1)
        dstName = connectorName(split+4:len_trim(connectorName))

        ! remove ensemble index from srcName
        split = index(srcName,"-")
        if (split .gt. 1) then
          srcName = srcName(1:split-1)
        endif
        ! remove ensemble index from dstName
        split = index(dstName,"-")
        if (split .gt. 1) then
          dstName = dstName(1:split-1)
        endif
        ! get source field list
        if (srcName .eq. "LND") then
          srcFlds=>fldsFrLnd
        elseif (srcName .eq. "HYD") then
          srcFlds=>fldsFrHyd
        elseif (srcName .eq. "GWR") then
          srcFlds=>fldsFrGwr
        elseif (srcName .eq. "MED") then
          nullify(srcFlds)
        else
          call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
            msg="ModifyCplList for "//trim(connectorName)//" not implemented", &
            line=__LINE__, file=__FILE__, rcToReturn=rc)
          return
        endif
        ! get destination field list
        if (dstName .eq. "LND") then
          dstFlds=>fldsToLnd
        elseif (dstName .eq. "HYD") then
          dstFlds=>fldsToHyd
        elseif (dstName .eq. "GWR") then
          dstFlds=>fldsToGwr
        elseif (dstName .eq. "MED") then
          nullify(dstFlds)
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
            findPos = index(cplList(j),":")
            if (findPos .gt. 0) then
              standardName = cplList(j)(1:findPos-1)
              defaultOpts = cplList(j)(findPos:LEN(cplList(j)))
              cplList(j) = trim(standardName)
            else
              standardName = trim(cplList(j))
              defaultOpts = ""
            endif
            if (associated(srcFlds)) then
              call field_find_standardname(fieldList=srcFlds, &
                standardName=standardName, &
                mask=fieldSrcMask, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            else
              fieldSrcMask=FLD_MASK_NNE
            endif
            if (associated(dstFlds)) then
              call field_find_standardname(fieldList=dstFlds, &
                standardName=standardName, &
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
            if (verbosity>0) then
              if (findPos .gt. 0) then
                write (msg,"(A,A,A,I0,A,A)") trim(connectorName),": ", &
                  "CplList(",j,")=",trim(cplList(j))
                call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__)) return  ! bail out
                write (msg,"(A,A,A,I0,A,A,A,A)") trim(connectorName),": ", &
                  "CplList(",j,")=",trim(standardName)," [REMOVED] ", &
                  trim(defaultOpts)
                call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__)) return  ! bail out
              else
                write (msg,"(A,A,A,I0,A,A)") trim(connectorName),": ", &
                  "CplList(",j,")=",trim(cplList(j))
                call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=__FILE__)) return  ! bail out
              endif
            endif
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
    else
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Invalid cpl_list option.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

  end subroutine

end module
