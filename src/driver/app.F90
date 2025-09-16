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

program esmApp

  !-----------------------------------------------------------------------------
  ! Application driver
  !-----------------------------------------------------------------------------

  use ESMF
  use ESM, only: esmSS => SetServices
  use NUOPC, only: &
    NUOPC_FieldDictionarySetup, &
    NUOPC_CompSearchPhaseMap, &
    label_ExternalReset

  implicit none

  integer                 :: rc, urc
  type(ESMF_Config)       :: config
  type(ESMF_GridComp)     :: esmComp
  character(len=20)       :: runMode
  type(ESMF_Time)         :: startTime
  type(ESMF_Time)         :: stopTime
  type(ESMF_Time)         :: currTime
  type(ESMF_Time)         :: stepEndTime
  type(ESMF_TimeInterval) :: timeStep
  type(ESMF_Clock)        :: clock
  integer                 :: rewindToYY
  integer                 :: rewindToMM
  integer                 :: rewindAtYY
  integer                 :: rewindAtMM
  type(ESMF_Time)         :: rewindAt
  integer                 :: phase

  character(len=32)       :: currTimeStr
  character(len=32)       :: startTimeStr
  character(len=32)       :: stopTimeStr
  character(len=32)       :: rewindAtStr
  character(len=32)       :: stepEndTimeStr
  character(len=160)      :: msgString

  ! Initialize ESMF
#if ESMF_VERSION_MAJOR >= 8 && ESMF_VERSION_MINOR >= 2
  call ESMF_Initialize(configFileName=NLC_CONFIG, &
    defaultCalkind=ESMF_CALKIND_GREGORIAN, &
    config=config, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
#else
#error "ESMF 8.2 or higher is required."
#endif

  call ESMF_LogWrite("NLC App STARTING", ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Setting up the NUOPC field dictionary
  call NUOPC_FieldDictionarySetup(fileName="fd_nlc.yaml", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Read clock configuration
  call getTimeFromConfig(config, "start_time:", startTime, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call getTimeFromConfig(config, "stop_time:", stopTime, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ConfigGetAttribute(config, value=runMode, &
    label="running_mode:", default="retrospective", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (runMode .eq. "ensemble smoother") then
    call ESMF_TimeGet(startTime, yy=rewindAtYY, mm=rewindAtMM, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (rewindAtMM .ge. 11) rewindAtYY = rewindAtYY + 1
    rewindAtMM = rewindAtMM + 2
    if (rewindAtMM .gt. 12) rewindAtMM = rewindAtMM - 12
    call ESMF_TimeSet(rewindAt, yy=rewindAtYY, mm=rewindAtMM, &
      dd=1, h=0, m=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (rewindAt .ge. stopTime) then
      timeStep = stopTime - startTime
      rewindAt = stopTime + timeStep
    else
      timeStep = rewindAt - startTime
    endif
  else
    timeStep = stopTime - startTime
    rewindAt = stopTime + timeStep
  endif

  ! Create the application Clock
  clock = ESMF_ClockCreate(name="Application Clock", &
    timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create the earth system Component
  esmComp = ESMF_GridCompCreate(name="esm", rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! SetServices for the earth system Component
  call ESMF_GridCompSetServices(esmComp, esmSS, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Call Initialize for the earth system Component
  call ESMF_GridCompInitialize(esmComp, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Explicit time stepping loop on the external level, here based on ESMF_Clock
  call NUOPC_CompSearchPhaseMap(esmComp, methodflag=ESMF_METHOD_FINALIZE, &
    phaseLabel=label_ExternalReset, phaseIndex=phase, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Print runMode
  call ESMF_TimeGet(startTime, timeString=startTimeStr, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_TimeGet(stopTime, timeString=stopTimeStr, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_LogWrite("APP: BEGIN RUN startTime="//trim(startTimeStr)// &
                     " stopTime="//trim(stopTimeStr)//  &
                     " runMode="//trim(runMode), ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  do while (.not.ESMF_ClockIsStopTime(clock, rc=rc))

    ! Clock diagnostics
    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    stepEndTime = currTime + timeStep
    call ESMF_TimeGet(currTime, timeString=currTimeStr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(rewindAt, timeString=rewindAtStr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_TimeGet(stepEndTime, timeString=stepEndTimeStr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_LogWrite("APP: BEFORE STEP currTime="//trim(currTimeStr)// &
                       " stepEndTime="//trim(stepEndTimeStr)//  &
                       " rewindAt="//trim(rewindAtStr), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Call run for earth the system component
    call ESMF_GridCompRun(esmComp, clock=clock, userRc=urc, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Advance the clock
    call ESMF_ClockAdvance(clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ! Clock diagnostics
    call ESMF_TimeGet(currTime, timeString=currTimeStr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_LogWrite("APP: AFTER STEP currTime="//trim(currTimeStr), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    if (currTime .ge. stopTime) then
      call ESMF_LogWrite("APP: End of simulation - currTime >= stopTime", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      continue
    elseif (currTime .ge. rewindAt) then
      call ESMF_LogWrite("APP: Rewind - currTime >= rewindAt", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      ! Rewind currTime to 1 month prior to rewindAt
      call ESMF_TimeGet(rewindAt, yy=rewindToYY, mm=rewindToMM, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (rewindToMM .eq. 1) rewindToYY = rewindToYY - 1
      rewindToMM = rewindToMM - 1
      if (rewindToMM .lt. 1) rewindToMM = rewindToMM + 12
      call ESMF_TimeSet(currTime, yy=rewindToYY, mm=rewindToMM, &
        dd=1, h=0, m=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      ! Increment rewindAt by 1 month
      call ESMF_TimeGet(rewindAt, yy=rewindAtYY, mm=rewindAtMM, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (rewindAtMM .ge. 12) rewindAtYY = rewindAtYY + 1
      rewindAtMM = rewindAtMM + 1
      if (rewindAtMM .gt. 12) rewindAtMM = rewindAtMM - 12
      call ESMF_TimeSet(rewindAt, yy=rewindAtYY, mm=rewindAtMM, &
        dd=1, h=0, m=0, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (rewindAt .ge. stopTime) then
        timeStep = stopTime - currTime
        rewindAt = stopTime + timeStep
      else
        timeStep = rewindAt - currTime
      endif

      call ESMF_ClockSet(clock, &
         timeStep=timeStep, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_GridCompFinalize(esmComp, phase=phase, clock=clock, &
        userRc=urc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)


      ! Clock diagnostics
      call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      stepEndTime = currTime + timeStep
      call ESMF_TimeGet(currTime, timeString=currTimeStr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_TimeGet(rewindAt, timeString=rewindAtStr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_TimeGet(stepEndTime, timeString=stepEndTimeStr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_TimeGet(stopTime, timeString=stopTimeStr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)
      call ESMF_LogWrite("APP: AFTER REWIND currTime="//trim(currTimeStr)// &
                         " stepEndTime="//trim(stepEndTimeStr)//  &
                         " rewindAt="//trim(rewindAtStr)// &
                         " stopTime="//trim(stopTimeStr), ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        call ESMF_Finalize(endflag=ESMF_END_ABORT)

    endif

  enddo

  ! Call Finalize for the earth system Component
  call ESMF_GridCompFinalize(esmComp, clock=clock, userRc=urc, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Destroy the earth system Component
  call ESMF_GridCompDestroy(esmComp, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  call ESMF_LogWrite("NLC App FINISHED", ESMF_LOGMSG_INFO, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
    line=__LINE__, &
    file=__FILE__)) &
    call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Finalize ESMF
  call ESMF_Finalize()

  contains

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

end program
