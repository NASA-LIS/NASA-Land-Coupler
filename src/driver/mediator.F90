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
    NUOPC_MediatorGet
  use Fields

  implicit none

  private

  type med_ext_conn_type
    type(ESMF_State)                :: frState
    type(ESMF_State)                :: toState
    type(med_fld_type), pointer     :: allFrFlds(:) => null()
    type(med_fld_type), pointer     :: allToFlds(:) => null()
    type(med_fld_type), allocatable :: connFrFlds(:)
    type(med_fld_type), allocatable :: connToFlds(:)
    type(ESMF_FieldBundle)          :: connFrFB
    type(ESMF_FieldBundle)          :: connToFB
  end type med_ext_conn_type

  type med_uni_conn_type
    type(med_fld_type), allocatable  :: dstFlds(:)
    type(med_fld_type), allocatable  :: srcFlds(:)
    type(ESMF_FieldBundle)           :: srcFB
    type(ESMF_FieldBundle)           :: dstFB
    type(ESMF_RouteHandle)           :: rhBilnr
    type(ESMF_RouteHandle)           :: rhFcopy
  end type med_uni_conn_type

  type(med_ext_conn_type) :: LND
  type(med_ext_conn_type) :: HYD

  type(med_uni_conn_type) :: toLND
  type(med_uni_conn_type) :: toHYD

  integer, parameter :: srcTermProcessing_Value = 0
  type(ESMF_PoleMethod_Flag), parameter :: polemethod=ESMF_POLEMETHOD_ALLAVG
  type(ESMF_UnmappedAction_Flag), parameter :: unmappedaction=ESMF_UNMAPPEDACTION_IGNORE

  type(fieldRemapFlag) :: remapDstLND = FLD_REMAP_UNKOWN
  type(fieldRemapFlag) :: remapDstHYD = FLD_REMAP_UNKOWN
  type(fieldMaskFlag)  :: maskFrLND   = FLD_MASK_UNK
  type(fieldMaskFlag)  :: maskToLND   = FLD_MASK_UNK
  type(fieldMaskFlag)  :: maskFrHYD   = FLD_MASK_UNK
  type(fieldMaskFlag)  :: maskToHYD   = FLD_MASK_UNK

  type(ESMF_Time)    :: time_invalidTimeStamp
  type(ESMF_Clock)   :: clock_invalidTimeStamp

  public SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

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
      specPhaseLabel="prepLND", specRoutine=MediatorPrepLND, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_TimestampExport, &
      specPhaseLabel="prepLND", &
      specRoutine=TimestampExport_prepLND, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set entry point for Run( phase = prep_hyd ) and specialize
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_RUN, &
      phaseLabelList=(/"prepHYD"/), &
      userRoutine=mediator_routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_Advance, &
      specPhaseLabel="prepHYD", specRoutine=MediatorPrepHYD, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_TimestampExport, &
      specPhaseLabel="prepHYD", &
      specRoutine=TimestampExport_prepHYD, rc=rc)
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

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(mediator, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: mediator
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    rc = ESMF_SUCCESS

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

    rc = ESMF_SUCCESS

    ! Attach field lists
    LND%allFrFlds => fldsFrLnd
    LND%allToFlds => fldsToLnd
    HYD%allFrFlds => fldsFrHyd
    HYD%allToFlds => fldsToHyd

    ! ###################
    ! # Fields from LND #
    ! ###################

    ! add to field dictionary
    call field_dictionary_add(LND%allFrFlds, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    !   use namespace in the importState
    call NUOPC_AddNamespace(importState, namespace="LND", &
      nestedState=LND%frState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    !   advertise fields in the nested state
    call field_advertise(LND%allFrFlds, LND%frState, "cannot provide", rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! #################
    ! # Fields to LND #
    ! #################

    ! add to field dictionary
    call field_dictionary_add(LND%allToFlds, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    !   use namespace in the exportState
    call NUOPC_AddNamespace(exportState, namespace="LND", &
      nestedState=LND%toState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    !   advertise fields in the nested state
    call field_advertise(LND%allToFlds, LND%toState, "cannot provide", rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! ###################
    ! # Fields from HYD #
    ! ###################

    ! add to field dictionary
    call field_dictionary_add(HYD%allFrFlds, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    !   use namespace in the importState
    call NUOPC_AddNamespace(importState, namespace="HYD", &
      nestedState=HYD%frState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    !   advertise fields in the nested state
    call field_advertise(HYD%allFrFlds, HYD%frState, "cannot provide", rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! #################
    ! # Fields to HYD #
    ! #################

    ! add to field dictionary
    call field_dictionary_add(HYD%allToFlds, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    !   use namespace in the exportState
    call NUOPC_AddNamespace(exportState, namespace="HYD", &
      nestedState=HYD%toState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    !   advertise fields in the nested state
    call field_advertise(HYD%allToFlds, HYD%toState, "cannot provide", rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

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

    rc = ESMF_SUCCESS

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

      if (present(rc)) rc = ESMF_SUCCESS

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
            call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
              value=transferAction, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            if (trim(transferAction)=="provide") then
              ! the Connector instructed the Mediator to provide geom object
              call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
                msg="Cannot fulfill request to provide geom object for "// &
                trim(itemNameList(item))//" in State "//trim(stateName), &
                line=__LINE__, &
                file=__FILE__, &
                rcToReturn=rc)
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
    type(ESMF_VM) :: vm
    integer       :: medPetCount

    rc = ESMF_SUCCESS

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
          call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
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
    type(ESMF_Field)  :: field
    character(len=64) :: attValue

    rc = ESMF_SUCCESS

    call realizeWithAcceptedGeom(importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call realizeWithAcceptedGeom(exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! Generate connect field lists
    call med_ext_conn_realize(LND, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call med_ext_conn_realize(HYD, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! Log Realized State
    call med_log_state(LND%frState,"MED: frLnd",rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call med_log_state(LND%toState,"MED: toLnd",rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call med_log_state(HYD%frState,"MED: frHyd",rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call med_log_state(HYD%toState,"MED: toHyd",rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! Log DistGrid
    call med_log_distgrid(LND%frState,"MED: frLnd",rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call med_log_distgrid(HYD%frState,"MED: frHyd",rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! Read Remapping Settings
    call ESMF_AttributeGet(mediator, name="RemapLND", value=attValue, &
      defaultValue="FLD_REMAP_REDIST", convention="NUOPC", purpose="Instance", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    remapDstLND = attValue
    call ESMF_AttributeGet(mediator, name="RemapHYD", value=attValue, &
      defaultValue="FLD_REMAP_REDIST", convention="NUOPC", purpose="Instance", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    remapDstHYD = attValue

    ! Read Mask Settings
    call ESMF_AttributeGet(mediator, name="MaskFrLND", value=attValue, &
      defaultValue="FLD_MASK_NNE", convention="NUOPC", purpose="Instance", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    maskFrLND = attValue
    call ESMF_AttributeGet(mediator, name="MaskToLND", value=attValue, &
      defaultValue="FLD_MASK_NNE", convention="NUOPC", purpose="Instance", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    maskToLND = attValue
    call ESMF_AttributeGet(mediator, name="MaskFrHYD", value=attValue, &
      defaultValue="FLD_MASK_NNE", convention="NUOPC", purpose="Instance", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    maskFrHYD = attValue
    call ESMF_AttributeGet(mediator, name="MaskToHYD", value=attValue, &
      defaultValue="FLD_MASK_NNE", convention="NUOPC", purpose="Instance", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    maskToHYD = attValue

    ! Compute RHs
    call med_compute_rh(srcCmpList=(/LND/), dstCmp=HYD, conn=toHyd, &
      label="MED: toHYD", mapping=remapDstHYD, &
      srcMask=maskFrLND, dstMask=maskToHYD, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call med_compute_rh(srcCmpList=(/HYD/), dstCmp=LND, conn=toLND, &
      label="MED: toLND", mapping=remapDstLND, &
      srcMask=maskFrHYD, dstMask=maskToLND, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine realizeWithAcceptedGeom(state, rc)
      ! Look at all of the fields in state, including in nested states. Realize
      ! with the accepted and adjusted geom object.
      type(ESMF_State)  :: state
      integer, optional :: rc
      ! local variables
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
              itemCount=itemCount, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
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
              itemCount=itemCount, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
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
              itemCount=itemCount, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return  ! bail out
            if (itemCount > 0) then
              allocate(ugUBound(itemCount))
              call ESMF_AttributeGet(field, name="UngriddedUBound", &
                convention="NUOPC", purpose="Instance", &
                valueList=ugUBound, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
            endif

            if (associated(ugLBound).and.associated(ugUBound)) then
              call ESMF_FieldEmptyComplete(field, typekind=LISHYDRO_TYPEKIND, &
                ungriddedLBound=ugLBound, ungriddedUBound=ugUBound, &
                gridToFieldMap=gridToFieldMap, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return  ! bail out
              deallocate(ugLBound, ugUBound)
            else
              call ESMF_FieldEmptyComplete(field, typekind=LISHYDRO_TYPEKIND, &
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
      call ESMF_StateGet(cmp%frState, nestedFlag=.true., &
        itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(tmpFlds(itemCount))
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of field list memory failed.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)) return ! bail out
      ! Find connected from fields
      connCount = 0
      do i=1, size(cmp%allFrFlds)
        call ESMF_StateGet(cmp%frState, itemName=cmp%allFrFlds(i)%stateName, &
          itemType=itemType, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (itemType .eq. ESMF_STATEITEM_FIELD) then
          connCount = connCount + 1
          tmpFlds(connCount) = cmp%allFrFlds(i)
          call ESMF_StateGet(cmp%frState, itemName=cmp%allFrFlds(i)%stateName, &
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
      call ESMF_StateGet(cmp%toState, nestedFlag=.true., &
        itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      allocate(tmpFlds(itemCount))
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg="Allocation of field list memory failed.", &
        file=__FILE__, &
        rcToReturn=rc)) &
        return ! bail out
      ! Find connected to fields
      connCount = 0
      do i=1, size(cmp%allToFlds)
        call ESMF_StateGet(cmp%toState, itemName=cmp%allToFlds(i)%stateName, &
          itemType=itemType, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=__FILE__)) return  ! bail out
        if (itemType .eq. ESMF_STATEITEM_FIELD) then
          connCount = connCount + 1
          tmpFlds(connCount) = cmp%allToFlds(i)
          call ESMF_StateGet(cmp%toState, itemName=cmp%allToFlds(i)%stateName, &
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

    subroutine med_compute_rh(srcCmpList, dstCmp, label, conn, mapping, &
      srcMask, dstMask, rc)
      type(med_ext_conn_type),intent(in)    :: srcCmpList(:)
      type(med_ext_conn_type),intent(in)    :: dstCmp
      character(*),intent(in)               :: label
      type(med_uni_conn_type),intent(inout) :: conn
      type(fieldRemapFlag),intent(in)       :: mapping
      type(fieldMaskFlag),intent(in)        :: srcMask
      type(fieldMaskFlag),intent(in)        :: dstMask
      integer,intent(out)                   :: rc
      ! local variables
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
      character(len=124) :: msg
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
          call ESMF_StateGet(srcCmpList(j)%frState, &
            itemName=srcCmpList(j)%connFrFlds(i)%stateName, &
            field=srcFld, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_FieldBundleAdd(conn%srcFB, fieldList=(/srcFld/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return  ! bail out
          call ESMF_StateGet(dstCmp%toState, &
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

#if 1
      remapStr = mapping
      srcMaskStr = srcMask
      dstMaskStr = dstMask
      write (msg,"(A10,3(A,A))") label, " remapping= ",trim(remapStr), &
        " srcMask=", trim(srcMaskStr), " dstMask=", trim(dstMaskStr)
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
#endif

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

    rc = ESMF_SUCCESS

    call DataReset(toHYD, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call DataReset(toLND, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! indicate that data initialization is complete (breaking out of init-loop)
    call NUOPC_CompAttributeSet(mediator, &
      name="InitializeDataComplete", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetRunClock(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)           :: mediatorClock, driverClock
    type(ESMF_Time)            :: currTime
    type(ESMF_TimeInterval)    :: timeStep
    character(len=*),parameter :: subname='(module_MEDIATOR:SetRunClock)'

    rc = ESMF_SUCCESS

    ! query the Mediator for clocks
    call NUOPC_MediatorGet(mediator, mediatorClock=mediatorClock, &
      driverClock=driverClock, rc=rc)
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

    rc = ESMF_SUCCESS

    call MediatorPrepLND(mediator, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call MediatorPrepHYD(mediator, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine MediatorPrepLND(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    ! local variables
    type(ESMF_Clock) :: clock

    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(mediator, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! HERE THE MEDIATOR ADVANCES: currTime -> currTime + timeStep
    if (remapDstLND .eq. FLD_REMAP_BILINR) then
      call ESMF_FieldBundleRegrid(toLND%srcFB, toLND%dstFB, &
        routehandle=toLND%rhBilnr, &
        zeroregion=ESMF_REGION_SELECT, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    elseif (remapDstLND .eq. FLD_REMAP_REDIST) then
      call ESMF_FieldBundleRedist(toLND%srcFB, toLND%dstFB, &
        routehandle=toLND%rhFcopy, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="Remapping method has not been implemented.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine TimestampExport_prepLND(mediator, rc)
    type(ESMF_GridComp)   :: mediator
    integer, intent(out)  :: rc

    ! This attaches an invalid timestamp on fields sometimes.
    ! Otherwise, it just sets the timestamp to the current clock.

    ! local variables
    integer                 :: n, fieldcount
    type(ESMF_Clock)        :: driverClock
    type(ESMF_Clock)        :: clock
    type(ESMF_time)         :: currtime

    rc = ESMF_SUCCESS

    ! query the Component for info
    call NUOPC_MediatorGet(mediator, driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_GridCompGet(mediator, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set the Clock to have the current time as the driverClock
    call ESMF_ClockGet(driverClock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_ClockSet(clock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !---------------------------
    ! validate all data by default
    !---------------------------

    call NUOPC_UpdateTimestamp(LND%toState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine MediatorPrepHYD(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    ! local variables
    type(ESMF_Clock) :: clock

    rc = ESMF_SUCCESS

    ! query the Component for its clock
    call ESMF_GridCompGet(mediator, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! HERE THE MEDIATOR ADVANCES: currTime -> currTime + timeStep
    if (remapDstHYD .eq. FLD_REMAP_BILINR) then
      call ESMF_FieldBundleRegrid(toHYD%srcFB, toHYD%dstFB, &
        routehandle=toHYD%rhBilnr, &
        zeroregion=ESMF_REGION_SELECT, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    elseif (remapDstHYD .eq. FLD_REMAP_REDIST) then
      call ESMF_FieldBundleRedist(toHYD%srcFB, toHYD%dstFB, &
        routehandle=toHYD%rhFcopy, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    else
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="Remapping method has not been implemented.", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine TimestampExport_prepHYD(mediator, rc)
    type(ESMF_GridComp)   :: mediator
    integer, intent(out)  :: rc

    ! This attaches an invalid timestamp on fields sometimes.
    ! Otherwise, it just sets the timestamp to the current clock.

    ! local variables
    integer                 :: n, fieldcount
    type(ESMF_Clock)        :: driverClock
    type(ESMF_Clock)        :: clock
    type(ESMF_time)         :: currtime

    rc = ESMF_SUCCESS

    ! query the Component for info
    call NUOPC_MediatorGet(mediator, driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_GridCompGet(mediator, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    ! set the Clock to have the current time as the driverClock
    call ESMF_ClockGet(driverClock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out
    call ESMF_ClockSet(clock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

    !---------------------------
    ! validate all data by default
    !---------------------------

    call NUOPC_UpdateTimestamp(HYD%toState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__)) return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine DataReset(toComp, rc)
    type(med_uni_conn_type) :: toComp
    integer, intent(out) :: rc
    ! local variables
    integer :: i
    type(ESMF_Field) :: field

    rc = ESMF_SUCCESS

    do i=1, size(toComp%srcFlds)
      call ESMF_FieldBundleGet(toComp%srcFB, toComp%srcFlds(i)%stateName, &
        field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_FieldFill(field, dataFillScheme="const", &
        const1=REAL(toComp%srcFlds(i)%fillValue,LISHYDRO_KIND), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    enddo

    do i=1, size(toComp%dstFlds)
      call ESMF_FieldBundleGet(toComp%dstFB, toComp%dstFlds(i)%stateName, &
        field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
      call ESMF_FieldFill(field, dataFillScheme="const", &
        const1=REAL(toComp%dstFlds(i)%fillValue,LISHYDRO_KIND), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=__FILE__)) return  ! bail out
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine med_log_state(state,label,rc)
    type(ESMF_State),intent(in)  :: state
    character(len=*),intent(in)  :: label
    integer,intent(out)          :: rc
    ! local variables
    integer                               :: stat
    integer                               :: itemCount
    character(len=64), allocatable        :: itemNameList(:)
    integer                               :: iIndex
    character(len=80)                     :: logMsg

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
        write (logMsg,"(A10,A1,I2.2,A2,A20)") label, &
          "(",iIndex,") ", itemNameList(iIndex)
          call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
      enddo
      deallocate(itemNameList,stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=trim(label)//" Deallocation of item list memory failed.", &
        file=__FILE__, &
        rcToReturn=rc)) &
        return ! bail out
    else
      write (logMsg,"(A10,A16)") label," state is empty."
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine med_log_distgrid(state,label,rc)
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
    character(len=80)                     :: logMsg
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
      write (logMsg,"(A10,A16)") label," state is empty."
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
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
      write (logMsg,"(A10,A21,"//trim(lenDim)//")") label, &
        " distgrid dimensions ",dimCount
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
      write (logMsg,"(A10,A18,"//trim(lenDe)//")") label, &
        " distgrid decount ",deCount
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
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
        write (logMsg,"(A10,A15,"//trim(lenDe)//",A2,A)") label, &
          " distgrid DE(",deIndex,") = ",trim(indexStr)
        call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
      enddo
      deallocate(minIndexPDe, maxIndexPDe,stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
        msg=trim(label)//" Deallocation of index list memory failed.", &
        file=__FILE__, &
        rcToReturn=rc)) &
        return ! bail out
    else
      write (logMsg,"(A10,A20)") label," no fields in state."
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    endif

  end subroutine

  !-----------------------------------------------------------------------------

end module
