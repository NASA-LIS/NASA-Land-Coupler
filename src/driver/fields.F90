module fields

  !-----------------------------------------------------------------------------
  ! Mediator Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC

  implicit none

  private

  type med_fld_type
    character(len=64)   :: standardName  = "dummy"
    character(len=64)   :: stateName     = "dummy"
    character(len=64)   :: units         = "-"
    character(len=64)   :: transferOffer = "cannot provide"
    character(len=64)   :: mapping       = "copy"
    real                :: fillValue     = 999999999
  end type med_fld_type

  type med_fld_syn_type
    character(len=64) :: standardName1 = "dummy1"
    character(len=64) :: standardName2 = "dummy2"
  end type med_fld_syn_type

  type(med_fld_type),target,dimension(14) :: fldsFrLnd = (/&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_1","smliqfracl1",&
                 "1","cannot provide","copy"      ),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_2","smliqfracl2",&
                 "1","cannot provide","copy"      ),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_3","smliqfracl3",&
                 "1","cannot provide","copy"      ),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_4","smliqfracl4",&
                 "1","cannot provide","copy"      ),&
    med_fld_type("soil_moisture_fraction_layer_1"          ,"smfracl1"   ,&
                 "1","cannot provide","copy"      ),&
    med_fld_type("soil_moisture_fraction_layer_2"          ,"smfracl2"   ,&
                 "1","cannot provide","copy"      ),&
    med_fld_type("soil_moisture_fraction_layer_3"          ,"smfracl3"   ,&
                 "1","cannot provide","copy"      ),&
    med_fld_type("soil_moisture_fraction_layer_4"          ,"smfracl4"   ,&
                 "1","cannot provide","copy"      ),&
    med_fld_type("soil_temperature_layer_1"                ,"soiltempl1" ,&
                 "K","cannot provide","copy"      ),&
    med_fld_type("soil_temperature_layer_2"                ,"soiltempl2" ,&
                 "K","cannot provide","copy"      ),&
    med_fld_type("soil_temperature_layer_3"                ,"soiltempl3" ,&
                 "K","cannot provide","copy"      ),&
    med_fld_type("soil_temperature_layer_4"                ,"soiltempl4" ,&
                 "K","cannot provide","copy"      ),&
    med_fld_type("time_step_infiltration_excess"           ,"infxsrt"    ,&
                 "mm","cannot provide","copy"      ),&
    med_fld_type("soil_column_drainage"                    ,"soldrain"   ,&
                 "mm","cannot provide","copy"      )/)

  type(med_fld_type),target,dimension(9)  :: fldsToLnd = (/&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_1","smliqfracl1",&
                 "1","cannot provide","copy"      ),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_2","smliqfracl2",&
                 "1","cannot provide","copy"      ),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_3","smliqfracl3",&
                 "1","cannot provide","copy"      ),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_4","smliqfracl4",&
                 "1","cannot provide","copy"      ),&
    med_fld_type("soil_moisture_fraction_layer_1"          ,"smfracl1"   ,&
                 "1","cannot provide","copy"      ),&
    med_fld_type("soil_moisture_fraction_layer_2"          ,"smfracl2"   ,&
                 "1","cannot provide","copy"      ),&
    med_fld_type("soil_moisture_fraction_layer_3"          ,"smfracl3"   ,&
                 "1","cannot provide","copy"      ),&
    med_fld_type("soil_moisture_fraction_layer_4"          ,"smfracl4"   ,&
                 "1","cannot provide","copy"      ),&
    med_fld_type("surface_water_depth"                     ,"sfcheadrt_f",&
                 "mm","cannot provide","copy"      )/)

  type(med_fld_type),target,dimension(9)  :: fldsFrHyd = (/&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_1","smliqfracl1",&
                 "1","cannot provide","copy"      ),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_2","smliqfracl2",&
                 "1","cannot provide","copy"      ),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_3","smliqfracl3",&
                 "1","cannot provide","copy"      ),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_4","smliqfracl4",&
                 "1","cannot provide","copy"      ),&
    med_fld_type("soil_moisture_fraction_layer_1"          ,"smfracl1"   ,&
                 "1","cannot provide","copy"      ),&
    med_fld_type("soil_moisture_fraction_layer_2"          ,"smfracl2"   ,&
                 "1","cannot provide","copy"      ),&
    med_fld_type("soil_moisture_fraction_layer_3"          ,"smfracl3"   ,&
                 "1","cannot provide","copy"      ),&
    med_fld_type("soil_moisture_fraction_layer_4"          ,"smfracl4"   ,&
                 "1","cannot provide","copy"      ),&
    med_fld_type("surface_water_depth"                     ,"sfcheadrt_f",&
                 "mm","cannot provide","copy"      )/)

  type(med_fld_type),target,dimension(14) :: fldsToHyd = (/&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_1","smliqfracl1",&
                 "1","cannot provide","na"      ),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_2","smliqfracl2",&
                 "1","cannot provide","na"      ),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_3","smliqfracl3",&
                 "1","cannot provide","na"      ),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_4","smliqfracl4",&
                 "1","cannot provide","na"      ),&
    med_fld_type("soil_moisture_fraction_layer_1"          ,"smfracl1"   ,&
                 "1","cannot provide","na"      ),&
    med_fld_type("soil_moisture_fraction_layer_2"          ,"smfracl2"   ,&
                 "1","cannot provide","na"      ),&
    med_fld_type("soil_moisture_fraction_layer_3"          ,"smfracl3"   ,&
                 "1","cannot provide","na"      ),&
    med_fld_type("soil_moisture_fraction_layer_4"          ,"smfracl4"   ,&
                 "1","cannot provide","na"      ),&
    med_fld_type("soil_temperature_layer_1"                ,"soiltempl1" ,&
                 "K","cannot provide","na"      ),&
    med_fld_type("soil_temperature_layer_2"                ,"soiltempl2" ,&
                 "K","cannot provide","na"      ),&
    med_fld_type("soil_temperature_layer_3"                ,"soiltempl3" ,&
                 "K","cannot provide","na"      ),&
    med_fld_type("soil_temperature_layer_4"                ,"soiltempl4" ,&
                 "K","cannot provide","na"      ),&
    med_fld_type("time_step_infiltration_excess"           ,"infxsrt"    ,&
                 "mm","cannot provide","na"      ),&
    med_fld_type("soil_column_drainage"                    ,"soldrain"   ,&
                 "mm","cannot provide","na"      )/)

  type(med_fld_syn_type),dimension(0) :: fldsLndToHyd
  type(med_fld_syn_type),dimension(0) :: fldsHydToLnd

  public med_fld_type
  public fldsFrLnd
  public fldsFrHyd
  public fldsToLnd
  public fldsToHyd
  public fldsLndToHyd
  public fldsHydToLnd
  public field_dictionary_add
  public field_synonym_add
  public field_advertise
  public field_find_standardname
  public field_find_statename

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine field_dictionary_add(fieldList, rc)
    type(med_fld_type), intent(in) :: fieldList(:)
    integer, intent(out) :: rc
    ! local variables
    integer :: n
    logical :: isPresent

    rc = ESMF_SUCCESS

    do n=1,size(fieldList)
      isPresent = NUOPC_FieldDictionaryHasEntry( &
        fieldList(n)%standardName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (.not.isPresent) then
        call NUOPC_FieldDictionaryAddEntry( &
          trim(fieldList(n)%standardName), &
          trim(fieldList(n)%units), &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine field_synonym_add(fieldSynoList, rc)
    type(med_fld_syn_type), intent(in) :: fieldSynoList(:)
    integer, intent(out) :: rc
    ! local variables
    integer :: n
    logical :: isPresent

    rc = ESMF_SUCCESS

    do n=1,size(fieldSynoList)
      isPresent = NUOPC_FieldDictionaryMatchSyno( &
        fieldSynoList(n)%standardName1, &
        fieldSynoList(n)%standardName2, &
        rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (.not.isPresent) then
        call NUOPC_FieldDictionarySetSyno( &
          (/ fieldSynoList(n)%standardName1, &
             fieldSynoList(n)%standardName2 /), &
          rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine field_advertise(fieldList, advertiseState, transferOffer, rc)
    type(med_fld_type), intent(in)  :: fieldList(:)
    type(ESMF_State), intent(inout) :: advertiseState
    character(*), intent(in)        :: transferOffer
    integer, intent(out)            :: rc
    ! local variables
    integer :: n

    rc = ESMF_SUCCESS

    do n=1,size(fieldList)
      call NUOPC_Advertise(advertiseState, &
        StandardName=fieldList(n)%standardName, &
        TransferOfferGeomObject=transferOffer, &
        name=fieldList(n)%stateName, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine field_find_standardname(fieldList, standardName, location, rc)
    type(med_fld_type), intent(in) :: fieldList(:)
    character(len=64), intent(in)  :: standardName
    integer, intent(out)           :: location
    integer, intent(out)           :: rc
    ! local variables
    integer           :: n

    rc = ESMF_RC_NOT_FOUND
    location = lbound(fieldList,1) - 1

    do n=lbound(fieldList,1),ubound(fieldList,1)
      if (fieldList(n)%standardName .eq. standardName) then
        location=n
        rc = ESMF_SUCCESS
        return
      endif
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine field_find_statename(fieldList, stateName, location, rc)
    type(med_fld_type), intent(in) :: fieldList(:)
    character(len=64), intent(in)  :: stateName
    integer, intent(out)           :: location
    integer, intent(out)           :: rc
    ! local variables
    integer           :: n

    rc = ESMF_RC_NOT_FOUND
    location = lbound(fieldList,1) - 1

    do n=lbound(fieldList,1),ubound(fieldList,1)
      if (fieldList(n)%stateName .eq. stateName) then
        location=n
        rc = ESMF_SUCCESS
        return
      endif
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

end module
