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

module Fields

  !-----------------------------------------------------------------------------
  ! Mediator Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use Flags

  implicit none

  private

  type med_fld_type
    sequence
    character(len=64)    :: standardName = "dummy"
    character(len=64)    :: stateName    = "dummy"
    character(len=64)    :: units        = "-"
    type(fieldRemapFlag) :: mapping      = FLD_REMAP_REDIST
    type(fieldMaskFlag)  :: mask         = FLD_MASK_NNE
    real                 :: fillValue    = 999999999
  end type med_fld_type

  type med_fld_syn_type
    sequence
    character(len=64) :: standardName1 = "dummy1"
    character(len=64) :: standardName2 = "dummy2"
  end type med_fld_syn_type

  type(med_fld_type),target,dimension(21) :: fldsFrLnd = (/&
    med_fld_type("bare_soil_evaporation"                   ,"edir"    ,&
                 "W m-2",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_1","smliqfracl1",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_2","smliqfracl2",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_3","smliqfracl3",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_4","smliqfracl4",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("precip_drip"                             ,"pcpdrp"    ,&
                 "kg m-2 s-1",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("soil_moisture_fraction_layer_1"          ,"smfracl1"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_2"          ,"smfracl2"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_3"          ,"smfracl3"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_4"          ,"smfracl4"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_temperature_layer_1"                ,"soiltempl1" ,&
                 "K",FLD_REMAP_BILINR,FLD_MASK_NNE,288),&
    med_fld_type("soil_temperature_layer_2"                ,"soiltempl2" ,&
                 "K",FLD_REMAP_BILINR,FLD_MASK_NNE,288),&
    med_fld_type("soil_temperature_layer_3"                ,"soiltempl3" ,&
                 "K",FLD_REMAP_BILINR,FLD_MASK_NNE,288),&
    med_fld_type("soil_temperature_layer_4"                ,"soiltempl4" ,&
                 "K",FLD_REMAP_BILINR,FLD_MASK_NNE,288),&
    med_fld_type("time_step_infiltration_excess"           ,"infxsrt"    ,&
                 "mm",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("soil_column_drainage"                    ,"soldrain"    ,&
                 "mm",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("total_water_flux_layer_1"                ,"wtrflx1"    ,&
                 "kg m-2 s-1",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("total_water_flux_layer_2"                ,"wtrflx2"    ,&
                 "kg m-2 s-1",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("total_water_flux_layer_3"                ,"wtrflx3"    ,&
                 "kg m-2 s-1",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("total_water_flux_layer_4"                ,"wtrflx4"    ,&
                 "kg m-2 s-1",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("vegetation_transpiration"                ,"et"   ,&
                 "W m-2",FLD_REMAP_BILINR,FLD_MASK_NNE,0)/)

  type(med_fld_type),target,dimension(15)  :: fldsToLnd = (/&
    med_fld_type("ground_water_storage"                    ,"wa"         ,&
                 "mm",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("liquid_fraction_of_soil_moisture"        ,"smliqfrac"  ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_1","smliqfracl1",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_2","smliqfracl2",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_3","smliqfracl3",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_4","smliqfracl4",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("porosity"                                ,"porosity"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("pressure"                                ,"pressure"   ,&
                 "m",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("saturation"                              ,"saturation" ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction"                  ,"smfrac"     ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_1"          ,"smfracl1"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_2"          ,"smfracl2"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_3"          ,"smfracl3"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_4"          ,"smfracl4"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("surface_water_depth"                     ,"sfcheadrt_f",&
                 "mm",FLD_REMAP_BILINR,FLD_MASK_NNE,0)/)

  type(med_fld_type),target,dimension(11)  :: fldsFrHyd = (/&
    med_fld_type("liquid_fraction_of_soil_moisture"        ,"smliqfrac"  ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_1","smliqfracl1",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_2","smliqfracl2",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_3","smliqfracl3",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_4","smliqfracl4",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction"                  ,"smfrac"     ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_1"          ,"smfracl1"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_2"          ,"smfracl2"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_3"          ,"smfracl3"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_4"          ,"smfracl4"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("surface_water_depth"                     ,"sfcheadrt_f",&
                 "mm",FLD_REMAP_BILINR,FLD_MASK_NNE,0)/)

  type(med_fld_type),target,dimension(17) :: fldsToHyd = (/&
    med_fld_type("liquid_fraction_of_soil_moisture"        ,"smliqfrac"  ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_1","smliqfracl1",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_2","smliqfracl2",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_3","smliqfracl3",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_4","smliqfracl4",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_column_drainage"                    ,"soldrain"   ,&
                 "mm",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("soil_moisture_fraction"                  ,"smfrac"     ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_1"          ,"smfracl1"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_2"          ,"smfracl2"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_3"          ,"smfracl3"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_4"          ,"smfracl4"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_temperature"                        ,"soiltemp"   ,&
                 "K",FLD_REMAP_BILINR,FLD_MASK_NNE,288),&
    med_fld_type("soil_temperature_layer_1"                ,"soiltempl1" ,&
                 "K",FLD_REMAP_BILINR,FLD_MASK_NNE,288),&
    med_fld_type("soil_temperature_layer_2"                ,"soiltempl2" ,&
                 "K",FLD_REMAP_BILINR,FLD_MASK_NNE,288),&
    med_fld_type("soil_temperature_layer_3"                ,"soiltempl3" ,&
                 "K",FLD_REMAP_BILINR,FLD_MASK_NNE,288),&
    med_fld_type("soil_temperature_layer_4"                ,"soiltempl4" ,&
                 "K",FLD_REMAP_BILINR,FLD_MASK_NNE,288),&
    med_fld_type("time_step_infiltration_excess"           ,"infxsrt"    ,&
                 "mm",FLD_REMAP_BILINR,FLD_MASK_WTR,0)/)

  type(med_fld_type),target,dimension(14)  :: fldsFrGwr = (/&
    med_fld_type("ground_water_storage"                    ,"wa"         ,&
                 "mm",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("liquid_fraction_of_soil_moisture"        ,"smliqfrac"  ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_1","smliqfracl1",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_2","smliqfracl2",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_3","smliqfracl3",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_4","smliqfracl4",&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("porosity"                                ,"porosity"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("pressure"                                ,"pressure"   ,&
                 "m",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("saturation"                              ,"saturation" ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction"                  ,"smfrac"     ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_1"          ,"smfracl1"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_2"          ,"smfracl2"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_3"          ,"smfracl3"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_4"          ,"smfracl4"   ,&
                 "-",FLD_REMAP_BILINR,FLD_MASK_NNE,1)/)

  type(med_fld_type),target,dimension(4) :: fldsToGwr = (/&
    med_fld_type("total_water_flux_layer_1"                ,"wtrflx1"    ,&
                 "kg m-2 s-1",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("total_water_flux_layer_2"                ,"wtrflx2"    ,&
                 "kg m-2 s-1",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("total_water_flux_layer_3"                ,"wtrflx3"    ,&
                 "kg m-2 s-1",FLD_REMAP_BILINR,FLD_MASK_NNE,0),&
    med_fld_type("total_water_flux_layer_4"                ,"wtrflx4"    ,&
                 "kg m-2 s-1",FLD_REMAP_BILINR,FLD_MASK_NNE,0)/)

  type(med_fld_syn_type),dimension(0) :: fldsLndToHyd
  type(med_fld_syn_type),dimension(0) :: fldsHydToLnd
  type(med_fld_syn_type),dimension(0) :: fldsLndToGwr
  type(med_fld_syn_type),dimension(0) :: fldsGwrToLnd

  public med_fld_type

  public fldsFrLnd
  public fldsFrHyd
  public fldsFrGwr
  public fldsToLnd
  public fldsToHyd
  public fldsToGwr

  public fldsLndToHyd
  public fldsHydToLnd
  public fldsLndToGwr
  public fldsGwrToLnd

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

  subroutine field_find_standardname(fieldList, standardName, location, &
    mapping, mask, fillValue, rc)
    type(med_fld_type), intent(in)              :: fieldList(:)
    character(len=64), intent(in)               :: standardName
    integer, intent(out), optional              :: location
    type(fieldRemapFlag), intent(out), optional :: mapping
    type(fieldMaskFlag), intent(out), optional  :: mask
    real,intent(out),optional                   :: fillValue
    integer, intent(out)           :: rc
    ! local variables
    integer           :: n

    rc = ESMF_RC_NOT_FOUND

    if (present(location)) location = lbound(fieldList,1) - 1
    if (present(mapping)) mapping = FLD_REMAP_UNKOWN
    if (present(mask)) mask = FLD_MASK_UNK
    if (present(fillValue)) fillValue = 999999999

    do n=lbound(fieldList,1),ubound(fieldList,1)
      if (fieldList(n)%standardName .eq. standardName) then
        if (present(location)) location = n
        if (present(mapping)) mapping = fieldList(n)%mapping
        if (present(mask)) mask = fieldList(n)%mask
        if (present(fillValue)) fillValue = fieldList(n)%fillValue
        rc = ESMF_SUCCESS
        return
      endif
    enddo

    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="Field not found in fieldList "//trim(standardName), &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine field_find_statename(fieldList, stateName, location, mapping, &
    mask, fillValue, rc)
    type(med_fld_type), intent(in)              :: fieldList(:)
    character(len=64), intent(in)               :: stateName
    integer, intent(out), optional              :: location
    type(fieldRemapFlag), intent(out), optional :: mapping
    type(fieldMaskFlag), intent(out), optional  :: mask
    real,intent(out),optional                   :: fillValue
    integer, intent(out)           :: rc
    ! local variables
    integer           :: n

    rc = ESMF_RC_NOT_FOUND

    if (present(location)) location = lbound(fieldList,1) - 1
    if (present(mapping)) mapping = FLD_REMAP_UNKOWN
    if (present(mask)) mask = FLD_MASK_UNK
    if (present(fillValue)) fillValue = 999999999

    do n=lbound(fieldList,1),ubound(fieldList,1)
      if (fieldList(n)%stateName .eq. stateName) then
        if (present(location)) location = n
        if (present(mapping)) mapping = fieldList(n)%mapping
        if (present(mask)) mask = fieldList(n)%mask
        if (present(fillValue)) fillValue = fieldList(n)%fillValue
        rc = ESMF_SUCCESS
        return
      endif
    enddo

    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="Field not found in fieldList "//trim(stateName), &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

end module
