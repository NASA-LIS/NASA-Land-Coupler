#include "settings.h"

module Fields

  !-----------------------------------------------------------------------------
  ! Mediator Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC

  implicit none

  private

  type fieldRemapFlag
    sequence
    private
      integer :: remap
  end type

  type(fieldRemapFlag), parameter ::       &
    FLD_REMAP_ERROR  = fieldRemapFlag(-1), &
    FLD_REMAP_UNKOWN = fieldRemapFlag(0),  &
    FLD_REMAP_REDIST = fieldRemapFlag(1),  &
    FLD_REMAP_BILINR = fieldRemapFlag(2),  &
    FLD_REMAP_CONSRV = fieldRemapFlag(3)

  type fieldMaskFlag
    sequence
    private
      integer :: mask
  end type

  type(fieldMaskFlag), parameter ::   &
    FLD_MASK_ERR = fieldMaskFlag(-1), &
    FLD_MASK_UNK = fieldMaskFlag(0),  &
    FLD_MASK_NNE = fieldMaskFlag(1),  &
    FLD_MASK_LND = fieldMaskFlag(2),  &
    FLD_MASK_WTR = fieldMaskFlag(3)

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

  type(med_fld_type),target,dimension(14) :: fldsFrLnd = (/&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_1","smliqfracl1",&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_2","smliqfracl2",&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_3","smliqfracl3",&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_4","smliqfracl4",&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_1"          ,"smfracl1"   ,&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_2"          ,"smfracl2"   ,&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_3"          ,"smfracl3"   ,&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_4"          ,"smfracl4"   ,&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
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
    med_fld_type("soil_column_drainage"                    ,"soldrain"   ,&
                 "mm",FLD_REMAP_BILINR,FLD_MASK_NNE,0)/)

  type(med_fld_type),target,dimension(9)  :: fldsToLnd = (/&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_1","smliqfracl1",&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_2","smliqfracl2",&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_3","smliqfracl3",&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_4","smliqfracl4",&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_1"          ,"smfracl1"   ,&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_2"          ,"smfracl2"   ,&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_3"          ,"smfracl3"   ,&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_4"          ,"smfracl4"   ,&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("surface_water_depth"                     ,"sfcheadrt_f",&
                 "mm",FLD_REMAP_BILINR,FLD_MASK_NNE,0)/)

  type(med_fld_type),target,dimension(9)  :: fldsFrHyd = (/&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_1","smliqfracl1",&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_2","smliqfracl2",&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_3","smliqfracl3",&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_4","smliqfracl4",&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_1"          ,"smfracl1"   ,&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_2"          ,"smfracl2"   ,&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_3"          ,"smfracl3"   ,&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_4"          ,"smfracl4"   ,&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("surface_water_depth"                     ,"sfcheadrt_f",&
                 "mm",FLD_REMAP_BILINR,FLD_MASK_NNE,0)/)

  type(med_fld_type),target,dimension(14) :: fldsToHyd = (/&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_1","smliqfracl1",&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_2","smliqfracl2",&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_3","smliqfracl3",&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("liquid_fraction_of_soil_moisture_layer_4","smliqfracl4",&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_1"          ,"smfracl1"   ,&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_2"          ,"smfracl2"   ,&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_3"          ,"smfracl3"   ,&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_moisture_fraction_layer_4"          ,"smfracl4"   ,&
                 "1",FLD_REMAP_BILINR,FLD_MASK_NNE,1),&
    med_fld_type("soil_temperature_layer_1"                ,"soiltempl1" ,&
                 "K",FLD_REMAP_BILINR,FLD_MASK_NNE,288),&
    med_fld_type("soil_temperature_layer_2"                ,"soiltempl2" ,&
                 "K",FLD_REMAP_BILINR,FLD_MASK_NNE,288),&
    med_fld_type("soil_temperature_layer_3"                ,"soiltempl3" ,&
                 "K",FLD_REMAP_BILINR,FLD_MASK_NNE,288),&
    med_fld_type("soil_temperature_layer_4"                ,"soiltempl4" ,&
                 "K",FLD_REMAP_BILINR,FLD_MASK_NNE,288),&
    med_fld_type("time_step_infiltration_excess"           ,"infxsrt"    ,&
                 "mm",FLD_REMAP_BILINR,FLD_MASK_WTR,0),&
    med_fld_type("soil_column_drainage"                    ,"soldrain"   ,&
                 "mm",FLD_REMAP_BILINR,FLD_MASK_NNE,0)/)

  type(med_fld_syn_type),dimension(0) :: fldsLndToHyd
  type(med_fld_syn_type),dimension(0) :: fldsHydToLnd

  public fieldRemapFlag
  public FLD_REMAP_ERROR
  public FLD_REMAP_UNKOWN
  public FLD_REMAP_REDIST
  public FLD_REMAP_BILINR
  public FLD_REMAP_CONSRV

  public fieldMaskFlag
  public FLD_MASK_ERR
  public FLD_MASK_UNK
  public FLD_MASK_NNE
  public FLD_MASK_LND
  public FLD_MASK_WTR

  public med_fld_type

  public fldsFrLnd
  public fldsFrHyd
  public fldsToLnd
  public fldsToHyd

  public fldsLndToHyd
  public fldsHydToLnd

  public operator(==), assignment(=)

  public field_dictionary_add
  public field_synonym_add
  public field_advertise
  public field_find_standardname
  public field_find_statename

  interface operator (==)
    module procedure field_rfeq
    module procedure field_mfeq
  end interface

  interface assignment (=)
    module procedure field_rfas_string
    module procedure field_mfas_string
    module procedure field_stringas_rf
    module procedure field_stringas_mf
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  function field_rfeq(rf1, rf2)
    logical field_rfeq
    type(fieldRemapFlag), intent(in) :: rf1, rf2
    field_rfeq = (rf1%remap == rf2%remap)
  end function

  !-----------------------------------------------------------------------------

  subroutine field_rfas_string(string, rfval)
    character(len=*), intent(out) :: string
    type(fieldRemapFlag), intent(in) :: rfval
    if (rfval == FLD_REMAP_UNKOWN) then
      write(string,'(a)') 'FLD_REMAP_UNKOWN'
    elseif (rfval == FLD_REMAP_REDIST) then
      write(string,'(a)') 'FLD_REMAP_REDIST'
    elseif (rfval == FLD_REMAP_BILINR) then
      write(string,'(a)') 'FLD_REMAP_BILINR'
    elseif (rfval == FLD_REMAP_CONSRV) then
      write(string,'(a)') 'FLD_REMAP_CONSRV'
    else
      write(string,'(a)') 'FLD_REMAP_ERROR'
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine field_stringas_rf(rfval, string)
    type(fieldRemapFlag), intent(out) :: rfval
    character(len=*), intent(in) :: string
    if (string .eq. 'FLD_REMAP_UNKOWN') then
      rfval = FLD_REMAP_UNKOWN
    elseif (string .eq. 'FLD_REMAP_REDIST') then
      rfval = FLD_REMAP_REDIST
    elseif (string .eq.'FLD_REMAP_BILINR') then
      rfval = FLD_REMAP_BILINR
    elseif (string .eq. 'FLD_REMAP_CONSRV') then
      rfval = FLD_REMAP_CONSRV
    else
      rfval = FLD_REMAP_ERROR
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  function field_mfeq(mf1, mf2)
    logical field_mfeq
    type(fieldMaskFlag), intent(in) :: mf1, mf2
    field_mfeq = (mf1%mask == mf2%mask)
  end function

  !-----------------------------------------------------------------------------

  subroutine field_mfas_string(string, mfval)
    character(len=*), intent(out) :: string
    type(fieldMaskFlag), intent(in) :: mfval
    if (mfval == FLD_MASK_UNK) then
      write(string,'(a)') 'FLD_MASK_UNK'
    elseif (mfval == FLD_MASK_NNE) then
      write(string,'(a)') 'FLD_MASK_NNE'
    elseif (mfval == FLD_MASK_LND) then
      write(string,'(a)') 'FLD_MASK_LND'
    elseif (mfval == FLD_MASK_WTR) then
      write(string,'(a)') 'FLD_MASK_WTR'
    else
      write(string,'(a)') 'FLD_MASK_ERR'
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine field_stringas_mf(mfval,string)
    type(fieldMaskFlag), intent(out) :: mfval
    character(len=*), intent(in) :: string
    if (string .eq. 'FLD_MASK_UNK') then
      mfval = FLD_MASK_UNK
    elseif (string .eq. 'FLD_MASK_NNE') then
      mfval = FLD_MASK_NNE
    elseif (string .eq. 'FLD_MASK_LND') then
      mfval = FLD_MASK_LND
    elseif (string .eq. 'FLD_MASK_WTR') then
      mfval = FLD_MASK_WTR
    else
      mfval = FLD_MASK_ERR
    endif
  end subroutine

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

  end subroutine

  !-----------------------------------------------------------------------------

end module
