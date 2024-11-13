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

module Flags

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
      integer :: opt
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
      integer :: opt
  end type

  type(fieldMaskFlag), parameter ::   &
    FLD_MASK_ERR = fieldMaskFlag(-1), &
    FLD_MASK_UNK = fieldMaskFlag(0),  &
    FLD_MASK_NNE = fieldMaskFlag(1),  &
    FLD_MASK_LND = fieldMaskFlag(2),  &
    FLD_MASK_WTR = fieldMaskFlag(3)

  type cplListFlag
    sequence
    private
      integer :: opt
  end type

  type(cplListFlag), parameter ::   &
    CPL_LIST_ERR = cplListFlag(-1), &
    CPL_LIST_NNE = cplListFlag(0),  &
    CPL_LIST_INT = cplListFlag(1)

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

  public cplListFlag
  public CPL_LIST_ERR
  public CPL_LIST_NNE
  public CPL_LIST_INT

  public operator(==), assignment(=)

  interface operator (==)
    module procedure field_remap_eq
    module procedure field_mask_eq
    module procedure cpl_list_eq
  end interface

  interface assignment (=)
    module procedure field_remap_frString
    module procedure field_mask_frString
    module procedure cpl_list_frString
    module procedure field_remap_toString
    module procedure field_mask_toString
    module procedure cpl_list_toString
  end interface

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  function field_remap_eq(val1, val2)
    logical field_remap_eq
    type(fieldRemapFlag), intent(in) :: val1, val2
    field_remap_eq = (val1%opt == val2%opt)
  end function

  !-----------------------------------------------------------------------------

  subroutine field_remap_toString(string, val)
    character(len=*), intent(out) :: string
    type(fieldRemapFlag), intent(in) :: val
    if (val == FLD_REMAP_UNKOWN) then
      write(string,'(a)') 'FLD_REMAP_UNKOWN'
    elseif (val == FLD_REMAP_REDIST) then
      write(string,'(a)') 'FLD_REMAP_REDIST'
    elseif (val == FLD_REMAP_BILINR) then
      write(string,'(a)') 'FLD_REMAP_BILINR'
    elseif (val == FLD_REMAP_CONSRV) then
      write(string,'(a)') 'FLD_REMAP_CONSRV'
    else
      write(string,'(a)') 'FLD_REMAP_ERROR'
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine field_remap_frString(val, string)
    type(fieldRemapFlag), intent(out) :: val
    character(len=*), intent(in) :: string
    character(len=32) :: ustring
    integer :: rc
    ustring = ESMF_UtilStringUpperCase(string, rc=rc)
    if (ustring .eq. 'FLD_REMAP_UNKOWN') then
      val = FLD_REMAP_UNKOWN
    elseif (ustring .eq. 'FLD_REMAP_REDIST') then
      val = FLD_REMAP_REDIST
    elseif (ustring .eq.'FLD_REMAP_BILINR') then
      val = FLD_REMAP_BILINR
    elseif (ustring .eq. 'FLD_REMAP_CONSRV') then
      val = FLD_REMAP_CONSRV
    else
      val = FLD_REMAP_ERROR
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  function field_mask_eq(val1, val2)
    logical field_mask_eq
    type(fieldMaskFlag), intent(in) :: val1, val2
    field_mask_eq = (val1%opt == val2%opt)
  end function

  !-----------------------------------------------------------------------------

  subroutine field_mask_toString(string, val)
    character(len=*), intent(out) :: string
    type(fieldMaskFlag), intent(in) :: val
    if (val == FLD_MASK_UNK) then
      write(string,'(a)') 'FLD_MASK_UNK'
    elseif (val == FLD_MASK_NNE) then
      write(string,'(a)') 'FLD_MASK_NNE'
    elseif (val == FLD_MASK_LND) then
      write(string,'(a)') 'FLD_MASK_LND'
    elseif (val == FLD_MASK_WTR) then
      write(string,'(a)') 'FLD_MASK_WTR'
    else
      write(string,'(a)') 'FLD_MASK_ERR'
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine field_mask_frString(val, string)
    type(fieldMaskFlag), intent(out) :: val
    character(len=*), intent(in) :: string
    character(len=32) :: ustring
    integer :: rc
    ustring = ESMF_UtilStringUpperCase(string, rc=rc)
    if (ustring .eq. 'FLD_MASK_UNK') then
      val = FLD_MASK_UNK
    elseif (ustring .eq. 'FLD_MASK_NNE') then
      val = FLD_MASK_NNE
    elseif (ustring .eq. 'FLD_MASK_LND') then
      val = FLD_MASK_LND
    elseif (ustring .eq. 'FLD_MASK_WTR') then
      val = FLD_MASK_WTR
    else
      val = FLD_MASK_ERR
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  function cpl_list_eq(val1, val2)
    logical cpl_list_eq
    type(cplListFlag), intent(in) :: val1, val2
    cpl_list_eq = (val1%opt == val2%opt)
  end function

  !-----------------------------------------------------------------------------

  subroutine cpl_list_toString(string, val)
    character(len=*), intent(out) :: string
    type(cplListFlag), intent(in) :: val
    if (val == CPL_LIST_NNE) then
      write(string,'(a)') 'CPL_LIST_NNE'
    elseif (val == CPL_LIST_INT) then
      write(string,'(a)') 'CPL_LIST_INT'
    else
      write(string,'(a)') 'CPL_LIST_ERR'
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine cpl_list_frString(val, string)
    type(cplListFlag), intent(out) :: val
    character(len=*), intent(in) :: string
    character(len=32) :: ustring
    integer :: rc
    ustring = ESMF_UtilStringUpperCase(string, rc=rc)
    if (ustring .eq. 'CPL_LIST_NNE') then
      val = CPL_LIST_NNE
    elseif (ustring .eq. 'CPL_LIST_INT') then
      val = CPL_LIST_INT
    else
      val = CPL_LIST_ERR
    endif
  end subroutine

  !-----------------------------------------------------------------------------

end module
