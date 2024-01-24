!*****************************************************************************
!>
!  Defines `integer(ip)` and `real(wp)` kind parameters.
!
!### Notes
!  * JW: 1/24/2024 : The original version of this file was rewritten.

module lsmrDataModule

  use iso_fortran_env

  implicit none

  private

#ifdef REAL32
  integer,parameter,public :: lsmr_wp = real32   !! real kind used by this module [4 bytes]
#elif REAL64
  integer,parameter,public :: lsmr_wp = real64   !! real kind used by this module [8 bytes]
#elif REAL128
  integer,parameter,public :: lsmr_wp = real128  !! real kind used by this module [16 bytes]
#else
  integer,parameter,public :: lsmr_wp = real64   !! real kind used by this module [8 bytes]
#endif

  integer, parameter,public :: lsmr_ip = int32 !! integer kind used by this module [4 bytes]

end module lsmrDataModule
