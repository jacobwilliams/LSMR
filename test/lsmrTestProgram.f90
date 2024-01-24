!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! File lsmrTestProgram.f90
!
!    lsmrTestProgram
!
! Main program for testing LSMR via subroutine lsmrtest in lsmrTestModule.
!
! Maintained by Michael Saunders <saunders@stanford.edu>.
!
! 16 Jul 2010: LSMR version derived from LSQR equivalent.
! 28 Jan 2014: ip added for integer(ip) declarations.
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

program lsmrTestProgram

  use   lsmrDataModule, only : ip, dp
  use   lsmrTestModule, only : lsmrtest
  implicit none

  !---------------------------------------------------------------------
  ! This program calls lsmrtest(...) to generate a series of test problems
  ! Ax = b or Ax ~= b and solve them with LSMR.
  ! The matrix A is m x n.  It is defined by routines in lsmrTestModule.
  ! We test 3 groups of problems with m>n, m=n, m<n respectively.
  !
  ! 23 Sep 2007: First version of lsmrTestProgram.f90.
  ! 24 Oct 2007: Use real(dp) instead of compiler option -r8.
  ! 19 Oct 2012: Add date and cpu output.
  ! 02 May 2014: Within each group, damp decreases toward zero,
  !              so the problems are increasingly ill-conditioned.
  !---------------------------------------------------------------------

  intrinsic     :: date_and_time, cpu_time

  ! Local variables
  integer(ip)   :: ios,m,n,nbar,ndamp,nduplc,npower,nout
  integer(ip)   :: localSize  ! No. of vectors involved in local reorthogonalization (>= 0)
  real(dp)      :: time1, time2
  real(dp)      :: damp
  character(8)  :: date
  character(10) :: time
  character(80) :: output_file


  nout   = 10
  output_file = 'LSMR.txt'
  open(nout, file=output_file, status='unknown', iostat=ios)

  if (ios /= 0) then
     write(*,*)
     write(*,*) "Error opening file ", trim(output_file)
     stop
  end if

  call date_and_time( date, time )
  write(*,*)
  write(*,*) 'Date: ', date, '        Time: ', time
  call cpu_time( time1 )

  nbar   = 1000
  nduplc =   40

  m = 2*nbar        ! Over-determined systems
  n = nbar
  localSize = 0

  do ndamp = 3,8
     npower = ndamp
     damp   = 10.0_dp**(-ndamp)
     if (ndamp == 8) damp = 0.0_dp
     call lsmrtest(m,n,nduplc,npower,damp,localSize,nout)
  end do

!  localsize = 10    ! Repeat last test with local reorthogonalization
!  call lsmrtest(m,n,nduplc,npower,damp,localSize,nout)


  m = nbar          ! Square systems
  n = nbar
  localSize = 0

  do ndamp = 3,8
     npower = ndamp
     damp   = 10.0_dp**(-ndamp)
     if (ndamp == 8) damp = 0.0_dp
     call lsmrtest(m,n,nduplc,npower,damp,localSize,nout)
  end do

!  localsize = 10    ! Repeat last test with local reorthogonalization
!  call lsmrtest(m,n,nduplc,npower,damp,localSize,nout)


  m = nbar          ! Under-determined systems
  n = 2*nbar
  localSize = 0

  do ndamp = 3,8
     npower = ndamp
     damp   = 10.0_dp**(-ndamp)
     if (ndamp == 8) damp = 0.0_dp
     call lsmrtest(m,n,nduplc,npower,damp,localSize,nout)
  end do

!  localsize = 10    ! Repeat last test with local reorthogonalization
!  call lsmrtest(m,n,nduplc,npower,damp,localSize,nout)

  close(nout)
  call cpu_time( time2 )

  write(*,'(a, f13.3)') " Total CPU time (seconds) ", time2-time1
  write(*,*) "Results are in output file   ", trim(output_file)
  write(*,*) "Search the file for 'appears'"
  write(*,*) "For example,    grep appears ", trim(output_file)
  write(*,*)

end program lsmrTestProgram
