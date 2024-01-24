!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! File lsmrCheckModule.f90
!
!    Acheck   xcheck
!
! Acheck tests if a user's matrix-vector product routines for
! computing y + A*x and x + A'*y are working with the same A.
! xcheck tests if a given x seems to be a solution of Ax = b or Ax ~= b.
!
! Maintained by Michael Saunders <saunders@stanford.edu>.
!
! 07 Sep 2007: Line by line translation of f77 file lsmrcheck.f to f90
!              by Eric Badel <badel@nancy.inra.fr>.
! 21 Sep 2007: lsmrCheckModule.f90 implemented.
! 24 Oct 2007: Use real(dp) instead of compiler option -r8.
! 19 Dec 2008: lsmrblasInterface module implemented.
! 16 Jul 2010: LSMR version derived from LSQR equivalent.
! 26 Oct 2012: tol is now an input parameter to xcheck.
! 28 Jan 2014: ip added for integer(ip) declarations.
!              BLAS ddot, dnrm2, dscal coded directly.
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

module lsmrCheckModule

  use  lsmrDataModule,    only : ip, dp
  implicit none
  public   :: Acheck, xcheck

contains

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine Acheck( m, n, Aprod1, Aprod2, nout, inform )

    integer(ip), intent(in)    :: m, n   ! No. of rows and cols of A
    integer(ip), intent(in)    :: nout   ! Output file number
    integer(ip), intent(out)   :: inform ! = 0 if Aprod1, Aprod2 seem ok
                                         ! = 1 otherwise
    interface
       subroutine Aprod1(m,n,x,y)                   ! y := y + A*x
         use lsmrDataModule, only : ip, dp
         integer(ip), intent(in)    :: m,n
         real(dp),    intent(in)    :: x(n)
         real(dp),    intent(inout) :: y(m)
       end subroutine Aprod1

       subroutine Aprod2(m,n,x,y)                   ! x := x + A'*y
         use lsmrDataModule, only : ip, dp
         integer(ip), intent(in)    :: m,n
         real(dp),    intent(inout) :: x(n)
         real(dp),    intent(in)    :: y(m)
       end subroutine Aprod2
    end interface

    !-------------------------------------------------------------------
    ! One-liner: Acheck checks Aprod1 and Aprod2 for LSMR.
    !
    ! Purpose:   Acheck tests the user subroutines Aprod1 and Aprod2
    !   called by LSMR.  For some m x n matrix A,
    !   Aprod1 computes y := y + A*x  from given x,y without altering x,
    !   Aprod2 computes x := x + A'*y from given x,y without altering y.
    !   Acheck tries to verify that A and A' refer to the same matrix.
    !
    ! Method:    We cook up some unlikely vectors x and y of unit length
    !   and test if  y'(y + Ax)  =  x'(x + A'y).
    !
    ! Parameter Constants:
    !   Param   Type   Description
    !   power   real   eps**power is the tolerance for judging if
    !                  y'(y + Ax) = x'(x + A'y) to sufficient accuracy.
    !                  power should be in the range (0.25, 0.9) say.
    !                  For example, power = 0.75 means we are happy
    !                  if 3/4 of the available digits agree.
    !                  power = 0.5 seems a reasonable requirement
    !                  (asking for half the digits to agree).
    !                    
    ! History:
    ! 04 Sep 1991  Initial design and code.
    !              Michael Saunders, Dept of Operations Research,
    !              Stanford University.
    ! 10 Feb 1992  Aprod added as parameter.
    !              tol defined via power.
    ! 10 Feb 1992: Acheck revised and xcheck implemented.
    ! 27 May 1993: Acheck and xcheck kept separate from test problems.
    ! 23 Sep 2007: Acheck implemented as part of this f90 module.
    !-------------------------------------------------------------------

    intrinsic           :: abs, epsilon, sqrt

    ! Local arrays and variables
    real(dp)            :: x(n), v(n), w(m), y(m)
    integer(ip)         :: i, j
    real(dp)            :: alfa, beta, eps, t, test1, test2, test3, tol

    ! Local constants
    real(dp), parameter :: one = 1.0_dp, power = 0.5_dp


    eps    = epsilon(eps)
    tol    = eps**power
    if (nout > 0) write(nout,1000)

    !===================================================================
    ! Cook up some unlikely vectors x and y of unit length.
    !===================================================================
    t = one
    do j=1,n
       t    = t + one
       x(j) = sqrt(t)
    end do
 
    t = one
    do i=1,m
       t    = t + one
       y(i) = one/sqrt(t)
    end do
 
    alfa = sqrt( dot_product(x,x) )     ! dnrm2 (n,x,1)
    beta = sqrt( dot_product(y,y) )     ! dnrm2 (m,y,1)
    x    = (one/alfa)*x                 ! call dscal (n, (one/alfa), x, 1)
    y    = (one/beta)*y                 ! call dscal (m, (one/beta), y, 1)
      
    !===================================================================
    ! Test if y'(y + Ax) = x'(x + A'y).
    !===================================================================
    w(1:m) = y(1:m)               ! First set  w = y + Ax,  v = x + A'y.
    v(1:n) = x(1:n)
    call Aprod1(m,n,x,w)
    call Aprod2(m,n,v,y)
      
    alfa   = dot_product(y,w)    ! Now set    alfa = y'w,  beta = x'v.
    beta   = dot_product(x,v)
    test1  = abs(alfa - beta)            
    test2  = one + abs(alfa) + abs(beta)
    test3  = test1 / test2

    if (test3 <= tol) then        ! See if alfa and beta are essentially
       inform = 0                 ! the same.
       if (nout > 0) write(nout,1010) test3
    else
       inform = 1
       if (nout > 0) write(nout,1020) test3
    end if

    return

 1000 format(//' Enter Acheck.')
 1010 format(1p, &
        ' Aprod1, Aprod2 seem OK.  Relative error =', e10.1)
 1020 format(1p, &
        ' Aprod1, Aprod2 seem incorrect.  Relative error =', e10.1)
	          
  end subroutine Acheck

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine xcheck( m, n, Aprod1, Aprod2, b, damp, x, &
                     Anorm, tol, nout,                 &
                     inform )

    integer(ip), intent(in)    :: m, n   ! No. of rows and cols of A
    integer(ip), intent(in)    :: nout   ! Output file number
    integer(ip), intent(out)   :: inform ! = 0 if b = 0 and x = 0.
                                         ! = 1 2 or 3 if x seems to
                                         ! solve systems 1 2 or 3 below.
    real(dp),    intent(in)    :: Anorm  ! An estimate of norm(A) or
                                         ! norm( A, delta*I ) if delta > 0.
                                         ! Provided by LSMR.
    real(dp),    intent(in)    :: tol    ! tolerance for judging residuals.
                                         ! Typically the tol used for computing x.
    real(dp),    intent(in)    :: damp   ! Defines problem 3 below.
    real(dp),    intent(in)    :: b(m)   ! The right-hand side of Ax ~= b.
    real(dp),    intent(in)    :: x(n)   ! The given solution estimate.

    interface
       subroutine Aprod1(m,n,x,y)                   ! y := y + A*x
         use lsmrDataModule, only : ip, dp
         integer(ip), intent(in)    :: m, n
         real(dp),    intent(in)    :: x(n)
         real(dp),    intent(inout) :: y(m)
       end subroutine Aprod1

       subroutine Aprod2(m,n,x,y)                   ! x := x + A'*y
         use lsmrDataModule, only : ip, dp
         integer(ip), intent(in)    :: m, n
         real(dp),    intent(inout) :: x(n)
         real(dp),    intent(in)    :: y(m)
       end subroutine Aprod2
    end interface

    !-------------------------------------------------------------------
    ! One-liner: xcheck tests if x solves a certain least-squares problem.
    !
    ! Purpose:   xcheck computes residuals and norms associated with
    ! the vector x and the least-squares problem solved by LSMR.
    ! It determines whether x seems to be a solution to any of three
    ! possible systems:  1. Ax = b
    !                    2. min norm(Ax - b)
    !                    3. min norm(Ax - b)^2 + damp^2 * norm(x)^2.
    !
    ! History:
    ! 07 Feb 1992  Initial design and code.
    !              Michael Saunders, Dept of Operations Research,
    !              Stanford University.
    ! 23 Sep 2007: xcheck implemented as part of this f90 module.
    ! 26 Oct 2012: tol is now an input parameter.
    !-------------------------------------------------------------------

    intrinsic           :: epsilon, max

    ! Local variables and arrays
    real(dp)            :: r(m), v(n)
    real(dp)            :: bnorm, dampsq, eps, rho1, rho2, sigma1, sigma2, &
                           test1, test2, test3, tol2, snorm, xnorm, xsnorm

    ! Local constants
    real(dp), parameter :: zero  = 0.0_dp


    eps    = epsilon(eps)
    dampsq = damp**2
    tol2   = max( tol, eps )

    r(1:m) = -b(1:m)        ! Compute the residual r = b - Ax
    call Aprod1(m,n,x,r)    ! via  r = -b + Ax,
    r(1:m) = -r(1:m)        !      r = -r. 

    v(1:n) = zero           ! Compute v = A'r
    call Aprod2(m,n,v,r)    ! via  v = 0,  v = v + A'r. 

    bnorm  = sqrt( dot_product(b,b) ) ! Compute the norms of b, x, r, v.
    xnorm  = sqrt( dot_product(x,x) )
    rho1   = sqrt( dot_product(r,r) )
    sigma1 = sqrt( dot_product(v,v) )
      
    if (nout > 0) write(nout,2200) damp, xnorm, rho1, sigma1

    if (damp == zero) then
       rho2   = rho1
       sigma2 = sigma1
    else
       v(1:n) = v(1:n) - dampsq*x(1:n)  ! v = A'r - damp**2 x.
       rho2   = sqrt(rho1**2 + dampsq*xnorm**2)
       sigma2 = sqrt( dot_product(v,v) )
       snorm  = rho1/damp
       xsnorm = rho2/damp
       if (nout > 0) write(nout,2300) snorm, xsnorm, rho2, sigma2
    end if

    !-------------------------------------------------------------------
    ! See if x seems to solve Ax = b  or  min norm(Ax - b)
    ! or the damped least-squares system.
    !-------------------------------------------------------------------
    if (bnorm == zero  .and.  xnorm == zero) then
       inform = 0
       test1  = zero
       test2  = zero
       test3  = zero
    else
       inform = 4
       test1  = rho1 / (bnorm + Anorm*xnorm)
       test2  = zero
       if (rho1  > zero) test2  = sigma1 / (Anorm*rho1)
       test3  = test2
       if (rho2  > zero) test3  = sigma2 / (Anorm*rho2)
       
       if (test3 <= tol2) inform = 3
       if (test2 <= tol2) inform = 2
       if (test1 <= tol2) inform = 1
    end if

    if (nout > 0) write(nout,3000) inform, tol2, test1, test2, test3
    return

 2200 format(1p                                             &
      // ' Enter xcheck.     Does x solve Ax = b, etc?'     &
      /  '    damp            =', e10.3                     &
      /  '    norm(x)         =', e10.3                     &
      /  '    norm(r)         =', e15.8, ' = rho1'          &
      /  '    norm(A''r)       =',e10.3, '      = sigma1')
 2300 format(1p/  '    norm(s)         =', e10.3            &
      /  '    norm(x,s)       =', e10.3                     &
      /  '    norm(rbar)      =', e15.8, ' = rho2'          &
      /  '    norm(Abar''rbar) =',e10.3, '      = sigma2')
 3000 format(1p/  '    inform          =', i2               &
      /  '    tol             =', e10.3                     &
      /  '    test1           =', e10.3, ' (Ax = b)'        &
      /  '    test2           =', e10.3, ' (least-squares)' &
      /  '    test3           =', e10.3, ' (damped least-squares)')

  end subroutine xcheck

end module lsmrCheckModule
