!*****************************************************************************
!>
!  This file contains the following BLAS routines
!  [[dcopy]], [[ddot]], [[dnrm2]], [[dscal]]
!  required by subroutines [[LSMR]] and [[Acheck]].
!
!### References
!
!  * Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!  * Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Algorithm 539,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.

   module lsmrblas

      use lsmrDataModule, only : ip => lsmr_ip, wp => lsmr_wp

      implicit none

      private

      public :: ddot, dnrm2, dscal, dcopy

#ifdef HAS_BLAS

   ! get BLAS from an externally-linked library (double precision only)

   interface

      function ddot  (n,dx,incx,dy,incy)
         implicit none
         integer, intent(in)          :: n,incx,incy
         double precision, intent(in) :: dx(*),dy(*)
         double precision             :: ddot
      end function ddot

      function dnrm2 (n,dx,incx)
         implicit none
         integer, intent(in)          :: n,incx
         double precision, intent(in) :: dx(*)
         double precision             :: dnrm2
      end function dnrm2

      subroutine dscal (n,sa,x,incx)
         implicit none
         integer, intent(in)             :: n,incx
         double precision, intent(in)    :: sa
         double precision, intent(inout) :: x(*)
      end subroutine dscal

      subroutine dcopy(n,dx,incx,dy,incy)
         implicit none
         double precision dx(*),dy(*)
         integer i,incx,incy,ix,iy,m,n
      end subroutine dcopy

   end interface


#else

   contains

!*****************************************************************************
!>
!  Copies a vector X to a vector Y.

      subroutine dcopy(n,dx,incx,dy,incy)

      implicit none
      real(wp) dx(*),dy(*)
      integer(ip) i,incx,incy,ix,iy,m,n

      if ( n <= 0 ) then
         return
      end if

      if ( incx == 1 .and. incy == 1 ) then

         m = mod ( n, 7 )

         if ( m /= 0 ) then
            dy(1:m) = dx(1:m)
         end if

         do i = m+1, n, 7
            dy(i) = dx(i)
            dy(i + 1) = dx(i + 1)
            dy(i + 2) = dx(i + 2)
            dy(i + 3) = dx(i + 3)
            dy(i + 4) = dx(i + 4)
            dy(i + 5) = dx(i + 5)
            dy(i + 6) = dx(i + 6)
         end do

        else

           if ( 0 <= incx ) then
              ix = 1
           else
              ix = ( -n + 1 ) * incx + 1
           end if

           if ( 0 <= incy ) then
              iy = 1
           else
              iy = ( -n + 1 ) * incy + 1
           end if

           do i = 1, n
              dy(iy) = dx(ix)
              ix = ix + incx
              iy = iy + incy
           end do
        end if

end subroutine dcopy

!*****************************************************************************
!>
!  Dot product of two vectors.

   real(wp) function ddot(n,dx,incx,dy,incy)

   integer(ip),intent(in) :: n !! the number of entries in the vectors.
   real(wp),intent(in) :: dx(*) !! the first vector
   integer(ip),intent(in) :: incx !! the increment between successive entries in DX.
   real(wp),intent(in) :: dy(*) !! the second vector
   integer(ip),intent(in) :: incy !! the increment between successive entries in DY.

   integer(ip) :: i,ix,iy,m
   real(wp) :: dtemp

   ddot = 0.0_wp
   dtemp = 0.0_wp
   if ( n <= 0 ) return

   if ( incx /= 1 .or. incy /= 1 ) then
      !  Code for unequal increments or equal increments
      !  not equal to 1.

      if ( 0 <= incx ) then
         ix = 1
      else
         ix = ( - n + 1 ) * incx + 1
      end if

      if ( 0 <= incy ) then
         iy = 1
      else
         iy = ( - n + 1 ) * incy + 1
      end if

      do i = 1, n
         dtemp = dtemp + dx(ix) * dy(iy)
         ix = ix + incx
         iy = iy + incy
      end do

   else
      ! Code for both increments equal to 1.

      m = mod ( n, 5 )

      do i = 1, m
         dtemp = dtemp + dx(i) * dy(i)
      end do

      do i = m+1, n, 5
         dtemp = dtemp + dx(i)*dy(i) + dx(i+1)*dy(i+1) + dx(i+2)*dy(i+2) &
                                     + dx(i+3)*dy(i+3) + dx(i+4)*dy(i+4)
      end do

   end if

   ddot = dtemp

end function ddot

!*****************************************************************************
!>
!  The euclidean norm of a vector `sqrt ( X' * X )`.

   real(wp) function dnrm2 ( n, x, incx)

   integer(ip),intent(in) :: n
   real(wp),intent(in) :: x(*)
   integer(ip),intent(in) :: incx

   integer(ip) :: ix
   real(wp) :: ssq,absxi,norm,scale

   if ( n < 1 .or. incx < 1 ) then
      norm  = 0.0_wp
   else if ( n == 1 ) then
      norm  = abs ( x(1) )
   else
      scale = 0.0_wp
      ssq = 1.0_wp

      do ix = 1, 1 + ( n - 1 )*incx, incx
         if ( x(ix) /= 0.0_wp ) then
            absxi = abs ( x(ix) )
            if ( scale < absxi ) then
               ssq = 1.0_wp + ssq * ( scale / absxi )**2
               scale = absxi
            else
               ssq = ssq + ( absxi / scale )**2
            end if
         end if
      end do
      norm  = scale * sqrt ( ssq )
   end if

   dnrm2 = norm

end function dnrm2

!*****************************************************************************
!>
!  Scales a vector by a constant.

   subroutine dscal(n,sa,x,incx)

   integer(ip),intent(in) :: n !! the number of entries in the vector.
   real(wp)   ,intent(in) :: sa !! the multiplier.
   real(wp),intent(inout) :: x(*) !! the vector to be scaled.
   integer(ip),intent(in) :: incx !! the increment between successive entries of X.

   integer(ip) :: i, ix
   integer(ip) :: m

   if ( n <= 0 ) then
      return
   else if ( incx == 1 ) then
      m = mod ( n, 5 )
      x(1:m) = sa * x(1:m)

      do i = m+1, n, 5
         x(i)   = sa * x(i)
         x(i+1) = sa * x(i+1)
         x(i+2) = sa * x(i+2)
         x(i+3) = sa * x(i+3)
         x(i+4) = sa * x(i+4)
      end do
   else
      if ( 0 <= incx ) then
         ix = 1
      else
         ix = ( - n + 1 ) * incx + 1
      end if

      do i = 1, n
         x(ix) = sa * x(ix)
         ix = ix + incx
      end do

   end if

end subroutine dscal

#endif

end module lsmrblas