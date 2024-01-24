!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     File lsmrblas.f90   (double precision)
!
!     This file contains the following BLAS routines
!        dcopy, ddot, dnrm2, dscal
!     required by subroutines LSMR and Acheck.
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! DCOPY copies a vector X to a vector Y.
!
!  Discussion:
!    This routine uses double precision real arithmetic.
!    The routine uses unrolled loops for increments equal to one.
!
!  Modified:
!    16 May 2005
!
!  Author:
!    Jack Dongarra
!    Fortran90 translation by John Burkardt.
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Algorithm 539,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.

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


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!! DDOT forms the dot product of two vectors.
!
!  Discussion:
!    This routine uses double precision real arithmetic.
!    This routine uses unrolled loops for increments equal to one.
!
!  Modified:
!    16 May 2005
!
!  Author:
!    Jack Dongarra
!    Fortran90 translation by John Burkardt.
!
!  Reference:
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Algorithm 539,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer N, the number of entries in the vectors.
!
!    Input, real ( kind = 8 ) DX(*), the first vector.
!
!    Input, integer INCX, the increment between successive entries in DX.
!
!    Input, real ( kind = 8 ) DY(*), the second vector.
!
!    Input, integer INCY, the increment between successive entries in DY.
!
!    Output, real ( kind = 8 ) DDOT, the sum of the product of the
!    corresponding entries of DX and DY.


      double precision function ddot(n,dx,incx,dy,incy)

      real(wp)    dx(*),dy(*),dtemp
      integer(ip) i,incx,incy,ix,iy,m,n

      ddot = 0.0_wp
      dtemp = 0.0_wp
      if ( n <= 0 ) return

!  Code for unequal increments or equal increments
!  not equal to 1.

      if ( incx /= 1 .or. incy /= 1 ) then

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

!  Code for both increments equal to 1.

        else

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!*****************************************************************************80
!
!! DNRM2 returns the euclidean norm of a vector.
!
!  Discussion:
!    This routine uses double precision real arithmetic.
!     DNRM2 ( X ) = sqrt ( X' * X )
!
!  Modified:
!    16 May 2005
!
!  Author:
!    Sven Hammarling
!    Fortran90 translation by John Burkardt.
!
!  Reference:
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Algorithm 539,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) X(*), the vector whose norm is to be computed.
!
!    Input, integer INCX, the increment between successive entries of X.
!
!    Output, real ( kind = 8 ) DNRM2, the Euclidean norm of X.
!

      real(wp) function dnrm2 ( n, x, incx)
      integer(ip) ix,n,incx
      real(wp)    x(*), ssq,absxi,norm,scale

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DSCAL scales a vector by a constant.
!
!  Discussion:
!    This routine uses double precision real arithmetic.
!
!  Modified:
!    08 April 1999
!
!  Author:
!    Jack Dongarra
!    Fortran90 translation by John Burkardt.
!
!  Reference:
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Algorithm 539,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) SA, the multiplier.
!
!    Input/output, real ( kind = 8 ) X(*), the vector to be scaled.
!
!    Input, integer INCX, the increment between successive entries of X.
!

      subroutine  dscal(n,sa,x,incx)

      integer(ip) i
      integer(ip) incx
      integer(ip) ix
      integer(ip) m
      integer(ip) n
      real(wp)    sa
      real(wp)    x(*)

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#endif

end module lsmrblas