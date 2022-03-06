module fista_mod

  use operator_mod                    !operador AVO Aki & Richards  

  implicit none

contains

  subroutine fista(t,w,lambda,itmax,cut_msft,o_prt,ref)

    !lo que entra/sale
    real(kind=4), intent(in), dimension(:)::t,w
     real(kind=4), intent(in)::lambda,cut_msft
    integer, intent(in)::itmax,o_prt

    real(kind=4), intent(out), dimension(:)::ref
    
    !variables locales
    real(kind=4), dimension(size(t))::hk
    real(kind=4), dimension(size(t)-size(w)+1)::xk,xkl1,yk,x0
    real(kind=4)::tk,tkl1,tau,m_eigen,misfit,l1,tol_pm
    integer::i,j,k
    
    !max eigenvalue
    call random_number(x0)
    tol_pm=1.0e-3
    call power_method(x0,w,tol_pm,m_eigen)
    if(o_prt>0)write(*,*)'maximum egienvalue=',m_eigen

    tau=lambda/(2.0*m_eigen)

    xk=0.0
    xkl1=0.0
    yk=0.0
    tk=1.0
    tkl1=0.0
    misfit=10.d10
    i=0

    if(o_prt>0)write(*,90)

    hk=0.0
    xk=0.0
       
    do while( i<itmax .and. misfit>cut_msft )

       xkl1=xk
       tkl1=tk

       hk=avo_operator_f(yk,w)
       xk=st_function(yk-(1.0/m_eigen)*avo_operator_a(hk-t,w),tau)
             
       misfit=sum(abs((hk-t)**2))
       l1=sum(abs(xk))

       tk=(1.0+sqrt(1.0+4.0*(tk**2)))/2.0
       yk=xk+((tkl1-1)/tk)*(xk-xkl1)

       i=i+1

       if(mod(i,o_prt)==0 .and. o_prt>0)then
          write(*,100)i,misfit,l1
       end if


    end do

    if(o_prt>0)then
       write(*,*)'misfit=',misfit,l1
       write(*,*)'itmax=',i
    end if
    
    ref=xk

    
90  format(3x,'iteracion',2x,'misfit',9x,'l1-norm')
100 format(i6,2x,3f15.7)

  end subroutine fista
  
  !============================================================================

  function st_function(in,tau)

    !Soft Shrinkage-Tresholding Function

    implicit none
    real(kind=4), dimension(:)::in
    real(kind=4), dimension(size(in))::st_function,norm2
    real(kind=4)::tau
    integer i

    do i=1,size(in)
       st_function(i)=max(0.0,abs(in(i))-tau)*sign(1.0,in(i))
    end do

    
    return

  end function st_function

  !============================================================================

  subroutine power_method(x0,w,tol_pm,m_eigen)


    !esta subrutina encuentra el maximo autovalor del operador usando el
    !metodo de las potencias de Rayleigth.

    real(kind=4), intent(in)::tol_pm
    real(kind=4), intent(inout), dimension(:)::x0
    real(kind=4), intent(in), dimension(:)::w
    real(kind=4), intent(out)::m_eigen

    !variables locales
    real(kind=4), dimension(size(x0))::y
    real(kind=4)::m_eigen_tmp
    integer::i


    
    m_eigen_tmp=0.0
    i=1
    do
   
       y=avo_operator_a(avo_operator_f(x0,w),w)
       m_eigen=maxval(y)

       if(m_eigen==0)exit
       if(abs((m_eigen-m_eigen_tmp)/m_eigen)<tol_pm)exit
       x0=y/m_eigen
       m_eigen_tmp=m_eigen
       i=i+1

    end do


  end subroutine power_method

end module fista_mod
