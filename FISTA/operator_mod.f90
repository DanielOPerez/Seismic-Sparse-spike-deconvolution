module operator_mod

  use tools_mod, only:convo
  
  implicit none
  
contains
  
  !======================================================================
  !Fordward model: reflectivity ---> traces
  !======================================================================

  function avo_operator_f(in,w)
    
    real(kind=4), intent(in), dimension(:)::in,w
    real(kind=4), dimension(size(in)+size(w)-1)::avo_operator_f

    avo_operator_f=convo(in,w)
   
    return

  end function avo_operator_f

  !======================================================================
  !Adjoint model: traces ---> reflectivity
  !======================================================================
  
  function avo_operator_a(in,w)

    real(kind=4), intent(in), dimension(:)::in,w
    real(kind=4), dimension(size(in)+size(w)-1)::conv_aux
    real(kind=4), dimension(size(in)-size(w)+1)::avo_operator_a
    
  
    conv_aux=convo(in,w(size(w):1:-1))
    avo_operator_a=conv_aux(size(w):size(in))
     
    return

  end function avo_operator_a

  
end module operator_mod
