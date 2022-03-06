module tools_mod

  implicit none

contains

  
  function count_files_lines(file_in)

    !==========================================================================
    ! Esta funcion cuenta la cantidad de lineas que hay en un archivo ascii
    !=========================================================================
    
    !in
    character(len=*):: file_in

    !out
    integer(kind=4)::count_files_lines

    !local
    integer(kind=4)::flag,unit_number,i,nsamples
    integer::io
    real(kind=4)aux

    !check if filneame is connected to any unit
    !if unit_name=-1 then file_in is not conected and then is opened,
    !otherwise is connected to unit_number
    inquire(file=file_in, number=unit_number)
    
    !if unit_number=-1 then open the file in a new unit
    !if a new unit is open, flag=-1
    !newunit option is used
    if(unit_number==-1)then
       flag=unit_number
       open(newunit=unit_number, &
            file=file_in,&
            action='read')
    end if

    
    count_files_lines=0
    do
       read(unit_number,*,iostat=io)aux
       if(io/=0)exit
       count_files_lines=count_files_lines+1
    end do
    rewind(unit_number)
        
    if(flag==-1)close(unit_number)
    
  end function count_files_lines

   !=========================================================================
  
  function convo(x,y)
    
    !==============================================
    ! Esta subrutina realiza la convoulcion z=x*y |
    !                                             |
    ! entrada x: primera serie                    |
    !         y: segunda serie                    |
    !                                             |
    ! salida  z: convolucion                      |
    !==============================================

    real(kind=4), dimension(:)::x,y
    real(kind=4), dimension(size(x)+size(y)-1)::convo
    integer::i,j

    convo=0.0

    do i=1,size(x)
       do j=1,size(y)
          convo(i+j-1)=convo(i+j-1)+x(i)*y(j)
       end do
    end do

    return

  end function convo

end module tools_mod
