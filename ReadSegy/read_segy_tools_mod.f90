module read_segy_tools_mod

  use word_definitions_mod, only:&
       ebcdic_table
  

  
contains

  !==========================================================================

  function ebcdic_to_ascii(ebcdic_in)

    !in
    character(len=1)::ebcdic_in

    !out
    character(len=1)::ebcdic_to_ascii

    !local
    character(len=1)::aux
    integer (kind=4)::i
  
    ebcdic_to_ascii=char(ebcdic_table(ichar(ebcdic_in)+1))
  
    return
    
  end function ebcdic_to_ascii

  !==========================================================================

  function ascii_to_ebcdic(ascii_in)

    !in
    character(len=1)::ascii_in

    !out
    character(len=1)::ascii_to_ebcdic

    !local
    integer(kind=4)::i
    integer(kind=4), dimension(1)::pos
    
    pos=pack([(i,i=1,256)],ebcdic_table==(ichar(ascii_in)))
    ascii_to_ebcdic=char(pos(1)-1)
        
    
    return
    
  end function ascii_to_ebcdic

  !==========================================================================

  subroutine check_file_exists(file_in)
    
    character(len=*):: file_in
    ! variables de la info del archivo
    integer, dimension(13) :: buff
    integer:: status

    call stat(file_in, buff, status)
    if (status /= 0 )then
       write(*,*)"The file do not exists or is not readable."
       stop
    end if
    
  end subroutine check_file_exists

  !==========================================================================
    
  subroutine check_open_segy_file(file_in,unit_number,flag)
    
    character(len=*):: file_in
    integer(kind=4)::unit_number,flag
    
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
            form = 'unformatted', &
            access = 'stream',&
            convert = 'native')
    end if
    
  end subroutine check_open_segy_file
  
  
end module read_segy_tools_mod
