module binary_reel_header_mod

  use word_definitions_mod, only:bw_name,&
       bw_bsize,&
       n_ebcdich,&
       n_binaryh
  use read_segy_tools_mod, only:check_open_segy_file
  implicit none

  interface rw_binh
     module procedure rw_binh_array,&
          rw_binh_file,&
          rw_binh_2byte_file,&
          rw_binh_4byte_file
  end interface rw_binh

  interface chw_binh
     module procedure chw_binh_array, chw_binh_file
  end interface chw_binh

contains

  !read binary header to array from file  
  function r_binh_array(filename)

    !in 
    character(len=*), intent(in):: filename

    !out
    integer(kind=4), dimension(n_binaryh)::r_binh_array

    !local
    integer(kind=4)::i,j,unit_number,flag,sum_bsize
    integer(kind=4), dimension(1)::word_pos
    integer(kind=2)::word2b


    !check if filneame is connected to any unit
    !if flag==-1, a new unit was openend and must be closed at program end
    call check_open_segy_file(filename,unit_number,flag)

    sum_bsize=0
    r_binh_array=0

    do i=1,size(bw_name)

       !read word_in value as int*2 or int*4 according binaryw_size
       !the ouptut is int*4 always
       sum_bsize=sum(bw_bsize(1:i-1))
       select case(bw_bsize(i))
       case(2)
          read(unit=unit_number,pos=n_ebcdich+sum_bsize+1)word2b
          r_binh_array(i)=int(word2b,kind=4)
       case(4)
          read(unit=unit_number,pos=n_ebcdich+sum_bsize+1)&
               r_binh_array(i)
       end select

    end do

    if(flag==-1)close(unit_number)

    return

  end function r_binh_array

  !=====================================================================

  !read binary header word from array
  function rw_binh_array(binaryh_in,word_in)

    !in
    integer(kind=4), intent(in), dimension(:)::binaryh_in
    character(len=*), intent(in)::word_in

    !out
    integer(kind=4)::rw_binh_array

    !local
    integer(kind=4)::i
    integer(kind=4), dimension(1)::word_pos


    !check if word exists
    if(any(bw_name==word_in)) then

       !read word_in value as int*2 or int*4 according binaryw_size
       !the ouptut is int*4 always

       word_pos=pack([(i,i=1,size(bw_name))],bw_name==word_in)
       rw_binh_array=binaryh_in(word_pos(1))

       return

    else

       write(*,*)"Error: no such binary word:",word_in
       stop

    end if


  end function rw_binh_array


  !=====================================================================

  !change binary header word in array
  subroutine chw_binh_array(binaryh_in,word_in,val_in)

    !in
    integer(kind=4), intent(inout), dimension(:)::binaryh_in
    integer(kind=4), intent(in):: val_in
    character(len=*), intent(in)::word_in

    !local
    integer(kind=4)::i
    integer(kind=4), dimension(1)::word_pos


    !check if word exists
    if(any(bw_name==word_in)) then

       !read word_in value as int*2 or int*4 according binaryw_size
       !the ouptut is int*4 always

       word_pos=pack([(i,i=1,size(bw_name))],bw_name==word_in)
       binaryh_in(word_pos(1))=val_in

       return

    else

       write(*,*)"Error: no such binary word:",word_in
       stop

    end if

  end subroutine chw_binh_array

  !=====================================================================

  !write binary header from array to file
  subroutine w_binh_file(binaryh_in,filename)

    !in 
    character(len=*), intent(in):: filename
    integer(kind=4), intent(in), dimension(:)::binaryh_in

    !local
    integer(kind=4)::i,unit_number,flag,sum_bsize

    !check if filneame is connected to any unit
    !if flag==-1, a new unit was openend and must be closed at program end
    call check_open_segy_file(filename,unit_number,flag)


    do i=1,size(bw_name)

       !read word_in value as int*2 or int*4 according binaryw_size
       !the ouptut is int*4 always
       sum_bsize=sum(bw_bsize(1:i-1))

       select case(bw_bsize(i))
       case(2)
          write(unit=unit_number,pos=n_ebcdich+sum_bsize+1)&
               int(binaryh_in(i),kind=2)
       case(4)
          write(unit=unit_number,pos=n_ebcdich+sum_bsize+1)&
               binaryh_in(i)
       end select

    end do

    !zeros to complete the header 
    do i=sum(bw_bsize),n_binaryh-1
       write(unit=unit_number,pos=n_ebcdich+i+1)&
            int(0,kind=1)
    end do


    if(flag==-1)close(unit_number)       


  end subroutine w_binh_file


  !=====================================================================

  !read binary header word from file
  function rw_binh_file(filename,word_in)

    !in 
    character(len=*), intent(in):: filename,word_in

    !out
    integer(kind=4)::rw_binh_file

    !local
    integer(kind=4)::i,unit_number,flag,sum_bsize
    integer(kind=4), dimension(1)::word_pos
    integer(kind=2)::word2b

    sum_bsize=0

    !check if word exists
    if(any(bw_name==word_in)) then

       !check if filneame is connected to any unit
       !if flag==-1, a new unit was openend and must be closed at program end
       call check_open_segy_file(filename,unit_number,flag)

       !check word_in position inside binaryw_name
       word_pos=pack([(i,i=1,size(bw_name))],bw_name==word_in)

       !read word_in value as int*2 or int*4 according binaryw_size
       !the ouptut is int*4 always
       sum_bsize=sum(bw_bsize(1:word_pos(1)-1))
       select case(bw_bsize(word_pos(1)))
       case(2)
          read(unit=unit_number,pos=n_ebcdich+sum_bsize+1)word2b
          rw_binh_file=int(word2b,kind=4)
       case(4)
          read(unit=unit_number,pos=n_ebcdich+sum_bsize+1)rw_binh_file
       end select

       if(flag==-1)close(unit_number)       

       return

    else

       write(*,*)"Error: no such binary word:",word_in
       stop

    end if


  end function rw_binh_file

    !=====================================================================

  !read binary header word from file, at a given byte
  function rw_binh_2byte_file(filename,byte_in)

    !in 
    character(len=*), intent(in):: filename
    integer(kind=2), intent(in)::byte_in

    !out
    integer(kind=2)::rw_binh_2byte_file

    !local
    integer(kind=4)::i,unit_number,flag,sum_bsize
    integer(kind=4), dimension(1)::word_pos
    
    if(byte_in+2.gt.n_binaryh)then
       write(*,*)'Byte out of boundaries'
       stop
    end if

    !check if filneame is connected to any unit
    !if flag==-1, a new unit was openend and must be closed at program end
    call check_open_segy_file(filename,unit_number,flag)

    read(unit=unit_number,pos=n_ebcdich+byte_in)rw_binh_2byte_file
    
    if(flag==-1)close(unit_number)   

    return

  end function rw_binh_2byte_file

  !=====================================================================

  !read binary header word from file, at a given byte
  function rw_binh_4byte_file(filename,byte_in)

    !in 
    character(len=*), intent(in):: filename
    integer(kind=4), intent(in)::byte_in

    !out
    integer(kind=4)::rw_binh_4byte_file

    !local
    integer(kind=4)::i,unit_number,flag,sum_bsize
    integer(kind=4), dimension(1)::word_pos
    
    if(byte_in+4.gt.n_binaryh)then
       write(*,*)'Byte out of boundaries'
       stop
    end if

    !check if filneame is connected to any unit
    !if flag==-1, a new unit was openend and must be closed at program end
    call check_open_segy_file(filename,unit_number,flag)

    read(unit=unit_number,pos=n_ebcdich+byte_in)rw_binh_4byte_file
    
    if(flag==-1)close(unit_number)   

    return

  end function rw_binh_4byte_file

  !=====================================================================

  !change binary header word in file
  subroutine chw_binh_file(filename,word_in,val_in)

    !in 
    character(len=*), intent(in):: filename,word_in
    integer(kind=4), intent(in):: val_in

    !local
    integer(kind=4)::i,unit_number,flag,sum_bsize
    integer(kind=4), dimension(1)::word_pos

    !check if word exists
    if(any(bw_name==word_in)) then

       !check if filneame is connected to any unit
       !if flag==-1, a new unit was openend and must be closed at program end
       call check_open_segy_file(filename,unit_number,flag)

       word_pos=pack([(i,i=1,size(bw_name))],bw_name==word_in)

       !read word_in value as int*2 or int*4 according binaryw_size
       !the ouptut is int*4 always
       sum_bsize=sum(bw_bsize(1:word_pos(1)-1))
       select case(bw_bsize(word_pos(1)))
       case(2)
          write(unit=unit_number,pos=n_ebcdich+sum_bsize+1)int(val_in,2)
       case(4)
          write(unit=unit_number,pos=n_ebcdich+sum_bsize+1)val_in
       end select


       if(flag==-1)close(unit_number) 

    else

       write(*,*)"Error: no such binary word:",word_in
       stop

    end if

  end subroutine chw_binh_file

  !=====================================================================

  !print binary header from file, to another file or screen
  subroutine print_binary_header(filename,file_out)

    !in
    character(len=*), intent(in)::filename,file_out

    !local
    integer(kind=4)::unit_number,i
    character(len=100), dimension(27)::def_array
    !check output unit
    if(file_out=='screen')then
       unit_number=6
    else
       open(newunit=unit_number,file=file_out,action='write')
    end if


    def_array=(/character(len=100)::"Job identification number",&
         "Line number",&
         "Reel number",&
         "Number of data traces per record",&
         "Number of auxiliary traces per record",&
         "Sample interval of this reel's data in microseconds",&
         "Sample interval of original field recording in microseconds",&
         "Number of samples per trace for this reel's data",&
         "Number of samples per trace in original field recording",&
         "Data sample format code",&
         "CDP fold",&
         "Trace sorting code",&
         "Vertical sum code",&
         "Sweep frequency at start in Hertz",&
         "Sweep frequency at end in Hertz",&
         "Sweep length in milliseconds",&
         "Sweep type code",&
         "Trace number of sweep channel",&
         "Sweep trace taper length at start in milliseconds",&
         "Sweep trace taper length at end in milliseconds",&
         "Taper type code",&
         "Correlated data traces",&
         "Binary gain recovered",&
         "Amplitude recovery method code",&
         "Measurement system",&
         "Impulse signal polarity",&
         "Vibratory polarity code"/)

    do i=1,size(def_array)
       write(unit_number,100)rw_binh_file(filename,bw_name(i)),def_array(i)
    end do

100 format(I10,2X,A65,1X)

  end subroutine  print_binary_header


  !=====================================================================


end module binary_reel_header_mod


