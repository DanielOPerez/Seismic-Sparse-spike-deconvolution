module traces_mod

  use word_definitions_mod, only:tw_name,tw_bsize,&
       n_ebcdich,n_binaryh,n_traceh
  use trace_reel_header_mod, only:rw_trch_file
  use read_segy_tools_mod, only:check_open_segy_file
  
  implicit none

contains

  function r_trace(filename,ntrace,nsmin,nsmax)

    !in 
    character(len=*), intent(in)::filename
    integer(kind=4), intent(in)::ntrace,nsmin,nsmax

    !out
    real(kind=4), dimension(nsmax-nsmin+1)::r_trace

    !local
    integer(kind=4)::flag,unit_number,i,nsamples
    
    !check if smin<=0 and smax>nsamples
    nsamples=rw_trch_file(filename,ntrace,'ns')
    if((nsmin.lt.1).or.(nsmax.gt.nsamples))then
       write(*,*)'Range (nsmin,nsmax) out of trace boundaries'
       stop
    end if

    !check if filneame is connected to any unit
    !if flag==-1, a new unit was openend and must be closed at program end
    call check_open_segy_file(filename,unit_number,flag)

    !read trace samples
    do i=nsmin,nsmax
       read(unit=unit_number,&
            pos=(n_ebcdich+n_binaryh+n_traceh)+(ntrace-1)*(n_traceh+4*nsamples)+1+4*(i-1))r_trace(i-nsmin+1)
    end do
       

    if(flag==-1)close(unit_number) 

    return
    
  end function r_trace

  !==========================================================================================================

  subroutine w_trace(filename,trace,ntrace)

    !in
    character(len=*), intent(in)::filename
    integer(kind=4), intent(in)::ntrace
    real(kind=4), intent(in), dimension(:)::trace
    
    !local
    integer(kind=4)::flag,unit_number,i,nsamples


    nsamples=size(trace)
    
    !check if filneame is connected to any unit
    !if flag==-1, a new unit was openend and must be closed at program end
    call check_open_segy_file(filename,unit_number,flag)  

    !write trace samples to file
    do i=1,nsamples
       write(unit=unit_number,&
            pos=(n_ebcdich+n_binaryh+n_traceh)+(ntrace-1)*(n_traceh+4*nsamples)+1+4*(i-1))trace(i)
    end do
       

    if(flag==-1)close(unit_number) 
    
  end subroutine w_trace


  function count_traces(filename,ns)

    !in
    character(len=*), intent(in)::filename
    integer(kind=4), intent(in)::ns

    !out
    integer(kind=4)::count_traces

    !local
    integer, dimension(13)::buff
    integer:: status
    
    call stat(filename, buff, status)

    count_traces=(buff(8)-n_ebcdich-n_binaryh)/(n_traceh+ns*4)
    
    return

  end function count_traces

  function maxval_traces(filename,tmin,tmax,smin,smax)

    !in
    character(len=*), intent(in)::filename
    integer(kind=4), intent(in)::tmin,tmax,smin,smax

    !out
    integer(kind=4)::maxval_traces

    !local
    integer::i
    real(kind=4)::mval_tmp
    
    maxval_traces=maxval(abs(r_trace(filename,tmin,smin,smax)))
    do i=tmin+1,tmax
       mval_tmp=maxval(abs(r_trace(filename,i,smin,smax)))
       if(mval_tmp.gt.maxval_traces)maxval_traces=mval_tmp
    end do
    
  end function maxval_traces

  
end module traces_mod

