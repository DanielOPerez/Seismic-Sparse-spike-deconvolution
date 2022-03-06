program ss_decon_main


  use ebcdic_reel_header_mod, only:w_ebch_file,&
       r_ebch_array
  use binary_reel_header_mod, only:rw_binh,&
       r_binh_array,&
       w_binh_file
  use trace_reel_header_mod, only:w_trch_file,&
       r_trch_array 
  use traces_mod, only:count_traces,&
       maxval_traces,&
       r_trace,&
       w_trace
  
  use fista_mod, only:fista
  use tools_mod, only:count_files_lines

  
  implicit none

  character(len=32):: filename_in,filename_out,filename_wav,char_aux
  real(kind=4)::mval,mval_tmp,lambda,cut_msft
  real(kind=4), allocatable::wavelet(:),ref(:)
  integer(kind=4)::i,nt,ns,nw,nr,nc,o_prt
  integer::itmax

  !se leen las opciones del progama
  open(unit=100,file='ss_decon_in',action='read')
  read(100,*)char_aux,filename_in     !archivo de entrada
  read(100,*)char_aux,filename_out    !archivo de salida
  read(100,*)char_aux,filename_wav    !ondicula
  read(100,*)char_aux,lambda          !lambda
  read(100,*)char_aux,itmax           !nro max de iteraciones
  read(100,*)char_aux,cut_msft        !minimo misfit
  read(100,*)char_aux,o_prt           !cada cuantas iter. muestra resultados
  close(100)

  open(unit=100, &
       file=filename_in,&
       form = 'unformatted', &
       access = 'stream',&
       convert = 'big_endian')

  open(unit=200, &
       file=filename_out,&
       form = 'unformatted', &
       access = 'stream',&
       convert = 'big_endian')

 
  
  !numero de muestras por traza
  ns=rw_binh(filename_in,'ns')
  !numero total de trazas del archivo de entrada
  nt=count_traces(filename_in,ns)
  !se encuentra el valor maximo (en valor absoluto) de una region del dato
  mval=maxval_traces(filename_in,1,nt,1,ns)
  !numero de muestras de la ondicula
  nw=count_files_lines(filename_wav)
  !numero de reflectividades
  nr=ns-nw+1
  nc=floor(nw/2.0)+1

  
  allocate(wavelet(nw),&
       ref(ns))
  
  
  !se lee la ondicula
  open(unit=300,file=filename_wav,action='read')
  do i=1,nw
     read(300,*)wavelet(i)
  end do
  close(300)
  
  !Leo el ebdic header y lo guardo en el archivo de salida
  call w_ebch_file(r_ebch_array(filename_in),filename_out)
  !Leo el binary header y lo guardo en el archivo de salida
  call w_binh_file(r_binh_array(filename_in),filename_out)
  
  do i=1,nt
     write(*,*)'trace:',i
     ref=0.0
     !se llama al fista
     call fista(r_trace(filename_in,i,1,ns)/mval,&
          wavelet,&
          lambda,&
          itmax,&
          cut_msft,&
          o_prt,&
          ref(nc:nc+nr-1))
     !guardo el trace header en la salida
     call w_trch_file(r_trch_array(filename_in,i),i,filename_out)
     !se guardan las reflectividades en el archivo de salida
     call w_trace(filename_out,ref,i)
      
  end do


  
  deallocate(wavelet,&
       ref)
    
  close(100)
  close(200)
end program ss_decon_main
