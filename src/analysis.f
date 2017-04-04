c**********************************************************************c
c                                                                      c
      subroutine a_init
c                                                                      c
c     initializes all analysis variables                               c
c                                                                      c
c     last change: 08/18/2009 David Meidinger                          c
c                                                                      c
c**********************************************************************c
 
      include 'common.f'

      integer i

      a_q_in_mean=0
      a_B_in_mean=0
      a_e_in_mean=0
      a_px_in_mean=0
      a_py_in_mean=0
      a_pz_in_mean=0
      a_q_in_sigma=0
      a_B_in_sigma=0
      a_e_in_sigma=0
      a_px_in_sigma=0
      a_py_in_sigma=0
      a_pz_in_sigma=0
      a_e_out_mean=0
      a_q_out_mean=0
      a_B_out_mean=0
      a_px_out_mean=0
      a_py_out_mean=0
      a_pz_out_mean=0
      a_e_out_sigma=0
      a_q_out_sigma=0
      a_B_out_sigma=0
      a_px_out_sigma=0
      a_py_out_sigma=0
      a_pz_out_sigma=0

      a_events_in=0
      a_events_out=0

      do 566 i=1,MAXEVENTSIN
         a_q_in(i)=0
         a_B_in(i)=0
         a_e_in(i)=0
         a_px_in(i)=0
         a_py_in(i)=0
         a_pz_in(i)=0
 566  continue

      do 577 i=1,MAXEVENTSOUT
         a_q_out(i)=0
         a_B_out(i)=0
         a_e_out(i)=0
         a_px_out(i)=0
         a_py_out(i)=0
         a_pz_out(i)=0
 577  continue

      return
      end

c**********************************************************************c
c                                                                      c
      subroutine a_addevent_in
c                                                                      c
c     adds observables of the current input event                      c
c     to the analysis arrays                                           c
c                                                                      c
c     last change: 08/18/2009 David Meidinger                          c
c                                                                      c
c**********************************************************************c

      include 'common.f'
      include 'compyth.f'

      integer i

      a_events_in=a_events_in+1

      do 81 i=1,eh_npart
         a_e_in(a_events_in)=a_e_in(a_events_in)+p_p0(i)
         a_px_in(a_events_in)=a_px_in(a_events_in)+p_px(i)
         a_py_in(a_events_in)=a_py_in(a_events_in)+p_py(i)
         a_pz_in(a_events_in)=a_pz_in(a_events_in)+p_pz(i)
         a_q_in(a_events_in)=a_q_in(a_events_in)+PYCHGE(p_id(i))
         if (abs(p_id(i)).LE.6) then
            a_B_in(a_events_in)=a_B_in(a_events_in)+1
         elseif ((abs(p_id(i)).GE.1000).AND.(abs(p_id(i)).LT.10000))
     .      then
            a_B_in(a_events_in)=a_B_in(a_events_in)+3
         endif
  81  continue
      
      a_q_in(a_events_in)=a_q_in(a_events_in)/3
      a_B_in(a_events_in)=a_B_in(a_events_in)/3

      return
      end


c**********************************************************************c
c                                                                      c
      subroutine a_addevent_out
c                                                                      c
c     adds observables of the current output event                     c
c     to the analysis arrays                                           c
c                                                                      c
c     last change: 08/18/2009 David Meidinger                          c
c                                                                      c
c**********************************************************************c

      include 'common.f'
      include 'compyth.f'

      integer i

      a_events_out=a_events_out+1

      do 82 i=1,oeh_npart
         a_e_out(a_events_out)=a_e_out(a_events_out)+op_p0(i)
         a_px_out(a_events_out)=a_px_out(a_events_out)+op_px(i)
         a_py_out(a_events_out)=a_py_out(a_events_out)+op_py(i)
         a_pz_out(a_events_out)=a_pz_out(a_events_out)+op_pz(i)
         a_q_out(a_events_out)=
     .                    a_q_out(a_events_out)+PYCHGE(op_id(i))
         if (abs(op_id(i)).LE.6) then
            a_B_out(a_events_out)=a_B_out(a_events_out)+1
         elseif ((abs(op_id(i)).GE.1000).AND.(abs(op_id(i)).LT.10000))
     .      then
            a_B_out(a_events_out)=a_B_out(a_events_out)+3
         endif
  82  continue

      a_q_out(a_events_out)=a_q_out(a_events_out)/3
      a_B_out(a_events_out)=a_B_out(a_events_out)/3

      return
      end


c**********************************************************************c
c                                                                      c
      subroutine a_analize
c                                                                      c
c     calculates mean values and standard deviations of the            c
c     analysis data and outputs the result                             c
c                                                                      c
c     last change: 08/14/2009 David Meidinger                          c
c                                                                      c
c**********************************************************************c

      include 'common.f'

      integer i

c calculate mean values and standard deviations for input events

      do 83 i=1,a_events_in
         a_e_in_mean=a_e_in_mean+a_e_in(i)
         a_q_in_mean=a_q_in_mean+a_q_in(i)
         a_B_in_mean=a_B_in_mean+a_B_in(i)
         a_px_in_mean=a_px_in_mean+a_px_in(i)
         a_py_in_mean=a_py_in_mean+a_py_in(i)
         a_pz_in_mean=a_pz_in_mean+a_pz_in(i)
  83  continue

      a_e_in_mean=a_e_in_mean/a_events_in
      a_q_in_mean=a_q_in_mean/a_events_in
      a_B_in_mean=a_B_in_mean/a_events_in
      a_px_in_mean=a_px_in_mean/a_events_in
      a_py_in_mean=a_py_in_mean/a_events_in
      a_pz_in_mean=a_pz_in_mean/a_events_in

      do 84 i=1,a_events_in
         a_e_in_sigma=a_e_in_sigma+(a_e_in(i)-a_e_in_mean)**2
         a_q_in_sigma=a_q_in_sigma+(a_q_in(i)-a_q_in_mean)**2
         a_B_in_sigma=a_B_in_sigma+(a_B_in(i)-a_B_in_mean)**2
         a_px_in_sigma=a_px_in_sigma+(a_px_in(i)-a_px_in_mean)**2
         a_py_in_sigma=a_py_in_sigma+(a_py_in(i)-a_py_in_mean)**2
         a_pz_in_sigma=a_pz_in_sigma+(a_pz_in(i)-a_pz_in_mean)**2
  84  continue

      a_e_in_sigma=sqrt(a_e_in_sigma/a_events_in)
      a_q_in_sigma=sqrt(a_q_in_sigma/a_events_in)
      a_B_in_sigma=sqrt(a_B_in_sigma/a_events_in)
      a_px_in_sigma=sqrt(a_px_in_sigma/a_events_in)
      a_py_in_sigma=sqrt(a_py_in_sigma/a_events_in)
      a_pz_in_sigma=sqrt(a_pz_in_sigma/a_events_in)

c calculate mean values and standard deviations for output events

      do 85 i=1,a_events_out
         a_e_out_mean=a_e_out_mean+a_e_out(i)
         a_q_out_mean=a_q_out_mean+a_q_out(i)
         a_B_out_mean=a_B_out_mean+a_B_out(i)
         a_px_out_mean=a_px_out_mean+a_px_out(i)
         a_py_out_mean=a_py_out_mean+a_py_out(i)
         a_pz_out_mean=a_pz_out_mean+a_pz_out(i)
  85  continue

      a_e_out_mean=a_e_out_mean/a_events_out
      a_q_out_mean=a_q_out_mean/a_events_out
      a_B_out_mean=a_B_out_mean/a_events_out
      a_px_out_mean=a_px_out_mean/a_events_out
      a_py_out_mean=a_py_out_mean/a_events_out
      a_pz_out_mean=a_pz_out_mean/a_events_out

      do 86 i=1,a_events_out
         a_e_out_sigma=a_e_out_sigma+(a_e_out(i)-a_e_out_mean)**2
         a_q_out_sigma=a_q_out_sigma+(a_q_out(i)-a_q_out_mean)**2
         a_B_out_sigma=a_B_out_sigma+(a_B_out(i)-a_B_out_mean)**2
         a_px_out_sigma=a_px_out_sigma+(a_px_out(i)-a_px_out_mean)**2
         a_py_out_sigma=a_py_out_sigma+(a_py_out(i)-a_py_out_mean)**2
         a_pz_out_sigma=a_pz_out_sigma+(a_pz_out(i)-a_pz_out_mean)**2
  86  continue

      a_e_out_sigma=sqrt(a_e_out_sigma/a_events_out)
      a_q_out_sigma=sqrt(a_q_out_sigma/a_events_out)
      a_B_out_sigma=sqrt(a_B_out_sigma/a_events_out)
      a_px_out_sigma=sqrt(a_px_out_sigma/a_events_out)
      a_py_out_sigma=sqrt(a_py_out_sigma/a_events_out)
      a_pz_out_sigma=sqrt(a_pz_out_sigma/a_events_out)

c output the results

      write (unit=ANAU,fmt='(7X,A64)',err=888)
     ." input.......................... output........................."
      write (unit=ANAU,fmt='("events ",2I32)',err=888)
     .        a_events_in, a_events_out
      write (unit=ANAU,fmt='(7X,2A32)',err=888)
     ." mean........... sigma..........",
     ." mean........... sigma.........."
      write (unit=ANAU,fmt='("E_tot  ",4E16.8)',err=888) 
     .       a_e_in_mean, a_e_in_sigma, a_e_out_mean, a_e_out_sigma
      write (unit=ANAU,fmt='("px_tot ",4E16.8)',err=888) 
     .       a_px_in_mean, a_px_in_sigma, a_px_out_mean, a_px_out_sigma
      write (unit=ANAU,fmt='("py_tot ",4E16.8)',err=888) 
     .       a_py_in_mean, a_py_in_sigma, a_py_out_mean, a_py_out_sigma
      write (unit=ANAU,fmt='("pz_tot ",4E16.8)',err=888) 
     .       a_pz_in_mean, a_pz_in_sigma, a_pz_out_mean, a_pz_out_sigma
      write (unit=ANAU,fmt='("Q_tot  ",4F16.3)',err=888) 
     .       a_q_in_mean, a_q_in_sigma, a_q_out_mean, a_q_out_sigma
      write (unit=ANAU,fmt='("B_tot  ",4F16.3)',err=888) 
     .       a_B_in_mean, a_B_in_sigma, a_B_out_mean, a_B_out_sigma

c output number of pythia errors

      write(unit=ANAU,fmt='(/,"PYTHIA errors encountered:",I7)',err=888)
     .       pyth_errors
      write(unit=ANAU,fmt='("PYTHIA errors resolved:   ",I7)',err=888)
     .       pyth_errors_resolved

      return

c**** ERRORS **********************************************************c

 888  write(ERRU,*) "ERROR while writing analysis file"
      stop

      end


 

