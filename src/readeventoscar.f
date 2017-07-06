c**********************************************************************c
c                                                                      c
      subroutine readeventoscar
c                                                                      c
c     reads an event in OSCAR 1997A format                             c
c                                                                      c
c     last change: 08/06/2009 David Meidinger                          c
c                                                                      c
c**********************************************************************c

      include 'common.f'

      integer i

c**** read event-header ***********************************************c

      read (unit=INU,fmt=911,err=311,end=310) 
     .     eh_event, eh_npart, eh_imppar, eh_angle, eh_ntim, eh_itim,
     .     eh_evsamp

c**** read particles **************************************************c

      do 111 i=1,eh_npart
         read (unit=INU,fmt=921,err=312,end=312)
     .        p_num(i), p_id(i), p_px(i), p_py(i), p_pz(i), p_p0(i),
     .        p_mass(i), p_rx(i), p_ry(i), p_rz(i), p_r0(i), 
     .        Thydro(i), c_vx(i), c_vy(i), c_vz(i),
     .        p_ipx(i), p_ipy(i), p_ipz(i), p_ip0(i), 
     .        p_s1(i), p_s2(i)
         p_fx(i)=p_rx(i)
         p_fy(i)=p_ry(i)
         p_fz(i)=p_rz(i)
         p_f0(i)=p_r0(i)

 111  continue

      return

c**** EOF reached at an appropriate position **************************c
 
 310  call endprog  

c**** errors **********************************************************c

 311  write (ERRU,*) 'ERROR while reading event-header'
      call endprog

 312  write (ERRU,*) 'ERROR while reading particle data'
      call endprog

      end

