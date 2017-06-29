c**********************************************************************c
c                                                                      c
      subroutine writeeventoscar
c                                                                      c
c    writes the processed event in OSCAR 1997A format                  c
c                                                                      c
c    last change: 08/06/2009 David Meidinger                           c
c                                                                      c
c**********************************************************************c

      include 'common.f'

      integer j

c**** write event-header **********************************************c

c      write (unit=OUTU,fmt=911,err=321) 
c     .     eh_event, oeh_npart, eh_imppar, eh_angle, eh_ntim, eh_itim,
c     .     eh_evsamp

      eh_imppar=0d0
      eh_angle=0d0
      eh_ntim=1
      eh_itim=1

      write (unit=OUTU,fmt=911,err=321) 
     .     eh_event, oeh_npart, eh_imppar, eh_angle, eh_ntim, eh_itim,
     .     NUMSAMP

c      write (unit=OUTU,fmt=*,err=321) eh_event,oeh_npart
c      write (unit=6,fmt=*,err=321) eh_event,oeh_npart

c**** write particles *************************************************c

      j=1
      do while (j.le.oeh_npart)
         write (unit=OUTU,fmt=921,err=322)
     .        op_num(j), op_id(j), op_px(j), op_py(j), op_pz(j), 
     .        op_p0(j), op_mass(j), op_rx(j), op_ry(j), op_rz(j),
     .        op_r0(j), oThydro(j), oc_vx(j), oc_vy(j), oc_vz(j),
     .        op_ipx(j), op_ipy(j), op_ipz(j), op_ip0(j)

c         write (unit=OUTU,fmt=*,err=322)
cc         write (unit=6,fmt=*,err=322)
c     .        op_id(j), op_px(j), op_py(j), op_pz(j) 
         j=j+1

      enddo

      return

c**** errors **********************************************************c

 321  write (ERRU,*) 'ERROR while writing event-header'
      stop    

 322  write (ERRU,*) 'ERROR while writing particle data'
      stop

      end

