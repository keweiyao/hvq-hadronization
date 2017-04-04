c**********************************************************************c
c                                                                      c
      subroutine writeevent13
c                                                                      c
c     writes the processed event in URQMD file 13 format               c
c                                                                      c
c     last change: 08/06/2009 David Meidinger                          c
c                                                                      c
c**********************************************************************c

      include 'common.f'

      integer i

c**** write event-header **********************************************c

      write (unit=OUTU,fmt='(a58)', err=321) DUMMY(1)
      write (unit=OUTU,fmt='(a68)', err=321) DUMMY(2)
      write (unit=OUTU,fmt='(a69)', err=321) DUMMY(3)
      write (unit=OUTU,fmt='(a94)', err=321) DUMMY(4)
      write (unit=OUTU,fmt='(a101)',err=321) DUMMY(5)
      write (unit=OUTU,fmt='(a101)',err=321) DUMMY(6)
      write (unit=OUTU,fmt='(a77)', err=321) DUMMY(7)
      write (unit=OUTU,fmt='(a77)', err=321) DUMMY(8)
      write (unit=OUTU,fmt='(a77)', err=321) DUMMY(9)
      write (unit=OUTU,fmt='(a158)',err=321) DUMMY(10)
      write (unit=OUTU,fmt='(a158)',err=321) DUMMY(11)
      write (unit=OUTU,fmt='(a158)',err=321) DUMMY(12)
      write (unit=OUTU,fmt='(a158)',err=321) DUMMY(13)
      write (unit=OUTU,fmt='(a171)',err=321) DUMMY(14)
      write (unit=OUTU,fmt=*,       err=321) oeh_npart,DUMMYTIME
      write (unit=OUTU,fmt='(a64)' ,err=321) DUMMY(15)
   
c**** write particles *************************************************c

      do 211 i=1,oeh_npart
         write (unit=OUTU,fmt=922,err=322)
     .        op_r0(i), op_rx(i), op_ry(i), op_rz(i),
     .        op_p0(i), op_px(i), op_py(i), op_pz(i),
     .        op_mass(i), op_ityp(i), op_iso3(i), op_charge(i),
     .        op_lstcoll_13(i), op_ncoll_13(i), op_origin(i),  
     .        op_f0(i), op_fx(i), op_fy(i), op_fz(i),
     .        op_fp0(i), op_fpx(i), op_fpy(i), op_fpz(i) 
 211  continue
      return

c**** errors **********************************************************c

 321  write (ERRU,*) 'ERROR while writing event-header'
      stop     

 322  write (ERRU,*) 'ERROR while writing particle data'
      stop

      end


