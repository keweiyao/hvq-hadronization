c**********************************************************************c
c                                                                      c
      subroutine readevent13
c                                                                      c
c     reads an event in URQMD file 13 format                           c
c                                                                      c
c     last change: 08/13/2009 David Meidinger                          c
c                                                                      c
c**********************************************************************c

      include 'common.f'

      integer i, pdgid

c**** read event-header ***********************************************c

      if (firstline.EQ.0) then
         read (unit=INU,fmt='(a58)',err=311,end=310) DUMMY(1)
      else
         read (unit=inputline,fmt='(a58)',err=311,end=311) DUMMY(1)
         firstline=0
      endif
      read (unit=INU,fmt='(a68)', err=311,end=311) DUMMY(2)
      read (unit=INU,fmt='(a69)', err=311,end=311) DUMMY(3)
      read (unit=INU,fmt='(a94)', err=311,end=311) DUMMY(4)
      read (unit=INU,fmt='(a101)',err=311,end=311) DUMMY(5)
      read (unit=INU,fmt='(a101)',err=311,end=311) DUMMY(6)
      read (unit=INU,fmt='(a77)', err=311,end=311) DUMMY(7)
      read (unit=INU,fmt='(a77)', err=311,end=311) DUMMY(8)
      read (unit=INU,fmt='(a77)', err=311,end=311) DUMMY(9)
      read (unit=INU,fmt='(a158)',err=311,end=311) DUMMY(10)
      read (unit=INU,fmt='(a158)',err=311,end=311) DUMMY(11)
      read (unit=INU,fmt='(a158)',err=311,end=311) DUMMY(12)
      read (unit=INU,fmt='(a158)',err=311,end=311) DUMMY(13)
      read (unit=INU,fmt='(a171)',err=311,end=311) DUMMY(14)
      read (unit=INU,fmt=*,       err=311,end=311) eh_npart,DUMMYTIME
      read (unit=INU,fmt='(a64)', err=311,end=311) DUMMY(15)

c**** read particles **************************************************c

      do 111 i=1,eh_npart
         read (unit=INU,fmt=922,err=312,end=312)
     .        p_r0(i), p_rx(i), p_ry(i), p_rz(i),
     .        p_p0(i), p_px(i), p_py(i), p_pz(i),
     .        p_mass(i), p_ityp(i), p_iso3(i), p_charge(i),
     .        p_lstcoll_13(i), p_ncoll_13(i), p_origin(i),  
     .        p_f0(i), p_fx(i), p_fy(i), p_fz(i),
     .        p_fp0(i), p_fpx(i), p_fpy(i), p_fpz(i)
c get PDG ID
         p_id(i)=pdgid(p_ityp(i),p_iso3(i))
 111  continue

      return

c**** EOF reached at an appropriate position **************************c
 
 310  call endprog

c**** errors **********************************************************c

 311  write (ERRU,*) 'ERROR while reading event-header1'
      call endprog     

 312  write (ERRU,*) 'ERROR while reading particle data'
      call endprog
 
      end

