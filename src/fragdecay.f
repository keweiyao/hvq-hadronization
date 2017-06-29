c**********************************************************************c
c                                                                      c
      subroutine fragdecay
c                                                                      c
c     uses PYTHIA to decay a particle                                  c
c                                                                      c
c     last change: 08/17/2009 David Meidinger                          c 
c                                                                      c
c**********************************************************************c

      include 'common.f'
      include 'compyth.f'

      integer i, j
      double precision t
      logical err,unresolved      

      pyth_try=1
      err=.false.
      unresolved=.false.
      
c reset event record                 
 345  N=1

c load particle into event record
      K(1,1)=1
      K(1,2)=p_id(ipin)
      K(1,3)=0
      K(1,4)=0
      K(1,5)=0
      P(1,1)=p_px(ipin)
      P(1,2)=p_py(ipin)
      P(1,3)=p_pz(ipin)
      P(1,4)=p_p0(ipin)
      P(1,5)=p_mass(ipin)
      V(1,1)=p_fx(ipin)*1.0D-12
      V(1,2)=p_fy(ipin)*1.0D-12
      V(1,3)=p_fz(ipin)*1.0D-12
      V(1,4)=p_f0(ipin)*1.0D-12
      V(1,5)=0
       
c let pythia decay it
      CALL PYEXEC

c check for pythia errors
      if (MSTU(24).GT.0) then
         err=.true.
         if (pyth_try.GT.MAXRETRIES) then
            unresolved=.true.
         else
            pyth_try=pyth_try+1
            goto 345
         endif      
      endif    
      if (err) then 
         pyth_errors=pyth_errors+1
         if(.NOT.unresolved) pyth_errors_resolved=pyth_errors_resolved+1
      endif

c process the event record 

c      call PYLIST(1)     

      do 55 i=1,N
 
c skip neutrinos
         if (NONEUTRINOS.AND.((abs(K(i,2)).EQ.12).OR.
     .      (abs(K(i,2)).EQ.14).OR.(abs(K(i,2)).EQ.16))) goto 55
c skip particles that no longer exist             
         if ((K(i,1).LT.1).OR.(K(i,1).GT.10)) goto 55  
c if ONLYHMELECTRONS is set to true, skip electrons or positrons
c whos parent is not  in hmlist
         if (ONLYHMELECTRONS.AND.(abs(K(i,2)).EQ.11)) then
            do 66 j=1,HMNUM
               if (abs(K(K(i,3),2)).EQ.hmlist(j)) goto 77
  66        continue
            goto 55
         endif
c otherwise read out particle data
  77     op_num(ipout)=ipout
         op_id(ipout)=K(i,2)
         call pdg2id(op_ityp(ipout),op_iso3(ipout),op_id(ipout))
         op_charge(ipout)=PYK(i,6)/3

         op_px(ipout)=P(i,1)
         op_py(ipout)=P(i,2)
         op_pz(ipout)=P(i,3)
         op_p0(ipout)=P(i,4)

         op_fpx(ipout)=P(i,1)
         op_fpy(ipout)=P(i,2)
         op_fpz(ipout)=P(i,3)
         op_fp0(ipout)=P(i,4)

         op_mass(ipout)=P(i,5)

         op_fx(ipout)=V(i,1)*1.0D12
         op_fy(ipout)=V(i,2)*1.0D12
         op_fz(ipout)=V(i,3)*1.0D12
         op_f0(ipout)=V(i,4)*1.0D12

c if the particle was created within the original simulation time
c let it propagate
         t=p_r0(ipin)-op_f0(ipout)
         if (t.GT.0) then
            op_rx(ipout)=op_fx(ipout)+(op_fpx(ipout)/op_fp0(ipout))*t
            op_ry(ipout)=op_fy(ipout)+(op_fpy(ipout)/op_fp0(ipout))*t
            op_rz(ipout)=op_fz(ipout)+(op_fpz(ipout)/op_fp0(ipout))*t
            op_r0(ipout)=p_r0(ipin)
c if not, then production vertex = position
         else
            op_rx(ipout)=op_fx(ipout)
            op_ry(ipout)=op_fy(ipout)
            op_rz(ipout)=op_fz(ipout)
            op_r0(ipout)=op_f0(ipout)
         endif
        
         op_ncoll_13(ipout)=1
         op_ncoll_osc(ipout)=1
         op_lstcoll_13(ipout)=0
         op_lstcoll_osc(ipout)=0
         op_origin(ipout)=p_origin(ipin)

         oThydro(ipout)=Thydro(ipin)
         oc_vx(ipout)=c_vx(ipin)
         oc_vy(ipout)=c_vy(ipin)
         oc_vz(ipout)=c_vz(ipin)

c pass the weight information to the daughter particles
         op_ip0(ipout)=p_ip0(ipin)
         op_ipx(ipout)=p_ipx(ipin)
         op_ipy(ipout)=p_ipy(ipin)
         op_ipz(ipout)=p_ipz(ipin)

c         if(op_id(ipout).eq.11.or.op_id(ipout).eq.-11) 
c     .       ipout=ipout+1

         if(abs(op_id(ipout)).eq.411.or.abs(op_id(ipout)).eq.421 
     .    .or.abs(op_id(ipout)).eq.413.or.abs(op_id(ipout)).eq.423) then
c     .    .or.abs(op_id(ipout)).eq.431.or.abs(op_id(ipout)).eq.433)
            ipout=ipout+1
            judge_HM=1
         endif

         if(abs(op_id(ipout)).eq.511.or.abs(op_id(ipout)).eq.521
     .    .or.abs(op_id(ipout)).eq.513.or.abs(op_id(ipout)).eq.523) then
            ipout=ipout+1
            judge_HM=1
         endif

  55  continue


      return
      end
