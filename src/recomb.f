ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       subroutine force_recomb(hadrflag,lightFlavor)
c do recombination of heavy-light quarks
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       include 'common.f' 
      
       integer hadrflag,i,loopcount,lightFlavor
       double precision rlu,phi,utheta,temp,plight
       double precision kmin,kmin2,plmax,plmaxcm,gacell,gacm
       double precision becell,becm,Elmax,Elmaxcm,pHQ,dum_random

c generate one light parton according to thermal distribution
       temp=Thydro(ipin)
       if(temp.lt.0.1d0) return

       if(lightFlavor.eq.1) then
          mlight=m_ud
       elseif(lightFlavor.eq.3) then
          mlight=m_s
       else
          write(6,*) "Flavor not available!"
          stop
       endif

       plmax=15d0*temp
       Elmax=sqrt(plmax**2+mlight**2)
       becell=sqrt(c_vx(ipin)**2+c_vy(ipin)**2+c_vz(ipin)**2)
       gacell=1d0/sqrt(1d0-becell**2)
       plmaxcm=gacell*(plmax+becell*Elmax)
       Elmaxcm=gacell*(Elmax+becell*plmax)
       pHQ=sqrt(p_p0(ipin)**2-p_mass(ipin)**2)
       becm=(pHQ+plmaxcm)/(p_p0(ipin)+Elmaxcm)
       gacm=1d0/sqrt(1d0-becm**2)
       kmin=gacm*(pHQ-becm*p_p0(ipin))
       kmin2=kmin**2
       if(pHQ.lt.plmaxcm) kmin2=-1d0

c calculate parameters for Wigner function
       reducedM0=mlight*p_mass(ipin)/(mlight+p_mass(ipin))
       sigma0=1d0/sqrt(reducedM0*omega_HM)

       loopcount=0

       do while(hadrflag.eq.1)
          call judgehadmech2(hadrflag,kmin2)
          loopcount=loopcount+1
          if(loopcount.gt.1D8) then
             write(6,*) "too many loops, no recomb"
             return
          endif
       enddo

c do recombination if hadrflag=2
       op_num(ipout)=ipout
       op_px(ipout)=p_px(ipin)+px_light
       op_py(ipout)=p_py(ipin)+py_light
       op_pz(ipout)=p_pz(ipin)+pz_light
       
c       if(abs(p_id(ipin)).eq.4) then ! c quark
c          if(lightFlavor.eq.1) then
c             op_id(ipout)=4210 ! D0 or D+
c             op_mass(ipout)=1.9d0
c          elseif(lightFlavor.eq.3) then
c             op_id(ipout)=4310 ! Ds+
c             op_mass(ipout)=2.1d0
c          endif
c       else
c          write(6,*) "HF not available!"
c          stop
c       endif

       dum_random=rlu(0)
       if(abs(p_id(ipin)).eq.4) then ! c quark
          if(dum_random.lt.1d0/8d0) then
             op_id(ipout)=411 ! D+
             op_mass(ipout)=1.87d0
          elseif(dum_random.lt.2d0/8d0) then
             op_id(ipout)=421 ! D0
             op_mass(ipout)=1.86d0
          elseif(dum_random.lt.5d0/8d0) then
             op_id(ipout)=413 ! D*+
             op_mass(ipout)=2.01d0
          else
             op_id(ipout)=423 ! D*0
             op_mass(ipout)=2.01d0
          endif
          if(p_id(ipin).lt.0) op_id(ipout)=-op_id(ipout)
       elseif(abs(p_id(ipin)).eq.5) then ! b quark
          if(dum_random.lt.1d0/8d0) then
             op_id(ipout)=511 ! B0
             op_mass(ipout)=5.28d0
          elseif(dum_random.lt.2d0/8d0) then
             op_id(ipout)=521 ! B+
             op_mass(ipout)=5.28d0
          elseif(dum_random.lt.5d0/8d0) then
             op_id(ipout)=513 ! B*0
             op_mass(ipout)=5.33d0
          else
             op_id(ipout)=523 ! B*+
             op_mass(ipout)=5.33d0
          endif
          if(p_id(ipin).gt.0) op_id(ipout)=-op_id(ipout)
       else
          write(6,*) "Heavy meson not listed"
          stop
       endif

c seperate frag. and recomb. meson, for checking purpose
c       if(abs(p_id(ipin)).eq.4) then ! c quark
c          op_id(ipout)=4210 ! D
c          op_mass(ipout)=1.9d0
c       elseif(abs(p_id(ipin)).eq.5) then ! b quark
c          op_id(ipout)=5210 ! B
c          op_mass(ipout)=5.3d0
c       else
c          write(6,*) "Heavy meson not listed"
c          stop
c       endif

       op_p0(ipout)=sqrt(op_px(ipout)**2+op_py(ipout)**2+
     &                  op_pz(ipout)**2+op_mass(ipout)**2)
       op_r0(ipout)=p_r0(ipin)
       op_rx(ipout)=p_rx(ipin)
       op_ry(ipout)=p_ry(ipin)
       op_rz(ipout)=p_rz(ipin) 
       oThydro(ipout)=Thydro(ipin)
       osdensity(ipout)=sdensity(ipin)
       oc_vx(ipout)=c_vx(ipin)
       oc_vy(ipout)=c_vy(ipin)
       oc_vz(ipout)=c_vz(ipin)
       op_ip0(ipout)=p_ip0(ipin)
       op_ipx(ipout)=p_ipx(ipin)
       op_ipy(ipout)=p_ipy(ipin)
       op_ipz(ipout)=p_ipz(ipin)
       op_s1(ipout)=p_s1(ipin)
       op_s2(ipout)=p_s2(ipin)
       call rotbos(0d0,0d0,c_vx(ipin),c_vy(ipin),c_vz(ipin),
     &            op_px(ipout),op_py(ipout),op_pz(ipout),op_p0(ipout))
       ipout=ipout+1
       count_recomb=count_recomb+1
       judge_HM=1
       frag=.true.

       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       subroutine judgehadmech2(hadrflag,kminsquare)
c judge the hadronization mechanism, frag. or recomb.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       include 'common.f'

       integer hadrflag
       double precision cmbetax,cmbetay,cmbetaz
c       double precision mxpt,mxvalue
       double precision qx_pair,qy_pair,qz_pair,qpair2,plight
       double precision probWigner,temp
       double precision rlu,utheta,phi
       double precision probmax,kminsquare,distribution,Egluon

c generate one light parton according to thermal distribution
       temp=Thydro(ipin)
c       temp = temp_common
c       temp=0.165d0

       if(kminsquare.lt.0) then
          probmax=1d0
       else
          probmax=exp(-kminsquare*sigma0**2)
       endif

c       mxpt=sqrt(2d0*temp**2+2d0*temp*sqrt(temp**2+mlight**2))
c       mxvalue=mxpt**2*exp(-sqrt(mlight**2+mxpt**2)/temp)
c       write(6,*) mxpt,mxvalue

c       mxvalue=0.07d0 ! for sampling efficiency, non-trivial
       plight=rlu(0)*15d0*temp
       p0_light=sqrt(plight**2+mlight**2)
       Egluon=sqrt(4d0*plight**2+m_g**2)
       distribution=6d0*plight**2/(exp(p0_light/temp)+1d0)+
     &              8d0*plight**2/(exp(Egluon/temp)-1d0)*16d0/3d0
       do while(rlu(0)*mxvalue.gt.distribution)
          plight=rlu(0)*15d0*temp
          p0_light=sqrt(plight**2+mlight**2)
          Egluon=sqrt(4d0*plight**2+m_g**2)
          distribution=6d0*plight**2/(exp(p0_light/temp)+1d0)+
     &              8d0*plight**2/(exp(Egluon/temp)-1d0)*16d0/3d0
       enddo

       utheta=rlu(0)*2d0-1d0
       phi=rlu(0)*2d0*pi
       px_light=plight*sqrt(1-utheta*utheta)*cos(phi)
       py_light=plight*sqrt(1-utheta*utheta)*sin(phi)
       pz_light=plight*utheta        

c       call rotbos(0d0,0d0,c_vx(ipin),c_vy(ipin),c_vz(ipin),
c     &            px_light,py_light,pz_light,p0_light)

c if use pure recombination mechanism, i.e. fW = 1
       if (hadr_flag.eq.2) then
          hadrflag = 2
          return
       endif

c boost heavy/light quark to the c.m. frame of the heavy-light system

       cmbetax=(p_px(ipin)+px_light)/(p_p0(ipin)+p0_light)
       cmbetay=(p_py(ipin)+py_light)/(p_p0(ipin)+p0_light)
       cmbetaz=(p_pz(ipin)+pz_light)/(p_p0(ipin)+p0_light)

       call rotbos(0d0,0d0,-cmbetax,-cmbetay,-cmbetaz,
     &            px_light,py_light,pz_light,p0_light)
       call rotbos(0d0,0d0,-cmbetax,-cmbetay,-cmbetaz,
     &            p_px(ipin),p_py(ipin),p_pz(ipin),p_p0(ipin))

c calculate the relative momentum
       qx_pair=(p0_light*p_px(ipin)-p_p0(ipin)*px_light)/
     &         (p0_light+p_p0(ipin))
       qy_pair=(p0_light*p_py(ipin)-p_p0(ipin)*py_light)/
     &         (p0_light+p_p0(ipin))
       qz_pair=(p0_light*p_pz(ipin)-p_p0(ipin)*pz_light)/
     &         (p0_light+p_p0(ipin))
       qpair2=qx_pair*qx_pair+qy_pair*qy_pair+qz_pair*qz_pair

       probWigner=exp(-qpair2*sigma0**2)  ! only part of the Wigner      

c use Monte Carlo to decide whether to recombine
       if(rlu(0)*probmax.lt.probWigner) then
          hadrflag=2
       endif 

c boost heavy/light quark back to the c.m. frame
       call rotbos(0d0,0d0,cmbetax,cmbetay,cmbetaz,
     &            px_light,py_light,pz_light,p0_light)
       call rotbos(0d0,0d0,cmbetax,cmbetay,cmbetaz,
     &            p_px(ipin),p_py(ipin),p_pz(ipin),p_p0(ipin))

       return
       end

