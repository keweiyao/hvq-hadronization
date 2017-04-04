c**********************************************************************c
c                                                                      c
      subroutine copypartoscar
c                                                                      c
c     copies a particle that has not to be decayed directly            c
c     to the corresponding OSCAR 1997A output variables.               c
c                                                                      c
c     last change: 08/06/2009 David Meidinger                          c
c                                                                      c
c**********************************************************************c


      include 'common.f'
      
      op_num(ipout)=ipout
      op_id(ipout)=p_id(ipin)
      op_px(ipout)=p_px(ipin)
      op_py(ipout)=p_py(ipin)
      op_pz(ipout)=p_pz(ipin)
      op_p0(ipout)=p_p0(ipin)
      op_mass(ipout)=p_mass(ipin)
      op_rx(ipout)=p_rx(ipin)
      op_ry(ipout)=p_ry(ipin)
      op_rz(ipout)=p_rz(ipin)
      op_r0(ipout)=p_r0(ipin)
      op_lstcoll_osc(ipout)=p_lstcoll_osc(ipin)
      op_fx(ipout)=p_fx(ipin)
      op_fy(ipout)=p_fy(ipin)
      op_fz(ipout)=p_fz(ipin)
      op_f0(ipout)=p_f0(ipin)
      op_ncoll_osc(ipout)=p_ncoll_osc(ipin)

      oThydro(ipout)=Thydro(ipin)
      oc_vx(ipout)=c_vx(ipin)
      oc_vy(ipout)=c_vy(ipin)
      oc_vz(ipout)=c_vz(ipin)      

      ipout=ipout+1
      return
      end
