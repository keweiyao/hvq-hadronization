c**********************************************************************c
c                                                                      c
      subroutine copypart13
c                                                                      c
c     copies a particle that has not to be decayed directly            c
c     to the corresponding URQMD file 13 output variables              c
c                                                                      c
c     last change: 08/17/2009 David Meidinger                          c
c                                                                      c
c**********************************************************************c

      include 'common.f'
     
      if (ANALYSIS) op_id(ipout)=p_id(ipin) !only needed for analysis 
      op_ityp(ipout)=p_ityp(ipin)
      op_iso3(ipout)=p_iso3(ipin)
      op_charge(ipout)=p_charge(ipin)

      op_px(ipout)=p_px(ipin)
      op_py(ipout)=p_py(ipin)
      op_pz(ipout)=p_pz(ipin)
      op_p0(ipout)=p_p0(ipin)

      op_mass(ipout)=p_mass(ipin)

      op_rx(ipout)=p_rx(ipin)
      op_ry(ipout)=p_ry(ipin)
      op_rz(ipout)=p_rz(ipin)
      op_r0(ipout)=p_r0(ipin)

      op_fx(ipout)=p_fx(ipin)
      op_fy(ipout)=p_fy(ipin)
      op_fz(ipout)=p_fz(ipin)
      op_f0(ipout)=p_f0(ipin)

      op_origin(ipout)=p_origin(ipin)

      op_fpx(ipout)=p_fpx(ipin)
      op_fpy(ipout)=p_fpy(ipin)
      op_fpz(ipout)=p_fpz(ipin)
      op_fp0(ipout)=p_fp0(ipin)

      op_ncoll_13(ipout)=p_ncoll_13(ipin)
      op_lstcoll_13(ipout)=p_lstcoll_13(ipin)
    
      ipout=ipout+1
      return
      end
