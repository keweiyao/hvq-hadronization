c**********************************************************************c
c                                                                      c
c     parameters, common blocks and format statements                  c 
c                                                                      c
c     last change: 09/03/2009 David Meidinger                          c
c                                                                      c
c**********************************************************************c

      implicit none


c**********************************************************************c
c**** PARAMETERS ******************************************************c
c**********************************************************************c

      integer MAXP                        !  max. number of  
      parameter (MAXP = 2000000)           !  particles per event

      logical HEAVYMESONDEC               !  if set to true, heavy 
c      parameter (HEAVYMESONDEC=.false.)    !  mesons will decay
      common/userFlag/HEAVYMESONDEC

      logical ONLYHMELECTRONS             !  if set to true, only
      parameter (ONLYHMELECTRONS=.true.) !  electrons and positrons
                                          !  produced in decays of the
                                          !  particles in hmlist will
                                          !  be kept

      integer ONLYFRAG                    !  limits the output:
      parameter (ONLYFRAG=2)              !  0: all events will be
                                          !     written
                                          !  1: unchanged events 
                                          !     will not be written
                                          !  2: as 1, but even in those
                                          !     events, only the "new"
                                          !     particles are kept

      integer NUMSAMP                     !  events containing particles
      parameter (NUMSAMP=1)             !  that decay or undergo
                                          !  fragmentation will be
                                          !  processed NUMSAMP times
    
      logical NONEUTRINOS                 !  skip neutrinos when writing      
      parameter (NONEUTRINOS=.true.)      !  outputfile

      logical ANALYSIS                    !  turns event analysis on/off
      parameter (ANALYSIS=.false.)

      integer MAXEVENTSIN, MAXEVENTSOUT   !  max. number of input and
      parameter (MAXEVENTSIN=10)          !  output events.
      parameter                           !  does only affect analysis
     . (MAXEVENTSOUT=NUMSAMP*MAXEVENTSIN) !


      integer MAXRETRIES                  ! max. number of retries when
      parameter (MAXRETRIES=10)           ! PYTHIA errors occur


      integer INU, OUTU, ERRU, ANAU       !  units for input, output,
                                          !  errors and analysis
      parameter (INU = 5)                !  5 and 6 correspond to
      parameter (OUTU = 20)               !  stdin/stdout, other numbers
      parameter (ERRU = 6)               !  mean that the file name is
      parameter (ANAU = 18)               !  to be read from the "ftnXY"
                                          !  environmental variable

c**********************************************************************c
c**** HEAVY MESONS & PARTONS ******************************************c
c**********************************************************************c

      integer HMNUM                      !  number of heavy mesons
      parameter (HMNUM=49)               !  that will be decayed
                                         !  (not counting antiparticles)

      integer hmlist(HMNUM)              !  list with the PDG IDs of
                                         !  those mesons
      data    hmlist/ 
c             D.......  D*.......... J/Psi  Psi'   Chi_C
     .        411, 421, 10411, 10421, 443, 100443, 10441,
     .        413,423,10413,10423,20413,20423,415,425,431,10431,433,
     .        10433,20433,435,
c             B.......... B*............... Y   Chi_B Y'    Chi_B'
     .        511,521,531,10511,10521,10531,553,10551,10553,20553,
     .        513,523,10513,10523,20513,20523,515,525,533,10533,20533,
     .        535,541,10541,543,10543,20543,545/
                   
      integer PANUM                      !  number of partons that
      parameter (PANUM=2)                !  will be fragmented
                                         !  (not counting antiparticles)
      
      integer palist(PANUM)              !  list with the PDG IDs of
                                         !  those partons
c      data    palist/
c              d  u  s  c  b  t  g
c     .        1, 2, 3, 4, 5, 6, 21/

c      data    palist/ 4,5,6/             !  only heavy quarks!
       
       data    palist/ 4,5/             !  only charm and bottom quarks!

c**********************************************************************c
c**** STABLE PARTICLES ************************************************c
c**********************************************************************c

      integer STABLENUM                  !  number of stable particles
      parameter (STABLENUM=7)            

      integer stablelist(STABLENUM)      !  contains PDG IDs of
                                         !  particles that are
                                         !  considered to be stable
                                         !  (antiparticles, if they
                                         !  exist are automaticaly
                                         !  included)
                       
      data stablelist/
c             n     p  pi+- pi0  eta  K+-   K0
     .     2112, 2212, 211, 111, 221, 321, 311/

c**********************************************************************c
c**** OSCAR 1997A *****************************************************c
c**********************************************************************c

c**** FILE-HEADER *****************************************************c

      character*12 fh_format, fh_content
      character*8  fh_modelname, fh_version
      integer      fh_tarmass, fh_tarcharge, fh_promass, fh_procharge
      character*4  fh_refframe
      real*8       fh_eibeam 
      integer      fh_parpernuc

      common /fileheader1/  fh_format, fh_content
      common /fileheader2/  fh_modelname, fh_version
      common /fileheader3/  fh_tarmass, fh_tarcharge, 
     .                      fh_promass, fh_procharge, fh_parpernuc
      common /fileheader4/  fh_refframe
      common /fileheader5/  fh_eibeam 

 901  format(a12)
 902  format(2(a8,2x),'(',i3,',',i6,')+(',i3,',',i6,')',
     .       2x,a4,2x,e10.4,2x,i8)
 903  format(2(a8,2x),1x,i3,1x,i6,3x,i3,1x,i6,3x,a4,2x,e10.4,2x,i8)

c**** EVENT-HEADER ****************************************************c

      integer      eh_event, eh_npart, eh_ntim, eh_itim, eh_evsamp
      real*8       eh_imppar, eh_angle

      common /eventheader/  eh_event, eh_npart, eh_imppar, eh_angle,
     .                      eh_ntim, eh_itim, eh_evsamp
     
 911  format(i10,2x,i10,2x,f8.3,2x,f8.3,2x,i4,2x,i4,2X,i7)

c     **** OUTPUT ****
    
      integer oeh_npart
      common /outputeh/  oeh_npart

c**** FOR RECOMBINATION (by Shanshan) *********************************c

      double precision PI,rDelta,rXi,hbarc,Econst,mlight,EPS
      double precision m_ud,m_s,m_g
      double precision omega_c,omega_b,omega_HM,omega_HB
      double precision omega_cM,omega_cB,omega_bM,omega_bB
      double precision myVolume,sigma0,reducedM0
      double precision prob_int,prob_int_c,prob_int_b
      double precision temp_common,beta_max
      integer count_large_beta,judge_HM,numOut
      integer prob_num,HQid,hadr_flag,prob_num_MAX,diffTemp
c      parameter (HQid=4) ! 4 for charm and 5 for bottom
      parameter (PI=3.1415927d0)
      parameter (Econst=2.71828d0)
      parameter (EPS=0.000001d0)
      parameter (hbarc=0.1973d0)
c      parameter (omega_c=0.106d0) ! in GeV 0.106
c      parameter (omega_b=0.059d0) ! in GeV
      parameter (rDelta=0.35d0) ! in fm
      parameter (rXi=10d0)
c      parameter (m_ud=0.3d0)
c      parameter (m_s=0.475d0)
c      parameter (prob_int_c=0.5d0)
c      parameter (prob_int_b=1d0)
      parameter (prob_num_MAX=41,diffTemp=35)
c      parameter (hadr_flag=1) ! 1: pure fragmentation
                              ! 2: pure recombination to D0 and D+
                              ! 3: frag. + recomb. 

      common/recombUser1/HQid,hadr_flag,prob_num
      common/recombUser2/omega_c,omega_b,m_ud,m_s,m_g,
     &                   prob_int_c,prob_int_b,prob_int,
     &                   omega_cM,omega_cB,omega_bM,omega_bB

      integer recombORfrag,count_recomb,light_count
      double precision p0_light,px_light,py_light,pz_light
      double precision plight_total,pincell,mxvalue
c      double precision prob_c(prob_num_MAX),
c     &                 DBR1(prob_num_MAX),DBR2(prob_num_MAX)
      double precision prob_c(1:prob_num_MAX,0:diffTemp),
     &                 DBR1(1:prob_num_MAX,0:diffTemp),
     &                 DBR2(1:prob_num_MAX,0:diffTemp)

      common /recomb1/ recombORfrag,count_recomb,light_count
      common /recomb2/ p0_light,px_light,py_light,pz_light,
     &                 plight_total,pincell,mlight
      common /recomb3/ reducedM0,myVolume,sigma0,omega_HM,omega_HB
      common /recomb4/ prob_c,DBR1,DBR2,temp_common
      common /recomb5/ mxvalue,beta_max
      common /recomb6/ count_large_beta,judge_HM,numOut

c**** PARTICLES *******************************************************c

      integer      p_num(MAXP), p_id(MAXP), p_ncoll_osc(MAXP)
      real*8       p_px(MAXP), p_py(MAXP), p_pz(MAXP), p_p0(MAXP),
     .             p_rx(MAXP), p_ry(MAXP), p_rz(MAXP), p_r0(MAXP),
     .             p_fx(MAXP), p_fy(MAXP), p_fz(MAXP), p_f0(MAXP),
     .             p_mass(MAXP), p_lstcoll_osc(MAXP),
     .             p_ipx(MAXP), p_ipy(MAXP), p_ipz(MAXP), p_ip0(MAXP),
     .             Thydro(MAXP), c_vx(MAXP), c_vy(MAXP), c_vz(MAXP),
     .             sdensity(MAXP), p_s1(MAXP), p_s2(MAXP)
  
      common /particles/  p_num, p_id, p_px, p_py, p_pz, p_p0, p_mass,
     .                    p_rx, p_ry, p_rz, p_r0,
     .                    p_ipx, p_ipy, p_ipz, p_ip0, p_s1, p_s2,
     .                    p_fx, p_fy, p_fz, p_f0,
     .                    Thydro, c_vx, c_vy, c_vz,sdensity

 921  format(i10,2x,i10,19(2x,d12.6))      

c     **** OUTPUT ****

      integer      op_num(MAXP), op_id(MAXP), op_ncoll_osc(MAXP)

      real*8       op_px(MAXP), op_py(MAXP), op_pz(MAXP), op_p0(MAXP),
     .             op_rx(MAXP), op_ry(MAXP), op_rz(MAXP), op_r0(MAXP),
     .             op_fx(MAXP), op_fy(MAXP), op_fz(MAXP), op_f0(MAXP),
     .             op_mass(MAXP), op_lstcoll_osc(MAXP), op_ipx(MAXP), 
     .             op_ipy(MAXP), op_ipz(MAXP), op_ip0(MAXP),
     .             oThydro(MAXP), oc_vx(MAXP), oc_vy(MAXP), oc_vz(MAXP),
     .             osdensity(MAXP), op_s1(MAXP), op_s2(MAXP)
  
      common /outputpart/  op_num, op_id, op_px, op_py, op_pz, op_p0,
     .                     op_mass, op_rx, op_ry, op_rz, op_r0, 
     .                     op_ipx, op_ipy, op_ipz, op_ip0, 
     .                     op_fx, op_fy, op_fz, op_f0, op_s1, op_s2,
     .                     oThydro, oc_vx, oc_vy, oc_vz


c**********************************************************************c
c**** URQMD FILE 13 ***************************************************c
c**********************************************************************c

      integer    p_ityp(MAXP),  p_iso3(MAXP), p_charge(MAXP),
     .           op_ityp(MAXP), op_iso3(MAXP), op_charge(MAXP),
     .           p_ncoll_13(MAXP), p_lstcoll_13(MAXP), p_origin(MAXP),
     .           op_ncoll_13(MAXP), op_lstcoll_13(MAXP),op_origin(MAXP)
      
      real*8     p_fpx(MAXP), p_fpy(MAXP), p_fpz(MAXP), p_fp0(MAXP),
     .           op_fpx(MAXP), op_fpy(MAXP), op_fpz(MAXP), op_fp0(MAXP)
                
c DUMMY variables used for storing the file13 event headers
      character*200  DUMMY(15)
      integer        DUMMYTIME
     
      common /file13/ p_ityp, p_iso3, p_charge,
     .                op_ityp, op_iso3, op_charge,
     .                p_ncoll_13, p_lstcoll_13, p_origin,
     .                op_ncoll_13, op_lstcoll_13, op_origin,
     .                p_fpx, p_fpy, p_fpz, p_fp0,
     .                op_fpx, op_fpy, op_fpz, op_fp0,
     .                DUMMY, DUMMYTIME

 922  format(9e16.8,i11,2i3,i9,i5,i4,8e16.8)

c**********************************************************************c
c**** COUNTERS, FLAGES ETC. *******************************************c
c**********************************************************************c
  
      character*58       inputline
      integer            ipin, ipout, fformat, firstline, remsamp
      logical            frag

      common /il/        inputline     
      common /counters/  ipin, ipout, remsamp
      common /flags/     fformat, firstline, frag

c**********************************************************************c
c**** ANALYSIS DATA ***************************************************c
c**********************************************************************c

      integer a_events_in, a_events_out
      real*8  a_q_in(MAXEVENTSIN) , a_q_out(MAXEVENTSOUT),
     .        a_B_in(MAXEVENTSIN) , a_B_out(MAXEVENTSOUT),
     .        a_e_in(MAXEVENTSIN) , a_e_out(MAXEVENTSOUT),
     .        a_px_in(MAXEVENTSIN), a_px_out(MAXEVENTSOUT),   
     .        a_py_in(MAXEVENTSIN), a_py_out(MAXEVENTSOUT),  
     .        a_pz_in(MAXEVENTSIN), a_pz_out(MAXEVENTSOUT)
      real*8  a_q_in_mean, a_q_out_mean,
     .        a_B_in_mean, a_B_out_mean,
     .        a_e_in_mean, a_e_out_mean,
     .        a_px_in_mean, a_px_out_mean,
     .        a_py_in_mean, a_py_out_mean,
     .        a_pz_in_mean, a_pz_out_mean
      real*8  a_q_in_sigma, a_q_out_sigma,
     .        a_B_in_sigma, a_B_out_sigma,
     .        a_e_in_sigma, a_e_out_sigma,
     .        a_px_in_sigma, a_px_out_sigma,
     .        a_py_in_sigma, a_py_out_sigma,
     .        a_pz_in_sigma, a_pz_out_sigma

      common /anavars/
     .        a_events_in, a_events_out, a_q_in, a_q_out,
     .        a_B_in, a_B_out, a_e_in, a_e_out,
     .        a_px_in, a_px_out, a_py_in, a_py_out, 
     .        a_pz_in, a_pz_out, 
     .        a_q_in_mean, a_q_out_mean,
     .        a_B_in_mean, a_B_out_mean,
     .        a_e_in_mean, a_e_out_mean,
     .        a_px_in_mean, a_px_out_mean,
     .        a_py_in_mean, a_py_out_mean,
     .        a_pz_in_mean, a_pz_out_mean,
     .        a_q_in_sigma, a_q_out_sigma,
     .        a_B_in_sigma, a_B_out_sigma,
     .        a_e_in_sigma, a_e_out_sigma,
     .        a_px_in_sigma, a_px_out_sigma,
     .        a_py_in_sigma, a_py_out_sigma,
     .        a_pz_in_sigma, a_pz_out_sigma
     
      save /anavars/ 

c**********************************************************************c
c**** PYTHIA ERROR HANDLING *******************************************c
c**********************************************************************c

      integer pyth_errors
      integer pyth_errors_resolved
      integer pyth_try

      common /pyth_err/ pyth_errors, pyth_errors_resolved, pyth_try
      save   /pyth_err/
