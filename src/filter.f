c**********************************************************************c
c                                                                      c
      program filter
c                                                                      c
c     1st version for HQ fragmentation: 08/18/2009 David Meidinger     c
c     2nd version for HQ frag.+recomb.: 05/21/2013 Shanshan Cao        c
c                                                                      c
c**********************************************************************c

      include 'common.f'
      logical isheavymeson
      logical isparton                 
      character*5 fortranunit
      character*77 filename
      integer i,j,k,rand_seed,numJudge
      double precision rlu,plength,recomb_prob,recomb_BR1,recomb_BR2
      double precision recomb_random,temp_cell
      integer ind_l,ind_p,ind_temp,ind_beta

      double precision max_ud(0:diffTemp)

! use an another temperature table from 0.15-0.22
!     temp = (/0.150d0, 0.152d0, 0.154d0, 0.156d0, 0.158d0,
!              0.160d0, 0.162d0, 0.164d0, 0.166d0, 0.168d0,
!              0.170d0, 0.172d0, 0.174d0, 0.176d0, 0.178d0,
!              0.180d0, 0.182d0, 0.184d0, 0.186d0, 0.188d0,
!              0.190d0, 0.192d0, 0.194d0, 0.196d0, 0.198d0,
!              0.200d0, 0.202d0, 0.204d0, 0.206d0, 0.208d0,
!              0.210d0, 0.212d0, 0.214d0, 0.216d0, 0.218d0,
!              0.220d0/)


      max_ud = (/0.047d0,0.049d0,0.051d0,0.053d0,0.056d0,
     &           0.059d0,0.062d0,0.064d0,0.067d0,0.070d0,
     &           0.073d0,0.076d0,0.079d0,0.082d0,0.085d0,
     &           0.089d0,0.092d0,0.096d0,0.099d0,0.103d0,
     &           0.107d0,0.110d0,0.114d0,0.118d0,0.122d0,
     &           0.126d0,0.130d0,0.135d0,0.139d0,0.143d0,
     &           0.148d0,0.153d0,0.157d0,0.162d0,0.167d0,
     &           0.172d0
     &          /)



      rand_seed=-1
      call sseed(rand_seed)

      call parameterRead

      numOut = 0

c debug
      if (hadr_flag.ne.1.and.hadr_flag.ne.2.and.hadr_flag.ne.3) then
         write(6,*) "Wrong value for hadr_flag ..."
         write(6,*) "Terminating ..."
         stop
      endif

c if necessary, open files
      if (INU.NE.5) then
         write (fortranunit,fmt='("ftn"I2.2)') INU
         call getenv(fortranunit,filename)
         open (unit=INU,file=filename,status='OLD',err=601)
      endif
      if (OUTU.NE.6) then
         write (fortranunit,fmt='("ftn"I2.2)') OUTU
         call getenv(fortranunit,filename)
         open (unit=OUTU,file=filename,err=602)
      endif
      if (ERRU.NE.6) then
         write (fortranunit,fmt='("ftn"I2.2)') ERRU
         call getenv(fortranunit,filename)
         open (unit=ERRU,file=filename,err=603)
      endif
      if (ANALYSIS) then
         write (fortranunit,fmt='("ftn"I2.2)') ANAU
         call getenv(fortranunit,filename)
         open (unit=ANAU,file=filename,err=604)
         call a_init
      endif

      call initpythia 

      call checkformat
c      fformat=19

      if(hadr_flag.eq.3) call read_recomb_table

      if (fformat.EQ.19) goto 19
      if (fformat.EQ.13) goto 13



c**********************************************************************c
c**** MAIN LOOP OSCAR 1997A *******************************************c


  19  continue
c event input 
         call readeventoscar

         if (ANALYSIS) call a_addevent_in
         remsamp=NUMSAMP

c recombination/fragmentation/decay

191      continue
         ipout=1
         count_recomb=0
         frag=.false.

         if(hadr_flag.eq.1) then ! pure fragmentation
            recomb_prob=0d0
         elseif(hadr_flag.eq.2) then ! pure recombination to D0 or D+
            recomb_prob=1d0          
            recomb_BR2=0d0
            recomb_BR1=1d0
         endif
        
         ipin = 1
         do while (ipin.le.eh_npart)
c check if particle has to be decayed
            if ((HEAVYMESONDEC.AND.isheavymeson()).OR.isparton()) then
               recombORfrag=1
               judge_HM=0
c boost to the local rest frame of cell for recombination
               call rotbos(0d0,0d0,-c_vx(ipin),-c_vy(ipin),-c_vz(ipin),
     &                    p_px(ipin),p_py(ipin),p_pz(ipin),p_p0(ipin))
               temp_cell = Thydro(ipin)
               ind_temp = int((temp_cell-0.150d0)/0.002d0+0.5d0)
               temp_common = 0.150d0+ind_temp*0.002d0
               if(ind_temp.gt.diffTemp) ind_temp=diffTemp
! modified by Yingru
               if(ind_temp.lt.0) then 
                 ind_temp = 0      
                 temp_common = 0.150d0
! end of modify by Yingru
c                write(6,*) temp_cell,ind_temp, recomb_prob
               endif

               if(hadr_flag.eq.3.and.ind_temp.ge.0) then ! frag. + recomb.
                  plength=sqrt(p_px(ipin)**2+p_py(ipin)**2+
     &                    p_pz(ipin)**2)
                  ind_p=int(plength/prob_int)
                  ind_l=ind_p+1
                  if(ind_l.ge.prob_num) then ! this is the correct version
                     recomb_prob=0d0
                  else
                     recomb_prob=prob_c(ind_l,ind_temp)-
     &                 (prob_c(ind_l,ind_temp)-prob_c(ind_l+1,ind_temp))
     &                 *(plength-ind_p*prob_int)/prob_int
                     recomb_BR1=DBR1(ind_l,ind_temp)-
     &                 (DBR1(ind_l,ind_temp)-DBR1(ind_l+1,ind_temp))
     &                 *(plength-ind_p*prob_int)/prob_int
c                     recomb_BR2=DBR2(ind_l)-(DBR2(ind_l)-DBR2(ind_l+1))
c     &                        *(plength-ind_p*prob_int)/prob_int
                     recomb_BR1=recomb_prob*recomb_BR1
c                     recomb_BR2=recomb_prob*recomb_BR2
                  endif
               endif

c 1/6/2015 the code is modified so that every c goes to D
c pay attention to boost back and forth
c               numJudge = 0
c               do while(judge_HM.eq.0)      
               
c               numJudge = numJudge+1
c               if(numJudge.gt.1D6) then
c                  write(6,*) "Large numJudge for ipin = ",ipin,
c     &                   beta_cell,recomb_BR1,recomb_random
c               endif

               recomb_random=rlu(0)
                      
               if(recomb_random.lt.recomb_prob) then ! do recomb
                  if(recomb_random.lt.recomb_BR1) then ! D0 and D+
                     mxvalue = max_ud(ind_temp)
                     call force_recomb(recombORfrag,1)
c                  elseif(recomb_random.lt.recomb_BR1+recomb_BR2) then
c                     call force_recomb(recombORfrag,3)  ! Ds
                  endif
               else
c if not recombined, boost back to global c.m. frame for fragmentation
                  call rotbos(0d0,0d0,c_vx(ipin),c_vy(ipin),c_vz(ipin),
     &                      p_px(ipin),p_py(ipin),p_pz(ipin),p_p0(ipin))
                  call fragdecay
               endif

c               enddo ! (judge_HM is now set to 1)

               frag=.true.
            else
c if ONLYFRAG=2, skip unprocessed particles
               if (ONLYFRAG.NE.2) call copypartoscar
            endif

            ipin = ipin+1
          enddo
c 190     continue
c final number of particles in this event
         oeh_npart=ipout-1
c event output (suppress for unchanged events, when ONLYFRAG is not 0)
         if ((ONLYFRAG.EQ.0).OR.frag) then
            call writeeventoscar
            if (ANALYSIS) call a_addevent_out
         endif

c process events with frag=.true. NUMSAMP times
         if (frag.AND.(remsamp.NE.1)) then
            remsamp=remsamp-1
            write(6,*) remsamp,'samples left'
            goto 191
         endif

      goto 19
      
c**********************************************************************c
c**** MAIN LOOP URQMD FILE 13 *****************************************c

  13  continue
c event input 
         call readevent13
         if (ANALYSIS) call a_addevent_in
         remsamp=NUMSAMP
c fragmentation/decay
 131     continue
         ipout=1
         frag=.false.
         do 130 ipin=1,eh_npart
c check if particle has to be decayed
c (no need to check for partons, since URQMD won't produce them)
            if (HEAVYMESONDEC.AND.isheavymeson()) then
              call fragdecay   
              frag=.true.
            else
c if ONLYFRAG=2, skip unprocessed particles
              if (ONLYFRAG.NE.2) call copypart13
            endif
 130     continue  
c final number of particles in this event
         oeh_npart=ipout-1
c event output (suppress for unchanged events, when ONLYFRAG is not 0)
         if ((ONLYFRAG.EQ.0).OR.frag) then
            call writeevent13
            if (ANALYSIS) call a_addevent_out
         endif 
c process events with frag=.true. NUMSAMP times
         if (frag.AND.(remsamp.NE.1)) then
            remsamp=remsamp-1
            goto 131
         endif
      goto 13
      
c**********************************************************************c
c**** ERRORS while opening files: always stdout ***********************c

 601  write (6,*) 'ERROR while opening input file!'
      stop
 602  write (6,*) 'ERROR while opening output file'
      stop
 603  write (6,*) 'ERROR while opening error file'
      stop
 604  write (6,*) 'ERROR while opening analysis file'
      stop

 605  continue
      write(6,*) "program ends successfully:)"      

      end

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine read_recomb_table

      include 'common.f'

      integer i
      double precision dummy_pc

      if(HQid.eq.4) then
         open(unit=11,file='recomb_c_tot.dat',
     &        status='old',form='formatted')
         open(unit=12,file='recomb_c_BR1.dat',
     &        status='old',form='formatted')
         prob_int=prob_int_c
         omega_HM=omega_cM
         omega_HB=omega_cB
      elseif(HQid.eq.5) then
         open(unit=11,file='recomb_b_tot.dat',
     &        status='old',form='formatted')
         open(unit=12,file='recomb_b_BR1.dat',
     &        status='old',form='formatted')
         prob_int=prob_int_b
         omega_HM=omega_bM
         omega_HB=omega_bB
      else
         write(*,*) "un-recognized heavy quark ID ..."
         stop
      endif
 
      i=1
      do while(i.le.prob_num)
         read(unit=11,fmt=2196,err=2198,end=2197) dummy_pc,
     &       prob_c(i,0:diffTemp)
         read(unit=12,fmt=2196,err=2198,end=2197) dummy_pc,
     &       DBR1(i,0:diffTemp)
c         write(6,*) dummy_pc,prob_c(i,0),prob_c(i,1),prob_c(i,21)
         i=i+1
      enddo

      write(6,*) "read in prob table successfully:)"

      return

 2196 format(E10.4,55(2X,E10.4))

 2197 continue
      write(6,*) "end"
      return

 2198 continue
      write(6,*) "error"
      return

      end
