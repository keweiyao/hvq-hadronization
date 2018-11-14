      subroutine parameterRead

      include 'common.f'
      character*10 flag
      character*256 inputstr
      integer open_status, judge, INU_param
      character*256 filename

c set default values
      hadr_flag=1
      HEAVYMESONDEC=.FALSE.
      HQid=4      ! Heavy quark flavor
      omega_c=0.215d0
      omega_b=0.102d0
      omega_cM=0.33d0 ! in GeV
      omega_cB=0.43d0 ! in GeV
      omega_bM=0.33d0 ! in GeV
      omega_bB=0.41d0 ! in GeV

      m_ud=0.3d0
      m_s=0.475d0
      m_g=0.6d0
      prob_num=41
      prob_int_c=0.5d0
      prob_int_b=0.5d0

c	  ftn30=parameter_hd.dat
      call getenv('ftn30',filename)
      INU_param = 30
      open (unit=INU_param,file=filename,
     &     STATUS='OLD',FORM='FORMATTED',IOSTAT=open_status)

      if (open_status.ne.0) then
         write(6,*) "Error occurs during reading parameters_hd.dat."
         write(6,*) "Use default parameters."
         goto 2
      endif

c read input lines
 1    continue
      read(unit=INU_param,fmt=99) flag,inputstr
 99   format(1A10,1A78)

c # : treat line as a comment
      if(flag(1:1).eq.'#') goto 1
      if(flag(1:4).eq."    ") goto 1
c xx: treat line as end of input marker
      if(flag(1:2).eq.'xx') goto 2

      SELECT CASE (flag)
         CASE ("HMDecay...")
            read(inputstr,fmt=*,err=88,end=88) judge
            if(judge.eq.1) then
               HEAVYMESONDEC=.TRUE.
            elseif(judge.eq.0) then
               HEAVYMESONDEC=.FALSE.
            else
               write(6,*) "Invalid value for HMDecay ..."
               write(6,*) "Terminating ..."
            endif
         CASE ("hadr_flag.")
            read(inputstr,fmt=*,err=88,end=88) hadr_flag
         CASE ("HQid......")
            read(inputstr,fmt=*,err=88,end=88) HQid
         CASE ("omega_c...")
            read(inputstr,fmt=*,err=88,end=88) omega_c
         CASE ("omega_b...")
            read(inputstr,fmt=*,err=88,end=88) omega_b
         CASE ("omega_cM..")
            read(inputstr,fmt=*,err=88,end=88) omega_cM
         CASE ("omega_bM..")
            read(inputstr,fmt=*,err=88,end=88) omega_bM
         CASE ("omega_cB..")
            read(inputstr,fmt=*,err=88,end=88) omega_cB
         CASE ("omega_bB..")
            read(inputstr,fmt=*,err=88,end=88) omega_bB
         CASE ("m_ud......")
            read(inputstr,fmt=*,err=88,end=88) m_ud
         CASE ("m_s.......")
            read(inputstr,fmt=*,err=88,end=88) m_s
         CASE ("m_g.......")
            read(inputstr,fmt=*,err=88,end=88) m_g
         CASE ("prob_int_c")
            read(inputstr,fmt=*,err=88,end=88) prob_int_c
         CASE ("prob_int_b")
            read(inputstr,fmt=*,err=88,end=88) prob_int_b
         CASE ("prob_num..")
            read(inputstr,fmt=*,err=88,end=88) prob_num
                  CASE DEFAULT
            write(6,*) flag,"is NOT a valid parameter!"
            write(6,*) "Terminating ..."
            stop
      END SELECT

      goto 1

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 2    continue
      if(open_status.eq.0)
     &   write(6,*) "Parameters for hadronization has been read in."
      if(.TRUE.) then
         write(6,*) "hadr_flag: ",hadr_flag
         write(6,*) "HQid: ",HQid
         write(6,*) "HEAVYMESONDEC: ",HEAVYMESONDEC
         write(6,*) "prob_num: ",prob_num
         write(6,*) "prob_int_c: ",prob_int_c
         write(6,*) "prob_int_b: ",prob_int_b
         write(6,*) "omega_c: ",omega_c
         write(6,*) "omega_b: ",omega_b
         write(6,*) "omega_cM: ",omega_cM
         write(6,*) "omega_cB: ",omega_cB
         write(6,*) "omega_bM: ",omega_bM
         write(6,*) "omega_bB: ",omega_bB
         write(6,*) "m_ud: ",m_ud
         write(6,*) "m_s: ",m_s
         write(6,*) "m_g: ",m_g
      endif 
      return

c error-exit
 88   write(6,*) 'Syntax-error in the parameters_hd.dat ...'
      write(6,*) 'Terminating ...'
      stop
      end

