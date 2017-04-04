c**********************************************************************c
c                                                                      c
      subroutine initpythia                                           
c                                                                      c
c     sets a couple of PYTHIA switches and parameter.                  c
c     random number generator initialization.                          c
c                                                                      c
c     last change: 09/03/2009 David Meidinger                          c
c                                                                      c
c**********************************************************************c

      include 'common.f'
      include 'compyth.f'
      
      logical ex
      integer i

c we don't want PYTHIA to print a title page      
      MSTU(12)=12345 

c and errors or wanings neither (errors get counted in analysis output)
      MSTU(22)=0
      MSTU(25)=0

c use Perterson/SLAC fragmentation for heavy quark
      MSTJ(11)=3

c declare particles listed in stablelist as stable.
c [to switch off individual decay channels, use MDME(IDC,1)] 
      do 44 i=1,STABLENUM
         MDCY(PYCOMP(stablelist(i)),1)=0
c if nonidentical antiparticle exists, it is stable, too
         if(KCHG(PYCOMP(stablelist(i)),3).EQ.1) then
            MDCY(PYCOMP(-1*stablelist(i)),1)=0
         endif 
  44  continue

c if HEAVYMESONDEC is set to .false., the particles in hmlist
c must be declared as stable (for the parton fragmentation)
      if (.NOT.HEAVYMESONDEC) then
         do 55 i=1,HMNUM
c particle is considered to be stable 
            MDCY(PYCOMP(hmlist(i)),1)=0
c if nonidentical antiparticle exists, it is stable, too
            if(KCHG(PYCOMP(hmlist(i)),3).EQ.1) then
               MDCY(PYCOMP(-1*hmlist(i)),1)=0
            endif 
  55     continue
      endif 

c set the random number generator to the state saved in RNDM.dat
      inquire(file='RNDM.dat',exist=ex)
      if (ex) then 
         open(unit=99,file='RNDM.dat',access='SEQUENTIAL',
     .        form='UNFORMATTED')
         call PYRSET(99,0)
         close(99)
      endif

      return
      end
