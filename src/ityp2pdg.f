cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      function pdgid (ityp, iso3)
c
c     Revision : 1.0
c
coutput pdgid  : Particle-ID according to Particle Data Group  
c 
c     converts UrQMD-Id to PDG-Id 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      include 'common.f'
      include 'idtab.f'

      integer pdgid
      integer ityp
      integer iso3

      logical anti
      integer abs_ityp
      integer norm_iso3
      integer first
      integer last
      integer next


c PYTHIA pdgid's (only need to subtract the offset!)
      if(abs(ityp).gt.1000) then
         if(ityp.gt.0) then
            pdgid=ityp-1000
         else
            pdgid=-1*(abs(ityp)-1000)
         endif
         return
      endif

cb check for antiparticles
      if (ityp.lt.0) then
cl its an antiparticle         
         anti = .true.
         abs_ityp = abs(ityp)
         norm_iso3 = -iso3
cl only mesons with odd isospin can have a negative ITYPE
         if ((abs_ityp.gt.minmes).and.
     .        (mod(isomes(abs_ityp),2).eq.0)) then
c            call error ('pdgid','Negative ITYP not allowed',
c     .           dble(ityp),3)
c            pdgid = 0
             goto 666
            return
         endif
      else
         anti = .false.
         abs_ityp = ityp
         norm_iso3 = iso3
      endif

cb search for the ITYP in IDTAB

      first = 1
      last = TAB_SIZE
      if (idtab(1,first).eq.abs_ityp) then 
         next = first 
         goto 200
      endif
      if (idtab(1,last).eq.abs_ityp) then
         next = last 
         goto 200
      endif

 100  continue

cl ITYP not found in IDTAB
      if (last.le.(first+1)) then
         pdgid = 0
         return
      endif

      next = (first+last)/2

      if (idtab(1,next).lt.abs_ityp) then 
         first = next
         goto 100
      elseif (idtab(1,next).gt.abs_ityp) then
         last = next
         goto 100
      endif

 200  continue

cl calculate the entry with the wanted ISO3
      next = next - (idtab(2,next)-norm_iso3)/2
      
cl check if we found the correct values in IDTAB
      if ((idtab(1,next).eq.abs_ityp).and.
     .    (idtab(2,next).eq.norm_iso3)) then
         if (anti) then
            pdgid = -idtab(3,next)
         else
            pdgid = idtab(3,next)
         endif
      else
c         call error ('pdgid','Error in tablelookup',dble(next),3)
c         pdgid = 0
          goto 666
      endif
      
      return

 666  write(ERRU,*) 'ERROR while converting URQMD ID to PDG ID'
      stop

      end







