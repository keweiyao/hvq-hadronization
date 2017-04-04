cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine pdg2id (ityp,iso3,pdgid)
c
c     Author   : Steffen A. Bass
c     Date     : 06/08/98
c     Revision : 1.0
c
c based on ityp2pdg.f from Henning Weber
c 
c     converts PDG-Id to  UrQMD-Id 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      implicit none

      include 'idtab.f'

      integer pdgid,i
      integer ityp
      integer iso3

c search for the ITYP in IDTAB

      do 99 i=1,TAB_SIZE
         if(idtab(3,i).eq.pdgid) then
            ityp=idtab(1,i)
            iso3=idtab(2,i)
            return
         elseif((pdgid.lt.0).and.(idtab(3,i).eq.abs(pdgid))) then
            ityp=(-1)*idtab(1,i)
            iso3=(-1)*idtab(2,i)
            return
         endif
 99   continue

c handle all unknown ityps from PYTHIA
      ityp=1000+abs(pdgid)
      if(pdgid.lt.0) ityp=(-1)*ityp
      iso3=0

      return
      end


