c**********************************************************************c
c                                                                      c
      logical function isheavymeson()                                  
c                                                                      c
c     returns .true. if it finds the present particle's ID in          c
c     the list of heavy mesons that have to be decayed                 c
c                                                                      c
c     last change: 08/07/2009 David Meidinger                          c
c                                                                      c
c**********************************************************************c

      include 'common.f'
      integer i
      do 1 i=1,HMNUM
         if (abs(p_id(ipin)).EQ.hmlist(i)) then
            isheavymeson=.true.
            return
         endif
   1  continue
      isheavymeson=.false.
      return
      end
