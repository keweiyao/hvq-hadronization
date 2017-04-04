c**********************************************************************c
c                                                                      c
      logical function isparton()                                  
c                                                                      c
c     returns .true. if it finds the present particle's ID in          c
c     the list of partons that have to be fragmented                   c
c                                                                      c
c     last change: 08/07/2009 David Meidinger                          c
c                                                                      c
c**********************************************************************c

      include 'common.f'
      integer i
      do 1 i=1,PANUM
         if (abs(p_id(ipin)).EQ.palist(i)) then
            isparton=.true.
            return
         endif
   1  continue
      isparton=.false.
      return
      end
