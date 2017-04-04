c**********************************************************************c
c                                                                      c
      subroutine checkformat                                           
c                                                                      c
c     determines whether input has URQMD file 13                       c
c     or OSCAR 1997A format                                            c
c                                                                      c
c     last change: 08/06/2009 David Meidinger                          c
c                                                                      c
c**********************************************************************c

      include 'common.f'
    
      character*8  chformat
      character*2  uqmdfmt

      firstline=1

      read (unit=INU,fmt='(a58)',err=331,end=331) inputline
      read (unit=inputline,fmt='(a8)',err=331,end=331) chformat
      if (chformat.EQ.'OSC1997A') then
         fformat=19
         call oscfileheaderio
      elseif (chformat.EQ.'UQMD   v') then
         read (unit=inputline,fmt='(56x,a2)',err=331,end=331) uqmdfmt
         if (uqmdfmt.EQ.'13') then
            fformat=13
         else
            goto 331
         endif
      else
         goto 331
      endif

      return

c**** ERRORS **********************************************************c

 331  write (ERRU,*) 'ERROR while determining file format'
      stop

      end


