c**********************************************************************c
c                                                                      c
      subroutine endprog
c                                                                      c
c     will be called after successful execution                        c
c     saves the random number generator state                          c
c                                                                      c
c     last change: 08/14/2009 David Meidinger                          c
c                                                                      c
c**********************************************************************c

      include 'common.f'
      include 'compyth.f'
      
c dump random number generator state to RNDM.dat
      open(unit=99,file='RNDM.dat',access='SEQUENTIAL',
     .     form='UNFORMATTED')
      call PYRGET(99,0)
      close(99)

c do the analysis if flag is set
      if (ANALYSIS) call a_analize

c close files
      if (INU.NE.5)  close (INU)
      if (OUTU.NE.6) close (OUTU)
      if (ERRU.NE.6) close (ERRU)
      if (ANALYSIS)  close (ANAU)
 
      write(6,*) "max cell v: ",beta_max
      write(6,*) "times to exceed 0.95c: ",count_large_beta
      write(6,*) "times to initialize outside the medium: ",numOut
 
      stop
      end





