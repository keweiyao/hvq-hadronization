c**********************************************************************c
c                                                                      c
c      PYTHIA common blocks (non-implicit)                             c
c                                                                      c
c      last change: 08/06/2009 David Meidinger                         c
c                                                                      c
c**********************************************************************c


      INTEGER PYK,PYCHGE,PYCOMP
      INTEGER N,NPAD,MSTU(200),MSTJ(200),K(4000,5),KCHG(500,4),
     .        MDCY(500,3),MDME(8000,2),KFDP(8000,5)
      DOUBLE PRECISION P(4000,5),V(4000,5),PARU(200),PARJ(200),
     .                 PMAS(500,4),PARF(2000),VCKM(4,4),BRAT(8000)
      COMMON/PYJETS/N,NPAD,K,P,V
      COMMON/PYDAT1/MSTU,PARU,MSTJ,PARJ
      COMMON/PYDAT2/KCHG,PMAS,PARF,VCKM
      COMMON/PYDAT3/MDCY,MDME,BRAT,KFDP
      SAVE /PYJETS/,/PYDAT1/,/PYDAT2/,/PYDAT3/
