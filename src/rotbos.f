cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE rotbos(THE,PHI,BEX,BEY,BEZ,p1,p2,p3,p4)
c
c INPUT: the,phi,bex,bey,bez,p
c OUTPUT: p
c 1)rotate 4-vector p according to the and phi 2/ boost 4-vector p
C####C##1#########2#########3#########4#########5#########6#########7##
      implicit none
      double precision P(4),BEX,BEY,BEZ,GA,BEP,GABEP,rot(3,3),the,phi
      double precision p1,p2,p3,p4,bb2
      integer j

      IF(THE**2+PHI**2.GT.1d-10) THEN
C...ROTATE
        ROT(1,1)=COS(THE)*COS(PHI)
        ROT(1,2)=-SIN(PHI)
        ROT(1,3)=SIN(THE)*COS(PHI)
        ROT(2,1)=COS(THE)*SIN(PHI)
        ROT(2,2)=COS(PHI)
        ROT(2,3)=SIN(THE)*SIN(PHI)
        ROT(3,1)=-SIN(THE)
        ROT(3,2)=0.
        ROT(3,3)=COS(THE)
        DO 108 J=1,3
 108       P(J)=ROT(J,1)*P1+ROT(J,2)*P2+ROT(J,3)*P3
        p(4)=p4
      else
        p(1)=p1
        p(2)=p2
        p(3)=p3
        p(4)=p4
      ENDIF

      bb2=BEX**2+BEY**2+BEZ**2
      IF(bb2.GT.1d-10) THEN
C...LORENTZ BOOST (TYPICALLY FROM REST TO MOMENTUM/ENERGY=BETA)
        GA=1D0/DSQRT(1D0-bb2)
        BEP=BEX*P(1)+BEY*P(2)+BEZ*P(3)
        GABEP=GA*(GA*BEP/(1D0+GA)+P(4))
        P(1)=P(1)+GABEP*BEX
        P(2)=P(2)+GABEP*BEY
        P(3)=P(3)+GABEP*BEZ
        P(4)=GA*(P(4)+BEP)
      ENDIF

      p1=p(1)
      p2=p(2)
      p3=p(3)
      p4=p(4)

      RETURN
      END

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
