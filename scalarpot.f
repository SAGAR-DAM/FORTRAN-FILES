C SCALAR FIELD POTENTIAL FOR INFLATION
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      OPEN(1, FILE='pn10')
      
      X=4.5D0
      W=10000.d0
      Y=-0.50D0
      Z=Y
      H=0.01d0
      N=abs(W/H)
c      DO I=1,2
      DO J=1,N
C      C1=H*F(X,Y)
C      C2=H*F(X+H/2,Y+C1/2)
C      C3=H*F(X+H/2,Y+C2/2)
C      C4=H*F(X+H,Y+C3)

c      IF(I.EQ.1)THEN
c      Y=Y+(C1+2*C2+2*C3+C4)/6
c      Y=Y+(C1+2*C2+2*C3+C4)/6
       C1=H*F(X,Y)
       C2=H*F(X+C1/2,Y+H/2)
       C3=H*F(X+C2/2,Y+H/2)
       C4=H*F(X+C4,Y+H)
c      Y=Y+(C1+2*C2+2*C3+C4)/6
c      Y=Y+(C1+2*C2+2*C3+C4)/6
c      Y=Y+(C1+2*C2+2*C3+C4)/6
c      Y=Y+(C1+2*C2+2*C3+C4)/6
c      Y=Y+(C1+2*C2+2*C3+C4)/6
c      Y=Y+(C1+2*C2+2*C3+C4)/6
              WRITE(1,*)X,Y
c      ELSE
c              WRITE(2,*)X,Y
c      ENDIF

C     Y=Y+(C1+2*C2+2*C3+C4)/6
C      X=X+H
      Y=Y+H
      X=X+(C1+2*C2+2*C3+C4)/6

      if(X.ge.10000.d0) exit
      enddo
c50    write(*,*)'programme ended'
c      Y=-Z
c      X=W
c      ENDDO
      STOP
      END


      DOUBLE PRECISION FUNCTION F(X,Y)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C     F=-(Y*(Y**2+X**2)**0.5D0+X)/Y
      F=-Y/(Y*(Y**2+X**2)**0.5D0+X)
      RETURN
      END

