C PROGRAMME FOR DOUBLE PENDULUM OSCILLATION
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      OPEN(2,FILE='OUP')
C      WRITE(*,*)'GIVE INITIAL VALUE OF THETA:  '
C      READ(*,*)A
C      Z=-A
      H=0.0001
      N=6*3.14159/H
C      DO I=1,N
      A=(2/(2+2**0.5))**0.5
      B=(2/(2-2**0.5))**0.5
      C=-(2/(2+2**0.5))**0.5
      D=-(2/(2-2**0.5))**0.5
      DO I=1,N
      X=2*COS(A*I*H+0.3)+3*COS(B*I*H+0.4)+4*COS(C*I*H+0.5)+5*COS(D*I*H)
      Z=-(A*2*SIN(A*I*H+0.3)+B*3*SIN(B*I*H+0.4))
      Y=Z-(C*4*SIN(C*I*H+0.5)+D*5*SIN(D*I*H))
      WRITE(2,*)X,Y
      ENDDO
      STOP
      END
