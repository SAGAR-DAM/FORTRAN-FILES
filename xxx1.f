C DIFFEQ
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      WRITE(*,*)'RUNGE KUTTA METHODE: '
      OPEN(2,FILE='OUP')

      X=0.D0
      Y=0.D0
      H=0.001D0
      WRITE(*,*)'ENTER LAST LIMIT: '
      READ(*,*)B
      N=(B-X)/H

      DO I=1,N
      C1=H*F(X,Y)
      C2=H*F(X+H/2,Y+C1/2)
      C3=H*F(X+H/2,Y+C2/2)
      C4=H*F(X+H,Y+C3)
C      Y=Y+(C1+2*C2+2*C3+C4)/6
      Y=Y+H*F(X,Y)+H**2*F1(X,Y)/2+H**3*F2(X,Y)/6
      X=X+H
      WRITE(2,*)X,Y
      ENDDO

      STOP
      END

      DOUBLE PRECISION FUNCTION F(X,Y)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      F=2.d0*X*DCOS(X**2)
      RETURN
      END

      double precision function F1(X,Y)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      F1=2.D0*DCOS(X**2)-4.D0*X**2*DSIN(X**2)
      RETURN 
      END

      DOUBLE PRECISION FUNCTION F2(X)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      F2=-4.D0*X*DSIN(X**2)-8.D0*X**2*DSIN(X**2)-8.D0*X**3*DCOS(X**2)
      end
