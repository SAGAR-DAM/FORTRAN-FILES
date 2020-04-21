C NAME: SAGAR DAM; ROLL: PHYS010; REG: 16120917010
C SOLUTION OF SCHRODINGER EQN FOR 1D LHO NUMERICALLY
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION P(5000),X(5000)
      A=3.0079836187D0*10**41
      OPEN(2,FILE='OUP')
      WRITE(*,*)'ENTER THE VALUE OF ENERGY IN MeV '
      READ(*,*)E1
c     E=E1*1.60217662D0*(10**(-13))
      E=E1
      WRITE(*,*)'ENTER THE PRIMARY POINT: '
      READ(*,*)X(1)
      WRITE(*,*)'ENTER THE VALUE OF PSI(X0): '
      READ(*,*) P(1)
      WRITE(*,*)'ENTER THE VALUE OF DERIVATIVE OF PSI(X0): '
      READ(*,*)Q
      WRITE(*,*)'ENTER THE INTERVAL LENGTH: '
      READ(*,*)H
      N=2*DABS(X(1))/H
      X(2)=X(1)+H
      P(2)=P(1)+Q*H
      WRITE(2,*)X(1),P(1)
      WRITE(2,*)X(2),P(2)

      DO 10 I=3,N
      X(I)=X(I-1)+H
      V0=(E-V(X(I-2)))
      V1=(E-V(X(I-1)))
      V2=(E-V(X(I)))

      P(I)=(2*(1.D0-5*V1*H**2/12)*P(I-1)-(1+V0*H**2/12)*P(I-2))/
     &(1+V2*H**2/12)

      WRITE(2,*)X(I),P(I)
10    CONTINUE
      STOP
      END

      DOUBLE PRECISION FUNCTION V(X)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
c      V=47.695817D0*10**15*X**2
      V=X**2
      RETURN
      END
