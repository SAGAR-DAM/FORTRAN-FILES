C PROGRAMME FOR COSMOLOGY
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      OPEN(2,FILE='d(comoving)')
      C=300000
      H0=70.D0
      Z=0.D0
      DO I=1,1000
      Z=Z+0.01D0
      D=0.0D0
      H=0.01D0/100
      N=Z/H
      X=0.0d0
      DO J=1,N
      
      D=D+F(X)*H
c      D=D+(H/3)*(F(J*H)+4*F((J+1)*H)+F((J+2)*H))
      X=X+H
      ENDDO
      D=D*C/H0
c      DM=(DLOG(D)/2.302585d0+6.D0)*5.D0-5.D0
      WRITE(2,*)Z,D
      ENDDO
      STOP
      END

      DOUBLE PRECISION FUNCTION F(X)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      F=(0.279d0*(1+X)**3+0.721d0+0.d0*(1+X)**2)**(-0.5D0)
      
      RETURN
      END

      
