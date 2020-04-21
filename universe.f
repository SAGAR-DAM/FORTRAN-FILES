C AGE OF UNIVERSE
      implicit DOUBLE PRECISION(A-H,O-Z)
      H0=70.D0
      B=10**5

      A=0.D0
      X=0.D0
      H=0.01
      N=(B-A)/H
      AGE=0.D0
      DO I=1,N
      AGE=AGE+F(X)/(1+X)*H
      X=X+H
      ENDDO
      AGE=AGE*30856778570831.27D0/(365*H0*86400*10**3)
      WRITE(*,*)'AGE OF UNIVERSE: ',AGE,'Gyr'
      STOP
      END

      DOUBLE PRECISION FUNCTION F(X)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      OM=0.279D0
      OL=0.721D0
      OC=0.0D0
      F=(0.25d0*(1+X)**3+0.d0+0.75d0*(1+X)**2)**(-0.5D0)
      RETURN
      END
