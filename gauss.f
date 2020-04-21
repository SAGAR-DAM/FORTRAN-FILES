C NAME:  SAGAR DAM; ROLL: PHYS010;  REG: 16120917010
C INTEGRATION BY 4 POINT GAUSS QUADRATURE
      implicit double precision(a-h,o-z)
      dimension u(4),w(4)
      open(2,file='oup')
      z=0.d0
      ext=f(1.d0)-1
      write(2,*)'VALUE OF INT[EXP(X)]dX FROM 0 TO 1'
      u(1)=(3.d0/7.d0-2.d0/7.d0*(6.d0/5.d0)**0.5d0)**0.5d0
      u(2)=u(1)*(-1.d0)
      u(3)=(3.d0/7.d0+2.d0/7.d0*(6.d0/5.d0)**0.5d0)**0.5d0
      u(4)=u(3)*(-1.d0)
      w(1)=(18.d0+30.d0**0.5d0)/36.d0
      w(2)=w(1)
      w(3)=(18.d0-30.d0**0.5d0)/36.d0
      w(4)=w(3)
      write(2,*)'THE EXACT RESULT IS: ',ext
      do 10 i=1,4
      x=u(i)/2+0.5d0
      z=z+f(x)*w(i)*0.5d0
10    continue
      write(2,*)'THE NUMERICAL RESULT IS: ',Z
      err=dabs(ext-z)
      write(2,*)'WITH AN ERROR: ',err
      write(2,*)'PERCENTAGE ERROR: ',err/ext*100.d0
      stop
      end

      double precision function f(x)
      implicit double precision(a-h,o-z)
      f=dexp(x)
      return
      end
