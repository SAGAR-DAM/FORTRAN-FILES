C NAME: SAGAR DAM; ROLL: PHYS010; ROLL: 16120917010
C SOLVING OF DIFFERENTIAL EQN BY TAYLOR AND RUNGE KUTTA METHODE

      implicit double precision(a-h,o-z)
      OPEN(2,FILE='oup')
      write(2,*)'SOLVING OF dy/dx = x+y BY TAYLOR & RUNGE KUTTA METHODE'
      write(2,*)'WITH INITIAL CONDITION: y(x=0)=1'
      WRITE(2,*)''
      write(2,*)''
110   format(2x,a,20x,a,20x,a,10x,a)
      write(2,*)'USING TAYLOR SERIES METHODE:'
      WRItE(2,*)''
      WRItE(2,*)''
      write(2,110)'x','y','exact value','error'
      x=0.d0
120   format(44x,a)
      write(2,120)'y = 2*exp(x)-x-1'
      y=1.d0
      h=0.1d0
100   formAT(2x,f18.16,3x,f18.16,3x,f18.16,3x,f18.16)
      do 10 i=1,21

      if(i.le.10)then
      y=y+h*g(x,y)+h**2*(1+g(x,y))/2+h**3*(1+g(x,y))/6
      x=x+h
      ext=f(x)
      err=dabs(ext-y)
      write(2,100)x,y,ext,err

      else if(i.eq.11) then
      x=0.d0
      y=1.d0
      write(2,*)''
      write(2,*)'USING RUNGE KUTTA METHODE:'
      write(2,*)''
      write(2,*)''
      write(2,110)'x','y','exact value','error'
      write(2,120)'y = 2*exp(x)-x-1'

      ELSE
      c1=h*g(x,y)
      c2=h*g(x+h/2,y+c1/2)
      c3=h*g(x+h/2,y+c2/2)
      c4=h*g(x+h,y+c3)
      y=y+(c1+2*c2+2*c3+c4)/6
      x=x+h
      ext=f(x)
      err=dabs(ext-y)
      write(2,100)x,y,ext,err

      endif
10    continue


      stop
      end


      double precision function f(x)
      implicit double precision(a-h,o-z)
      f=2*dexp(x)-x-1.d0
      return
      end

      double precision function g(x,y)
      implicit double precision(a-h,o-z)
      g=x+y
      return
      end
