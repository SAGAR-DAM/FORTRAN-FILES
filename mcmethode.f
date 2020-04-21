C NAME: SAGAR DAM; ROLL NO: PHYS010; REG: 16120917010
c programme for statistical integation
      implicit double precision(a-h,o-z)
      open(2,file='oup')
c      write(2,*)'INTEGRATION USING STATISTICAL METHODE'
c      write(2,*)'INTEGRATION OF: 2*INT[1/(1-X^2)^(1/2)]dX FROM 0 TO 1.0'
      write(*,*)''
      a=0.d0
      b=1.d0
      n=100
      ext=dacos(-1.d0)
50    format(8x,a)

      write(*,50)'n       INTEGRAL               % ERROR'
c      write(2,50)'n       INTEGRAL               % ERROR'
30    format(i10,3x,f20.15,3x,f20.15,1x,a)
      do 10 i=1,10000
      k=0
      do 20 j=1,n
      x=a+(b-a)*rand()
      z=f(a)*rand()
      if(z.le.f(x))then
      k=k+1
      endif
20    continue
      y=k*1.d0
      w=n*1.d0
      v=(b-a)*f(a)*(y/w)*4.d0
      e=abs(ext-v)/ext*100.d0
      write(*,30)n,v,e,'%'
      write(2,*)n,v
      n=n+10
10    continue
      stop
      end

      double precision function f(x)
      implicit double precision(a-h,o-z)
      f=1/(1+x**2)
      return
      end
