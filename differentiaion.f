c NAME: SAGAR DAM; ROLL NO: PHYS010 ; REG NO: 16120917010
c programme for differentiation
      implicit double precision(a-h,o-z)
      open(1,file='inp')
      open(2,file='oup')
      x=0.3d0
      ext=df(x)
      write(2,*)'PROGRAMME FOR DIFFERENTIATION IN FORWARD & CENTRAL
     &DIFFERENCE'
      write(2,*)'FOR THE FUNCTION f(x)=x*exp(-x/2) AT x=',x
      write(2,*)''
      write(2,*)'EXACT VALUE UPTO DOUBLE PRECISION: ',ext
      write(2,*)''
      WRITE(2,*)'FORWARD DIFFERENCE'
      write(2,*)''
30    format(f6.4,f30.25,f30.25,f30.25)
40    format(a,8x,a,13x,a,28x,a)
      write(2,40)'h','differentiation val','error','error/h'
      write(2,*)''
c differentiation working......
      do 12 i=1,7

c forward difference
      if(i.le.3)then
      
      read(1,*)h
      y=(f(x+h)-f(x))/h
      err=dabs(ext-y)
      order=err/h
      write(2,30)h,y,err,order
      write(2,*)''

c central difference     
      else
      if(i.eq.4)then
      close(1)
      open(1,file='inp')
      write(2,*)'CENTRAL  DIFFERENCE'
      write(2,*)''
      write(2,40)'h','differentiation val','error','error/(h^2)'
      else
      read(1,*)h
      y=(f(x+h)-f(x-h))/(2*h)
      err=dabs(ext-y)
      order=err/(h**2)
      write(2,*)''
      write(2,30)h,y,err,order
      endif
      endif
12    continue
      STOP
      end

c functions
      double precision function f(x)
      implicit double precision(a-h,o-z)
      f=x*dexp(-x/2)
      return
      end

      double precision function df(x)
      implicit double precision(a-h,o-z)
      df=dexp(-x/2)-0.5d0*x*dexp(-x/2)
      return
      end
