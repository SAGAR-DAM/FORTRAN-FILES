c NAME: SAGAR DAM, REG: 16120917010,  ROLL: PHYS010
c integraion programme
      implicit double precision(a-h,o-z)
      open(1,file='inp')
      open(2,file='oup')
      write(2,*)'INTEGRATION PROGRAMME:.....  :)'
      WRITE(2,*)''
      write(2,*)'INTEGRATION USING TRAPEZOIDAL METHODE:'
      write(2,*)''
      a=0.d0
      b=dacos(-1.d0)      
      ext=dcos(a)-dcos(b)
11    format(i2,2x,f20.18,3x,f20.18,3x,f20.18)
12    format(a,3x,a,16x,a,18x,a)
      write(2,12)'n','integal','error','% error'
C DOING THE INTEGRAL
      do 10 i=1,9
      y=0.d0
      z=0.d0
      k=0

C TRAPEZOIDAL METHODE
      if(i.le.4)then
      read(1,*)n
      h=(b-a)/n
      do 20 j=1,n
      y=y+(f(a+j*h)+f(a+(j-1)*h))*h/2
20    CONTINUE 
      err=dabs(ext-y) 
      p=err*100/ext
      WRITE(2,*)''
      write(2,11)n,y,err,p

C END OF TRAPEZOIDAL CALCULATION
      else
      if(i.eq.5)then
      rewind(1)
      write(2,*)''
      write(2,*)'INTEGRATION USING SIMPSON 1/3rd METHODE'
      WRITE(2,*)''
      write(2,12)'n','integral','error','% error'
      
C SYMPSON METHODE
      else
      read(1,*)n
      h=(b-a)/n
      do 30 j=0,n-1
      k=k+1
      if(k.le.(n/2))then
      l=2*j
      z=z+(h/3)*(f(a+l*h)+4*f(a+(l+1)*h)+f(a+(l+2)*h))
      else
      endif
30    continue
      err=dabs(ext-z)
      p=err*100/ext
      write(2,*)''
      write(2,11)n,z,err,p
      endif
      endif      
10    continue
      stop
      end

C FUNCTION
      double precision function f(x)
      implicit double precision(a-h,o-z)
      f=dsin(x)
      return
      end
