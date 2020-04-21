C NAME: SAGAR DAM; ROLL: PHYS010; REG: 16120917010          
          implicit double precision(a-h,o-z)
          OPEN(2,FILE='OUP')
          write(2,*)'BISECTION CALCULATOR BY SAGAR'
          z=0.d0
          x=0.d0
          y=0.d0
          n=0        
          write(2,*)'   x       f(x)'  
          do 1 i=0,10
          r=f(i+0.d0)
200       format(3x,i2,6x,f25.20)
          write(*,*)'THE VALUE OF f(',i,') IS: ',r
          write(2,200)i,r
    1	  continue
          write(*,*)''
          write(*,*)'NO OF BISECTION YOU WANT: '
          read(*,*)n
          write(*,*)'CHOOSE SEED POINTS: '
          read(*,*)x,y
          if(f(x)==0.d0.or.f(y)==0.d0)then
              if(f(x)==0.d0)then
              write(*,*)'THE EXACT SOLUTION:  ',x
              else
              write(*,*)'THE EXACT SOLUTION:  ',y
              endif
          else
              if(f(x)*f(y).ge.0.d0)then
              write(*,*)'THESE ARE NOT SEED POINTS.......  :('
              
              else
210           format(a,6x,a,16x,a,16x,a,16x,a,13x,a,13x,a)
              write(*,210)'Etn','x','y','z','f(x)','f(y)','f(z)'
              write(2,210)'Etn','x','y','z','f(x)','f(y)','f(z)'
              write(2,*)''
              
              do 2 i=1,n
              z=(x+y)/2
  20   format(i2,6x,f15.12,2x,f15.12,2x,f15.12,2x,f15.12,2x,f15.12,2x,f
     &15.12)           
              write(*,20)i,x,y,z,f(x),f(y),f(z)
              write(2,20)i,x,y,z,f(x),f(y),f(z)
  100             format(i6,3x,f50.40)  
                  if(f(x).eq.0.d0)then
                  write(*,*)'The exact solution is:  ',x
                  go to 50
                  elseif(f(y).eq.0.d0)then
                  write(*,*)'The exact solution is:  ',y
                  go to 50
                  elseif(f(x)*f(z).ge.0.d0)then
                  x=z
                  else
                  y=z
                  endif
  2           continue
  10          format(a,f50.45)
              write(*,*)''
      write(2,10)'The numerical solution after your choosen stps is: ',z
      write(*,*)''
      write(2,10)'With a precicion of:  f(x)=',f(z)
              endif
          endif
50    write(*,*)''
          stop
          end
          
          
          double precision function f(x)
          implicit double precision(a-h,o-z)
          f=dexp(-x/2)*dacosh(dexp(x/2))-dexp(-x**2)
          return
          end
          
          
          

