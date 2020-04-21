csddddd
      implicit double precision(a-h,o-z)
      dimension a(20,20),x(20)
      write(*,*)'enter order:'
      read(*,*)n
      write(*,*)'input augmented matrix'
      do i=1,n
      read(*,*)(a(i,j),j=1,n+1)
      enddo

      do k=1,n
      do i=k+1,n
      u=a(k,k)/a(i,k)
      do j=k,n+1
      a(i,j)=a(k,j)-u*a(i,j)
      enddo
      enddo
      enddo

      write(*,*)'reduced form:'
      do i=1,n
      write(*,*)(a(i,j),j=1,n+1)
      enddo
      x(n)=a(n,n+1)/a(n,n)

      do k=n-1,1,-1
      s=0.d0
      do j=k+1,n
      s=s+x(j)*a(k,j)
      enddo
      x(k)=(a(k,n+1)-s)/a(k,k)
      enddo

      do i=1,n
      write(*,*)x(i)
      enddo
      stop
      end



