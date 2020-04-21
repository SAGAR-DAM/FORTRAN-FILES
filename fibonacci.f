c fibonacci lattice
      dimension a(500,2,2),ab(2,2),x(2),y(2)
      open(2,file='oup')
      write(*,*)'enter Energy: '
      read(*,*)e
      write(*,*)'enter Ea: '
      read(*,*)ea
      write(*,*)'enter Eb: '
      read(*,*)eb
      write(*,*)'enter t: '
      read(*,*)t
      write(*,*)'enter the order of fibonacci lattice: '
      read(*,*)n
      a(1,1,1)=(e-eb)/t
      a(1,1,2)=-1.d0
      a(1,2,1)=1.d0
      a(1,2,2)=0.d0

      a(2,1,1)=(e-ea)/t
      a(2,1,2)=-1.d0
      a(2,2,1)=1.d0
      a(2,2,2)=0.d0
      m=n+1

      do i=3,n
      do j=1,2
      do k=1,2
      a(i,j,k)=0.d0
      do l=1,2
      a(i,j,k)=a(i,j,k)+a(i-2,j,l)*a(i-1,l,k)
      enddo
      enddo
      enddo
      enddo

      write(*,*)'The transfer matrix of the given lattice: '
      do i=1,2
      write(*,*)(a(n,i,j),j=1,2)
      enddo

      x(1)=1
      x(2)=0
      ab(1,1)=a(n,1,1)
      ab(1,2)=a(n,1,2)
      ab(2,1)=a(n,2,1)
      ab(2,2)=a(n,2,2)

      do i=3,500
      do j=1,2
      y(j)=0.d0
      do k=1,2
      y(j)=y(j)+ab(j,k)*x(k)
      enddo
      enddo
      x(2)=x(1)
      x(1)=y(1)
      write(2,*)i,y(1)
      enddo

      stop
      end
      
