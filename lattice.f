C LATTICE PROGRAMME
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(2,2),X(2),Y(2)
      OPEN(2,FILE='OUP')
      WRITE(*,*)'ENTER VALUE OF ENERGY: '
      READ(*,*)E
      WRITE(*,*)'ENTER VALUE OF EPSA: '
      READ(*,*)P
      WRITE(*,*)'ENTER VALUE OF HOPPING PARAMETER: '
      READ(*,*)T
      WRITE(*,*)'ENTER NO OF ORDERING PARAMETER: '
      READ(*,*)N
      DATA(X(I),I=1,2) /0,1/
c      B=(E-P)/T
c      A(1,1)=B
      A(1,2)=-1
      DATA(A(2,I),I=1,2) /1, 0/

      DO I=1,N
      T1=T**2/(E-P)
      P1=P+2*T**2/(E-P)
      T=T1
      P=P1
      ENDDO

      B=(E-P)/T
      A(1,1)=B
      write(*,*)P,T
      DO I=1,200
      
      DO J=1,2
      Y(J)=0.D0
      DO K=1,2
      
      Y(J)=Y(J)+A(J,K)*X(K)
      ENDDO
      ENDDO
      WRITE(2,*)I,Y(1)
      X(2)=X(1)
      X(1)=Y(1)
      ENDDO

      STOP
      END
