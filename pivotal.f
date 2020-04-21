C NAME: SAGAR DAM; ROLL: PHYS010; REG: 16120917010
C PIVOTAL ELIMINATION
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(2,2),S(2),B(2),X(2)
      N=2
      OPEN(2,FILE='OUP')

      DATA (A(1,J),J=1,2) /0.2205,0.1254/
      DATA (A(2,J),J=1,2) /0.4457,0.2506/
      DATA (B(I), I=1,2)  /0.6606,0.8897/
      WRITE(2,*)'PIVOTAL GAUSS ELIMINATION.....'
      WRITE(2,*)'THE AUGMENTED MATRIX IS: '
      DO I=1,N
      WRITE(2,*)(A(I,J),J=1,2),B(I)
      ENDDO 
      
      DO K=1,N-1
      DO I=K,N
      S(I)=0.D0
      DO J=K,N
      S(I)=MAX(S(I),abs(a(I,J)))
      ENDDO
      ENDDO

      P=ABS(A(K,K)/S(K))
      L=K
      DO J=K+1,N
      IF(ABS(A(J,K)/S(J)).LT.P)THEN
      P=ABS(A(J,K)/S(J))
      L=J
      ENDIF
      ENDDO
      
      IF(P.EQ.0.D0) THEN
      WRITE(2,*)'THE MATRIX IS SINGULAR'
      ENDIF

      IF(L.NE.K)THEN
      DO J=K,N
      F=A(K,J)
      A(K,J)=A(L,J)
      A(L,J)=F
      ENDDO
      
      F=B(K)
      B(K)=B(L)
      B(L)=F
      ENDIF

      DO I=K+1,N
      C=A(I,K)/A(K,K)
      A(I,K)=0.D0
      B(I)=B(I)*C*B(K)
      DO J=K+1,N
      A(I,J)=A(I,J)-C*A(K,J)
      ENDDO
      ENDDO
      ENDDO
      
      WRITE(2,*)'REDUCED FORM'
      DO I=1,N
      WRITE(2,*)(A(I,J),J=1,2),B(I)
      ENDDO

      X(N)=B(N)/A(N,N)
      DO I=N-1,1,-1
      C=0.D0
      DO J=I+1,N
      C=C+A(I,J)*X(J)
      ENDDO
      X(I)=(B(I)-C)/A(I,I)
      ENDDO
      WRITE(2,*)''

      WRITE(2,*)'THE RESUTLT (SOLUTION): '
      DO I=1,N
      WRITE(2,*)X(I)
      WRITE(*,*)X(I)
      ENDDO
      STOP
      END
