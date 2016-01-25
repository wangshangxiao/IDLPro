;************************************************************************************
;���漸��������HANTS���㷨����,(����*֮��)
FUNCTION MATINV,N,C,D

	A=FLTARR(N+1,N+1)
	B=FLTARR(N+1,N+1)
	A[1:N,1:N]=C[1:N*N]
	B[1:N,1:N]=D[1:N*N]
	IPIV=FLTARR(200)
	IFREE=FLTARR(200)

	FOR I=1,N DO BEGIN
	 	IFREE[I]=1
	 	FOR J=1,N DO BEGIN
	    	IF (I EQ J) THEN BEGIN
	    		B[I,J]=1.0
	    	ENDIF ELSE BEGIN
	    	  	B[I,J]=0.0
	    	ENDELSE
	    ENDFOR
   ENDFOR

FOR K=1,N DO BEGIN
	XMAX=0.0
	FOR J=1,N DO BEGIN
	    IF (IFREE[J] EQ 1) THEN BEGIN
	       	IF (ABS(A[J,K]) GE XMAX) THEN BEGIN
		  		XMAX=ABS(A[J,K])
		  		JMAX=J
	       	ENDIF
	    ENDIF
   	ENDFOR
   	;�ҳ�����A��ÿ�е����Ԫ�����ڵ���,���Ѹ���ֵ������JMAX��

	IFREE[JMAX]=0
	IPIV[K]=JMAX
	J=JMAX
	DIV=1.0/A[JMAX,K]

 	FOR I=1,N DO BEGIN
	    A[J,I]=A[J,I]*DIV
	    B[J,I]=B[J,I]*DIV
   	ENDFOR

	FOR J=1,N DO BEGIN
	    X=A[J,K]
	    IF (J NE JMAX) THEN BEGIN
	       	FOR I=1,N DO BEGIN
		  		A[J,I]=A[J,I]-X*A[JMAX,I]
		  		B[J,I]=B[J,I]-X*B[JMAX,I]
   			ENDFOR
	    ENDIF
   	ENDFOR

ENDFOR

FOR K=1,N DO BEGIN
	J=IPIV[K]
	FOR I=1,N DO BEGIN
		A[K,I]=B[J,I]
   	ENDFOR
ENDFOR

FOR I=1,N DO BEGIN
	FOR J=1,N DO BEGIN
	    B[I,J]=A[I,J]
  	ENDFOR
ENDFOR

RESULT=FLTARR(N*N+1)
RESULT[1:N*N]=B[1:N,1:N]
RETURN,RESULT

END

FUNCTION QRANK ,N,A,R

	ST=INTARR(40)
	EN=INTARR(40)

	FOR I=1,N DO BEGIN
	 	R(I)=I
	ENDFOR

    SP=1
    ST(SP)=1
    EN(SP)=N

    WHILE (SP GE 1) DO BEGIN
	 	IS=ST(SP)
	 	IE=EN(SP)

   		JUMP10:

        I=IS
	    J=IE
	    X=A(R((I+J)/2))

   		JUMP20:

        WHILE (A(R(I)) LT X) DO BEGIN
	      	I=I+1
	    ENDWHILE
	    WHILE (A(R(J)) GT X) DO BEGIN
	        J=J-1
	    ENDWHILE

	    IF (I GT J) THEN GOTO,JUMP30

	    K=R(I)
	    R(I)=R(J)
	    R(J)=K

	    I=I+1
	    J=J-1

	    IF (I LE J) THEN GOTO,JUMP20

   		JUMP30:

        N1=J-IS+1
	    N2=IE-I+1

        IF (N1 GT N2) THEN BEGIN
	       	ST(SP)=IS
	       	EN(SP)=J
	       	SP=SP+1
	       	IS=I
	    ENDIF ELSE BEGIN
	       IF (N2 GT 1) THEN BEGIN
	          ST(SP)=I
	          EN(SP)=IE
	          SP=SP+1
	       ENDIF
	       IE=J
	    ENDELSE

	 	IF ((N1 GT 1)&&(N2 GT 1)) THEN GOTO,JUMP10

	 	SP=SP-1
	ENDWHILE

   	RETURN,R
END

FUNCTION SPINT,NI,IST,ISTEP,NF,PA,PER,AMP,PHI,SCALE


;		ISTΪ��ʼʱ��
;		ISTEPΪ����
;		NP,NLΪͼ��ĸߺͿ�,������Ӧ�ö�Ϊ1
;
	CS=FLTARR(1200)
	SN=FLTARR(1200)
	OUT_DATA=FLTARR(NI+1)

  	PI=ATAN(1.0)*4.0
  	RD=PI/180.0

    FOR I=1,PER DO BEGIN
	 	ANG=2.0*PI*(I-1)/PER
	 	CS(I)=COS(ANG)
	 	SN(I)=SIN(ANG)
    ENDFOR

    IEN=IST+(NI-1)*ISTEP

 	;��ǰ��Ĵ�����HPINT�н�AMP�е���ֵ����IODAT,�������ְ�IODAT�е���ֵ����AMP
	;����ʹ����ȫ�ֱ���,�Ͳ��ý������ֱ����Ĵ�����.
	;ע�������¼���

	IO=0
	FOR II=IST,IEN,ISTEP DO BEGIN
	  	SUM=AMP(1)
	    FOR I=1,NF DO BEGIN
	     	IFR=FIX(FLOAT(PER)/PA(I))
		  	INDEX=1+(IFR*(II-1) MOD PER)
	 	  	RE=AMP(I+1)*COS(RD*PHI(I+1))
	 	  	IM=AMP(I+1)*SIN(RD*PHI(I+1))
		  	SUM=SUM+RE*CS(INDEX)+IM*SN(INDEX)
	   	ENDFOR
	    IO=IO+1
	    OUT_DATA[IO]=SUM
	ENDFOR

    RETURN,OUT_DATA
    END
;-----------------------------------------------------------------
;�������������������,������һ���������HANTS�����ع�����Ҫ�Ĳ���
;�������Щ�����Ժ�,�Ϳ���ͨ��SYNTHP�����ߵ��ع�
FUNCTION HANTSNEW ,NI,PER,Y,TS,NF,PA,HILO,IDRT,FET,DOD,AMP,PHI


;	CALL HANTSNEW (NI,PER,VAL,ITS,NF,PA,HILO,IDRT,FET,DOD,AMP,PHI)
;	��ʽ��HPINT�еĵ������
;
;	NI�� 	NUMBER OF IMAGES
;	PER��	PERIOD�����ڴ�С��Ϊ360��
;	Y:		����Ϊһ������,Ҳ����Ϊָ������,����ŵ���һ��������ʱ�����е�����,���СΪNI
;	TS:		Ϊһ������,��NI��С��ͬ,������ÿ��ͼ���ʱ��,���ļ��б��ж������ļ����ƺ���������л�ȡ
;	NF:		NUMBER OF FREQUENCIES,ʱ���ڵ����ڸ�����Ϊ2��3
;	PA��	���岻�Ǻ����ף���һ�����飬ΪPER/NF��PER/��NF-1����������������
;	HILO��	�ߵ�ֵ��־����ָ��ȥ���Ǹ�ֵ���ǵ�ֵ
;	IDRT��	�ߵ�����ֵ�趨��-500����˼����
;	FET��	��������趨��50/100/500/1000��������������
;	DOD��	DOD���������趨��2/3/6��>2*NR_OF_FREQUENCIES+1��,ʣ��������
;	AMP:	Ϊһ��ָ��,ָ��	PAMP=MALLOC((NF+1)*4),������Ϊ���,AMPLITUDES
;	PHI:	Ϊһ��ָ��,ָ��	PPHI=MALLOC((NF+1)*4)

	MAT	=	FLTARR((2*NF+1)+1,NI+1)
	YR	=	FLTARR(NI+1)
	CS	=	FLTARR(PER+1)
	SN	=	FLTARR(PER+1)

	A	=	FLTARR(1600)
	B	=	FLTARR(1600)
	ZR	=	FLTARR(40)
	ZA	=	FLTARR(40)

	MAXERR=	FLOAT(0)
	DIFF=	FLTARR(NI+1)
	ERR	=	FLTARR(NI+1)
	EL	=	FLOAT(0)

	P=INTARR(NI+1)

	RANK=INTARR(1200)

	OUT_DATA=FLTARR(NI+1)
      IFIRST=1

    IF (IFIRST EQ 1) THEN BEGIN
    	SHILO=0
	 	IF (HILO EQ 'HI') THEN SHILO=-1
        IF (HILO EQ 'LO') THEN SHILO=1
        NR=2*NF+1
        IF (NR GT NI) THEN NR=NI
        NOUTMAX=NI-NR-DOD

        PI=4.*ATAN(1.0)
        RD=PI/180.0
        DG=180.0/PI
;		RD:	ÿ�ȽǵĻ���
;		DG:	�ڻ��ȵĽǶ�
;
;
       	FOR J=1,NI DO BEGIN
	   		MAT(1,J)=1.0
        ENDFOR
        FOR I=1,PER DO BEGIN
	    	ANG=2.0*PI*(I-1)/PER
	    	CS(I)=COS(ANG)
	    	SN(I)=SIN(ANG)
	 	ENDFOR

	 	IFIRST=0
	ENDIF

    FOR I=1,NF DO BEGIN

	 	IFR=FIX(FLOAT(PER)/PA(I))
	 	FOR J=1,NI DO BEGIN
	    	INDEX=1+(IFR*(TS(J)-1) MOD PER)
	    	MAT(2*I  ,J)=CS(INDEX)
	    	MAT(2*I+1,J)=SN(INDEX)
		ENDFOR
  	ENDFOR
    NOUT=0

   	FOR IP=1,NI DO BEGIN
	 	P(IP)=1
	 	IF (IDRT LT 0) THEN BEGIN
 	    	IF (Y(IP) LT IDRT*(-1)) THEN BEGIN
	       		P(IP)=0
	       		NOUT=NOUT+1
	    	ENDIF
	 	ENDIF ELSE	BEGIN
	    	IF (Y(IP) GT IDRT) THEN BEGIN
	       		P(IP)=0
	       		NOUT=NOUT+1
	    	ENDIF
	 	ENDELSE
	ENDFOR

   	IF (NOUT GT NOUTMAX) THEN BEGIN
    	FOR I=1,NF+1 DO BEGIN
	    	AMP(I)=0.0
	    	PHI(I)=0.0
      	ENDFOR
        RETURN,0
  	ENDIF

   	READY=0

  	WHILE (~READY) DO BEGIN	;NOT READY

    	FOR I=1,NR DO BEGIN
	    	ZA(I)=0.0
	    	FOR J=1,NI DO BEGIN
	       		ZA(I)=ZA(I)+P(J)*MAT(I,J)*Y(J)
	    	ENDFOR
	 	ENDFOR

		FOR I=1,NR DO BEGIN
	    	IEL=I
	    	FOR J=1,NR DO BEGIN
	       		EL=0.0
	       		FOR K=1,NI DO BEGIN
		  			EL=EL+P(K)*MAT(I,K)*MAT(J,K)
	       		ENDFOR
	       		A(IEL)=EL
	       		IEL=IEL+NR
	    	ENDFOR
		ENDFOR

		;*****************************************************************************************
		;		CALL MATINV(NR,A,B)
		;		���������Ҫ����д,���ڴ˴�ע�͵�

		TEMP=MATINV(NR,A,B)
		B[1:NR*NR]=TEMP[1:NR*NR]
		A=B
		;*****************************************************************************************

  	 	FOR I=1,NR DO BEGIN
	    	ZR(I)=0.0
	    	IEL=I
	    	FOR J=1,NR DO BEGIN
	       		ZR(I)=ZR(I)+B(IEL)*ZA(J)
	       		IEL=IEL+NR
	    	ENDFOR
	 	ENDFOR

	 	FOR I=1,NI DO BEGIN
	    	YR(I)=0
	    	FOR J=1,NR DO BEGIN
	       		YR(I)=YR(I)+MAT(J,I)*ZR(J)
	    	ENDFOR
	    	DIFF(I)=SHILO*(YR(I)-Y(I))
  	    	ERR(I)=P(I)*DIFF(I)
	 	ENDFOR

		;*****************************************************************************************
		;CALL QRANK(NI,ERR,RANK)
		;�ú���Ҳ��Ҫ��������,��û�п�ʼ����

		RANK=QRANK(NI,ERR,RANK)

		;*****************************************************************************************
	 	MAXERR=DIFF(RANK(NI))
	 	READY=((MAXERR LE FET)||(NOUT EQ NOUTMAX))


	 	IF (~READY) THEN BEGIN
	    	I=NI
	    	J=RANK(I)
	    	WHILE ((P(J)*DIFF(J) GT MAXERR*0.5)&&(NOUT LT NOUTMAX)) DO BEGIN
	       		P(J)=0
	       		NOUT=NOUT+1
	       		I=I-1
	       		J=RANK(I)
	    	ENDWHILE
	 	ENDIF
   	ENDWHILE
    AMP(1)=ZR(1)
    PHI(1)=0.0
    ZR(NI+1)=0.0
   	FOR I=2,NR,2 DO BEGIN
	 	IFR=(I+2)/2
      	RA=ZR(I)
	 	RB=ZR(I+1)
	 	AMP(IFR)=SQRT(RA*RA+RB*RB)
   	 	PHASE=ATAN(RB,RA)*DG
	 	IF (PHASE LT 0.0) THEN PHASE=PHASE+360.0
	 	PHI(IFR)=PHASE
  	ENDFOR

  	IST=10
  	ISTEP=10

	OUT_DATA=SPINT(NI,IST,ISTEP,NF,PA,PER,AMP,PHI,SCALE)

  	RETURN,OUT_DATA
END
;***********************************************************************************