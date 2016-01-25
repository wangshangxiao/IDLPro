;������ʱ��ת��Ѯ����ť�����޸ģ�
FUNCTION DAY_TO_TENDAY,StartEndTime,Prompt = Prompt

	StartYear    = StartEndTime[0]
	StartMonth   = StartEndTime[1]
	StartTenday  = StartEndTime[2]
	StartDay     = StartEndTime[3]

	EndYear   = StartEndTime[4]
	EndMonth  = StartEndTime[5]
	EndTenday = StartEndTime[6]
	EndDay    = StartEndTime[7]

	CASE 1 OF
		FIX(StartYear) GT FIX(EndYear) : BEGIN
			Prompt = '��ʼ��ݲ��ܴ��ڽ�����ݣ�'
			RETURN,'Error'
		END

		FIX(StartMonth) GT FIX(EndMonth) : BEGIN
			IF FIX(StartYear) EQ FIX(EndYear) THEN BEGIN
				Prompt = '��ʼ�·ݲ��ܴ��ڽ����·ݣ�'
				RETURN,'Error'
			ENDIF
		END
		FIX(StartDay) GT FIX(EndDay) : BEGIN
			IF (FIX(StartYear) EQ FIX(EndYear)) AND (FIX(StartMonth) EQ FIX(EndMonth)) THEN BEGIN
				Prompt = '��ʼ�ղ��ܴ��ڽ����գ�'
				RETURN,'Error'
			ENDIF
		END
		else :
	ENDCASE

	startd = julday(FIX(StartMonth),FIX(StartDay),FIX(StartYear))
	endd = julday(FIX(EndMonth),FIX(EndDay),FIX(EndYear))

	timearr = timegen(start=startd,final=endd)
	caldat,timearr,month,day,year

	AllTime = strarr(n_elements(day))

	for i=0,n_elements(day)-1,1 do begin

		AllTime[i] = strtrim(string(year[i]),2)+strtrim(string(month[i],format='(I02)'),2)+strtrim(string(fix(day[i]/11)+1,format='(I02)'),2)
	endfor

	AllTime = AllTime[UNIQ(AllTime)]

	RETURN,AllTime

END

;;===============�Զ������:��Ӱ��ͼ(��ֱ��ͼ�δ�����)=============================
;;PRO FZ_Draw_Resultimage,ImageData  $                 ;Ϊָ������.Ҫ��ͼ������,
;;				    ,WID_DRAW	$                  ;DRAW���.
;;					,MUPLE_ISX_Y = muple_isx_y    ;���صĲ���Ϊ��Ԫ�ص�����,һ��Ϊ����,һ����ʶ���ĸ�����.
;;   WIDGET_CONTROL,/HOURGLASS
;;   WIDGET_CONTROL,WID_DRAW,GET_VALUE = drawID
;;   ERASE,COLOR=!D.N_COLORS-1
;;
;;   r=widget_info(WID_DRAW,/GEOMETRY)     ;&&&&&&&&&&���
;;
;;  	data = *ImageData
;;  	Datasize=SIZE(data,/DIMENSIONS)
;;    ImageMatchWidth = Datasize[0]  & ImageMatchHeight = Datasize[1]
;;
;;    Muple = (FLOAT(ImageMatchWidth)/r.SCR_XSIZE) > (FLOAT(ImageMatchHeight)/r.SCR_YSIZE)  ;;������ø�����,�����������,
;;
;;    dataRe=CONGRID(data,ROUND(ImageMatchWidth/Muple),ROUND(ImageMatchHeight/Muple))  ;��ͼ������ͬ����,�Ա�����ʾ�ڴ�����,ͬʱ����ͼ��ԭʼ��.
;;
;;  ;  WSET, drawID
;;	dataDis = BYTSCL(dataRe,TOP = !D.TABLE_SIZE-1);,MIN=0,MAX=1)  ;ʹ��BYTSCL��,һ��ʹ��TV,������TVSCL
;;    ;-----------------------
;;    DEVICE,GET_DECOMPOSED=old_color                               ;��ȡ��ǰDECOMPOSEDֵ
;;    DEVICE, RETAIN=2, DECOMPOSED=0      ;��IDL�ṩ�󱸴洢,ʹ����ɫ��ѯ��(ͣ����ɫ�ֽ⹦��),
;;
;;;   XLOADCT
;;
;;    LOADCT,39
;;;    TVLCT,255,255,255    ;ʹ0����ɫ��������ɫΪ��ɫ
;;   ;-------------------------
;;    TV,dataDis,/ORDER
;;;    TVSCL,dataDis,/ORDER
;;;    ������Ϊָ��ȡֵ��׼��.�����"0.0"��ʾX�᷽��ƥ����ʾӰ��,"1.0"���ʾY�᷽��ƥ����ʾӰ��
;;	IsX_Y = ((ImageMatchWidth/r.SCR_XSIZE) GE (ImageMatchHeight/r.SCR_YSIZE) ? 0.0 : 1.0)
;;
;;	muple_isx_y = [Muple,IsX_Y]
;;	IF SIZE(*ImageData,/TYPE) EQ 1 THEN BEGIN        ;������ֽ�����ת������,Ϊ��ͼ����
;;		WIDGET_CONTROL,WID_DRAW,SET_UVALUE = FIX(*ImageData)  ;��Ȼ��ʾָ��ֵʱ��"�ַ���غ���"���������.
;;	ENDIF ELSE BEGIN
;;		WIDGET_CONTROL,WID_DRAW,SET_UVALUE = *ImageData
;;	ENDELSE
;;	PTR_FREE,ImageData
;;	;------------------
;;  DEVICE,DECOMPOSED=old_color                                   ;����ԭ����DECOMPOSEDֵ,,�뻹ԭ.
;;
;;END
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;���漸��������HANTS���㷨����,(����*֮��)
FUNCTION FZ_MATINV,N,C,D

	on_error,2

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

FUNCTION FZ_QRANK ,N,A,R

	on_error,2

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

FUNCTION FZ_SPINT,NI,IST,ISTEP,NF,PA,PER,AMP,PHI,SCALE

	on_error,2

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
FUNCTION FZ_HANTSNEW ,NI,PER,Y,TS,NF,PA,HILO,IDRT,FET,DOD,AMP,PHI

;	CALL FZ_HANTSNEW (NI,PER,VAL,ITS,NF,PA,HILO,IDRT,FET,DOD,AMP,PHI)
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
		;		CALL FZ_MATINV(NR,A,B)
		;		���������Ҫ����д,���ڴ˴�ע�͵�
		;print,'a',a[0:9]
		;print,'b',b[0:9]
		TEMP=FZ_MATINV(NR,A,B)
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
		;CALL FZ_QRANK(NI,ERR,RANK)
		;�ú���Ҳ��Ҫ��������,��û�п�ʼ����

		RANK=FZ_QRANK(NI,ERR,RANK)

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

	OUT_DATA=FZ_SPINT(NI,IST,ISTEP,NF,PA,PER,AMP,PHI,SCALE)

  	RETURN,OUT_DATA
END
;;***********************************************************************************
;;************������*******������*******������***************************************
;;PRO MulCropping_Hants,Zonefile,NDVIfile,Outputfile
;;	;********************************************************************************************
;;	;�����������е�HANTS������Ҫʹ�õĲ���,HANTS����ԭ������Ϊ"�ɼ̻���ʿ(2003��)"
;;	;	NI�� 	NUMBER OF IMAGES
;;	;	PER��	PERIOD�����ڴ�С��Ϊ360��
;;	;	Y:		����Ϊһ������,Ҳ����Ϊָ������,����ŵ���һ��������ʱ�����е�����,���СΪNI
;;	;	TS:		Ϊһ������,��NI��С��ͬ,������ÿ��ͼ���ʱ��,���ļ��б��ж������ļ����ƺ���������л�ȡ
;;	;	NF:		NUMBER OF FREQUENCIES,ʱ���ڵ����ڸ�����Ϊ2��3,Ƶ��
;;	;	PA��	���岻�Ǻ����ף���һ�����飬ΪPER/NF��PER/��NF-1����������������
;;	;	HILO��	�ߵ�ֵ��־����ָ��ȥ���Ǹ�ֵ���ǵ�ֵ
;;	;	IDRT��	�ߵ�����ֵ�趨��-500����˼����
;;	;	FET��	��������趨��50/100/500/1000��������������
;;	;	DOD��	DOD���������趨��2/3/6��>2*NR_OF_FREQUENCIES+1��,ʣ��������
;;	;	AMP:	Ϊһ��ָ��,ָ��	PAMP=MALLOC((NF+1)*4),������Ϊ���,AMPLITUDES
;;	;	PHI:	Ϊһ��ָ��,ָ��	PPHI=MALLOC((NF+1)*4)
;;
;;	WIDGET_CONTROL ,/HOURGLASS
;;
;;;;    MulcroppingZone='I:\����ָ������\Addfarm_MulCroppingZone'       ;���븴��ָ������
;;	OPENR,LUN_mulcropping,Zonefile,/GET_LUN                           ;���븴��ָ������
;;    TempMulcropping = ASSOC(LUN_mulcropping,BYTARR(1))
;;
;;;;    NDVI_1_10Month='I:\����ָ������\2005ndvi'                       ;������Ӻ��NDVI����.
;;	OPENR,LUN_ndvi,NDVIfile,/GET_LUN                                  ;������Ӻ��NDVI����.
;;    TempNDVI30 = ASSOC(LUN_ndvi,BYTARR(30))
;;	;********************************************************************************************
;;	HILO='LO'
;;	IDRT=-10
;;	FET=5
;;	DOD=5
;;;;	SCALE=1
;;
;;    TempHead = BYTARR(1)
;;    ValueNum = 25440000L            ;����Ϊ5300*4800=25440000
;;
;;    AllCroppingValue = BYTARR(ValueNum)
;;
;;	progressTimer = Obj_New("ShowProgress", tlb,/CancelButton)
;;	progressTimer->start
;;
;;    FOR i=0L,ValueNum-1 DO BEGIN
;;
;;    	cancelled = progressTimer->CheckCancel()
;;	   IF cancelled THEN BEGIN
;;	    	ok = Dialog_Message('�����ѱ�����ֹ!!!')
;;		    progressTimer->Destroy ;����������
;; 			OBJ_DESTROY,progressTimer
;;	      RETURN
;;	   ENDIF
;;	   progressTimer->Update, (i/25440000. * 100.0) ;��������
;;
;;             NDVI30=TempNDVI30[i]
;;             CroppingZone=TempMulcropping[i]
;;
;;        IF CroppingZone[0] NE 0 THEN BEGIN
;;            NI=30
;;            PER=NI*10
;;        	TS=FINDGEN(NI+1)*10
;;        	NF=CroppingZone[0]                   ;ȡ���������е�ֵ��ΪƵ��
;;        	PA=INDGEN(NF+1)
;;			FOR k=1,NF DO BEGIN
;;				PA[k]=PER/k
;;			ENDFOR
;;			AMP=FLTARR((NF+1)+2)
;;			PHI=FLTARR((NF+1)+2)
;;
;;            Y_TEMP = [TempHead,NDVI30]                      ;����������һ��ͷԪ��,��Hants �����Ҫ��.
;;     		ARR_RESULT=FZ_HANTSNEW(NI,PER,Y_TEMP,TS,NF,PA,HILO,IDRT,FET,DOD,AMP,PHI)
;;
;;            IF ARRAY_EQUAL(ARR_RESULT,0) NE 1 THEN BEGIN    ;ƽ���������в�Ϊ0,��������ȱ����̫����ܻ�����0
;;               CASE CroppingZone[0] OF
;;              	1 : BEGIN
;;             	    Tempsmooth = ARR_RESULT[13:27]   ;һ���ȡ5-9�¹�15Ѯ
;;             	    Num = 15
;;    			END
;;
;;    			2 : BEGIN
;;    	  			  Tempsmooth = ARR_RESULT[7:27]    ;�����ȡ3-9�¹�21Ѯ
;;    			    Num = 21
;;    			END
;;
;;    			3 : BEGIN
;;    	   			 Tempsmooth = ARR_RESULT[1:30]          ;�����ȡ1-10�¹�30Ѯ
;;    			    Num = 30
;;    		     END
;;
;;    		     ELSE :
;;   			    ENDCASE
;;
;;                Diff1 = INTARR(Num-1)
;;                FOR m=0,Num-2 DO BEGIN
;;                     IF (Tempsmooth[m+1]-Tempsmooth[m]) GE 0 THEN BEGIN   ;�ú�һ��ֵ����ǰһ��ֵ,���жϲ�ֵ������.
;;                         Diff1[m]=1
;;                     ENDIF ELSE BEGIN
;;                         Diff1[m]=-1
;;                     ENDELSE
;;                ENDFOR
;;
;;                Diff2 = INTARR(Num-2)
;;                FOR m=0,Num-3 DO BEGIN
;;                   Diff2[m]=Diff1[m+1]-Diff1[m]
;;                ENDFOR
;;                Temp = WHERE(Diff2 EQ -2,FreNum)        ;�м���-2,���м�����.
;;
;;           		  IF MAX(Tempsmooth) LT 85 THEN BEGIN  ;ȥ������Ϸ�ֵ,85Ϊ����ֵ,������������.
;;              	     Cropingvalue = 0
;;              	  ENDIF ELSE BEGIN
;;              		 Cropingvalue =  FreNum
;;             	  ENDELSE
;;              ENDIF ELSE BEGIN
;;				  Cropingvalue = 0
;;			  ENDELSE
;;          ENDIF ELSE BEGIN
;;             Cropingvalue = 0
;;          ENDELSE
;;
;;          AllCroppingValue[i]=BYTE(Cropingvalue)
;;
;;       ENDFOR
;;
;;    FREE_LUN,LUN_mulcropping
;;  	FREE_LUN,LUN_ndvi
;;
;;    AllCroppingValue = REFORM(AllCroppingValue,5300,4800,/overwrite)
;;
;;;	MulcroppingValue='I:\����ָ������\CroppingValue2005_test'
;;	OPENW,LUN,Outputfile,/GET_LUN
;;    WRITEU,LUN,AllCroppingValue
;;	FREE_LUN,LUN
;;
;;	progressTimer->destroy
;;
;;   AA=DIALOG_MESSAGE('����ָ���������!',TITLE='��ʾ:',/INFORMATION)
;;
;;END
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;************���㸴��ָ���¼�*****************************************************
PRO FZ_IndexCalculateEV,EVENT

	on_error,2

	CAUTION = DIALOG_MESSAGE('����ָ��������Ҫ���ѽϳ�ʱ�䣬ȷ��Ҫ���м��㣿',TITLE='��ʾ',/QUESTION)

	if CAUTION EQ 'No' then return

	WIDGET_CONTROL,Event.top,GET_UVALUE=PA
	WIDGET_CONTROL,/HOURGLASS

	WIDGET_CONTROL,(*PA).NDVIPrefix,GET_VALUE    = NDVIPrefix
	WIDGET_CONTROL,(*PA).NDVISuffix,GET_VALUE    = NDVISuffix
	WIDGET_CONTROL,(*PA).InputNDVIPath,GET_VALUE = InputNDVIPath

	WIDGET_CONTROL,(*PA).NDVI_LIST,GET_UVALUE    = NdviList      ;ע�����û�ֵ
	WIDGET_CONTROL,(*PA).CroppingZone,GET_VALUE = CroppingZone
	WIDGET_CONTROL,(*PA).OutCropping,GET_VALUE  = OutCropping

;	WIDGET_CONTROL,(*PA).MenXian,GET_VALUE   = MenXian  & MenXian  = MenXian[0]
;	WIDGET_CONTROL,(*PA).RongXian,GET_VALUE  = RongXian & RongXian = RongXian[0]
;	WIDGET_CONTROL,(*PA).DOD,GET_VALUE       = DOD		& DOD      = DOD[0]
	MenXian = '-5'
	RongXian = (*PA).TXT_TOLERANCE_HANTS
	DOD = (*PA).TXT_DOD_HANTS

	IF DC_JudgeInputChar(MenXian,Desc='"����"',/INTEGER,/NEGATIVE) EQ -1 THEN RETURN
	IF DC_JudgeInputChar(RongXian,Desc='"����"',/INTEGER) EQ -1 THEN RETURN
	IF DC_JudgeInputChar(DOD,Desc='"DOD"',/INTEGER) EQ -1 THEN RETURN

	CASE 1 OF
		ARRAY_EQUAL(NdviList[0],'',/NO_TYPECONV) : BEGIN
			INFO = DIALOG_MESSAGE('NDVI�ļ��б���û���ļ�!',TITLE='����')
			RETURN
		END
		ARRAY_EQUAL(CroppingZone[0],'',/NO_TYPECONV) : BEGIN
			IFNO = DIALOG_MESSAGE('��ѡ����ָ���������������ļ�!',TITLE='����')
			RETURN
		END
		ARRAY_EQUAL(OutCropping[0],'',/NO_TYPECONV) : BEGIN
			INFO = DIALOG_MESSAGE('��ָ������ָ���������ļ���!',TITLE='����')
			RETURN
		END
		ELSE:
	ENDCASE

	outputpath = strmid(OutCropping[0],0,strpos(OutCropping[0],'\',/reverse_search)+1)

	if file_test(outputpath,/directory) eq 0 then begin
		CAUTION = DIALOG_MESSAGE('���·�������ڣ����������룡',TITLE='����')
		return
	endif

	NDVIFile = InputNDVIPath[0]+NDVIPrefix[0]+NdviList+NDVISuffix[0]

	NDVIHDRFILE = NDVIFile + '.hdr'
	for i=0,n_elements(NDVIFile)-1,1 do begin
		if file_test(NDVIFile[i]) eq 0 or file_test(NDVIHDRFILE[i]) eq 0 then begin
			CAUTION = DIALOG_MESSAGE('���������в��������ļ����𻵣����飡',TITLE='����')
			return
		endif
	endfor

	N_Info = DC_ReadHead_file(NDVIFile[0],PROMPT_DES='NDVI�ļ�')	;NDVI�ļ���ͷ�ļ���Ϣ ,����ֻȡ��һ�����ж�

	if n_tags(N_Info) eq 1 then return

	C_Info = DC_ReadHead_file(CroppingZone[0],PROMPT_DES='����ָ�������ļ�');����ָ�������ļ�ͷ�ļ���Ϣ

	if n_tags(C_Info) eq 1 then return

	CASE 1 OF
		SIZE(N_Info,/TYPE) NE 8 : RETURN	;���ǽṹ��,��˵����ȡʧ��.
		SIZE(C_Info,/TYPE) NE 8 : RETURN
		FIX(N_Info.samples) NE FIX(C_Info.samples) OR FIX(N_Info.lines) NE FIX(C_Info.lines) :BEGIN
			INFO = DIALOG_MESSAGE('NDVI�ļ��븴��ָ�������ļ���С��һ��!',TITLE='����')
			RETURN
	    END
	ELSE:
	ENDCASE

	TendayNum = N_ELEMENTS(NDVIFile)		;�õ�NDVIѮ��
	if(TendayNum gt 36) then begin
		INFO = DIALOG_MESSAGE(['��ʼ����ʱ��β��ܳ���36ѮNDVI����!',$
									   'һ��ȡ1-10������'],/INFOR,TITLE='��ʾ')
		return
	endif

	FOR i=1,TendayNum-1 DO BEGIN			;���һ�����Ƚ�,�ʴ�index 1��ʼ,������index 0
		TempName = NDVIPrefix[0]+NdviList[i]+NDVISuffix[0]
		Temp = DC_ReadHead_file(NDVIFile[i],PROMPT_DES='�ļ�"'+TempName+'"')

		if n_tags(Temp) eq 1 then return

		CASE 1 OF
			SIZE(Temp,/TYPE) NE 8 : RETURN	;���ǽṹ��,��˵����ȡʧ��.
			FIX(N_Info.samples) NE FIX(Temp.samples) OR FIX(N_Info.lines) NE FIX(Temp.lines) :BEGIN
				INFO = DIALOG_MESSAGE('NDVI�ļ���С��һ��!',TITLE='����')
				RETURN
		    END
		ELSE:
		ENDCASE
	ENDFOR

    MaxIndex = MAX(DC_Read_ENVIData(CroppingZone[0]))

	 if MaxIndex[0] eq -1 then return

	CASE MaxIndex OF
		1:BEGIN
			IF (FIX((*PA).StartMonth) GT 5) OR (FIX((*PA).EndMonth) LT 9) or (TendayNum lt 15) THEN BEGIN
				INFO = DIALOG_MESSAGE(['��������������ҪΪһ���,����Ӧȡͬһ��5-9��15ѮNDVI����!',$
									   'һ��ȡ1-10������'],/INFOR,TITLE='��ʾ')
				RETURN
			ENDIF
		END
		2:BEGIN
			IF (FIX((*PA).StartMonth) GT 3) OR (FIX((*PA).EndMonth) LT 9)  or (TendayNum lt 21)THEN BEGIN
				INFO = DIALOG_MESSAGE(['��������������ҪΪ�����,����Ӧȡͬһ��3-9��21ѮNDVI����!',$
									   'һ��ȡ1-10������'],/INFOR,TITLE='��ʾ')
				RETURN
			ENDIF
		END

		3:BEGIN
			IF (FIX((*PA).StartMonth) GT 1) OR (FIX((*PA).EndMonth) LT 10) or (TendayNum lt 30)THEN BEGIN
				INFO = DIALOG_MESSAGE(['��������������ҪΪ�����,����Ӧȡͬһ��1-10��30ѮNDVI����!',$
									   'һ��ȡ1-10������'],/INFOR,TITLE='��ʾ')
				RETURN
			ENDIF
		END

	    ELSE:
	ENDCASE

	CD ,CURRENT=CurrentPath

	PROGRESSTIMER = OBJ_NEW("SHOWPROGRESS",event.top,/CANCELBUTTON,TITLE='����ָ������' $
							,MESSAGE='���������Ҫ�ϳ�ʱ��,�����ĵȴ�!')
	PROGRESSTIMER->START


	BsqFile=CurrentPath+'\TempBsqfile'
	OPENW,LUN_OUTPUT,BsqFile,/GET_LUN

	SAMPLES   = FIX(N_Info.samples)   &   LINES = FIX(N_Info.lines)
	TypeCode  = FIX(N_Info.datatype)

	AllTime = DOUBLE(TendayNum)*SAMPLES*LINES

	FOR I=0,TendayNum-1 DO BEGIN

		PROGRESSTIMER->UPDATE, ((I+1)/AllTime * 100.0) ;��������

	    ARR_DATA=MAKE_ARRAY(SAMPLES,LINES,TYPE=TypeCode)

		OPENR,Lun,NDVIFile[I],/GET_LUN

		result = fstat(Lun)
    	if result.size ne (LONG(samples)*LONG(lines)) then begin
    		OBJ_DESTROY,PROGRESSTIMER
	    	Info = DIALOG_MESSAGE(NDVIFile[I]+'�����ļ�����ȷ!',TITLE='����')
;	    	widget_control,event.top,SENSITIVE=1
		 	return
	 	endif
		READU,Lun,ARR_DATA
		FREE_LUN,Lun
		WRITEU,LUN_OUTPUT,ARR_DATA   ;д��BSQ�Ķನ���ļ�.
	ENDFOR

	FREE_LUN,LUN_OUTPUT

	ARR_DATA =MAKE_ARRAY(SAMPLES,LINES*LONG(TendayNum),TYPE=TypeCode,/NOZERO)

	OPENR,Lun,BsqFile,/GET_LUN
	READU,Lun,ARR_DATA
	FREE_LUN,Lun

	FILE_DELETE,BsqFile,/NOEXPAND_PATH

	ZoneDataFile = FILE_DIRNAME(CroppingZone[0],/MARK)+FILE_BASENAME(CroppingZone[0],'.hdr') ;ȥ��.hdr
	OPENR,LunMul,ZoneDataFile,/GET_LUN 	                  ;���븴��ָ������
	Temp =MAKE_ARRAY(1,TYPE=FIX(C_Info.datatype),/NOZERO)
    TempMulcropping = ASSOC(LunMul,Temp)
	;********************************************************************************************
	HILO='LO'				;	HILO��	�ߵ�ֵ��־����ָ��ȥ���Ǹ�ֵ���ǵ�ֵ
	IDRT=FIX(MenXian)		;	IDRT��	�ߵ�����ֵ�趨��-500����˼����
	FET=FIX(RongXian)		;	FET��	��������趨��50/100/500/1000��������������
	DOD=FIX(DOD)			;	DOD��	DOD���������趨��2/3/6��>2*NR_OF_FREQUENCIES+1��,ʣ��������

    TempHead = MAKE_ARRAY(1,TYPE=TypeCode)
    IF TypeCode EQ 1 THEN Threshold = 157 ELSE Threshold = 0.24  ;�˴���Ŀ��,�������˵��.
	MONTH = FIX(STRMID(NdviList,4,2))							;��NDVIֵΪBYTE��,���򿴳�FLOAT��.����ֵThreshold

    AllCroppingValue = BYTARR(LONG(SAMPLES)*LINES,/NOZERO)
    N = 0L
    FOR I=0L,LINES-1L DO BEGIN
       FOR J = 0l,SAMPLES-1L DO BEGIN

			CANCELLED = PROGRESSTIMER->CHECKCANCEL()
			IF CANCELLED THEN BEGIN
				OK = DIALOG_MESSAGE('�û���ֹ�˲���',TITLE='����')
;				PROGRESSTIMER->DESTROY ;����������
				OBJ_DESTROY,PROGRESSTIMER
				log, '����ָ��-ָ����ȡ', 0

			    RETURN
			ENDIF
		    PROGRESSTIMER->UPDATE,((N+1+TendayNum)/AllTime * 100.0) ;��������

             NDVI30 = TRANSPOSE(ARR_DATA[J,I+INDGEN(TendayNum)*LINES])
             C_Zone=TempMulcropping[N]

        IF C_Zone[0] NE 0 THEN BEGIN
            NI  = TendayNum
            PER = NI*10
        	TS  = FINDGEN(NI+1)*10
        	NF  = C_Zone[0]+1                   ;ȡ���������е�ֵ��ΪƵ��,�޸���20061204,����1
        	PAy  = INDGEN(NF+1)
			FOR k=1,NF DO BEGIN
				PAy[k]=PER/k
			ENDFOR
			AMP=FLTARR((NF+1)+2)
			PHI=FLTARR((NF+1)+2)

            Y_TEMP = [TempHead,NDVI30]              ;����������һ��ͷԪ��,��Hants �����Ҫ��.
     		ARR_RESULT=FZ_HANTSNEW(NI,PER,Y_TEMP,TS,NF,PAy,HILO,IDRT,FET,DOD,AMP,PHI)

            IF ARRAY_EQUAL(ARR_RESULT,0) NE 1 THEN BEGIN    ;ƽ���������в�Ϊ0,��������ȱ����̫����ܻ�����0
               CASE C_Zone[0] OF
              	 1:Tempsmooth = ARR_RESULT[WHERE(MONTH GE 5 AND MONTH LE 9,Num)]   ;һ���ȡ5-9�¹�15Ѯ

    			 2:Tempsmooth = ARR_RESULT[WHERE(MONTH GE 3 AND MONTH LE 9,Num)]   ;�����ȡ3-9�¹�21Ѯ

    			 3:Tempsmooth = ARR_RESULT[WHERE(MONTH GE 1 AND MONTH LE 10,Num)]  ;�����ȡ1-10�¹�30Ѯ

    		     ELSE:
   			    ENDCASE

                Diff1 = INTARR(Num-1)
                FOR m=0,Num-2 DO BEGIN
                     IF (Tempsmooth[m+1]-Tempsmooth[m]) GE 0 THEN BEGIN   ;�ú�һ��ֵ����ǰһ��ֵ,���жϲ�ֵ������.
                         Diff1[m]=1
                     ENDIF ELSE BEGIN
                         Diff1[m]=-1
                     ENDELSE
                ENDFOR

                Diff2 = INTARR(Num-2)
                FOR m=0,Num-3 DO BEGIN
                   Diff2[m]=Diff1[m+1]-Diff1[m]
                ENDFOR
                CC = WHERE(Diff2 EQ -2,FreNum)        ;�м���-2,���м�����.
					;����ȥ������Ϸ�ֵ,,��������������85Ϊ����ֵҳP33,��NDVI���칫ʽDN/250-0.1
           		  IF MAX(Tempsmooth) LT Threshold THEN BEGIN  ;����Ĳ������NDVI���칫ʽΪDN/127-1,��Ϊ157(��׼ֵΪ0.24))
              	     Cropingvalue = 0
              	  ENDIF ELSE BEGIN
              		 Cropingvalue =  FreNum
             	  ENDELSE
              ENDIF ELSE BEGIN
				  Cropingvalue = 0
			  ENDELSE
          ENDIF ELSE BEGIN
             Cropingvalue = 0
          ENDELSE

          AllCroppingValue[N]=BYTE(Cropingvalue)
		  N+=1L
       ENDFOR
      ENDFOR

    FREE_LUN,LunMul
	ARR_DATA = 0B  ;���¸�ֵ,�Լ����ڴ�ռ��

    AllCroppingValue = REFORM(AllCroppingValue,SAMPLES,LINES,/OVERWRITE)

    DataType = SIZE(AllCroppingValue,/TYPE)
    ImageData = PTR_NEW(AllCroppingValue,/NO_COPY)

    ULX  = N_Info.ULX             &  ULY = N_Info.ULY
    Resolution=N_Info.Resolution  &  CenterMedian=N_Info.CenterMedian


	DC_SaveImageFile,OutCropping[0],ImageData,SAMPLES,LINES,DataType,'AVHRR' $
					,ULX,ULY,Resolution ,CenterMedian
   ;ͼ����ʾ
;	WIDGET_CONTROL,(*PA).WID_DRAW,SET_UVALUE = DC_Read_ENVIData(OutCropping[0]) ;������
;	WIDGET_CONTROL,(*PA).WID_DRAW,SET_UVALUE = *ImageData
;    PTR_FREE,ImageData
;	DC_Draw_image,OutCropping[0],(*PA).WID_DRAW,oView=oView;,/WHITE
;	OBJ_DESTROY,(*PA).oView
;	(*PA).oView = oView

;	SelectedFile = DIALOG_PICKFILE(TITLE='ѡ��������ļ�',GET_PATH=SelectedPath,/MUST_EXIST $
;                            	  ,FILTER ='*.hdr;*.HDR',PATH = v_fz_out_path,DIALOG_PARENT=event.id)  ;����ɸѡ��׺
;	IF SelectedFile EQ '' THEN RETURN
;
	SelectedDataFile = STRMID(OutCropping[0],0,STRPOS(OutCropping[0],'.',/REVERSE_SEARCH))

	PSTATE = PA
;	SelectedDataFile=OutCropping[0]
	fileinfo = read_file(SelectedDataFile)
	ARR_DATA=fileinfo.dataarr

 	pstaff_display = (*PSTATE).pstaff_display
 	ptr_free,(*pstaff_display).image
 	(*pstaff_display).image = ptr_new(ARR_DATA, /no_copy)

	(*pstaff_display).startx = fileinfo.startx
	(*pstaff_display).starty = fileinfo.starty
	(*pstaff_display).xsize	= fileinfo.xsize
	(*pstaff_display).ysize = fileinfo.ysize
	(*pstaff_display).pixelsize = fileinfo.pixelsize

	(*pstaff_display).shapefile = '.\data_vector\province.shp'

 	refresh, pstaff_display

;;	PROGRESSTIMER->DESTROY
	OBJ_DESTROY,PROGRESSTIMER
;	WIDGET_CONTROL,(*PA).Output_JPG,SENSITIVE=1

   AA=DIALOG_MESSAGE('����ָ���������!',/INFORMATION,TITLE='��ʾ')
	log, '����ָ��-ָ����ȡ', 1
END
;888888888888888888888������Ĳ����¼�����8888888888888888888888888888888888888888888888888888
PRO FZ_Calculate_event, Event

	on_error,2

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ? widget_info(Event.id, /trLee_root) : event.id)

     CATCH, Error_status               ;��ȡ����.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['������ֹ,ԭ������:',[!ERROR_STATE.MSG]],TITLE='����',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;������������,������ִ����������,�Ի���ִ���.
     ENDIF

	wWidget =  Event.top
	WIDGET_CONTROL,Event.top,GET_UVALUE=PA
	WIDGET_CONTROL,/HOURGLASS
	common current_date, c_year, c_month, c_day
	COMMON COMMON_SETPATH,ppath
	v_fz_in_path  = (*ppath).fz_in_path
	v_fz_out_path  = (*PA).v_fz_out_path

  CASE wTarget OF
	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='Area_DR'): BEGIN
        (*PA).ProID =  (*PA).ProIDList[EVENT.INDEX]

	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='StartYear'): BEGIN
		(*PA).StartYear = (*PA).ARR_YEAR[EVENT.INDEX]
;		CalcYear = c_year
		CalcYear = (*PA).StartYear
		ProName = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]
		WIDGET_CONTROL, (*PA).OutCropping, set_value=v_fz_out_path+CalcYear+ProName+'����ָ��.hdr'
	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='EndYear'): BEGIN
		(*PA).EndYear = (*PA).ARR_YEAR[EVENT.INDEX]
		(*PA).CalcYear = (*PA).EndYear		;������ݵ��ڽ������
	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='StartMonth'): BEGIN
		StartMonth = (*PA).ARR_MONTH[EVENT.INDEX]
		IF STRLEN(StartMonth) EQ 1 THEN StartMonth='0'+StartMonth
		(*PA).StartMonth = StartMonth
	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='EndMonth'): BEGIN
		EndMonth = (*PA).ARR_MONTH[EVENT.INDEX]
		IF STRLEN(EndMonth) EQ 1 THEN EndMonth='0'+EndMonth
		(*PA).EndMonth = EndMonth
	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='StartTendayDay_DR'): BEGIN
		(*PA).StartTenday = (*PA).ARR_TENDAY[EVENT.INDEX]
	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='EndTendayDay_DR'): BEGIN
		(*PA).EndTenday  = (*PA).ARR_TENDAY[EVENT.INDEX]
	END

	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='Calculate_BU'): BEGIN
		FZ_IndexCalculateEV,event
	END

	WIDGET_INFO(wWidget,FIND_BY_UNAME='InputCroppingBU') : BEGIN
		INFO=DIALOG_MESSAGE('����Ĭ�ϵĸ�ָ�����������ļ�,ȷ��Ҫ����ѡ����?',TITLE='ѯ��',/QUESTION)
		IF INFO EQ 'No' THEN RETURN

    	CroppingZoneFile = DIALOG_PICKFILE(TITLE='ѡ��ָ�����������ļ�',PATH=DC_PathSetting() $
    						,FILTER ='*.hdr;*.HDR',FILE='ProCroppingZone.hdr',/MUST_EXIST $
    						,DIALOG_PARENT=EVENT.ID)

		IF CroppingZoneFile EQ '' THEN  RETURN

		WIDGET_CONTROL,(*PA).CroppingZone,SET_VALUE=CroppingZoneFile
	END

	WIDGET_INFO(wWidget,FIND_BY_UNAME='OutCropping_BU') : BEGIN
;		CalcYear = (*PA).CalcYear
;		ProName = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]
    	OutCroppingFile = DIALOG_PICKFILE(TITLE='���渴��ָ�����' $
                        	,PATH=v_fz_out_path,/WRITE,FILTER ='*.hdr;*.HDR' $;PATH=DC_PathSetting()
                        	,/OVERWRITE_PROMPT $
                        	,DEFAULT_EXTENSION='hdr',DIALOG_PARENT=EVENT.ID)

		IF OutCroppingFile EQ '' THEN BEGIN
		   RETURN
		ENDIF

		(*PA).v_fz_out_path = FILE_DIRNAME(OutCroppingFile,/MARK_DIRECTORY)

		;temp = DC_PathSetting(WRITEPATH1=PathNew)       ;��·��д�뱣������.
		WIDGET_CONTROL,(*PA).OutCropping,SET_VALUE=OutCroppingFile

		WIDGET_CONTROL,EVENT.TOP,SET_UVALUE=PA
	END

	WIDGET_INFO(wWidget,FIND_BY_UNAME='NDVIList_refresh') : begin
		CalcYear = (*PA).StartYear
		ProName = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]
		WIDGET_CONTROL, (*PA).OutCropping, set_value=v_fz_out_path+CalcYear+ProName+'����ָ��.hdr'

		TimePoint = [(*PA).StartYear,(*PA).StartMonth,'',(*PA).StartDay,$
					 (*PA).EndYear,(*PA).EndMonth,'',(*PA).EndDay]

;		AllTime = FZ_Create_Composite_Time(TimePoint,'DAY',Prompt = Prompt)
		AllTime = DAY_TO_TENDAY(TimePoint,Prompt = Prompt)
		IF ARRAY_EQUAL(AllTime,'Error',/NO_TYPECONV) THEN BEGIN
			Info = DIALOG_MESSAGE(Prompt,TITLE='����')
			RETURN
		ENDIF

		WIDGET_CONTROL,(*PA).NDVIPrefix,GET_VALUE=NDVIPrefix
		WIDGET_CONTROL,(*PA).NDVISuffix,GET_VALUE=NDVISuffix

		InputFileName = STRTRIM(NDVIPrefix[0]+AllTime+NDVISuffix[0],2)   ;���·���ļ���
		WIDGET_CONTROL,(*PA).InputNDVIPath,GET_VALUE=NdviFilePath
		IF NdviFilePath EQ '' THEN BEGIN
		   WIDGET_CONTROL,(*PA).InputNDVIPath,SET_VALUE=''
		   WIDGET_CONTROL,(*PA).NDVI_LIST,SET_VALUE='',SET_UVALUE=''   ;ע���б�������û�ֵ
		   WIDGET_CONTROL,(*PA).NDVIList_La,SET_VALUE='�ļ��б�'
		   RETURN
		ENDIF

		FullFile = NdviFilePath[0]+InputFileName           ;����·���ļ���(��ȫ·���ļ���)
		ExistFileID = WHERE(FILE_TEST(FullFile) EQ 1,count)
		IF count NE 0 THEN BEGIN
			InputFileName = InputFileName[ExistFileID]  ;���·���ļ���
			FileTime = AllTime[ExistFileID]			    ;�ļ�ʱ��
		ENDIF ELSE BEGIN
			Info = DIALOG_MESSAGE(['������ѡ���"NDVI�ļ�����·��"��,�Ҳ����κ�ʱ������Ӧ','��ʼ-����ʱ����,����ʽ��"'+InputFileName[0]+'"���ļ�!'],TITLE='��ʾ',/INFORMATION)
			WIDGET_CONTROL,(*PA).NDVI_LIST,SET_VALUE='',SET_UVALUE=''
			WIDGET_CONTROL,(*PA).NDVIList_La,SET_VALUE='�ļ��б�'
			RETURN
		ENDELSE

		temp = DC_PathSetting(WRITEPATH2=NdviFilePath)       ;��·��д�뱣������.
		WIDGET_CONTROL,(*PA).NDVI_LIST,SET_VALUE=InputFileName,SET_UVALUE=FileTime
		WIDGET_CONTROL,(*PA).NDVIList_La,SET_VALUE=['�ļ��б�','('+STRTRIM(count,2)+'��)']

	end

	WIDGET_INFO(wWidget,FIND_BY_UNAME='InputNDVIBU') : BEGIN

		TimePoint = [(*PA).StartYear,(*PA).StartMonth,'',(*PA).StartDay,$
					 (*PA).EndYear,(*PA).EndMonth,'',(*PA).EndDay]

		AllTime = FZ_Create_Composite_Time(TimePoint,'DAY',Prompt = Prompt)
		IF ARRAY_EQUAL(AllTime,'Error',/NO_TYPECONV) THEN BEGIN
			Info = DIALOG_MESSAGE(Prompt,TITLE='����')
			RETURN
		ENDIF

		WIDGET_CONTROL,(*PA).NDVIPrefix,GET_VALUE=NDVIPrefix
		WIDGET_CONTROL,(*PA).NDVISuffix,GET_VALUE=NDVISuffix

		InputFileName = STRTRIM(NDVIPrefix[0]+AllTime+NDVISuffix[0],2)   ;���·���ļ���
		NdviFilePath = DIALOG_PICKFILE(PATH=v_fz_in_path,/DIRECTORY,TITLE='ѡ��ѮNDVI�ļ���',DIALOG_PARENT=EVENT.ID)
		IF NdviFilePath EQ '' THEN BEGIN
		   WIDGET_CONTROL,(*PA).NDVI_LIST,SET_VALUE='',SET_UVALUE=''   ;ע���б�������û�ֵ
		   WIDGET_CONTROL,(*PA).NDVIList_La,SET_VALUE='�ļ��б�'
		   RETURN
		ENDIF

		WIDGET_CONTROL,(*PA).InputNDVIPath,SET_VALUE=NdviFilePath

		FullFile = NdviFilePath+InputFileName           ;����·���ļ���(��ȫ·���ļ���)
		ExistFileID = WHERE(FILE_TEST(FullFile) EQ 1,count)
		IF count NE 0 THEN BEGIN
			InputFileName = InputFileName[ExistFileID]  ;���·���ļ���
			FileTime = AllTime[ExistFileID]			    ;�ļ�ʱ��
		ENDIF ELSE BEGIN
			Info = DIALOG_MESSAGE(['������ѡ���"NDVI�ļ�����·��"��,�Ҳ����κ�ʱ������Ӧ','��ʼ-����ʱ����,����ʽ��"'+InputFileName[0]+'"���ļ�!'],TITLE='��ʾ',/INFORMATION)
			WIDGET_CONTROL,(*PA).NDVI_LIST,SET_VALUE='',SET_UVALUE=''
			WIDGET_CONTROL,(*PA).NDVIList_La,SET_VALUE='�ļ��б�'
			RETURN
		ENDELSE

		;temp = DC_PathSetting(WRITEPATH2=NdviFilePath)       ;��·��д�뱣������.
		WIDGET_CONTROL,(*PA).NDVI_LIST,SET_VALUE=InputFileName,SET_UVALUE=FileTime
		WIDGET_CONTROL,(*PA).NDVIList_La,SET_VALUE=['�ļ��б�','('+STRTRIM(count,2)+'��)']

	  END

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='SelectFile'): BEGIN

        SelectedFile = DIALOG_PICKFILE(TITLE='ѡ��������ļ�',GET_PATH=SelectedPath,/MUST_EXIST $
                            	  ,FILTER ='*.hdr;*.HDR',PATH = v_fz_out_path,DIALOG_PARENT=event.id)  ;����ɸѡ��׺
		IF SelectedFile EQ '' THEN RETURN

		RelaFile = STRMID(SelectedFile,STRPOS(SelectedFile,'\',/REVERSE_SEARCH)+1)

		SelectedDataFile = STRMID(SelectedFile,0,STRPOS(SelectedFile,'.',/REVERSE_SEARCH))
        RelaDataFile = STRMID(RelaFile,0,STRPOS(RelaFile,'.',/REVERSE_SEARCH))

        CASE 1 OF
        	~FILE_TEST(SelectedFile): BEGIN
        		Info = DIALOG_MESSAGE('�Ҳ���ָ���ļ�"'+RelaFile+'"!',TITLE='��ʾ')
        	 	RETURN
        	 END

        	~FILE_TEST(SelectedDataFile): BEGIN
        		Info = DIALOG_MESSAGE('�Ҳ���ָ���ļ�"'+RelaFile+'"��Ӧ�������ļ�"'+RelaDataFile+'"!',TITLE='��ʾ')
        	 	RETURN
        	 END
        	ELSE:
        ENDCASE

;		WIDGET_CONTROL,(*PA).WID_DRAW,SET_UVALUE = DC_Read_ENVIData(SelectedDataFile)
;
;		DC_Draw_image,SelectedDataFile,(*PA).WID_DRAW,oView=oView;,/WHITE
;		OBJ_DESTROY,(*PA).oView
;		(*PA).oView = oView
;
;        temp = DC_PathSetting(WritePath2=SelectedPath)
		PSTATE = PA
		fileinfo = read_file(SelectedDataFile)
		ARR_DATA=fileinfo.dataarr

	 	pstaff_display = (*PSTATE).pstaff_display
	 	ptr_free,(*pstaff_display).image
	 	(*pstaff_display).image = ptr_new(ARR_DATA, /no_copy)

		(*pstaff_display).startx = fileinfo.startx
		(*pstaff_display).starty = fileinfo.starty
		(*pstaff_display).xsize	= fileinfo.xsize
		(*pstaff_display).ysize = fileinfo.ysize
		(*pstaff_display).pixelsize = fileinfo.pixelsize

		(*pstaff_display).shapefile = '.\data_vector\province.shp'

	 	refresh, pstaff_display

    END

   WIDGET_INFO(WWIDGET, FIND_BY_UNAME='Output_JPG'): BEGIN
          WIDGET_control,(*PA).WID_DRAW,GET_VALUE=oWindow
	   oWindow->Draw,oViewGroup
	   oWindow->GetProperty,IMAGE_DATA = image

       ProName = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]
       OutputFile = ProName+(*PA).CalcYear+'�긴��ָ���ֲ�ͼ.bmp'
       Result = DIALOG_WRITE_IMAGE (image,FILE = OutputFile,TITLE='����Ϊ:' $
  	                                ,/WARN_EXIST,TYPE='.BMP')
   END

     Widget_Info(wWidget, FIND_BY_UNAME='WID_DRAW'): BEGIN

        IF NOT OBJ_VALID((*PA).oView) THEN RETURN   ;����ǿն����򷵻�.

;		ParaInfo = (*PA).ProjectPara    			;�õ�ʡ��Χ�ڵ�ͶӰ����
;        IF SIZE(ParaInfo,/TYPE) NE 8 THEN BEGIN     ;�����صĲ��ǽṹ��,���ǿ�.
;        	ProviceName = (*PA).Province[WHERE((*PA).proIDlist EQ (*PA).proID)]
;        	PRMPT = DIALOG_MESSAGE('û��'+ProviceName+'�Ļ���������Ϣ,��鿴��Ӧ�Ĳ��������ļ�!')
;        	RETURN
;        ENDIF

		IF Event.type EQ 0 OR Event.type EQ 2 THEN BEGIN
;			sample=(*PA).ImageSize.sample   & lines=(*PA).ImageSize.lines
			widget_Control,EVENT.ID,GET_UVALUE = Imagedata
			ImageSize = SIZE(Imagedata,/DIMENSIONS)
		    (*PA).oView->GetProperty,Location = viewLoc,Dimensions = viewDim
			xSize = ImageSize[0]		;Ӱ����
			ySize = ImageSize[1]
			Zoom = 1.*viewDim[0]/xSize
			nPos = ([event.x,event.y]-viewLoc)/zoom

			IF (nPos[0] LT -1) OR (nPos[0] GT xSize+2) THEN Return
			IF (nPos[1] LT -1) OR (nPos[1] GT ySize+2) THEN Return

			pos = [xSize-1, ySize-1]<nPos>0

			IF Max(pos) LE 5 THEN Return
;            widget_Control,EVENT.ID,GET_UVALUE = Imagedata
        	Imagedata = REVERSE(TEMPORARY(Imagedata),2)    ;֮���԰��з�ת,����ΪDRAW��������Ǵ����½ǿ�ʼ��.
		    dataValue = Imagedata[pos[0],pos[1]]

		    IF dataValue EQ 0.0 THEN ProText='' ELSE ProText=STRTRIM(FLOAT(dataValue),2)
			WIDGET_CONTROL,EVENT.ID,TOOLTIP=ProText
			WIDGET_CONTROL,(*PA).CusorValue_TEXT,SET_VALUE=ProText
		ENDIF
    END

    Widget_Info(wWidget, FIND_BY_UNAME='Help_bu'):begin
			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '����ָ������', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('�Ҳ��������ĵ�',title='����')
			endelse
		end
;    ONLINE_HELP,BOOK='HELP\HELP.chm', /FULL_PATH,'����ָ������'
	WIDGET_INFO(WWIDGET, FIND_BY_UNAME='Close_BU'): BEGIN
		common_log,'�رո���ָ������'

		WIDGET_CONTROL,EVENT.TOP,/DESTROY
	END

	ELSE:
	ENDCASE
END
;&&&&&&&&&&&&&&&&&&&&&&����ָ�����������洰��:�����������%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PRO FZ_Calculate, GROUP_LEADER=wGroup

	common_log,'��������ָ������'
	IF ( XREGISTERED('FZ_Calculate') NE 0 ) THEN RETURN

	TLB_BASE = Widget_Base( GROUP_LEADER=wGroup, UNAME='TLB_BASE'  $
			,XOFFSET=180 ,YOFFSET=200,TITLE='����ָ������' $
			,XPAD=1 ,YPAD=1,space=1,/ROW,TLB_FRAME_ATTR=1,TAB_MODE=1);,/TLB_KILL_REQUEST_EVENTS)
	;------------------------------------------------------

	ARRAY_YEAR = STRTRIM(INDGEN(36)+1980,2)         ;��������ݱ仯,ϵͳ���б��е����Ҳ��仯.

 	LEFT_BASE = Widget_Base(TLB_BASE, UNAME='LEFT_BASE' ,FRAME=0 $
 	   ,SPACE=2,XPAD=2 ,YPAD=2,/BASE_ALIGN_LEFT,/COLUMN)
;
  ;--------------�����������--------------------------------
  	BaseWidth = 295
;  	Distroct_TimeFre_BASE = Widget_Base(LEFT_BASE  $
;      ,FRAME=0 ,SCR_XSIZE=BaseWidth-1 ,SCR_YSIZE=26 ,SPACE=3 ,XPAD=3 ,YPAD=3)
;
	  Province = ['������','�����','�ӱ�','ɽ��','���ɹ�','����','����','������','�Ϻ���','����' $
					,'�㽭','��΢','����','����','ɽ��','����','����','����','�㶫','����','����' $
					,'������','�Ĵ�','����','����','����','����','����','�ຣ','����','�½�']
	  ProIDList = ['11','12','13','14','15','21','22','23','31','32','33','34','35','36','37',$
					'41','42','43','44','45','46','50','51','52','53','54','61','62','63','64','65']

;	  Area_DR = Widget_Droplist(Distroct_TimeFre_BASE, UNAME='Area_DR'  $
;	      ,SCR_XSIZE=140 ,SCR_YSIZE=24 ,TITLE='����ʡ�ݣ�',VALUE=Province,xoffset=2)
  ;===========��ʼ����ʱ�䲿��===========================================
  common current_date, c_year, c_month, c_day
  TimeBase = Widget_Base(LEFT_BASE, UNAME='TimeBase' ,FRAME=1  $
      ,SPACE=2, XPAD=1 ,YPAD=1,/COLUMN, xsize=BaseWidth-4)

  subbase = widget_base(TimeBase,SPACE=3 ,XPAD=3,YPAD=0,/row)
  WID_LABEL = Widget_Label(subbase,/ALIGN_LEFT ,VALUE='��ʼʱ�䣺')
  WID_LABEL_date = Widget_text(subbase,xsize=13, /ALL_EVENTS, $
      VALUE=strtrim(c_year,2)+'-'+strtrim('1',2)+'-'+strtrim('1',2))
  CMD_pick_date_s = Widget_Button(subbase,SCR_XSIZE=30,SCR_YSIZE=20, $
  		/ALIGN_CENTER, uname='CMD_pick_date_s', $
  VALUE='.\Image\Calendar.bmp',/BITMAP,event_pro='ActiveXCal', $
  		uvalue={text_id:WID_LABEL_date, pointer:PTR_NEW(), detail:'CMD_pick_date_s_fz'})

  subbase = widget_base(TimeBase,SPACE=3 ,XPAD=3,YPAD=0,/row)
  WID_LABEL = Widget_Label(subbase,/ALIGN_LEFT ,VALUE='����ʱ�䣺')
  WID_LABEL_date = Widget_text(subbase,xsize=13, /ALL_EVENTS, $
      VALUE=strtrim(c_year,2)+'-'+strtrim('12',2)+'-'+strtrim('31',2))
  CMD_pick_date_e = Widget_Button(subbase,SCR_XSIZE=30,SCR_YSIZE=20,/ALIGN_CENTER, uname='CMD_pick_date_e', $
  VALUE='.\Image\Calendar.bmp',/BITMAP,event_pro='ActiveXCal',uvalue={text_id:WID_LABEL_date, pointer:PTR_NEW(), detail:'CMD_pick_date_e_fz'})


;  StartBASE = Widget_Base(TimeBase, UNAME='StartBASE' ,FRAME=0  $
;	      ,SPACE=3 ,XPAD=1,YPAD=1,/row)
;
;  WID_LABEL_2 = Widget_Label(StartBASE, UNAME='WID_LABEL_2', /ALIGN_LEFT  $
;      ,VALUE='��ʼʱ�䣺')
;
;  StartYear = Widget_combobox(StartBASE, UNAME='StartYear'  $
;      ,XOFFSET=66 ,YOFFSET=3 ,SCR_XSIZE=60)
;  label = widget_label(StartBASE, value='��')
;
;  StartMonth = Widget_Droplist(StartBASE, UNAME='StartMonth'  $
;      ,XOFFSET=148 ,YOFFSET=3 ,SCR_XSIZE=40)
;  label = widget_label(StartBASE, value='��')
;
;  StartTendayDay_DR = Widget_Droplist(StartBASE,  $
;      UNAME='StartTendayDay_DR' ,SCR_XSIZE=42  $
;      ,VALUE=['��','��','��'])
;  label = widget_label(StartBASE, value='Ѯ')
;
;	;------------------------����ʱ��----------------------------
;	EndBASE = Widget_Base(TimeBase, UNAME='StartBASE' ,FRAME=0  $
;	      ,SPACE=3 ,XPAD=1,YPAD=1,/row)
;
;	WID_LABEL_4 = Widget_Label(EndBASE, UNAME='WID_LABEL_4' ,XOFFSET=4  $
;	,YOFFSET=8 ,/ALIGN_LEFT  $
;	,VALUE='����ʱ�䣺')
;
;	EndYear = Widget_combobox(EndBASE, UNAME='EndYear' ,XOFFSET=66  $
;	,YOFFSET=3 ,SCR_XSIZE=60)
;    label = widget_label(EndBASE, value='��')
;
;	EndMonth = Widget_Droplist(EndBASE, UNAME='EndMonth'  $
;      ,XOFFSET=148 ,YOFFSET=3 ,SCR_XSIZE=40)
;  label = widget_label(EndBASE, value='��')
;
;  EndTendayDay_DR = Widget_Droplist(EndBASE,  $
;      UNAME='EndTendayDay_DR' ,SCR_XSIZE=42  $
;      ,VALUE=['��','��','��'])
;  label = widget_label(EndBASE, value='Ѯ')

;=========================����T2====================================
  T2_BASE = Widget_Base(LEFT_BASE, UNAME='T2_BASE' ,FRAME=1  $
      ,SPACE=1, XPAD=1 ,YPAD=1,/COLUMN, xsize=BaseWidth-4)

  T1InputBASE = Widget_Base(T2_BASE, UNAME='T1InputBASE' ,FRAME=0  $
      ,SPACE=120 ,XPAD=1 ,YPAD=1, /row)
  label = widget_label(T1InputBASE, value='�ϳ�NDVI�ļ�·����')
  InputNDVIBU = Widget_Button(T1InputBASE,  $
      UNAME='InputNDVIBU' ,/ALIGN_CENTER,SCR_XSIZE=36 ,SCR_YSIZE=18  $
      ,TOOLTIP='Ѯ�ϳ�NDVI�ļ�·��' ,VALUE='open.bmp' ,/BITMAP)
  T3InputBASE = Widget_Base(T2_BASE, UNAME='T3InputBASE' ,FRAME=0  $
      ,SPACE=1 ,XPAD=1 ,YPAD=1, /row)
  InputNDVIPath = Widget_Text(T3InputBASE,  $
      UNAME='InputNDVIPath' ,FRAME=0,SCR_XSIZE=270)

  T2InputBASE = Widget_Base(T2_BASE, UNAME='T2InputBASE' ,FRAME=0  $
      ,SPACE=3 ,XPAD=1 ,YPAD=1, /row)
  T2NDVITime_L = Widget_Label(T2InputBASE, UNAME='T2NDVITime_L'  $
      ,/ALIGN_LEFT,VALUE='�ļ�������ʽ��')
  NDVIPrefix = Widget_Text(T2InputBASE,  $
      UNAME='NDVIPrefix',SCR_XSIZE=45,/EDITABLE)
  WID_LABEL_15 = Widget_Label(T2InputBASE, UNAME='WID_LABEL_15'  $
      ,/ALIGN_LEFT ,VALUE=' + ����Ѯ + ')
  NDVISuffix = Widget_Text(T2InputBASE,  $
      UNAME='NDVISuffix' ,XOFFSET=241 ,YOFFSET=3  $
      ,SCR_XSIZE=45,/EDITABLE,VALUE='NDVI')
  newBASE = Widget_Base(T2_BASE, FRAME=0  $
      ,SPACE=3 ,XPAD=1 ,YPAD=1, /row)
;  label = widget_label(newBASE, value='��˵�����ϡ��С���Ѯ�ֱ�������01��02��03��ʾ��')

	  T2SeperateLine0_BASE = Widget_Base(T2_BASE  $
	      ,FRAME=0 ,/row, SPACE=0, XPAD=1 ,YPAD=0)
	  NDVIList_La = Widget_text(T2SeperateLine0_BASE, UNAME='NDVIList_La'  $
	      ,XOFFSET=2 ,YOFFSET=52 , /WRAP , XSIZE = 10, YSIZE = 2 $
	      ,/ALIGN_LEFT ,VALUE='�ļ��б�',SCR_YSIZE=100)
	  NDVI_LIST = Widget_List(T2SeperateLine0_BASE, UNAME='NDVI_LIST'  $
	      ,FRAME=1 ,SCR_XSIZE=160 ,SCR_YSIZE=100  $
	      ,UVALUE='')
	  Refresh = widget_button(T2SeperateLine0_BASE, UNAME='NDVIList_refresh',VALUE='ˢ��')

	;*****************************************************************************
	;���������������������������
	  T2SeperateLine2_BASE = Widget_Base(T2_BASE ,FRAME=0 ,xsize=1,ysize=1,MAP=0)
	  CroppingZone = Widget_Text(T2SeperateLine2_BASE, UNAME='CroppingZone'  $
	      ,FRAME=1 ,XOFFSET=2 ,YOFFSET=168 ,SCR_XSIZE=288 ,SCR_YSIZE=24  $
	      ,XSIZE=20 ,YSIZE=1,value='./data_grid/ProCroppingZone.hdr')
	  BestNDVI_LABEL = Widget_Label(T2SeperateLine2_BASE, UNAME='BestNDVI_LABEL'  $
	      ,XOFFSET=2 ,YOFFSET=149 ,SCR_XSIZE=150 ,SCR_YSIZE=14  $
	      ,/ALIGN_LEFT ,VALUE='����ָ���������������ļ���')
	  InputCroppingBU = Widget_Button(T2SeperateLine2_BASE,  $
	      UNAME='InputCroppingBU' ,XOFFSET=245 ,YOFFSET=147  $
	      ,SCR_XSIZE=36 ,SCR_YSIZE=18 ,/ALIGN_CENTER  $
	      ,TOOLTIP='ѡ����ָ���������������ļ�' ,VALUE='open.bmp' ,/BITMAP)
	;*****************************************************************************
;================HANTS����BASE=================================
  BASE_PARAMETER_HANTS = Widget_Base(LEFT_BASE,FRAME=1,SCR_XSIZE=BaseWidth-2 $
  			,SPACE=1 ,XPAD=1,YPAD=1,/row,/BASE_ALIGN_TOP)
;  Twidth = 30
;  HANTSLa = Widget_Label(HANTS_BASE,SCR_XSIZE=60 ,SCR_YSIZE=15,VALUE='HANTS������')
;
; MenXianB = Widget_Base(HANTS_BASE,SCR_YSIZE=22,SPACE=1,XPAD=0,YPAD=0,/ROW,/BASE_ALIGN_TOP)
;  MenXianLa = Widget_Label(MenXianB,SCR_XSIZE=Twidth+6,SCR_YSIZE=15,VALUE='����->')
;  MenXian = Widget_Text(MenXianB,SCR_XSIZE=Twidth-5,SCR_YSIZE=18,/EDITABLE,VALUE='-5')
;
; RongXianB = Widget_Base(HANTS_BASE,SCR_YSIZE=22,SPACE=1,XPAD=0,YPAD=0,/ROW,/BASE_ALIGN_TOP)
;  RongXianLa = Widget_Label(RongXianB,SCR_XSIZE=Twidth+6 ,SCR_YSIZE=15,VALUE='����->')
;  RongXian = Widget_Text(RongXianB,SCR_XSIZE=Twidth-5,SCR_YSIZE=18,/EDITABLE,VALUE='5')
;
; DODB = Widget_Base(HANTS_BASE,SCR_YSIZE=22,SPACE=1,XPAD=0,YPAD=0,/ROW,/BASE_ALIGN_TOP)
;  DODLa = Widget_Label(DODB,SCR_XSIZE=Twidth,SCR_YSIZE=15,VALUE='DOD->')
;  DOD = Widget_Text(DODB,SCR_XSIZE=Twidth-5,SCR_YSIZE=18,/EDITABLE,VALUE='2')

  BASE_PARAMETER_HANTS_RX = widget_base(BASE_PARAMETER_HANTS,/row,SPACE=3)
  LBL_TOLERANCE_HANTS = WIDGET_LABEL(BASE_PARAMETER_HANTS_RX,/ALIGN_LEFT ,VALUE='���ޣ�')
  TXT_TOLERANCE_HANTS = widget_slider(BASE_PARAMETER_HANTS_RX,VALUE=5,MAXIMUM=500, MINIMUM=0,XSIZE=85,event_pro='slider_event_fz',uname='TOLERANCE')

  BASE_PARAMETER_HANTS_DOD = widget_base(BASE_PARAMETER_HANTS,/row,SPACE=3)
  LBL_DOD_HANTS = WIDGET_LABEL(BASE_PARAMETER_HANTS_DOD,/ALIGN_LEFT ,VALUE='    DOD��')
  TXT_DOD_HANTS = widget_slider(BASE_PARAMETER_HANTS_DOD,VALUE=2,MAXIMUM=100, MINIMUM=1,XSIZE=85,event_pro='slider_event_fz',uname='DOD')
;*****************************************************************************
	  T2_OutBASE = Widget_Base(LEFT_BASE, UNAME='T2_OutBASE' ,FRAME=1  $
      ,SPACE=1, XPAD=1 ,YPAD=2,/COLUMN, xsize=BaseWidth-4)

  T1InputBASE = Widget_Base(T2_OutBASE, UNAME='T1InputBASE' ,FRAME=0  $
      ,SPACE=120 ,XPAD=1 ,YPAD=1, /row)
  label = widget_label(T1InputBASE, value='����ָ������ļ���')
  OutCropping_BU = Widget_Button(T1InputBASE, UNAME='OutCropping_BU'  $
      ,/ALIGN_CENTER,SCR_XSIZE=36 ,SCR_YSIZE=18  $
      ,TOOLTIP='ѡ������ļ�' ,VALUE='open.bmp' ,/BITMAP)
  T3InputBASE = Widget_Base(T2_OutBASE, UNAME='T3InputBASE' ,FRAME=0  $
      ,SPACE=1 ,XPAD=1 ,YPAD=1, /row)
  OutCropping = Widget_Text(T3InputBASE,  $
      UNAME='OutCropping' ,FRAME=0,SCR_XSIZE=270)


 ;=================�ұ߻�ͼBASE================================
 Draw_BASE = Widget_Base(TLB_BASE,FRAME=0,XPAD=0 ,YPAD=3,space=3,/COLUMN $
 		,/BASE_ALIGN_LEFT)

 base_id = Widget_Base(Draw_BASE,SCR_XSIZE=395 ,SCR_YSIZE=430,/col,xpad=0,ypad=0,space=0,/frame)
	colorLevel = $
	[[255B,	255B,	255B],$;1��0
	 [0B,	0B,		255B],$;2��1
	 [255B,	255B,	0B],$;3��2
	 [0B,	255B,	  0B],$;4��3
	 [255B,	128B,	  0B]];,$;5��4
	 ;[255B,	  0B,	  0B]];6��5
	class=1
	staff_display = {$
		base_id  :base_id,$
		image    :ptr_new(/ALLOCATE_HEAP,/no_copy),$
		startx	 :0.0    , $
	    starty	 :0.0    , $
	    xsize	 :0.0    , $
	    ysize	 :0.0    , $
	    pixelsize:0.0    , $
		palette	 :colorLevel, $
		shapefile:'',$
		legend   :'',$
		class	 :class ,$
		title    :''}
	pstaff_display = ptr_new(staff_display, /NO_COPY)
	widget_control, base_id, set_uvalue=pstaff_display
	display,pstaff_display


;	  WID_DRAW = Widget_Draw(Draw_BASE, UNAME='WID_DRAW' ,FRAME=1  $
;	      ,YOFFSET=1 ,SCR_XSIZE=395 ,SCR_YSIZE=360,GRAPHICS_LEVEL=2 $
;	      ,/BUTTON_EVENTS,/MOTION_EVENTS,RETAIN=2)

;	  WID_BASE_9 = Widget_Base(Draw_BASE, UNAME='WID_BASE_9' ,FRAME=1  $
;	      ,SPACE=3 ,XPAD=70 ,YPAD=2,/row)
;
;		  WID_LABEL_12 = Widget_Label(WID_BASE_9, UNAME='WID_LABEL_12'  $
;		      ,XOFFSET=78 ,YOFFSET=7 ,/ALIGN_LEFT  $
;		      ,VALUE='ָ�봦��ֵ��')
;		  CusorValue_TEXT = Widget_Text(WID_BASE_9, UNAME='CusorValue_TEXT'  $
;		      ,XOFFSET=159 ,YOFFSET=2 ,XSIZE=20  $
;		      ,YSIZE=1)
;		  SelectFile = Widget_Button(WID_BASE_9, UNAME='SelectFile'  $
;		      ,XOFFSET=255 ,YOFFSET=4 ,SCR_XSIZE=36 ,SCR_YSIZE=18  $
;		      ,/ALIGN_CENTER ,VALUE='open.bmp' ,/BITMAP,TOOLTIP='ѡ��������ļ�')

 ;=============����\�رհ�ť����========================================
  BU_BASE = Widget_Base(LEFT_BASE,FRAME=1,SCR_XSIZE=BaseWidth-1,SPACE=5  $
      ,XPAD=5 ,YPAD=1,/row)

	  Calculate_BU = Widget_Button(BU_BASE, UNAME='Calculate_BU'  $
	      ,XOFFSET=14 ,YOFFSET=3 ,SCR_XSIZE=62 ,SCR_YSIZE=22  $
	      ,/ALIGN_CENTER ,VALUE='����')

	  SelectFile = Widget_Button(BU_BASE, UNAME='SelectFile'  $
		      ,XOFFSET=14 ,YOFFSET=3 ,SCR_XSIZE=62 ,SCR_YSIZE=22  $
		      ,/ALIGN_CENTER ,VALUE='��',TOOLTIP='ѡ��������ļ�')

	  Help_bu = Widget_Button(BU_BASE, UNAME='Help_bu' ,XOFFSET=116  $
	      ,YOFFSET=3 ,SCR_XSIZE=64 ,SCR_YSIZE=23 ,/ALIGN_CENTER  $
	      ,VALUE='����')

	  Close_BU = Widget_Button(BU_BASE, UNAME='Close_BU' ,XOFFSET=219  $
	      ,YOFFSET=3 ,SCR_XSIZE=64 ,SCR_YSIZE=23 ,/ALIGN_CENTER  $
	      ,VALUE='�ر�')
;-----------------------------------------------------------------------------

;--------------------------------------------------------
	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	ProCode = STRMID(PROVINCE_CODE,0,2)      ;����������������Ϊ����ϵͳ������Ԥ��Ĳ���,����ϵͳ����ʱӦ���޸�

;====�������޸ģ�20070906======================================
;	CalcYear = strmid(systime(),3,4,/REVERSE_OFFSET)
;==============================================================

;  YearNum = FIX(STRMID(SYSTIME(),3,4,/REVERSE_OFFSET))-1990
;   Datayear  = STRTRIM(INDGEN(36)+1980,2)
;   DataMonth = STRTRIM(INDGEN(12)+1,2)

;  CurrentYear = Datayear[YearNum]

;   WIDGET_CONTROL,StartYear,SET_VALUE=Datayear,SET_combobox_SELECT=WHERE(Datayear EQ CalcYear)
;   WIDGET_CONTROL,EndYear, SET_VALUE=Datayear, SET_combobox_SELECT=WHERE(Datayear EQ CalcYear)
;   WIDGET_CONTROL,StartMonth,SET_VALUE=DataMonth,SET_DROPLIST_SELECT=0
;   WIDGET_CONTROL,EndMonth,  SET_VALUE=DataMonth,SET_DROPLIST_SELECT=11
;   WIDGET_CONTROL,EndTendayDay_DR,SET_DROPLIST_SELECT=2

;  WIDGET_CONTROL,Area_DR,SET_DROPLIST_SELECT=WHERE(ProIDList EQ ProCode)  				;Ĭ��Ϊ����

   CD,CURRENT=CurrentDir
;  WIDGET_CONTROL,CroppingZone,SET_VALUE='.\data_grid\ProCroppingZone.hdr'
   Widget_Control, /REALIZE, TLB_BASE
   Widget_Control,Close_BU,/INPUT_FOCUS

	COMMON COMMON_SETPATH,ppath
	v_fz_in_path  = (*ppath).fz_in_path
	v_fz_out_path  = (*ppath).fz_out_path

;   	WIDGET_CONTROL,WID_DRAW,GET_VALUE=OwinMap
;	OwinMap->ERASE,COLOR=255
   STATE = { $
            ProNameList			:	Province		,$				;ʡ���б�
            ProIDList			:	ProIDList		,$				;ʡID�б�
            ProID				:	ProCode			,$				;��ѡʡID
;            ARR_YEAR     		:	Datayear      	,$
;         	ARR_MONTH    		:	DataMonth 		,$
;         	ARR_TENDAY   		:	['01','02','03']   ,$
			StartYear			:	strtrim(c_year,2)		,$
			StartMonth			:	strtrim('1',2)			,$
			StartDay			:	strtrim('1',2)			,$
			EndYear				:	strtrim(c_year,2)		,$
			EndMonth			:	strtrim('12',2)			,$
			EndDay				:	strtrim('31',2)			,$
;            CalcYear			:	strtrim(c_year,2)		,$
;            MenXian				:	MenXian			,$    		;HANTS��������TEXT���
;            RongXian			:	RongXian		,$
;            DOD					:	DOD				,$
            NDVIPrefix			:	NDVIPrefix		,$
            NDVISuffix			:	NDVISuffix		,$
            InputNDVIPath		:	InputNDVIPath	,$
            v_fz_out_path	:	v_fz_out_path	,$
			NDVIList_La 		:	NDVIList_La		,$
 			NDVI_LIST 			:	NDVI_LIST   	,$
            CroppingZone		:	CroppingZone	,$
  			OutCropping			:	OutCropping		,$
;			oView				:	OBJ_NEW()		,$
;			ImageSize			:	{samples:0,lines:0} ,$		;WID_DRAW��Ӱ��Ĵ�С
;			Muple_isx_y  		:	FLTARR(2)		,$			;��¼�йز���,�Ի�ͼ��ʾ��.
;            WID_DRAW			:	WID_DRAW		,$
;            CusorValue_TEXT		:	CusorValue_TEXT	,$
            TXT_TOLERANCE_HANTS :	'5',$
            TXT_DOD_HANTS		:	'2',$
            pstaff_display 		: pstaff_display $
            }


    PA = PTR_NEW(STATE, /NO_COPY)

    WIDGET_CONTROL, TLB_BASE, SET_UVALUE=PA

	WIDGET_CONTROL, CMD_pick_date_s, get_uvalue=staff
    staff.pointer = PA
    WIDGET_CONTROL, CMD_pick_date_s, set_uvalue=staff

	WIDGET_CONTROL, CMD_pick_date_e, get_uvalue=staff
    staff.pointer = PA
    WIDGET_CONTROL, CMD_pick_date_e, set_uvalue=staff


	ProName = (*PA).ProNameList[WHERE((*PA).ProIDList EQ (*PA).ProID)]
	WIDGET_CONTROL, (*PA).InputNDVIPath, set_value=v_fz_in_path
	WIDGET_CONTROL, (*PA).OutCropping, set_value=v_fz_out_path+strtrim(c_year,2)+ProName+'����ָ��.hdr'

    XManager, 'FZ_Calculate', TLB_BASE, CLEANUP='DC_CleanAllHeap',/NO_BLOCK

END

pro slider_event_fz, event
	WIDGET_CONTROL, event.top, GET_UVALUE=PSTATE
	slider = widget_info(event.id, /UNAME)
	widget_control, event.id, GET_VALUE=vv

	case slider of
  		'TOLERANCE':begin
			(*PSTATE).TXT_TOLERANCE_HANTS = strtrim(vv,2)
		end
		'DOD':begin
			(*PSTATE).TXT_DOD_HANTS = strtrim(vv,2)
		end
		else:begin
		end
	endcase
end