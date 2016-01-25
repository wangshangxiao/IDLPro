;**********************************************************************************
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
		;print,'a',a[0:9]
		;print,'b',b[0:9]
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
;***********************************************************************************
; IDL EVENT CALLBACK PROCEDURES
; HANTS_EVENTCB
;
; GENERATED ON:	12/13/2004 16:49.27
;
;-----------------------------------------------------------------


;-----------------------------------------------------------------
FUNCTION DST_YEAR_START_HANTS, EVENT

     WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
     INDEX=EVENT.INDEX

     (*PSTATE).YEAR_START=(*PSTATE).ARR_YEAR[INDEX]
     WIDGET_CONTROL,(*PSTATE).DST_YEAR_END_HANTS,SET_DROPLIST_SELECT=INDEX
     (*PSTATE).YEAR_END=(*PSTATE).YEAR_START
     RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.

END
;-----------------------------------------------------------------

;-----------------------------------------------------------------
FUNCTION DST_MONTH_START_HANTS, EVENT

     WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
     INDEX=EVENT.INDEX
     (*PSTATE).MONTH_START=(*PSTATE).ARR_MONTH[INDEX]
     RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.

END
;-----------------------------------------------------------------


;-----------------------------------------------------------------
FUNCTION DST_XUN_START_HANTS, EVENT

     WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
     INDEX=EVENT.INDEX
     (*PSTATE).XUN_START=(*PSTATE).ARR_XUN[INDEX]
     RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.

END
;-----------------------------------------------------------------


;-----------------------------------------------------------------
FUNCTION DST_YEAR_END_HANTS, EVENT

     WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
     INDEX=EVENT.INDEX

     (*PSTATE).YEAR_END=(*PSTATE).ARR_YEAR[INDEX]
     WIDGET_CONTROL,(*PSTATE).DST_YEAR_START_HANTS,SET_DROPLIST_SELECT=INDEX
     (*PSTATE).YEAR_START=(*PSTATE).YEAR_END
     RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.

END
;-----------------------------------------------------------------

;-----------------------------------------------------------------
FUNCTION DST_MONTH_END_HANTS, EVENT

     WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
     INDEX=EVENT.INDEX
     (*PSTATE).MONTH_END=(*PSTATE).ARR_MONTH[INDEX]
     RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.

END
;-----------------------------------------------------------------


;-----------------------------------------------------------------
FUNCTION DST_XUN_END_HANTS, EVENT

     WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
     INDEX=EVENT.INDEX
     (*PSTATE).XUN_END=(*PSTATE).ARR_XUN[INDEX]
     RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.

END
;-----------------------------------------------------------------


;-----------------------------------------------------------------
FUNCTION CMD_OK_HANTS, EVENT



     PRINT,'��ʼ���ݿ�Ĳ���'
	 ;���ȼ�����ݿ�������Ƿ�ɹ�,�粻�ɹ��򲻽�������
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate

     IF ((*PSTATE).dbco_id EQ 0) THEN BEGIN
     	TEXT=DIALOG_MESSAGE('�����������ݿ�����!',/information)
     	CLOSE,/ALL
     	RETURN,0
     ENDIF
     OD = OBJ_NEW('IDLDBDATABASE')
     OD=(*PSTATE).DBCO


    ;********************************************************************************************
     ;����������������Լ���,
     ;�������Ĳ���������,�򲻽��м���,
     ;���ԶԻ������ʽ��ʾ�û���������

	WIDGET_CONTROL,(*pstate).TXT_DOD_HANTS,get_value=DOD
     IF(DOD EQ '') THEN BEGIN
         msg=DIALOG_MESSAGE('������DOD(���������)!',/INFORMATION)
         CLOSE,/all
         RETURN,0
     ENDIF
     DOD=FIX(DOD[0])

	WIDGET_CONTROL,(*pstate).TXT_TOLERANCE_HANTS,get_value=TOLERANCE
     IF(TOLERANCE EQ '') THEN BEGIN
         msg=DIALOG_MESSAGE('�����������ع�����!',/INFORMATION)
         CLOSE,/all
         RETURN,0
     ENDIF
     TOLERANCE=FIX(TOLERANCE[0])

	WIDGET_CONTROL,(*pstate).TXT_FREQUENCY_HANTS,get_value=FREQUENCY
     IF(FREQUENCY EQ '') THEN BEGIN
         msg=DIALOG_MESSAGE('������Ƶ��!',/INFORMATION)
         CLOSE,/all
         RETURN,0
     ENDIF
     FREQUENCY=FIX(FREQUENCY[0])

	WIDGET_CONTROL,(*pstate).TXT_MENXIAN_HANTS,get_value=MENXIAN
     IF(MENXIAN EQ '') THEN BEGIN
         msg=DIALOG_MESSAGE('����������!',/INFORMATION)
         CLOSE,/all
         RETURN,0
     ENDIF
     MENXIAN=FIX(MENXIAN[0])

	;��ø�������
	LAND_TYPE=(*pstate).LAND_TYPE

	;��ÿ�ʼ�ͽ�����ʱ�����
	;��
	YEAR_START=(*pstate).YEAR_START
	YEAR_END=(*pstate).YEAR_END

	;��
	MONTH_START=(*pstate).MONTH_START
	MONTH_END=(*pstate).MONTH_END

	;Ѯ
	XUN_START=(*pstate).XUN_START
	XUN_END=(*pstate).XUN_END

	;���ɲ�ѯ��ʱ�䷶Χ����
	TENDAY_START_36	=(MONTH_START-1)*3+XUN_START
	TENDAY_END_36	=(MONTH_END	-1)*3+XUN_END

	;�������������ʶ���ݵĳ���(�Ǹ�������Ĳ������ɵĳ���)
	NI_INPUT	=TENDAY_END_36-TENDAY_START_36+1


	PROVINCE=(*PSTATE).PROVINCE
	COUNTRY	=(*PSTATE).COUNTRY
	COUNTY	=(*PSTATE).COUNTY
	PROVINCE_CODE	=(*PSTATE).PROVINCE_CODE
	COUNTRY_CODE	=(*PSTATE).COUNTRY_CODE
	COUNTY_CODE		=(*PSTATE).COUNTY_CODE



	WIDGET_CONTROL,(*pstate).DRAW_HANTS,get_value=DRAW_HANTS


	;********************************************************************************************
	;********************************************************************************************
	;���ݿ������������,����������������ݵĺ����Լ���
	;�������Ҫ��,�������ʾ

	;1)��ʼʱ�����Ƚ���ʱ����
	IF(MONTH_START GE MONTH_END) THEN BEGIN
		msg=DIALOG_MESSAGE('�����·ݱ������ʼ�·ݴ�!',/INFORMATION)
		CLOSE,/all
    	RETURN,0
	END

	;2)��HANTS�������еĲ������м��
	IF(DOD GE 34) THEN BEGIN
		msg=DIALOG_MESSAGE('��Ч��ĸ������ܳ���34!',/INFORMATION)
		CLOSE,/all
    	RETURN,0
	END

	IF(FREQUENCY GE 6) THEN BEGIN
		msg=DIALOG_MESSAGE('���ڲ��ܳ���5!',/INFORMATION)
		CLOSE,/all
    	RETURN,0
	END

	IF(MENXIAN GT 0) THEN BEGIN
		msg=DIALOG_MESSAGE('����ֵ����С����!',/INFORMATION)
		CLOSE,/all
    	RETURN,0
	END

	;3)����ѡ�Ĺ��Һ�ʡ�����ж�
	IF(PROVINCE EQ '��ѡ��') THEN BEGIN
		msg=DIALOG_MESSAGE('����ʡ�н���ѡ��!',/INFORMATION)
		CLOSE,/all
    	RETURN,0
	END

	IF(COUNTY EQ '��ѡ��') THEN BEGIN
		msg=DIALOG_MESSAGE('��ѡ����!',/INFORMATION)
		CLOSE,/all
    	RETURN,0
	END
;	PRINT,YEAR_START,MONTH_START,XUN_START
;	PRINT,YEAR_END,MONTH_END,XUN_END
;	PRINT,COUNTRY,PROVINCE,COUNTY
;	PRINT,DOD,MENXIAN,TOLERANCE,FREQUENCY
;	PRINT,LAND_TYPE


	;********************************************************************************************
	;��PLACE�������и�ֵ,�����������ֵ�������Ѿ��ڹ���������������˵��

	;�����������ֵ���빫��������

	;********************************************************************************************
	;������������ݿ����ݵĶ�ȡ,����Ҫ��Ϊ�˵�ǰ��Ӧ��

	;����һ������,�����洢�����ݿ��ж�ȡ����������
	ARR_PLOWLAND=FLTARR(36)
	ARR_PADDY_FIELD=FLTARR(36)
	ARR_DRY_LAND=FLTARR(36)
	;����һ�����������洢��Ч��¼�ĸ���
	COUNT=0
	;�ϳ�ʱ������,��SQL���ʹ��


	;PLACE Ϊ1��ʾ��ȫ����������
	;�������������������,һ����һ�δ���һ����,��һ����һ�δ������е���
	IF(STRTRIM(PROVINCE,2) EQ 'ȫ��') THEN BEGIN	;һ�δ���ȫ�������ص�����
		PRINT,'WHOLE COUNTRY'

		;��ʾһ����������ִ�е�״̬��
		;F_READ_DATA=READING_DATA( GROUP_LEADER=(*PSTATE).BASE_TOP_HANTS)

		;������Ҫʹ�õ�����
		ARR_SHOW=STRARR(2*NI_INPUT+3,3*2500);������2500����

		;���Ȱ��ش�������ݼ���ȡ����
		SQL='SELECT CODE,NAME,NF FROM '
		SQL=SQL+'(select county_code.code,county_code.name,county_hants_parameter.nf'
		SQL=SQL+' from county_code,county_hants_parameter'
		SQL=SQL+' where county_code.code=county_hants_parameter.county_code)'
		ORS = OBJ_NEW('IDLDBRECORDSET', OD, SQL=SQL)
		TEMP=ORS->MOVECURSOR(/LAST)
		TOTAL_COUNTY=ORS-> CurrentRecord()+1

		IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN

			COUNT_COUNTY=0
			;****************************************************************
			progressTimer = Obj_New("ShowProgress", tlb,/CancelButton)
			progressTimer->start
			;****************************************************************
			WHILE (ORS->MOVECURSOR(/NEXT)) DO BEGIN
				;****************************************************************
				cancelled = progressTimer->CheckCancel()
				IF cancelled THEN BEGIN
					ok = Dialog_Message('�û���ֹ�˲���')
					progressTimer->Destroy ;����������
					RETURN,0
				ENDIF
				progressTimer->Update, (FLOAT(COUNT_COUNTY)/TOTAL_COUNTY * 100.0) ;��������
				;****************************************************************

				;�����������ش���
		 		COUNTY_CODE	=ORS -> GETFIELD(0)
				COUNTY		=ORS -> GETFIELD(1)
				NF_DB		=ORS -> GETFIELD(2)
				NF_DB		=3
				;************************************************************************************
				SQL='SELECT AVG_PLOWLAND,AVG_PADDY_FIELD,AVG_DRY_LAND'
				SQL=SQL+' FROM CROP.NDVI_TENDAY_PROCESS_COUNTY WHERE '
				SQL=SQL+'( ("COUNTY_CODE" = '+"'"+COUNTY_CODE+"' "+')'
				SQL=SQL+' AND (YEAR ='+STRTRIM(STRING(YEAR_START),2)+')'
				SQL=SQL+' AND (((MONTH-1)*3+TENDAY) >='+STRTRIM(STRING(TENDAY_START_36),2)+')'
				SQL=SQL+' AND (((MONTH-1)*3+TENDAY) <='+STRTRIM(STRING(TENDAY_END_36),2)+')'
				SQL=SQL+')'
				SQL=SQL+'ORDER BY YEAR,MONTH,TENDAY'
				ORS_1 = OBJ_NEW('IDLDBRECORDSET', OD, SQL=SQL)
				COUNT=0

				;��ȡһ���ص�����
				IF(ORS_1->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
					ARR_PLOWLAND[COUNT] = ORS_1 -> GETFIELD(0)
					ARR_PADDY_FIELD[COUNT] = ORS_1 -> GETFIELD(1)
					ARR_DRY_LAND[COUNT] = ORS_1 -> GETFIELD(2)
					COUNT=COUNT+1
					WHILE (ORS_1->MOVECURSOR(/NEXT)) DO BEGIN
		 				ARR_PLOWLAND[COUNT] = ORS_1 -> GETFIELD(0)
						ARR_PADDY_FIELD[COUNT] = ORS_1 -> GETFIELD(1)
						ARR_DRY_LAND[COUNT] = ORS_1 -> GETFIELD(2)
						COUNT=COUNT+1
					ENDWHILE
				ENDIF
				;�Ѷ�ȡ�����ݵ�RECORDSET�����ͷŵ�
				OBJ_DESTROY, ORS_1

				IF(COUNT EQ NI_INPUT) THEN BEGIN ;ֻ����ʵ�ʵ����ݳ��Ⱥ�ѡ������ݳ�����ͬʱ�Ž��м���
					;Ϊ����Ӧ�㷨,������һ����һ������
					ARR_PLOWLAND_TEMP=FLTARR(COUNT+1)
					ARR_PLOWLAND_TEMP[1:COUNT]=ARR_PLOWLAND[0:COUNT-1]
					ARR_PADDY_FIELD_TEMP=FLTARR(COUNT+1)
					ARR_PADDY_FIELD_TEMP[1:COUNT]=ARR_PADDY_FIELD[0:COUNT-1]
					ARR_DRY_LAND_TEMP=FLTARR(COUNT+1)
					ARR_DRY_LAND_TEMP[1:COUNT]=ARR_DRY_LAND[0:COUNT-1]

					;�ֶ������������������Ŵ�����Ľ��
					ARR_PLOWLAND_HANTS=FLTARR(COUNT+1)
					ARR_PADDY_FIELD_HANTS=FLTARR(COUNT+1)
					ARR_DRY_LAND_HANTS=FLTARR(COUNT+1)



					;********************************************************************************************
					;�����������е�HANTS������Ҫʹ�õĲ���,
					NI=COUNT
					FET=TOLERANCE
					NF=FREQUENCY

					;�ڴ���һ����������ݵ�ʱ��
					;ʹ�����ݿ��и��ݸ���ָ���ƶ���HANTS���ز���
					IF(NI EQ 36 ) THEN BEGIN
						IF((NF_DB GE 1) AND (NF_DB LE 3)) THEN BEGIN;����Ч��Χ�ڲ�ʹ��
							NF=NF_DB
						ENDIF ELSE BEGIN
							NF=3
						ENDELSE
					ENDIF

					IDRT=MENXIAN
					SCALE=1
					PER=NI*10
					DOD=DOD
					HILO='LO'
					TS=FINDGEN(NI+1)*10
					PA=INDGEN(10)
					FOR I=1,NF DO BEGIN
						PA[I]=PER/I
					ENDFOR

					AMP=FLTARR((NF+1)+1)
					PHI=FLTARR((NF+1)+1)
					;Y=O_DATA*10
					;********************************************************************************************

					Y=ARR_PLOWLAND_TEMP*100+10
					ARR_PLOWLAND_HANTS=(HANTSNEW(NI,PER,Y,TS,NF,PA,HILO,IDRT,FET,DOD,AMP,PHI)-10)/100.0
					Y=ARR_PADDY_FIELD_TEMP*100+10
					ARR_PADDY_FIELD_HANTS=(HANTSNEW(NI,PER,Y,TS,NF,PA,HILO,IDRT,FET,DOD,AMP,PHI)-10)/100.0
					Y=ARR_DRY_LAND_TEMP*100+10
					ARR_DRY_LAND_HANTS=(HANTSNEW(NI,PER,Y,TS,NF,PA,HILO,IDRT,FET,DOD,AMP,PHI)-10)/100.0
					;********************************************************************************************

					;********************************************************************************************
					;����һ������������һ�������ļ�¼(��������,�ֱ��Ǹ���\ˮ��\����),�����˴�����˵����ݺ�ԭʼ����

					ARR_SHOW[0,0+COUNT_COUNTY*3]=COUNTY
					ARR_SHOW[1,0+COUNT_COUNTY*3]=COUNTY_CODE
					ARR_SHOW[2,0+COUNT_COUNTY*3]='����'
					ARR_SHOW[3:NI+2,0+COUNT_COUNTY*3]=ARR_PLOWLAND_HANTS[1:NI]
					ARR_SHOW[NI+3:2*NI+2,0+COUNT_COUNTY*3]=ARR_PLOWLAND[0:NI-1]

					ARR_SHOW[0,1+COUNT_COUNTY*3]=COUNTY
					ARR_SHOW[1,1+COUNT_COUNTY*3]=COUNTY_CODE
					ARR_SHOW[2,1+COUNT_COUNTY*3]='ˮ��'
					ARR_SHOW[3:NI+2,1+COUNT_COUNTY*3]=ARR_PADDY_FIELD_HANTS[1:NI]
					ARR_SHOW[NI+3:2*NI+2,1+COUNT_COUNTY*3]=ARR_PADDY_FIELD[0:NI-1]

					ARR_SHOW[0,2+COUNT_COUNTY*3]=COUNTY
					ARR_SHOW[1,2+COUNT_COUNTY*3]=COUNTY_CODE
					ARR_SHOW[2,2+COUNT_COUNTY*3]='����'
					ARR_SHOW[3:NI+2,2+COUNT_COUNTY*3]=ARR_DRY_LAND_HANTS[1:NI]
					ARR_SHOW[NI+3:2*NI+2,2+COUNT_COUNTY*3]=ARR_DRY_LAND[0:NI-1]
					COUNT_COUNTY=COUNT_COUNTY+1

				ENDIF

				;************************************************************************************
			ENDWHILE	;�����ص�ѭ������
			progressTimer->destroy ;

			ARR_SHOW_1=STRARR(2*NI_INPUT+3,3*COUNT_COUNTY);������2500����;����ʵ�����ݴ�С���������
			ARR_SHOW_1[*,0:3*COUNT_COUNTY-1]=ARR_SHOW[*,0:3*COUNT_COUNTY-1]

			WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,TABLE_XSIZE=2*NI_INPUT+3
			WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,TABLE_YSIZE=3*COUNT_COUNTY
			WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,SET_VALUE=ARR_SHOW_1

			;�ͷ�һЩ�Ƚϴ������
			ARR_SHOW	=0
			ARR_SHOW_1	=0
			ARR_PLOWLAND_TEMP=0
			ARR_PLOWLAND_HANTS=0
			ARR_PADDY_FIELD_TEMP=0
			ARR_PADDY_FIELD_HANTS=0
			ARR_DRY_LAND_TEMP=0
			ARR_DRY_LAND_HANTS=0


			(*PSTATE).HANTS_DONE=1
			(*PSTATE).NI=NI_INPUT

		ENDIF
		;WIDGET_CONTROL,(*F_READ_DATA).BASE_READ_DATA,/DESTROY
		MSG='��ɼ���'+STRING(BYTE(10))+STRING(BYTE(13))
		MSG=MSG+'��������'+STRTRIM(STRING(COUNT_COUNTY),2)+'����'
		TEMP=DIALOG_MESSAGE(MSG,TITLE='�������',/INFORMATION)

	ENDIF ELSE BEGIN	;����һ���ص�����

		SQL='SELECT AVG_PLOWLAND,AVG_PADDY_FIELD,AVG_DRY_LAND'
		SQL=SQL+' FROM CROP.NDVI_TENDAY_PROCESS_COUNTY WHERE '
		SQL=SQL+'( ("COUNTY_CODE" = '+"'"+COUNTY_CODE+"' "+')'
		SQL=SQL+' AND (YEAR ='+STRTRIM(STRING(YEAR_START),2)+')'
		SQL=SQL+' AND (((MONTH-1)*3+TENDAY) >='+STRTRIM(STRING(TENDAY_START_36),2)+')'
		SQL=SQL+' AND (((MONTH-1)*3+TENDAY) <='+STRTRIM(STRING(TENDAY_END_36),2)+')'
		SQL=SQL+')'
		SQL=SQL+'ORDER BY YEAR,MONTH,TENDAY'
		PRINT,'SQL���:'
		PRINT,SQL
		oRS = OBJ_NEW('IDLdbRecordset', oD, SQL=SQL)
		PRINT,'AAA'
		COUNT=0

		IF(oRS->MoveCursor(/FIRST) EQ 1)THEN BEGIN
			ARR_PLOWLAND[COUNT] = ORS -> GetField(0)
			ARR_PADDY_FIELD[COUNT] = ORS -> GetField(1)
			ARR_DRY_LAND[COUNT] = ORS -> GetField(2)
			COUNT=COUNT+1
			WHILE (ORS->MoveCursor(/NEXT)) DO BEGIN
		 		ARR_PLOWLAND[COUNT] = ORS -> GetField(0)
				ARR_PADDY_FIELD[COUNT] = ORS -> GetField(1)
				ARR_DRY_LAND[COUNT] = ORS -> GetField(2)
				COUNT=COUNT+1
			ENDWHILE

			;HELP, record, /STRUCTURE
			;PRINT,RECORD.AVG_VALUE
		ENDIF
		IF(COUNT EQ 0) THEN BEGIN
			MSG=DIALOG_MESSAGE('û���ҵ���Ӧ������!',TITLE='����δ�ҵ�',/INFORMATION)
			CLOSE,/ALL
			RETURN,0
		ENDIF
		IF(COUNT LT NI_INPUT) THEN BEGIN
			TEMP='��ѡ���ʱ�䷶Χ�ڵ����ݲ�ȫ,'
			TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))
			TEMP=TEMP+'������ѡ��ʱ�䷶Χ�ٽ�������'
			MSG=DIALOG_MESSAGE(TEMP,TITLE='���ݲ�ȫ',/INFORMATION)
			CLOSE,/ALL
			RETURN,0
		ENDIF
		;Ϊ����Ӧ�㷨,������һ����һ������
		ARR_PLOWLAND_TEMP=FLTARR(COUNT+1)
		ARR_PLOWLAND_TEMP[1:COUNT]=ARR_PLOWLAND[0:COUNT-1]

		ARR_PADDY_FIELD_TEMP=FLTARR(COUNT+1)
		ARR_PADDY_FIELD_TEMP[1:COUNT]=ARR_PADDY_FIELD[0:COUNT-1]

		ARR_DRY_LAND_TEMP=FLTARR(COUNT+1)
		ARR_DRY_LAND_TEMP[1:COUNT]=ARR_DRY_LAND[0:COUNT-1]


		;�ֶ������������������Ŵ�����Ľ��
		ARR_PLOWLAND_HANTS=FLTARR(COUNT+1)
		ARR_PADDY_FIELD_HANTS=FLTARR(COUNT+1)
		ARR_DRY_LAND_HANTS=FLTARR(COUNT+1)


		;�����ݽ���HANTS����
		;********************************************************************************************
		;�����������е�HANTS������Ҫʹ�õĲ���,
		NI=COUNT
		FET=TOLERANCE
		NF=FREQUENCY
		IDRT=MENXIAN
		SCALE=1
		PER=NI*10
		DOD=DOD
		HILO='LO'
		TS=FINDGEN(NI+1)*10
		PA=INDGEN(10)
		FOR I=1,NF DO BEGIN
			PA[i]=PER/I
		ENDFOR

		;********************************************************************************************
		IF(COUNT LT NF*2+DOD+1) THEN BEGIN
			TEMP='���ݵ㲻��,�볢��:'
			TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))
			TEMP=TEMP+'(1)���ӵ�ĸ���'
			TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))
			TEMP=TEMP+'(2)����Ƶ��'
			TEMP=TEMP+STRING(BYTE(13))+STRING(BYTE(10))
			TEMP=TEMP+'(3)����DOD��ĸ���'
			MSG=DIALOG_MESSAGE(TEMP,TITLE='���ݵ㲻��',/INFORMATION)
			CLOSE,/ALL
			RETURN,0
		ENDIF
		;********************************************************************************************
		;�����ﶨ���Ҫ������,
		;�����ó���������ߵ��ع�,�����ع���������.
		AMP=FLTARR((NF+1)+1)
		PHI=FLTARR((NF+1)+1)
		;Y=O_DATA*10

		Y=ARR_PLOWLAND_TEMP*100+10
		ARR_PLOWLAND_HANTS=(HANTSNEW(NI,PER,Y,TS,NF,PA,HILO,IDRT,FET,DOD,AMP,PHI)-10)/100.0
		Y=ARR_PADDY_FIELD_TEMP*100+10
		ARR_PADDY_FIELD_HANTS=(HANTSNEW(NI,PER,Y,TS,NF,PA,HILO,IDRT,FET,DOD,AMP,PHI)-10)/100.0
		print,'a'
		Y=ARR_DRY_LAND_TEMP*100+10
		ARR_DRY_LAND_HANTS=(HANTSNEW(NI,PER,Y,TS,NF,PA,HILO,IDRT,FET,DOD,AMP,PHI)-10)/100.0
		;********************************************************************************************

		;********************************************************************************************
		;����һ������������һ�������ļ�¼(��������,�ֱ��Ǹ���\ˮ��\����),�����˴�����˵����ݺ�ԭʼ����
		PRINT,ARR_PLOWLAND_HANTS
		PRINT,ARR_PADDY_FIELD_HANTS
		PRINT,ARR_DRY_LAND_HANTS

		ARR_SHOW=STRARR(2*NI_INPUT+3,3)

		ARR_SHOW[0,0]=COUNTY
		ARR_SHOW[1,0]=COUNTY_CODE
		ARR_SHOW[2,0]='����'
		ARR_SHOW[3:NI+2,0]=ARR_PLOWLAND_HANTS[1:NI]
		ARR_SHOW[NI+3:2*NI+2,0]=ARR_PLOWLAND[0:NI-1]

		ARR_SHOW[0,1]=COUNTY
		ARR_SHOW[1,1]=COUNTY_CODE
		ARR_SHOW[2,1]='ˮ��'
		ARR_SHOW[3:NI+2,1]=ARR_PADDY_FIELD_HANTS[1:NI]
		ARR_SHOW[NI+3:2*NI+2,1]=ARR_PADDY_FIELD[0:NI-1]

		ARR_SHOW[0,2]=COUNTY
		ARR_SHOW[1,2]=COUNTY_CODE
		ARR_SHOW[2,2]='����'
		ARR_SHOW[3:NI+2,2]=ARR_DRY_LAND_HANTS[1:NI]
		ARR_SHOW[NI+3:2*NI+2,2]=ARR_DRY_LAND[0:NI-1]
		WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,TABLE_XSIZE=2*NI_INPUT+3
		WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,TABLE_YSIZE=3
		WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,SET_VALUE=ARR_SHOW

		PRINT,'COUNT',COUNT
		(*PSTATE).NI=COUNT
		(*PSTATE).HANTS_DONE=1
	ENDELSE


    ;********************************************************************************************
    ;�����ﶨ���Ҫ������,
    ;�����ó���������ߵ��ع�,�����ع���������.
    ;********************************************************************************************

	;���û���ҵ��㹻�ļ�¼
	;�򲻽��м���,������������ж�

    IF(COUNT EQ 0) THEN BEGIN
		msg=DIALOG_MESSAGE('û���ҵ���Ӧ������!',/INFORMATION)
		CLOSE,/all
    	RETURN,0
	ENDIF
    ;********************************************************************************************

     RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.

END
;-----------------------------------------------------------------


;-----------------------------------------------------------------
FUNCTION CMD_CLOSE_HANTS, EVENT

     CLOSE,/all
     WIDGET_CONTROL, event.top, /destroy

     RETURN, Event ; By Default, return the event.

END
;-----------------------------------------------------------------


;-----------------------------------------------------------------
FUNCTION CMD_DB_WRITE_HANTS, EVENT

   	PRINT,'��ʼ����������'
	;���ȼ�����ݿ�������Ƿ�ɹ�,�粻�ɹ��򲻽�������
	WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
    IF ((*PSTATE).dbco_id EQ 0) THEN BEGIN
     	TEXT=DIALOG_MESSAGE('�����������ݿ�����!',/information)
     	CLOSE,/ALL
     	RETURN,0
    ENDIF

    ;2)��HANTS�������еĲ������м��
	IF((*PSTATE).HANTS_DONE EQ 0) THEN BEGIN
		msg=DIALOG_MESSAGE('���Ƚ���г���������ع�!',/INFORMATION)
		CLOSE,/all
    	RETURN,0
	END

	;��ȡһЩ�ع�ʱ����ز���,�������ݿ������
	YEAR_START	= (*PSTATE).YEAR_START
	YEAR		= STRTRIM(STRING(YEAR_START),2)
    MONTH_START = (*PSTATE).MONTH_START
    XUN_START   = (*PSTATE).XUN_START
    MONTH_END 	= (*PSTATE).MONTH_END
    XUN_END		= (*PSTATE).XUN_END
	PLACE		=	(*PSTATE).PLACE
	OD = OBJ_NEW('IDLDBDATABASE')
    OD=(*PSTATE).DBCO


     WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,GET_VALUE=ARR_TABLE


	;����һ���ṹ��,��Ҫ��������д��һ���ṹ������
	RESULT = { $
    	COUNTY_CODE	:	''		, $
    	YEAR 		:  	0		, $
    	MONTH 		:  	0		, $
    	XUN  		:  	0		, $
    	PLOWLAND	:	0.0		, $
    	PADDY_FIELD	:	0.0		, $
    	DRY_LAND	:	0.0		, $
    	SENSOR_TYPE	:	''		 $
      		}

	;ARR_RESULT=REPLICATE(RESULT, NI)

	;������д��ṹ��

	;��ȡ���ݼ��Ĵ�С
	LINES=(SIZE(ARR_TABLE))[2];���LINES�ŵ�����ʾ�ı��е�����
	LINES=LINES/3
	PRINT,LINES
	;********************************************************
   	PROGRESSTIMER = OBJ_NEW("SHOWPROGRESS", TLB,/CANCELBUTTON)
	PROGRESSTIMER->START
	;********************************************************

	FOR J=0,LINES-1 DO BEGIN
		;********************************************************
		CANCELLED = PROGRESSTIMER->CHECKCANCEL()
		IF CANCELLED THEN BEGIN
			OK = DIALOG_MESSAGE('�û���ֹ�˲���')
			PROGRESSTIMER->DESTROY ;����������
			RETURN,0
		ENDIF
		PROGRESSTIMER->UPDATE, (FLOAT(J)/LINES * 100.0) ;��������
		;********************************************************
		FOR I=MONTH_START*3+XUN_START,MONTH_END*3+XUN_END DO BEGIN

			RESULT.COUNTY_CODE	=	STRTRIM(ARR_TABLE[1,J*3])
			RESULT.YEAR			=	YEAR_START
			RESULT.MONTH		=(I-1)/3
			RESULT.XUN			=((I-1) MOD 3)+1
			RESULT.PLOWLAND		=ARR_TABLE[I-MONTH_START*3-XUN_START+3,0+J*3]
			RESULT.PADDY_FIELD	=ARR_TABLE[I-MONTH_START*3-XUN_START+3,1+J*3]
			RESULT.DRY_LAND		=ARR_TABLE[I-MONTH_START*3-XUN_START+3,2+J*3]
			RESULT.SENSOR_TYPE	='AVHRR'
			;PRINT,RESULT

			;ɾ�����ݿ�����Ҫ����ļ�¼ʱ��������ͬ�ļ�¼
			SQL='DELETE FROM CROP.NDVI_TENDAY_COUNTY_HANTS WHERE ( '
			SQL=SQL+'("COUNTY_CODE" = '+"'"+RESULT.COUNTY_CODE+"' "+')'
  			SQL=SQL+' AND ("YEAR" = '+STRTRIM(STRING(RESULT.YEAR),2)+')'
  			SQL=SQL+' AND ("MONTH" = '+STRTRIM(STRING(RESULT.MONTH),2)+')'
  			SQL=SQL+' AND ("TENDAY" = '+STRTRIM(STRING(RESULT.XUN),2)+')'
  			SQL=SQL+' AND ("SENSOR_TYPE" = '+"'"+RESULT.SENSOR_TYPE+"'"+'))'
  			;PRINT,SQL
  			OD->EXECUTESQL,SQL

			;ɾ�����ٲ����µļ�¼
			SQL='INSERT INTO "CROP"."NDVI_TENDAY_COUNTY_HANTS"
			SQL=SQL+'("COUNTY_CODE" ,"YEAR" ,"MONTH" ,"TENDAY"'
 			SQL=SQL+',"PLOWLAND","PADDY_FIELD","DRY_LAND","SENSOR_TYPE" ) '
 			SQL=SQL+'VALUES ('
 			SQL=SQL+"'"+STRTRIM(STRING(RESULT.COUNTY_CODE),2)	+"'"+','
 			SQL=SQL+STRTRIM(STRING(RESULT.YEAR),2)			+','
 			SQL=SQL+STRTRIM(STRING(RESULT.MONTH),2)			+','
 			SQL=SQL+STRTRIM(STRING(RESULT.XUN),2)			+','
 			SQL=SQL+STRTRIM(STRING(RESULT.PLOWLAND),2)		+','
 			SQL=SQL+STRTRIM(STRING(RESULT.PADDY_FIELD),2)	+','
 			SQL=SQL+STRTRIM(STRING(RESULT.DRY_LAND),2)		+','
 			SQL=SQL+"'"+STRTRIM(STRING(RESULT.SENSOR_TYPE),2)	+"'"+')'
 			;PRINT,SQL

 			OD->EXECUTESQL,SQL
		ENDFOR
	ENDFOR
	;********************************************************
	PROGRESSTIMER->DESTROY ;
	;********************************************************
	MSG=DIALOG_MESSAGE('����װ�����',TITLE='���',/INFORMATION)
 	RETURN,1

END
;-----------------------------------------------------------------


;-----------------------------------------------------------------
FUNCTION DST_LAND_TYPE_HANTS, EVENT

     WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
     INDEX=EVENT.INDEX
     (*PSTATE).LAND_TYPE=(*PSTATE).ARR_LAND_TYPE[INDEX]
     RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.

END
;-----------------------------------------------------------------
;-----------------------------------------------------------------
PRO TABLE_RESULT_HANTS0, EVENT

	IF (EVENT.SEL_LEFT EQ -1) THEN RETURN
	WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
	IF ((*PSTATE).HANTS_DONE EQ 0) THEN RETURN
	;HELP,EVENT,/STRUCTURE
	TOP=EVENT.SEL_TOP
	NI=(*PSTATE).NI
	PRINT,'NI',NI
	WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,SET_TABLE_SELECT=[0, top,(*PSTATE).NI*2+3-1,TOP]
	WIDGET_CONTROL,(*PSTATE).TABLE_RESULT_HANTS,GET_VALUE=ARR_RESULT
	PRINT,ARR_RESULT[*,TOP]
	ARR_H=ARR_RESULT[3:NI+3-1,TOP]
	ARR_O=ARR_RESULT[3+NI:NI*2+3-1,TOP]
	DRAW_HANTS=(*PSTATE).DRAW_HANTS

	;********************************************************************************************
	;��ѡ��Ľ����ͼ

	WIDGET_CONTROL,(*PSTATE).DRAW_HANTS,GET_VALUE=DRAW_HANTS
	WSET, DRAW_HANTS
	white=!D.N_COLORS-1

	;***********************************************
	device, set_font='����', /TT_FONT

	;����ԭʼ������
	X=FINDGEN(NI)*10
	PLOT,X,ARR_H,BACKGROUND=WHITE,color=0,XMARGIN=[7,2],YMARGIN=[3,1],PSYM=1,SYMSIZE=0.6$
		,XTITLE='��λ:��',YRANGE=[0.1,0.9],FONT=2,charsize=1
	XYOUTS,6,150,'ֲ',FONT=2,charsize=1,COLOR=0,/DEVICE
	XYOUTS,6,120,'��',FONT=2,charsize=1,COLOR=0,/DEVICE
	XYOUTS,6,90	,'ָ',FONT=2,charsize=1,COLOR=0,/DEVICE
	XYOUTS,6,60	,'��',FONT=2,charsize=1,COLOR=0,/DEVICE

	OPLOT,X,ARR_O,COLOR=0
	;�����ع��������
	OPLOT,X,ARR_H,PSYM=2,SYMSIZE=0.6,COLOR=255
	OPLOT,X,ARR_H,COLOR=255

	;��ϵͳ�������������,�Ա���к��ֵ���ʾ
	;DEVICE, SET_FONT = 'kai'
	;��ԭʼ���ߵ�ͼ��
	PLOTS,30+50,185,COLOR=0,PSYM=1,SYMSIZE=0.6,/DEVICE
	PLOTS,40+50,185,COLOR=0,PSYM=1,SYMSIZE=0.6,/DEVICE
	PLOTS,50+50,185,COLOR=0,PSYM=1,SYMSIZE=0.6,/DEVICE
	PLOTS,60+50,185,COLOR=0, /DEVICE ,/CONTINUE,PSYM=1,SYMSIZE=0.6
	PLOTS,30+50,185,COLOR=0,/DEVICE
	PLOTS,60+50,185,COLOR=0, /DEVICE ,/CONTINUE
	XYOUTS,65+50,181,'Original',/DEVICE,COLOR=0,FONT=2


	;���ع�������ߵ�ͼ��
	PLOTS,180+10,185,COLOR=255+256L*(0L+0*256L),PSYM=2,SYMSIZE=0.6,/DEVICE
	PLOTS,190+10,185,COLOR=255+256L*(0L+0*256L),PSYM=2,SYMSIZE=0.6,/DEVICE
	PLOTS,200+10,185,COLOR=255+256L*(0L+0*256L),PSYM=2,SYMSIZE=0.6,/DEVICE
	PLOTS,210+10,185,COLOR=255+256L*(0L+0*256L), /DEVICE ,/CONTINUE,PSYM=2,SYMSIZE=0.6
	PLOTS,180+10,185,COLOR=255+256L*(0L+0*256L),/DEVICE
	PLOTS,210+10,185,COLOR=255+256L*(0L+0*256L), /DEVICE ,/CONTINUE
	XYOUTS,215+10,181,'HANTS',/DEVICE,COLOR=255+256L*(0L+0*256L),FONT=2

END
;-----------------------------------------------------------------


;-----------------------------------------------------------------
FUNCTION DST_COUNTRY_HANTS, EVENT

     WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
     INDEX=EVENT.INDEX
     (*PSTATE).COUNTRY=(*PSTATE).ARR_COUNTRY[INDEX]

     ;���ݹ��ҵ����ƽ���ʡ(��)��ѡ��
     COUNTRY=(*PSTATE).COUNTRY
     CASE COUNTRY OF
   		'�й�': BEGIN
   			oRS = OBJ_NEW('IDLdbRecordset', (*PSTATE).DBCO, TABLE='crop.PROVINCE_CODE')
    		;�����ʡ����ʼ�ĸ�����1,������0
    		;������Ϊ��һ��ѡ����'��ѡ��'
    		num_of_PROVINCE=1

	   		IF(oRS->MoveCursor(/FIRST) EQ 1)THEN BEGIN

	   			;���ҳ�ʡ�ĸ���
				WHILE (oRS->MoveCursor(/next) eq 1) DO BEGIN
					num_of_PROVINCE=num_of_PROVINCE+1
				ENDWHILE

				;Ȼ�󽫹��ҵ����ƶ�������
				ARR_PROVINCE=STRARR(NUM_OF_PROVINCE+1+1)
				ARR_PROVINCE_CODE=STRARR(NUM_OF_PROVINCE+1+1)
				TEMP=oRS->MoveCursor(/FIRST)
				COUNT=0
				ARR_PROVINCE[COUNT]='��ѡ��'
				COUNT=COUNT+1
				ARR_PROVINCE[COUNT]='ȫ��'
				COUNT=COUNT+1
				ARR_PROVINCE[COUNT]=ORS->GetField(1)
				ARR_PROVINCE_CODE[COUNT]=ORS->GetField(0)

				WHILE (oRS->MoveCursor(/next) eq 1) DO BEGIN
					COUNT=COUNT+1
					ARR_PROVINCE[COUNT]=ORS->GetField(1)
					ARR_PROVINCE_CODE[COUNT]=ORS->GetField(0)
				ENDWHILE
      			WIDGET_CONTROL,(*pstate).DST_PROVINCE_HANTS,SET_VALUE=ARR_PROVINCE
      			(*PSTATE).ARR_PROVINCE=ARR_PROVINCE
      			(*PSTATE).ARR_PROVINCE_CODE=ARR_PROVINCE_CODE
      			(*PSTATE).NUM_OF_PROVINCE=num_of_PROVINCE + 2
	   		ENDIF
   			END
   		'����': BEGIN
   			oRS = OBJ_NEW('IDLdbRecordset',  (*PSTATE).DBCO, TABLE='crop.STATE_CODE_USA')
    		;�����ʡ����ʼ�ĸ�����1,������0
    		;������Ϊ��һ��ѡ����'��ѡ��'
    		num_of_PROVINCE=1

	   		IF(oRS->MoveCursor(/FIRST) EQ 1)THEN BEGIN

	   			;���ҳ�ʡ�ĸ���
				WHILE (oRS->MoveCursor(/next) eq 1) DO BEGIN
					num_of_PROVINCE=num_of_PROVINCE+1
				ENDWHILE

				;Ȼ�󽫹��ҵ����ƶ�������
				ARR_PROVINCE=STRARR(NUM_OF_PROVINCE+1+1)
				ARR_PROVINCE_CODE=STRARR(NUM_OF_PROVINCE+1+1)
				TEMP=oRS->MoveCursor(/FIRST)
				COUNT=0
				ARR_PROVINCE[COUNT]='��ѡ��'
				COUNT=COUNT+1
				ARR_PROVINCE[COUNT]='ȫ��'
				COUNT=COUNT+1
				ARR_PROVINCE[COUNT]=ORS->GetField(1)
				ARR_PROVINCE_CODE[COUNT]=ORS->GetField(0)

				WHILE (oRS->MoveCursor(/next) eq 1) DO BEGIN
					COUNT=COUNT+1
					ARR_PROVINCE[COUNT]=ORS->GetField(1)
					ARR_PROVINCE_CODE[COUNT]=ORS->GetField(0)
				ENDWHILE
      			WIDGET_CONTROL,(*pstate).DST_PROVINCE_HANTS,SET_VALUE=ARR_PROVINCE
      			(*PSTATE).ARR_PROVINCE=ARR_PROVINCE
      			(*PSTATE).ARR_PROVINCE_CODE=ARR_PROVINCE_CODE
      			(*PSTATE).NUM_OF_PROVINCE=num_of_PROVINCE+2

	   		ENDIF
   			END

		ELSE: BEGIN
			;�¾���ж���������֤���ʱ����ҵ������б���
			;�Ѿ������ݿ��Թ�ѡ��
			IF((STRLEN(COUNTRY) GT 2) AND (COUNTRY NE '��ѡ��' )) THEN BEGIN
				NUM_OF_PROVINCE=0
				ARR_PROVINCE=STRARR(NUM_OF_PROVINCE+1+1)
				COUNT=0
				ARR_PROVINCE[COUNT]='��ѡ��'
				COUNT=COUNT+1
				ARR_PROVINCE[COUNT]='ȫ��'
				WIDGET_CONTROL,(*pstate).DST_PROVINCE_HANTS,SET_VALUE=ARR_PROVINCE
				(*PSTATE).ARR_PROVINCE=ARR_PROVINCE

      			(*PSTATE).NUM_OF_PROVINCE=num_of_PROVINCE+2
      		ENDIF
			END
	ENDCASE

	;�����û��ڹ��ҵ�ѡ����ѡ����ʲô,ʡ�б��еĵ�һ��ѡ��϶���'��ѡ��'
	(*PSTATE).PROVINCE='��ѡ��'
    RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.

END
;-----------------------------------------------------------------


;-----------------------------------------------------------------
FUNCTION DST_PROVINCE_HANTS, EVENT

    WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE

    INDEX=EVENT.INDEX
    (*PSTATE).PROVINCE=(*PSTATE).ARR_PROVINCE[INDEX]
    (*PSTATE).PROVINCE_CODE=(*PSTATE).ARR_PROVINCE_CODE[INDEX]

	PROVINCE		=(*PSTATE).PROVINCE
	PROVINCE_CODE	=(*PSTATE).PROVINCE_CODE
	PRINT,PROVINCE,PROVINCE_CODE

	;��һЩ����ǲ���Ҫ�����ص����ݵĶ�ȡ��,�ȶ���Щ������д���
	IF(((*PSTATE).PROVINCE EQ '��ѡ��') OR ((*PSTATE).PROVINCE EQ 'ȫ��')) THEN BEGIN
		TEMP=0
		PRINT,'A'
		WIDGET_CONTROL,(*PSTATE).DST_COUNTY_HANTS,SENSITIVE=0
		RETURN,0
	ENDIF
	WIDGET_CONTROL,(*PSTATE).DST_COUNTY_HANTS,SENSITIVE=1

    ;�ȶ��ؽ��й��㴦��
    TEMP=['']
    WIDGET_CONTROL,(*pstate).DST_COUNTY_HANTS,SET_VALUE=TEMP
	;����ĳ��ʡ�е���
    SQL='select * from crop.COUNTY_CODE WHERE ("PROVINCE_CODE" = '+"'"+PROVINCE_code+"' "+')'
    PRINT,SQL
    oRS = OBJ_NEW('IDLdbRecordset', (*PSTATE).DBCO, SQL=SQL)
    	;�����ʡ����ʼ�ĸ�����1,������0
    	;������Ϊ��һ��ѡ����'��ѡ��'
    	num_of_COUNTY=1

	   	IF(oRS->MoveCursor(/FIRST) EQ 1)THEN BEGIN

	   		;���ҳ�ʡ�ĸ���
			WHILE (oRS->MoveCursor(/next) eq 1) DO BEGIN
				num_of_COUNTY=num_of_COUNTY+1
			ENDWHILE

			;Ȼ�󽫹��ҵ����ƶ�������
			ARR_COUNTY		=STRARR(num_of_COUNTY)
			ARR_COUNTY_CODE	=STRARR(num_of_COUNTY)
			TEMP=oRS->MoveCursor(/FIRST)
			COUNT=0

			ARR_COUNTY[COUNT]		=ORS->GetField(1)
			ARR_COUNTY_CODE[COUNT]	=ORS->GetField(0)
			WHILE (oRS->MoveCursor(/next) eq 1) DO BEGIN
				COUNT=COUNT+1
				ARR_COUNTY[COUNT]=ORS->GetField(1)
				ARR_COUNTY_CODE[COUNT]	=ORS->GetField(0)
			ENDWHILE
      		WIDGET_CONTROL,(*pstate).DST_COUNTY_HANTS,SET_VALUE=ARR_COUNTY
      		(*PSTATE).ARR_COUNTY=ARR_COUNTY
      		(*PSTATE).ARR_COUNTY_CODE=ARR_COUNTY_CODE
      		(*PSTATE).COUNTY=ARR_COUNTY[0]
      		(*PSTATE).COUNTY_CODE=ARR_COUNTY_CODE[0]
      		(*PSTATE).NUM_OF_COUNTY=num_of_COUNTY
      		PRINT,(*PSTATE).NUM_OF_COUNTY
	   	ENDIF

     RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.

END
;-----------------------------------------------------------------

;-----------------------------------------------------------------
FUNCTION DST_COUNTY_HANTS, EVENT

     WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE
     INDEX=EVENT.INDEX
     (*PSTATE).COUNTY=(*PSTATE).ARR_COUNTY[INDEX]
     (*PSTATE).COUNTY_CODE=(*PSTATE).ARR_COUNTY_CODE[INDEX]
     PRINT,(*PSTATE).COUNTY,(*PSTATE).COUNTY_CODE
     RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.

END
;-----------------------------------------------------------------



;-----------------------------------------------------------------
;
; EMPTY STUB PROCEDURE USED FOR AUTOLOADING.
;
PRO HANTS_EVENTCB
END
;
; IDL WIDGET INTERFACE PROCEDURES. THIS CODE IS AUTOMATICALLY
;     GENERATED AND SHOULD NOT BE MODIFIED.

;
; GENERATED ON:	12/13/2004 16:49.27
;
PRO BASE_TOP_HANTS_EVENT, EVENT

  WTARGET = (WIDGET_INFO(EVENT.ID,/NAME) EQ 'TREE' ?  $
      WIDGET_INFO(EVENT.ID, /TREE_ROOT) : EVENT.ID)


  WWIDGET =  EVENT.TOP

  CASE WTARGET OF

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_YEAR_START_HANTS'): BEGIN
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_MONTH_START_HANTS'): BEGIN
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_XUN_START_HANTS'): BEGIN
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_YEAR_END_HANTS'): BEGIN
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_MONTH_END_HANTS'): BEGIN
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_XUN_END_HANTS'): BEGIN
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_OK_HANTS'): BEGIN
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_CLOSE_HANTS'): BEGIN
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_DB_WRITE_HANTS'): BEGIN
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_LAND_TYPE_HANTS'): BEGIN
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_COUNTRY_HANTS'): BEGIN
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_PROVINCE_HANTS'): BEGIN
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='DST_COUNTY_HANTS'): BEGIN
    END
    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='TABLE_RESULT_HANTS'): BEGIN
      	IF( TAG_NAMES(EVENT, /STRUCTURE_NAME) EQ 'WIDGET_TABLE_CELL_SEL' )THEN $
        	TABLE_RESULT_HANTS, EVENT
    	END
    ELSE:
  ENDCASE

END

PRO BASE_TOP_HANTS,F,GROUP_LEADER=WGROUP

  RESOLVE_ROUTINE, 'HANTS_EVENTCB',/COMPILE_FULL_FILE  ; LOAD EVENT CALLBACK ROUTINES

  BASE_TOP_HANTS = WIDGET_BASE( GROUP_LEADER=WGROUP,  $
      UNAME='BASE_TOP_HANTS' ,XOFFSET=150 ,YOFFSET=150 ,SCR_XSIZE=610  $
      ,SCR_YSIZE=419 ,TITLE='г�����ع�' ,SPACE=3 ,XPAD=3 ,YPAD=3  $
      ,TLB_FRAME_ATTR=1)


  BASE_INPUT_HANTS = WIDGET_BASE(BASE_TOP_HANTS,  $
      UNAME='BASE_INPUT_HANTS' ,FRAME=1 ,XOFFSET=8 ,YOFFSET=8  $
      ,SCR_XSIZE=231 ,SCR_YSIZE=377 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  BASE_DATE_HANTS = WIDGET_BASE(BASE_INPUT_HANTS,  $
      UNAME='BASE_DATE_HANTS' ,FRAME=1 ,XOFFSET=6 ,YOFFSET=7  $
      ,SCR_XSIZE=217 ,SCR_YSIZE=116 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  BASE_DATE_START_HANTS = WIDGET_BASE(BASE_DATE_HANTS,  $
      UNAME='BASE_DATE_START_HANTS' ,FRAME=1 ,XOFFSET=7 ,YOFFSET=24  $
      ,SCR_XSIZE=202 ,SCR_YSIZE=30 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  LBL_YEAR_START_HANTS = WIDGET_LABEL(BASE_DATE_START_HANTS,  $
      UNAME='LBL_YEAR_START_HANTS' ,XOFFSET=4 ,YOFFSET=8  $
      ,SCR_XSIZE=16 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='��:')


  DST_YEAR_START_HANTS = WIDGET_DROPLIST(BASE_DATE_START_HANTS,  $
      UNAME='DST_YEAR_START_HANTS' ,XOFFSET=21 ,YOFFSET=4  $
      ,SCR_XSIZE=50 ,SCR_YSIZE=20 ,EVENT_FUNC='DST_YEAR_START_HANTS'  $
      ,VALUE=[ '90', '91', '92', '93', '94', '95', '96', '97', '98',  $
      '99', '00', '01', '02', '03', '04', '05', '06' ])


  LBL_MONTH_START_HANTS = WIDGET_LABEL(BASE_DATE_START_HANTS,  $
      UNAME='LBL_MONTH_START_HANTS' ,XOFFSET=74 ,YOFFSET=8  $
      ,SCR_XSIZE=17 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='��:')


  DST_MONTH_START_HANTS = WIDGET_DROPLIST(BASE_DATE_START_HANTS,  $
      UNAME='DST_MONTH_START_HANTS' ,XOFFSET=93 ,YOFFSET=4  $
      ,SCR_XSIZE=40 ,SCR_YSIZE=20 ,EVENT_FUNC='DST_MONTH_START_HANTS' )


  LBL_XUN_START_HANTS = WIDGET_LABEL(BASE_DATE_START_HANTS,  $
      UNAME='LBL_XUN_START_HANTS' ,XOFFSET=138 ,YOFFSET=8  $
      ,SCR_XSIZE=18 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='Ѯ:')


  DST_XUN_START_HANTS = WIDGET_DROPLIST(BASE_DATE_START_HANTS,  $
      UNAME='DST_XUN_START_HANTS' ,XOFFSET=159 ,YOFFSET=4  $
      ,SCR_XSIZE=35 ,SCR_YSIZE=20 ,EVENT_FUNC='DST_XUN_START_HANTS'  $
      ,VALUE=[ '��Ѯ', '��Ѯ', '��Ѯ' ])


  LBL_DATE_START_HANTS = WIDGET_LABEL(BASE_DATE_HANTS,  $
      UNAME='LBL_DATE_START_HANTS' ,XOFFSET=11 ,YOFFSET=8  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=16 ,/ALIGN_LEFT ,VALUE='��ʼʱ��:')


  LBL_DATE_END_HANTS = WIDGET_LABEL(BASE_DATE_HANTS,  $
      UNAME='LBL_DATE_END_HANTS' ,XOFFSET=11 ,YOFFSET=60  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=16 ,/ALIGN_LEFT ,VALUE='����ʱ��:')


  BASE_DATE_END_HANTS = WIDGET_BASE(BASE_DATE_HANTS,  $
      UNAME='BASE_DATE_END_HANTS' ,FRAME=1 ,XOFFSET=7 ,YOFFSET=77  $
      ,SCR_XSIZE=203 ,SCR_YSIZE=30 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  LBL_YEAR_END_HANTS = WIDGET_LABEL(BASE_DATE_END_HANTS,  $
      UNAME='LBL_YEAR_END_HANTS' ,XOFFSET=4 ,YOFFSET=8 ,SCR_XSIZE=16  $
      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='��:')


  DST_YEAR_END_HANTS = WIDGET_DROPLIST(BASE_DATE_END_HANTS,  $
      UNAME='DST_YEAR_END_HANTS' ,XOFFSET=21 ,YOFFSET=4 ,SCR_XSIZE=50  $
      ,SCR_YSIZE=20 ,EVENT_FUNC='DST_YEAR_END_HANTS' )


  LBL_MONTH_END_HANTS = WIDGET_LABEL(BASE_DATE_END_HANTS,  $
      UNAME='LBL_MONTH_END_HANTS' ,XOFFSET=74 ,YOFFSET=8  $
      ,SCR_XSIZE=17 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='��:')


  DST_MONTH_END_HANTS = WIDGET_DROPLIST(BASE_DATE_END_HANTS,  $
      UNAME='DST_MONTH_END_HANTS' ,XOFFSET=93 ,YOFFSET=4  $
      ,SCR_XSIZE=40 ,SCR_YSIZE=20 ,EVENT_FUNC='DST_MONTH_END_HANTS' )


  LBL_XUN_END_HANTS = WIDGET_LABEL(BASE_DATE_END_HANTS,  $
      UNAME='LBL_XUN_END_HANTS' ,XOFFSET=138 ,YOFFSET=8 ,SCR_XSIZE=17  $
      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='Ѯ:')


  DST_XUN_END_HANTS = WIDGET_DROPLIST(BASE_DATE_END_HANTS,  $
      UNAME='DST_XUN_END_HANTS' ,XOFFSET=159 ,YOFFSET=4 ,SCR_XSIZE=35  $
      ,SCR_YSIZE=20 ,EVENT_FUNC='DST_XUN_END_HANTS')


  BASE_CMD_HANTS = WIDGET_BASE(BASE_INPUT_HANTS,  $
      UNAME='BASE_CMD_HANTS' ,FRAME=1 ,XOFFSET=6 ,YOFFSET=322  $
      ,SCR_XSIZE=217 ,SCR_YSIZE=38 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  CMD_OK_HANTS = WIDGET_BUTTON(BASE_CMD_HANTS, UNAME='CMD_OK_HANTS'  $
      ,XOFFSET=7 ,YOFFSET=6 ,SCR_XSIZE=60 ,SCR_YSIZE=24  $
      ,EVENT_FUNC='CMD_OK_HANTS' ,/ALIGN_CENTER ,VALUE='�����ع�')


  CMD_CLOSE_HANTS = WIDGET_BUTTON(BASE_CMD_HANTS,  $
      UNAME='CMD_CLOSE_HANTS' ,XOFFSET=149 ,YOFFSET=6 ,SCR_XSIZE=60  $
      ,SCR_YSIZE=24 ,EVENT_FUNC='CMD_CLOSE_HANTS' ,/ALIGN_CENTER  $
      ,VALUE='�ر�')


  CMD_DB_WRITE_HANTS = WIDGET_BUTTON(BASE_CMD_HANTS,  $
      UNAME='CMD_DB_WRITE_HANTS' ,XOFFSET=80 ,YOFFSET=6 ,SCR_XSIZE=56  $
      ,SCR_YSIZE=24 ,EVENT_FUNC='CMD_DB_WRITE_HANTS' ,/ALIGN_CENTER  $
      ,VALUE='����װ��')


  BASE_LAND_TYPE_HANTS = WIDGET_BASE(BASE_INPUT_HANTS,  $
      UNAME='BASE_LAND_TYPE_HANTS' ,FRAME=1 ,XOFFSET=6 ,YOFFSET=131  $
      ,SCR_XSIZE=217 ,SCR_YSIZE=31 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  LBL_LAND_TYPE_HANTS = WIDGET_LABEL(BASE_LAND_TYPE_HANTS,  $
      UNAME='LBL_LAND_TYPE_HANTS' ,XOFFSET=8 ,YOFFSET=7 ,SCR_XSIZE=66  $
      ,SCR_YSIZE=13 ,/ALIGN_LEFT ,VALUE='��������:')


  DST_LAND_TYPE_HANTS = WIDGET_DROPLIST(BASE_LAND_TYPE_HANTS,  $
      UNAME='DST_LAND_TYPE_HANTS' ,XOFFSET=79 ,YOFFSET=4  $
      ,SCR_XSIZE=105 ,SCR_YSIZE=23 ,EVENT_FUNC='DST_LAND_TYPE_HANTS')


  BASE_PARAMETER_HANTS = WIDGET_BASE(BASE_INPUT_HANTS,  $
      UNAME='BASE_PARAMETER_HANTS' ,FRAME=1 ,XOFFSET=6 ,YOFFSET=172  $
      ,SCR_XSIZE=217 ,SCR_YSIZE=60 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  LBL_MENXIAN_HANTS = WIDGET_LABEL(BASE_PARAMETER_HANTS,  $
      UNAME='LBL_MENXIAN_HANTS' ,XOFFSET=8 ,YOFFSET=9 ,SCR_XSIZE=34  $
      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='����:')


  TXT_MENXIAN_HANTS = WIDGET_TEXT(BASE_PARAMETER_HANTS,  $
      UNAME='TXT_MENXIAN_HANTS' ,XOFFSET=45 ,YOFFSET=5 ,SCR_XSIZE=51  $
      ,SCR_YSIZE=20 ,VALUE=[ '-5' ] ,XSIZE=20 ,YSIZE=1,/EDITABLE)


  TXT_TOLERANCE_HANTS = WIDGET_TEXT(BASE_PARAMETER_HANTS,  $
      UNAME='TXT_TOLERANCE_HANTS' ,XOFFSET=146 ,YOFFSET=5 ,/EDITABLE $
      ,SCR_XSIZE=51 ,SCR_YSIZE=20 ,VALUE=[ '5' ] ,XSIZE=20 ,YSIZE=1)


  LBL_TOLERANCE_HANTS = WIDGET_LABEL(BASE_PARAMETER_HANTS,  $
      UNAME='LBL_TOLERANCE_HANTS' ,XOFFSET=109 ,YOFFSET=9  $
      ,SCR_XSIZE=34 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='����:')


  TXT_DOD_HANTS = WIDGET_TEXT(BASE_PARAMETER_HANTS,  $
      UNAME='TXT_DOD_HANTS' ,XOFFSET=45 ,YOFFSET=33 ,SCR_XSIZE=51  $
      ,SCR_YSIZE=20 ,VALUE=[ '2' ] ,XSIZE=20 ,YSIZE=1,/EDITABLE)


  LBL_DOD_HANTS = WIDGET_LABEL(BASE_PARAMETER_HANTS,  $
      UNAME='LBL_DOD_HANTS' ,XOFFSET=8 ,YOFFSET=37 ,SCR_XSIZE=34  $
      ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='DOD :')


  TXT_FREQUENCY_HANTS = WIDGET_TEXT(BASE_PARAMETER_HANTS,  $
      UNAME='TXT_FREQUENCY_HANTS' ,XOFFSET=146 ,YOFFSET=32 ,/EDITABLE $
      ,SCR_XSIZE=51 ,SCR_YSIZE=20 ,VALUE=[ '3' ] ,XSIZE=20 ,YSIZE=1)


  LBL_FREQUENCY_HANTS = WIDGET_LABEL(BASE_PARAMETER_HANTS,  $
      UNAME='LBL_FREQUENCY_HANTS' ,XOFFSET=109 ,YOFFSET=36  $
      ,SCR_XSIZE=34 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='Ƶ��:')


  BASE_PLACE_HANTS = WIDGET_BASE(BASE_INPUT_HANTS,  $
      UNAME='BASE_PLACE_HANTS' ,FRAME=1 ,XOFFSET=6 ,YOFFSET=249  $
      ,SCR_XSIZE=217 ,SCR_YSIZE=59 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  LBL_COUNTRY_HANTS = WIDGET_LABEL(BASE_PLACE_HANTS,  $
      UNAME='LBL_COUNTRY_HANTS' ,XOFFSET=8 ,YOFFSET=8 ,SCR_XSIZE=34  $
      ,SCR_YSIZE=13 ,/ALIGN_LEFT ,VALUE='����:')


  DST_COUNTRY_HANTS = WIDGET_DROPLIST(BASE_PLACE_HANTS,  $
      UNAME='DST_COUNTRY_HANTS' ,XOFFSET=43 ,YOFFSET=4 ,SCR_XSIZE=60  $
      ,SCR_YSIZE=23 ,EVENT_FUNC='DST_COUNTRY_HANTS')


  DST_PROVINCE_HANTS = WIDGET_DROPLIST(BASE_PLACE_HANTS,  $
      UNAME='DST_PROVINCE_HANTS' ,XOFFSET=140 ,YOFFSET=4  $
      ,SCR_XSIZE=66 ,SCR_YSIZE=23 ,EVENT_FUNC='DST_PROVINCE_HANTS')


  LBL_PROVINCE_HANTS = WIDGET_LABEL(BASE_PLACE_HANTS,  $
      UNAME='LBL_PROVINCE_HANTS' ,XOFFSET=116 ,YOFFSET=8  $
      ,SCR_XSIZE=22 ,SCR_YSIZE=13 ,/ALIGN_LEFT ,VALUE='ʡ:')


  DST_COUNTY_HANTS = WIDGET_DROPLIST(BASE_PLACE_HANTS,  $
      UNAME='DST_COUNTY_HANTS' ,XOFFSET=43 ,YOFFSET=32 ,SCR_XSIZE=120  $
      ,SCR_YSIZE=23 ,EVENT_FUNC='DST_COUNTY_HANTS')


  LBL_COUNTY_HANTS = WIDGET_LABEL(BASE_PLACE_HANTS,  $
      UNAME='LBL_COUNTY_HANTS' ,XOFFSET=8 ,YOFFSET=36 ,SCR_XSIZE=34  $
      ,SCR_YSIZE=13 ,/ALIGN_LEFT ,VALUE='��  :')


  BASE_DRAW_HANTS = WIDGET_BASE(BASE_TOP_HANTS,  $
      UNAME='BASE_DRAW_HANTS' ,FRAME=1 ,XOFFSET=248 ,YOFFSET=8  $
      ,SCR_XSIZE=347 ,SCR_YSIZE=229 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  LBL_DRAW_HANTS = WIDGET_LABEL(BASE_DRAW_HANTS,  $
      UNAME='LBL_DRAW_HANTS' ,XOFFSET=117 ,YOFFSET=2 ,SCR_XSIZE=128  $
      ,SCR_YSIZE=14 ,/ALIGN_LEFT ,VALUE='������Ͻ����ʾ')


  DRAW_HANTS = WIDGET_DRAW(BASE_DRAW_HANTS, UNAME='DRAW_HANTS'  $
      ,FRAME=1 ,YOFFSET=17 ,SCR_XSIZE=346 ,SCR_YSIZE=210)

  BASE_TABLE_HANTS = WIDGET_BASE(BASE_TOP_HANTS,  $
      UNAME='BASE_TABLE_HANTS' ,FRAME=1 ,XOFFSET=248 ,YOFFSET=238  $
      ,SCR_XSIZE=347 ,SCR_YSIZE=147 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  TABLE_RESULT_HANTS = WIDGET_TABLE(BASE_TABLE_HANTS,  $
      UNAME='TABLE_RESULT_HANTS' ,SCR_XSIZE=346 ,SCR_YSIZE=145  $
       ,EVENT_PRO='TABLE_RESULT_HANTS0',XSIZE=6 ,YSIZE=6,/ALL_EVENTS)

  ;F=0
  STATE = { $
  		F	:	F	,$

        TXT_TOLERANCE_HANTS	:	TXT_TOLERANCE_HANTS	, $
        TXT_MENXIAN_HANTS	:	TXT_MENXIAN_HANTS	, $
        TXT_DOD_HANTS		:	TXT_DOD_HANTS	, $
        TXT_FREQUENCY_HANTS	:	TXT_FREQUENCY_HANTS	, $

        ARR_COUNTRY				:	STRARR(70)		,$
        ARR_COUNTRY_CODE		:	STRARR(70)		,$
        ARR_PROVINCE			:	STRARR(60)		,$;ȡ60����Ϊ��������ʮ�����
        ARR_PROVINCE_CODE		:	STRARR(60)		,$
        ARR_COUNTY				:	STRARR(200)		,$
        ARR_COUNTY_CODE			:	STRARR(200)		,$

		COUNTRY				:	'��ѡ��'		,$
        COUNTY				:	''		,$
        PROVINCE			:	''		,$

        INDEX_COUNTRY			:	0		, $
		INDEX_PROVINCE			:	0		, $
		INDEX_COUNTY			:	0		, $

        ARR_YEAR		:	[ '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998','1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006',$
        						'2007', '2008', '2009', '2010', '2011', '2012', '2013'] ,$
		ARR_MONTH		:	[ '1', '2', '3','4', '5', '6', '7', '8', '9', '10', '11', '12' ],$
		ARR_XUN			:	['1','2','3'] ,$
		ARR_LAND_TYPE	:	['����','����','ˮ��','����'],$

		LAND_TYPE		:	'����',$
		YEAR_START			:	1990	,$
		MONTH_START			:	1		,$
		XUN_START			:	1		,$
		YEAR_END			:	1990	,$
		MONTH_END			:	1		,$
		XUN_END				:	1		,$

        NUM_OF_COUNTRY			:	0		,$
        NUM_OF_PROVINCE			:	0		,$
        NUM_OF_COUNTY			:	0		,$


		PLACE					:	0		, $
		;(1)�й����ص�����		PLACE=1
     	;(2)�й�������������ȫ�������� 	PLACE=2
     	;(3)�������ݵ�����		PLACE=3

        HANTS_DONE				:	0		, $
        NI						:	0		, $
        ARR_HANTS				:	FLTARR(36),	$
        PROVINCE_CODE			:	''		, $
        COUNTRY_CODE			:	''		, $
        COUNTY_CODE				:	''		,$

		BASE_TOP_HANTS			:	BASE_TOP_HANTS		,$
		DRAW_HANTS				:	DRAW_HANTS			,$
		TABLE_RESULT_HANTS		:	TABLE_RESULT_HANTS	,$
		DST_YEAR_START_HANTS	:	DST_YEAR_START_HANTS,$
		DST_YEAR_END_HANTS		:	DST_YEAR_END_HANTS	,$
		DST_COUNTRY_HANTS		:	DST_COUNTRY_HANTS	,$
		DST_PROVINCE_HANTS		:	DST_PROVINCE_HANTS	,$
		DST_COUNTY_HANTS		:	DST_COUNTY_HANTS	,$

		DBCO_ID					:	(*F).DBCO_ID	, $
       	DBCO					:	OBJ_NEW('IDLDBDATABASE')		$
        }

    PSTATE = PTR_NEW(STATE, /NO_COPY)
    WIDGET_CONTROL, BASE_TOP_HANTS, SET_UVALUE=PSTATE
   	;************************************************************************
  	;�������б�ֵ,�е�������Ҫ�����ݿ��ж�ȡ
  	(*PSTATE).DBCO=(*F).DBCO
  	WIDGET_CONTROL,DST_YEAR_END_HANTS	,SET_VALUE=(*PSTATE).ARR_YEAR
  	WIDGET_CONTROL,DST_YEAR_START_HANTS	,SET_VALUE=(*PSTATE).ARR_YEAR
  	WIDGET_CONTROL,DST_MONTH_END_HANTS	,SET_VALUE=(*PSTATE).ARR_MONTH
  	WIDGET_CONTROL,DST_MONTH_START_HANTS,SET_VALUE=(*PSTATE).ARR_MONTH
  	WIDGET_CONTROL,DST_XUN_END_HANTS	,SET_VALUE=(*PSTATE).ARR_XUN
  	WIDGET_CONTROL,DST_XUN_START_HANTS	,SET_VALUE=(*PSTATE).ARR_XUN
  	WIDGET_CONTROL,DST_LAND_TYPE_HANTS	,SET_VALUE=(*PSTATE).ARR_LAND_TYPE

  	WIDGET_CONTROL,(*pstate).DST_COUNTRY_HANTS,SET_VALUE=['�й�']

  	WIDGET_CONTROL, /REALIZE, BASE_TOP_HANTS
  	;************************************************************************
  	;��ʡ�����ƶ�����
  	oRS = OBJ_NEW('IDLdbRecordset', (*PSTATE).DBCO, TABLE='crop.PROVINCE_CODE')
	;�����ʡ����ʼ�ĸ�����1,������0
	;������Ϊ��һ��ѡ����'��ѡ��'
	num_of_PROVINCE=1

	IF(oRS->MoveCursor(/FIRST) EQ 1)THEN BEGIN

		;���ҳ�ʡ�ĸ���
		WHILE (oRS->MoveCursor(/next) eq 1) DO BEGIN
			num_of_PROVINCE=num_of_PROVINCE+1
		ENDWHILE

		;Ȼ�󽫹��ҵ����ƶ�������
		ARR_PROVINCE=STRARR(NUM_OF_PROVINCE+1+1)
		ARR_PROVINCE_CODE=STRARR(NUM_OF_PROVINCE+1+1)
		TEMP=oRS->MoveCursor(/FIRST)
		COUNT=0
		ARR_PROVINCE[COUNT]='��ѡ��'
		COUNT=COUNT+1
		ARR_PROVINCE[COUNT]='ȫ��'
		COUNT=COUNT+1
		ARR_PROVINCE[COUNT]=ORS->GetField(1)
		ARR_PROVINCE_CODE[COUNT]=ORS->GetField(0)

		WHILE (oRS->MoveCursor(/next) eq 1) DO BEGIN
			COUNT=COUNT+1
			ARR_PROVINCE[COUNT]=ORS->GetField(1)
			ARR_PROVINCE_CODE[COUNT]=ORS->GetField(0)
		ENDWHILE
		WIDGET_CONTROL,(*pstate).DST_PROVINCE_HANTS,SET_VALUE=ARR_PROVINCE
		(*PSTATE).ARR_PROVINCE=ARR_PROVINCE
		(*PSTATE).ARR_PROVINCE_CODE=ARR_PROVINCE_CODE
		(*PSTATE).NUM_OF_PROVINCE=num_of_PROVINCE + 2
	ENDIF

  	;************************************************************************

  	XMANAGER, 'BASE_TOP_HANTS', BASE_TOP_HANTS, /NO_BLOCK
  	white=!D.N_COLORS-1
  	WIDGET_CONTROL,DRAW_HANTS,GET_VALUE=TMEP
	WSET,TMEP
	TEMP=INDGEN(2)
	PLOT,TEMP,BACKGROUND=WHITE

END
;
; EMPTY STUB PROCEDURE USED FOR AUTOLOADING.
;
PRO HANTS, F,GROUP_LEADER=WGROUP
  BASE_TOP_HANTS, F,GROUP_LEADER=WGROUP
END
