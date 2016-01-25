;̫���춥�ǡ���λ�ǡ�̫������Ǽ������
;2009��12��24��
;2010��1��12������޸ı�ԭ���������8�����ҵ��ٶ�,�����˷ֿ鴦��ķ������������ݴﵽ�ܴ�(���Դ���һ��HJ��CCD����)��
;ֻҪ���ζ�������ݲ������ڴ��������Ƽ���������

;ť����

;���ǵ���̡߳�������������أ��ܾ�ȷ����������ĳһʱ�̶�Ӧ��̫���춥�Ǻ�̫����λ��
;ʱ���ȴ�-2000��6000��(һ���ֻ�ܼ���1950��2050��)
;���㾫�ȸߣ���������0.0003��(һ��ļ��㷽�������ڡ�0.01��)
;1620��2014��(�ر���1955��2014��)�ľ�����Ը���һЩ����Ҫ�����о�ȷ������ʱ����ѧʱ��ֵ�Ĺ۲�����

;�ο����ף�
;[1]Solar Position Algorithm for Solar Radiation Applications,National Renewable Energy Laboratory
;[2]Slope Images & Aspect Images, Field Guide, Erdas

;��������
FUNCTION COMMON_BLOCK_PARA

	COMMON PARA,PI

	PI=3.1415926535898D
;����������(Earth Periodic Terms)

;��ȡLֵ
	L_INPUT=PTR_NEW(dblarr(3,129,/NOZERO),/NO_COPY)
	OPENR,LUN,'PARA\L.txt',/GET_LUN
	readf,lun,*L_INPUT
	free_lun,lun

	L0=PTR_NEW((*L_INPUT)[*,0:63],/NO_COPY)
	L1=PTR_NEW((*L_INPUT)[*,64:97],/NO_COPY)
	L2=PTR_NEW((*L_INPUT)[*,98:117],/NO_COPY)
	L3=PTR_NEW((*L_INPUT)[*,118:124],/NO_COPY)
	L4=PTR_NEW((*L_INPUT)[*,125:127],/NO_COPY)
	L5=PTR_NEW((*L_INPUT)[*,128],/NO_COPY)

	PTR_FREE,L_INPUT

;��ȡBֵ
	B_INPUT=PTR_NEW(dblarr(3,7,/NOZERO),/NO_COPY)
	OPENR,LUN,'PARA\B.txt',/GET_LUN
	readf,lun,*B_INPUT
	free_lun,lun

	B0=PTR_NEW((*B_INPUT)[*,0:4],/NO_COPY)
	B1=PTR_NEW((*B_INPUT)[*,5:6],/NO_COPY)

	PTR_FREE,B_INPUT
;��ȡRֵ
	R_INPUT=PTR_NEW(dblarr(3,59,/NOZERO),/NO_COPY)
	OPENR,LUN,'PARA\R.txt',/GET_LUN
	readf,lun,*R_INPUT
	free_lun,lun

	R0=PTR_NEW((*R_INPUT)[*,0:39],/NO_COPY)
	R1=PTR_NEW((*R_INPUT)[*,40:49],/NO_COPY)
	R2=PTR_NEW((*R_INPUT)[*,50:55],/NO_COPY)
	R3=PTR_NEW((*R_INPUT)[*,56:57],/NO_COPY)
	R4=PTR_NEW((*R_INPUT)[*,58],/NO_COPY)

	PTR_FREE,R_INPUT

	Y=PTR_NEW(dblarr(5,63,/NOZERO),/NO_COPY)
	OPENR,LUN,'PARA\Y.txt',/GET_LUN
	readf,lun,*Y
	free_lun,lun

	PE=PTR_NEW(dblarr(4,63,/NOZERO),/NO_COPY)
	OPENR,LUN,'PARA\PE.txt',/GET_LUN
	readf,lun,*PE
	free_lun,lun

	para_a=PTR_NEW((*PE)[0,*],/NO_COPY)
	para_b=PTR_NEW((*PE)[1,*],/NO_COPY)
	para_c=PTR_NEW((*PE)[2,*],/NO_COPY)
	para_d=PTR_NEW((*PE)[3,*],/NO_COPY)

	PTR_FREE,PE

	return,{L0:L0,L1:L1,L2:L2,L3:L3,L4:L4,L5:L5,B0:B0,B1:B1,R0:R0,R1:R1,R2:R2,R3:R3,R4:R4 $
			,Y:Y,para_a:para_a,para_b:para_b,para_c:para_c,para_d:para_d}

END

;���ݷָ�
Function DATA_SPLIT,data,inc,block,split_temp,split_name

	DIMS=size((*data),/dimensions)

	if n_elements(DIMS) eq 3 then begin
		col=DIMS[1]
		row=DIMS[2]
	endif else begin
		col=DIMS[0]
		row=DIMS[1]
	endelse

	x=floor(col/block)
	if (col mod block) ne 0 then x+=1
	y=floor(row/block)
	if (row mod block) ne 0 then y+=1

	for i=0,x-1,1 do begin
		if col gt block then begin
			case i of
				0 : 	begin
							temp_col=PTR_NEW((*data)[0:(block+inc-1),*],/NO_COPY)
;							data=PTR_NEW((*data)[block-inc:*,*],/NO_COPY)
					 	end
				x-1 : begin
							temp_col=PTR_NEW((*data)[i*block-inc:*,*],/NO_COPY)
						end
				else : begin
							temp_col=PTR_NEW((*data)[i*block-inc:((i+1)*block+inc-1),*],/NO_COPY)
;							*data=(*data)[block:*,*]
						 end
			endcase
		endif else begin
			temp_col=PTR_NEW((*data),/NO_COPY)
		endelse

		for j=0,y-1,1 do begin
			if row gt block then begin
				case j of
					0	: begin
							temp=PTR_NEW((*temp_col)[*,0:(block+inc-1)],/NO_COPY)
							*temp_col=(*temp_col)[*,(block-inc):*]
						end
					y-1 : begin
							temp=PTR_NEW((*temp_col)[*,0:*],/NO_COPY)
						end
					else : begin
							temp=PTR_NEW((*temp_col)[*,0:(block+2*inc-1)],/NO_COPY)
							*temp_col=(*temp_col)[*,block:*]
						end
				endcase
			endif else begin
				temp=PTR_NEW((*temp_col),/NO_COPY)
			endelse

;			print,size(*temp,/dimensions)
			temp_name=split_temp+split_name+strtrim(string(i),2)+'_'+strtrim(string(j),2)+'.temp'

			SAVE,temp,FILENAME=temp_name
			GC_STR={temp:temp}
			HEAP_FREE,GC_STR
		endfor
		PTR_FREE,temp_col
	endfor

	PTR_FREE,data
;	help,data,/structure

	return,{x:x,y:y,col:col,row:row}

END

Function DATA_MERGE,x,y,col,row,inc,block,merge_temp,merge_name,index
	new_data=fltarr(col,row,/NOZERO)
	temp_size=0
	c=0
	for i=0,x-1,1 do begin
		r=0
		for j=0,y-1,1 do begin
			temp_name=merge_temp+merge_name+strtrim(string(i),2)+'_'+strtrim(string(j),2)+'.temp'
			RESTORE,temp_name
			FILE_DELETE,temp_name,/NOEXPAND_PATH
			if block lt col then begin
				if i eq 0 then *SOLAR_POSITION=(*SOLAR_POSITION)[0:(block-1),*]
				if i eq x-1 then *SOLAR_POSITION=(*SOLAR_POSITION)[inc:*,*]
				if (i ne 0)and(i ne x-1) then *SOLAR_POSITION=(*SOLAR_POSITION)[inc:(block+inc-1),*]
			endif

			if block lt row then begin
				if j eq 0 then *SOLAR_POSITION=(*SOLAR_POSITION)[*,0:(block-1)]
				if j eq y-1 then *SOLAR_POSITION=(*SOLAR_POSITION)[*,inc:*]
				if (j ne 0)and(j ne y-1) then *SOLAR_POSITION=(*SOLAR_POSITION)[*,inc:(block+inc-1)]
			endif

			temp_size=size(*SOLAR_POSITION,/dimensions)
			new_data[c:(c+temp_size[0]-1),r:r+temp_size[1]-1]=*SOLAR_POSITION
			PTR_FREE,SOLAR_POSITION
			r+=temp_size[1]
		endfor
		c=c+temp_size[0]
	endfor

	return,new_data
END

;��������ʱ��
FUNCTION COST_TIME_CAL,START_TIME,END_TIME

	S_TIME_ARR=strsplit(START_TIME,': ',/EXTRACT)
	E_TIME_ARR=strsplit(END_TIME,': ',/EXTRACT)

	S_TIME=S_TIME_ARR[3]*60*60+S_TIME_ARR[4]*60+S_TIME_ARR[5]
	E_TIME=E_TIME_ARR[3]*60*60+E_TIME_ARR[4]*60+E_TIME_ARR[5]

	C_TIME=E_TIME-S_TIME

	C_HOUR=floor(C_TIME/3600)
	C_MINUTE=floor((C_TIME mod 3600)/60)
	C_SECOND=(C_TIME mod 3600) mod 60

	return,{COST_TIME, HOUR:C_HOUR,MINUTE:C_MINUTE,SECOND:C_SECOND}
END

;�޶��Ƕ���0��360�ķ�Χ
FUNCTION LIMIT_DEGREES360,DEGREES
;��������Ƕ�(DEGREES)
;��������޶���0��360��Χ�ڵĽǶ�

	*DEGREES=360.0*((*DEGREES)/360.0-floor(temporary(*DEGREES)/360.0))

;	if DEGREES lt 0.0 then DEGREES += 360
	index=where(*DEGREES lt 0.0)
	if index ne -1 then *DEGREES[index]+=360
	return,DEGREES
END

;��������ʱ����ѧʱ�Ĳ�ֵ
FUNCTION DELTA_T_CAL,YEAR

	if (YEAR ge 1620) and (YEAR le 2014) then begin

		deltaT_tab=dblarr(2,395,/NOZERO)

		OPENR,LUN,'PARA\delta_T.txt',/GET_LUN
		readf,lun,deltaT_tab
		free_lun,lun

		index=YEAR-1620

;		if YEAR le 1955.5 then begin
;
;			u=(YEAR-1955.5)/100.0
;			delta_T=deltaT_tab[index]-0.91072*(n+26.0)*u^2
;		endif else begin
			delta_T=deltaT_tab[1,index]
	endif else begin
		T=(YEAR-2000)/100.0
		delta_T=102.3+123.5*T+32.5*T^2
	endelse

	return,delta_T
END

;�����¶ȡ�����
FUNCTION SLO_ASP_CAL,DEM,PIXEL_SIZE

	DEM_SIZE=size(*DEM,/dimensions)

	delta_XI=fltarr(DEM_SIZE[0],DEM_SIZE[1],/NOZERO)
	delta_YI=fltarr(DEM_SIZE[0],DEM_SIZE[1],/NOZERO)

	delta_XI[0,*]=(*DEM)[1,*]-(*DEM)[0,*]
	delta_XI[DEM_SIZE[0]-1,*]=(*DEM)[DEM_SIZE[0]-1,*]-(*DEM)[DEM_SIZE[0]-2,*]
	for i=1,DEM_SIZE[0]-2,1 do begin
		delta_XI[i,*]=(*DEM)[i+1,*]-(*DEM)[i-1,*]
	endfor

	delta_YI[*,0]=(*DEM)[*,0]-(*DEM)[*,1]
	delta_YI[*,DEM_SIZE[1]-1]=(*DEM)[*,DEM_SIZE[1]-2]-(*DEM)[*,DEM_SIZE[1]-1]
	for j=1,DEM_SIZE[1]-2,1 do begin
		delta_YI[*,j]=(*DEM)[*,j-1]-(*DEM)[*,j+1]
	endfor

;	PTR_FREE,DEM

	delta_XI=PTR_NEW(temporary(delta_XI),/NO_COPY)
	delta_YI=PTR_NEW(temporary(delta_YI),/NO_COPY)

	delta_X=fltarr(DEM_SIZE[0],DEM_SIZE[1],/NOZERO)
	delta_Y=fltarr(DEM_SIZE[0],DEM_SIZE[1],/NOZERO)

	delta_X[0,*]=((*delta_XI)[0,*]+(*delta_XI)[1,*])/(2*PIXEL_SIZE)
	delta_X[DEM_SIZE[0]-1,*]=((*delta_XI)[DEM_SIZE[0]-1,*]+(*delta_XI)[DEM_SIZE[0]-2,*])/(2*PIXEL_SIZE)
	for i=1,DEM_SIZE[0]-2,1 do begin
		delta_X[i,*]=((*delta_XI)[i-1,*]+(*delta_XI)[i,*]+(*delta_XI)[i+1,*])/(3*PIXEL_SIZE)
	endfor

	delta_Y[*,0]=((*delta_YI)[*,0]+(*delta_YI)[*,1])/(2*PIXEL_SIZE)
	delta_Y[*,DEM_SIZE[1]-1]=((*delta_YI)[*,DEM_SIZE[1]-1]+(*delta_YI)[*,DEM_SIZE[1]-2])/(2*PIXEL_SIZE)
	for j=1,DEM_SIZE[1]-2,1 do begin
		delta_Y[*,j]=((*delta_YI)[*,j-1]+(*delta_YI)[*,j]+(*delta_YI)[*,j+1])/(3*PIXEL_SIZE)
	endfor

	PTR_FREE,delta_XI,delta_YI

	delta_X=PTR_NEW(temporary(delta_X),/NO_COPY)
	delta_Y=PTR_NEW(temporary(delta_Y),/NO_COPY)


	S=sqrt((*delta_X)^2+(*delta_Y)^2)

	SLO=atan(temporary(S),2)*180.0/!PI

	SLOPE=PTR_NEW(temporary(SLO),/NO_COPY)

	ASP=atan((*delta_X),(*delta_Y))*180/!PI+180.0

	PTR_FREE,delta_X,delta_Y

;������תΪ���淽λ��ת�ǣ������Ͽ�ʼ��Ϊ��������Ϊ��
	ASP = 180.0 - temporary(ASP)

	ASPECT=PTR_NEW(temporary(ASP),/NO_COPY)

	return,{SA, SLOPE:SLOPE,ASPECT:ASPECT}
END

;�޶������е�ֵ��-PI��PI֮��
;FUNCTION ATAN2,Y,X
;
;	COMMON PARA,PI
;
;	if X ne 0 then begin
;		result=2*atan(Y/(sqrt(X^2+Y^2)+X))
;	endif else begin
;		result=(PI/2)*(Y/abs(Y))
;	endelse
;
;	return,result
;END

;����������
FUNCTION JD_CAL,Y,M,D,H,Mi,S
;�������Y(��),M(��),D(��),H(ʱ),Mi(��),S(��)
;�������������

	; ��ȡ��������ĸ���
	NP = n_params()

	; ����Ĭ�ϵ�ʱ���֡���
	d_Second = 0d
	d_Minute = 0d
	d_Hour = 0d

	SWITCH NP OF
		6:	d_Second = S
		5: d_Minute = Mi+d_Second/60.0d
		4: d_Hour = H+d_Minute/60.0d
		3: BEGIN
			d_Y = double(Y)
			d_M = double(M)
			d_D = D+d_Hour/24.0d
			break
			END
		0: RETURN,SYSTIME(/JULIAN);δ�������ʱ�򷴻�ϵͳ��ǰʱ���������
		ELSE: BEGIN
			;;print,'Incorrect number of arguments.'
			RETURN,-999999.9
			END
	ENDSWITCH

	A=floor(d_Y/100)
	B=2-A+floor(A/4)

	if d_M le 2 then begin
		d_Y=d_Y-1
		d_M=d_M+12
	endif

	Julian_Day=double(floor(365.25*(d_Y+4716))+floor(30.6001*(d_M+1))+d_D-1524.5d)

	if Julian_Day gt 2299160.0 then Julian_Day+=B

	return,Julian_Day

END

;�������ľ�γ�Ⱥ͵���ʸ���뾶
FUNCTION HEL_CAL,HEL,JME,L0,L1,L2,L3,L4,L5,B0,B1,R0,R1,R2,R3,R4
;����������������������HEL)����������ǧ��JME
;������������������

	COMMON PARA,PI

;�����������ȷ���������ľ���(L)������γ��(B)���뾶ʸ��(R)
	switch HEL of
		'L':	begin
				STACK=ptrarr(6)
				STACK[0]=PTR_NEW(*L0,/NO_COPY)
				STACK[1]=PTR_NEW(*L1,/NO_COPY)
				STACK[2]=PTR_NEW(*L2,/NO_COPY)
				STACK[3]=PTR_NEW(*L3,/NO_COPY)
				STACK[4]=PTR_NEW(*L4,/NO_COPY)
				STACK[5]=PTR_NEW(*L5,/NO_COPY)
				break
				end
		'B':	begin
				STACK=ptrarr(2)
				STACK[0]=PTR_NEW(*B0,/NO_COPY)
				STACK[1]=PTR_NEW(*B1,/NO_COPY)
				break
				end
		'R':	begin
				STACK=ptrarr(5)
				STACK[0]=PTR_NEW(*R0,/NO_COPY)
				STACK[1]=PTR_NEW(*R1,/NO_COPY)
				STACK[2]=PTR_NEW(*R2,/NO_COPY)
				STACK[3]=PTR_NEW(*R3,/NO_COPY)
				STACK[4]=PTR_NEW(*R4,/NO_COPY)
				break
				end
		else:	;;print,'ERROR!'
	endswitch

;������������
	HEL_ARR=dblarr(n_elements(STACK),/NOZERO)

;����������ÿ������ֵ
	for i=0,n_elements(STACK)-1,1 do begin
		STACK_TEMP=DOUBLE(*STACK[i])
		TEMP_HEL=double(STACK_TEMP[0,*]*cos(STACK_TEMP[1,*]+STACK_TEMP[2,*]*JME))
		HEL_ARR[i]=TOTAL(TEMP_HEL)
	endfor
	PTR_FREE,STACK
;��ӡ��������
	for i=0,n_elements(HEL_ARR)-1,1 do begin
		out_text='"'+HEL+strtrim(string(i),2)+'"'
		;;print,HEL_ARR[i],format='('+out_text+',d21.7)'
	endfor

;�������Ĳ���
	HEL_PARA=HEL_ARR[0]
	for i=1,n_elements(HEL_ARR)-1,1 do begin
		HEL_PARA+=HEL_ARR[i]*JME^i
	endfor
	HEL_PARA=HEL_PARA/10e7

;����ǵ���ʸ���뾶��ֱ�ӷ�����ֵ
	if HEL eq 'R' then return,PTR_NEW(temporary(HEL_PARA))

;����ת�Ƕ�
	HEL_PARA=HEL_PARA*180/PI

	if HEL eq 'B' then return,PTR_NEW(temporary(HEL_PARA))

	HEL_PARA=PTR_NEW(temporary(HEL_PARA))
	HEL_PARA=LIMIT_DEGREES360(HEL_PARA)

	return,HEL_PARA

END

;���㾭�Ⱥ�����¶�
FUNCTION NUTATION_LON_OBL,JCE,Y,a,b,c,d

	COMMON PARA,PI

;��������ƽ��������(X0,��)
	X0=297.85036D + 445267.111480D * JCE - 0.0019142D * JCE^2 + (JCE^3) / 189474D

;������(��)ƽ��ƽ�����(X1,��)
	X1=357.52772D + 35999.050340D * JCE -0.0001603D * JCE^2 - (JCE^3) / 300000D

;��������ƽ��ƽ�����(X2,��)
	X2=134.96298D + 477198.867398D * JCE + 0.0086972D * JCE^2 + (JCE^3) / 56250D

;��������γ�ȷ���(X3,��)
	X3=93.27191D + 483202.017538D * JCE - 0.0036825D * JCE^2 + (JCE^3) / 327270D

;����Ƶ�ƽ��������ƽ����������㾭��
	X4=125.04452D - 1934.136261D * JCE + 0.0020708D * JCE^2 + (JCE^3) / 450000D

	X=TRANSPOSE([X0,X1,X2,X3,X4])

	term_size=size(*Y,/dimensions)

	temp_cal=X[0]*(*Y)[0,*]*PI/180
	for i=1,term_size[0]-1,1 do begin
		temp_cal+=X[i]*(*Y)[i,*]*PI/180
	endfor

;���㾭���¶�������(��λ����)
	term_PSI=((*a)+(*b)*JCE)*sin(temp_cal)

;��������¶�������(��λ����)
	term_EPSILON=((*c)+(*d)*JCE)*cos(temp_cal)

;���㾭���¶�
	delta_PSI=total(term_PSI)/36000000d

;��������¶�
	delta_EPSILON=total(term_EPSILON)/36000000d

	delta_term={delta_PSI:delta_PSI,delta_EPSILON:delta_EPSILON}
	return,delta_term
END

;������ʵ�Ƶ����
FUNCTION EPSILON_CAL,JME,delta_EPSILON

	U=JME/10.0D

;����ƽ���Ƶ����
	EP_MEAN=84381.448d -4680.93*U-1.55*U^2+ $
			1999.25*U^3-51.38*U^4-249.67*U^5- $
			39.05*U^6+7.12*U^7+27.87*U^8+5.79*U^9+ $
			2.45*U^10

;������ʵ�Ƶ����
	EP=(EP_MEAN/3600.0D)+delta_EPSILON

	return,EP
END

;�������У��
FUNCTION ABERRATION_CORRECTION,R

	delta_TAU=-20.4898d/(3600d*R)
	return,delta_TAU
END

;����������������ʱ
FUNCTION ASTG_CAL,JD,JC,delta_PSI,EPSILON

	COMMON PARA,PI
;����ƽ���������κ���ʱ
	NU_MEAN=280.46061837D + 360.98564736629D * (JD - 2451545D) + $
			0.000387933 * JC^2 - (JC^3)/38710000D

	NU_MEAN=PTR_NEW(temporary(NU_MEAN))
	NU_MEAN=LIMIT_DEGREES360(NU_MEAN)

;������������κ���ʱ
	NU=(*NU_MEAN) + delta_PSI * cos(EPSILON*PI/180)

	return,NU
END

;�������̫���ྭ
FUNCTION GSRS_CAL,LAMDA,EPSILON,BETA

	COMMON PARA,PI

;����ķ�����
	ALPHA_NUMER=sin(LAMDA*PI/180.0)*cos(EPSILON*PI/180.0)- $
			tan(BETA*PI/180.0)*sin(EPSILON*PI/180.0)
;����ķ�ĸ��
	ALPHA_DENOMIN=cos(LAMDA*PI/180.0)

	ALPHA=PTR_NEW(atan(ALPHA_NUMER,ALPHA_DENOMIN)*180/PI)

	ALPHA=LIMIT_DEGREES360(ALPHA)

	return,*ALPHA
END

;�������̫����γ
FUNCTION GSD_CAL,BETA,EPSILON,LAMDA

	COMMON PARA,PI

	DELTA=asin(sin(BETA*PI/180.0) * cos(EPSILON*PI/180.0) + $
			cos(BETA*PI/180.0)*sin(EPSILON*PI/180.0)*sin(LAMDA*PI/180.0))

	DELTA=DELTA*180.0/PI

	return,DELTA
END

;����۲�ط�ʱ��
FUNCTION OLHA_CAL,NU,O_LONGITUDE,ALPHA

	H=PTR_NEW((NU+*O_LONGITUDE-ALPHA),/NO_COPY)
	H=LIMIT_DEGREES360(temporary(H))

	return,H
END

;�������̫���ྭ
FUNCTION TSRA_CAL,R,ALPHA,DELTA,H,O_LATITUDE,O_ELEVATION

	COMMON PARA,PI
;����̫�������ƽ�Ӳ�
	XI=8.794D/3600D*R

	U=PTR_NEW((atan(0.99664719d*tan((*O_LATITUDE)*PI/180.0d))),/NO_COPY)

	X=PTR_NEW((cos(*U) + ((*O_ELEVATION)/6378140)*cos((*O_LATITUDE)*PI/180.0d)),/NO_COPY)

;����̫���ྭ�Ӳ�
	NUMER_A=PTR_NEW((-(*X)*sin(XI*PI/180.0d)*sin((*H)*PI/180.0d)),/NO_COPY)
	DENOMIN_A=PTR_NEW((cos(DELTA*PI/180.0d)-(*X)*sin(XI*PI/180.0d)*cos((*H)*PI/180.0d)),/NO_COPY)

	delta_ALPHA=atan(temporary(*NUMER_A),temporary(*DENOMIN_A))

	PTR_FREE,NUMER_A,DENOMIN_A

	delta_ALPHA=PTR_NEW((temporary(delta_ALPHA)*180.0d/PI),/NO_COPY)

;�������̫���ྭ
;	ALPHA_T=temporary(ALPHA)+(*delta_ALPHA)
;	ALPHA_T=PTR_NEW(temporary(ALPHA_T),/NO_COPY)

	Y=PTR_NEW((0.99664719d*sin(*U)+((*O_ELEVATION)/6378140)*sin((*O_LATITUDE)*PI/180.0d)),/NO_COPY)

	PTR_FREE,U

	NUMER_T=PTR_NEW(((sin(DELTA*PI/180.0d)-(*Y)*sin(XI*PI/180.0d))*cos((*delta_ALPHA)*PI/180.0d)),/NO_COPY)

	PTR_FREE,Y

	DENOMIN_T=PTR_NEW((cos(DELTA*PI/180.0d)-(*X)*sin(XI*PI/180.0d)*cos((*H)*PI/180.0d)),/NO_COPY)
	PTR_FREE,X
	DELTA_T=atan(temporary(*NUMER_T),temporary(*DENOMIN_T))

	PTR_FREE,NUMER_T,DENOMIN_T

;�������̫����γ
	DELTA_T=temporary(DELTA_T)*180/PI
	DELTA_T=PTR_NEW(temporary(DELTA_T),/NO_COPY)

;	return,{TOPO,ALPHA_T:ALPHA_T,DELTA_T:DELTA_T,delta_ALPHA:delta_ALPHA}
	return,{TOPO,DELTA_T:DELTA_T,delta_ALPHA:delta_ALPHA}
END

;��������춥��
FUNCTION TZA_CAL,O_LATITUDE,DELTA_T,H_T,PRESSURE,TEMPERATURE

	COMMON PARA,PI

;�������߶Ƚ�(�����Ǵ����������)
	E0=PTR_NEW((asin(sin((*O_LATITUDE)*PI/180.0)*sin((*DELTA_T)*PI/180.0)+ $
		cos((*O_LATITUDE)*PI/180.0)*cos((*DELTA_T)*PI/180.0)*cos((*H_T)*PI/180.0))),/NO_COPY)
	E0=PTR_NEW((temporary(*E0)*180.0/PI),/NO_COPY)

;����������������delta_E
	delta_E=PTR_NEW(((PRESSURE/1010.0D)*(283.0D/(273+TEMPERATURE))* $
		(1.02/(60*tan(((*E0)+(10.3/((*E0)+5.11)))*PI/180.0)))),/NO_COPY)

;�������߶Ƚ�E
	E=PTR_NEW((temporary(*E0)+temporary(*delta_E)),/NO_COPY)

	PTR_FREE,E0
;��������춥��
	S_ZENITH=PTR_NEW((90.0 - temporary(*E)),/NO_COPY)

	PTR_FREE,E

	return,S_ZENITH
END

FUNCTION TAA_CAL,H_T,O_LATITUDE,DELTA_T

	COMMON PARA,PI

;����������ķ�λ��
	NUMER_G=PTR_NEW((sin((*H_T)*PI/180.0)),/NO_COPY)

	DENOMIN_G=PTR_NEW((cos((*H_T)*PI/180.0)*sin((*O_LATITUDE)*PI/180.0)-tan((*DELTA_T)*PI/180.0) $
		*cos((*O_LATITUDE)*PI/180.0)),/NO_COPY)

	PTR_FREE,O_LATITUDE,DELTA_T,H_T

	GAMMA=PTR_NEW(atan(*NUMER_G,*DENOMIN_G)*180.0/PI)

	PTR_FREE,NUMER_G,DENOMIN_G

	GAMMA=LIMIT_DEGREES360(temporary(GAMMA))

	S_AZIMUTH=PTR_NEW(*GAMMA+180.0)

	S_AZIMUTH=LIMIT_DEGREES360(temporary(S_AZIMUTH))

	return,{AZ,S_AZIMUTH:S_AZIMUTH,GAMMA:GAMMA}
END

;��������������������
FUNCTION IASOAD_CAL,SOLAR_ZENITH,SLOPE,GAMMA,ASPECT

	COMMON PARA,PI

	I = PTR_NEW((acos(cos((*SOLAR_ZENITH)*PI/180.0)*cos((*SLOPE)*PI/180.0)+ $
		sin((*SLOPE)*PI/180.0)*sin((*SOLAR_ZENITH)*PI/180.0)*cos(((*GAMMA)+ $
		(*ASPECT))*PI/180.0))),/NO_COPY)

	PTR_FREE,ASPECT,GAMMA

	I=PTR_NEW(temporary(*I)*180.0/PI)
	I=LIMIT_DEGREES360(temporary(I))
	return,I
END

FUNCTION SOLAR_POS,YEAR,MONTH,DAY,UT_HOUR,UT_MINUTE,UT_SECOND $
						,LON0,LAT0,O_ELEVATION $
						,PRESSURE,TEMPERATURE $
						,DIMS,PIXEL,DEM_PIXEL $
						,inc,block,split_temp,merge_temp,outputfile $
						,geotiff

	on_error,2

	;UT(Universal Time)-����ʱ����������ʱ��
	;TAI(International Atomic Time)-����ԭ��ʱ
	;UTC(Coordinated Universal Time)-Э������ʱ
	;TDT/TT(Terrestrial Dynaical/Terrestrial Time)-������ѧʱ
	;delta_T-UT��TT�Ĳ�ֵ, delta_T=TT-UT

	PROGRESSTIMER = OBJ_NEW("SHOWPROGRESS", TLB,/CANCELBUTTON)
	PROGRESSTIMER->START

	CANCELLED = PROGRESSTIMER->CHECKCANCEL()
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('�û���ֹ�˲���')
		PROGRESSTIMER->DESTROY ;����������
		RETURN,0
	ENDIF

	PARA=COMMON_BLOCK_PARA()
	L0=PARA.L0
	L1=PARA.L1
	L2=PARA.L2
	L3=PARA.L3
	L4=PARA.L4
	L5=PARA.L5

	B0=PARA.B0
	B1=PARA.B1

	R0=PARA.R0
	R1=PARA.R1
	R2=PARA.R2
	R3=PARA.R3
	R4=PARA.R4
	Y=PARA.Y
	para_a=PARA.para_a
	para_b=PARA.para_b
	para_c=PARA.para_c
	para_d=PARA.para_d

;��������ʱ����ѧʱ�Ĳ�ֵ
	delta_T=DELTA_T_CAL(YEAR)
	;;print,delta_T,format='("delta_T : ",G)'

;**********���������ա����������ա����͡�ǧ��**********

	;�����գ�Julian Day, JD��
	JD=JD_CAL(YEAR,MONTH,DAY,UT_HOUR,UT_MINUTE,UT_SECOND)
	;print,JD,FORMAT='("Juldian Day: ",F15.6)'

	;���������գ�Juilan Ephemeris Day, JDE��
	JDE=JD+delta_T/86400.0d

	;�������ͣ���һ��������ĳ���(Julian Century, JC)��2000���׼
	JC=(JD-2451545)/36525.0d

	;������������(Julian Ephemeris Century, JCE)
	JCE=(JDE-2451545)/36525.0d

	;��������ǧ��(Julian Ephemeris Millennium, JME)
	JME=JCE/10.0d

;**********���������ա����������ա����͡�ǧ��**********

;*********����������ľ�(L)γ(B)�ȺͰ뾶(R)***********

	L=HEL_CAL('L',JME,L0,L1,L2,L3,L4,L5,B0,B1,R0,R1,R2,R3,R4);���ľ���
	;print,L,format='("L : ",G)'
	B=HEL_CAL('B',JME,L0,L1,L2,L3,L4,L5,B0,B1,R0,R1,R2,R3,R4);����γ��
	;print,B,format='("B : ",G)'
	R=HEL_CAL('R',JME,L0,L1,L2,L3,L4,L5,B0,B1,R0,R1,R2,R3,R4);����ʸ���뾶
	;print,R,format='("R : ",G," AU")'

	PTR_FREE,L0,L1,L2,L3,L4,L5,B0,B1,R0,R1,R2,R3,R4
;*********����������ľ�(L)γ(B)�ȺͰ뾶(R)***********

;************������ľ�(THETA)γ��(BETA)*************

;������ľ���
	THETA=PTR_NEW((*L+180),/NO_COPY)
	PTR_FREE,L
	THETA=LIMIT_DEGREES360(THETA)
	;print,THETA,format='("THETA : ",G)'
;�������γ��
	BETA=PTR_NEW((-(*B)),/NO_COPY)
	PTR_FREE,B
	;print,BETA,format='("BETA : ",G)'

;************������ľ�(THETA)γ��(BETA)*************

;*********���㾭��(delta_PSI)�����(delta_EPSILON)�¶�*********
	delta_PE=NUTATION_LON_OBL(JCE,Y,para_a,para_b,para_c,para_d)

	delta_PSI=delta_PE.delta_PSI
	;print,delta_PSI,format='("delta_PSI: ",G)'

	delta_EPSILON=delta_PE.delta_EPSILON
	;print,delta_EPSILON,format='("delta_EPSILON: ",G)'
;*********���㾭��(delta_PSI)�����(delta_EPSILON)�¶�*********
	PTR_FREE,Y,para_a,para_b,para_c,para_d
;***������ʵ�ĻƵ����(True Obliquity of the Ecliptic,EPSILON,��)***
	EPSILON=EPSILON_CAL(JME,delta_EPSILON)
	;print,EPSILON,format='("EPSILON : ",G)'
;***������ʵ�ĻƵ����(True Obliquity of the Ecliptic,EPSILON,��)***

;*****************�������У��(delta_TAU,��)*************
	delta_TAU=ABERRATION_CORRECTION(*R)
	;print,delta_TAU,format='("delta_TAU : ",G)'
;*****************�������У��(delta_TAU,��)*************

;****************������̫������(LAMDA,��)****************
	LAMDA=(*THETA)+delta_PSI+delta_TAU
	PTR_FREE,THETA
	;print,LAMDA,format='("LAMDA : ",G)'
;****************������̫������(LAMDA,��)****************

;����������������ʱ(Apparent Sidereal Time at Greenwich, NU)
	NU=ASTG_CAL(JD,JC,delta_PSI,EPSILON)
	;print,NU,format='("NU : ",G)'
;����������������ʱ(Apparent Sidereal Time at Greenwich, NU)

;�������̫���ྭ(Geocentric Sun Right Ascension, ALPHA)
	ALPHA=GSRS_CAL(LAMDA,EPSILON,*BETA)
	;print,ALPHA,format='("ALPHA : ",G)'
;�������̫���ྭ(Geocentric Sun Right Ascension, ALPHA)

;�������̫����γ(Geocentric Sun Declination, DELTA)
	DELTA=GSD_CAL(*BETA,EPSILON,LAMDA)
	PTR_FREE,BETA
	;print,DELTA,format='("DELTA : ",G)'
;�������̫����γ(Geocentric Sun Declination, DELTA)

	PROGRESSTIMER->UPDATE, 5
;**************************���ݷָ�׼��*******************************
	if n_elements(DIMS) eq 3 then begin
		col=DIMS[1]
		row=DIMS[2]
	endif else begin
		col=DIMS[0]
		row=DIMS[1]
	endelse

	DEM_SIZE = size(*O_ELEVATION,/dimensions)

	if (col ne DEM_SIZE[0]) or (row ne DEM_SIZE[1]) then begin
		PTR_FREE,O_ELEVATION
		O_ELEVATION = PTR_NEW(fltarr(col,row,/NOZERO),/NO_COPY)
	endif

	DATA_INFO=DATA_SPLIT(O_ELEVATION,inc,block,split_temp,'dem')

	PTR_FREE,O_ELEVATION


	O_LATITUDE=PTR_NEW(fltarr(col,row,/NOZERO),/NO_COPY)
	for j =0,row-1,1 do begin
		(*O_LATITUDE)[*,j]= LAT0 - j * pixel
	endfor

;	*O_LATITUDE=reverse(temporary(*O_LATITUDE),2)
	LAT_INFO=DATA_SPLIT(O_LATITUDE,inc,block,split_temp,'lat')

	PTR_FREE,O_LATITUDE

	O_LONGITUDE=PTR_NEW(fltarr(col,row,/NOZERO))
	for i = 0, col-1,1 do begin
		(*O_LONGITUDE)[i,*] = LON0 + i * pixel
	endfor
;	*O_LONGITUDE=reverse(temporary(*O_LONGITUDE),2)
	LON_INFO=DATA_SPLIT(O_LONGITUDE,inc,block,split_temp,'lon')

	PTR_FREE,O_LONGITUDE

;**************************���ݷָ�׼��*******************************

	PROGRESSTIMER->UPDATE, 10
;***************���ָ�õ����ݷֱ���д���***************
	for i=0,DATA_INFO.x-1,1 do begin
		for j=0,DATA_INFO.y-1,1 do begin

			temp_name=split_temp+'dem'+strtrim(string(i),2)+'_'+strtrim(string(j),2)+'.temp'
			RESTORE,temp_name
			FILE_DELETE,temp_name,/NOEXPAND_PATH
			O_ELEVATION=PTR_NEW(((*temp)),/NO_COPY)
			PTR_FREE,temp

			temp_name=split_temp+'lat'+strtrim(string(i),2)+'_'+strtrim(string(j),2)+'.temp'
			RESTORE,temp_name
			FILE_DELETE,temp_name,/NOEXPAND_PATH
			O_LATITUDE=PTR_NEW(((*temp)),/NO_COPY)
			PTR_FREE,temp

			temp_name=split_temp+'lon'+strtrim(string(i),2)+'_'+strtrim(string(j),2)+'.temp'
			RESTORE,temp_name
			FILE_DELETE,temp_name,/NOEXPAND_PATH
			O_LONGITUDE=PTR_NEW(((*temp)),/NO_COPY)
			PTR_FREE,temp
		;���߳�������ο�Ӱ���С��ƥ��ʱ���߳�ȫ��Ϊ0
			SLO_ASP=SLO_ASP_CAL(O_ELEVATION,DEM_PIXEL)
			SLOPE=SLO_ASP.SLOPE
			ASPECT=SLO_ASP.ASPECT

		;����۲�ط�ʱ��(Observer Local Hour Angle, H,��)
			H=OLHA_CAL(float(NU),O_LONGITUDE,float(ALPHA))
			;print,H,format='("H : ",G)'
		;����۲�ط�ʱ��(Observer Local Hour Angle, H,��)
			PTR_FREE,O_LONGITUDE

		;��������̫���ྭ����γ(Topocentric Sun Right Ascension, ALPHA_T/Topocentric Sun Declination, DELTA_T,��)

			TOPO_ASC_DEC=TSRA_CAL(*R,ALPHA,DELTA,H,O_LATITUDE,O_ELEVATION)

			PTR_FREE,O_ELEVATION
		;	ALPHA_T=TOPO_ASC_DEC.ALPHA_T
			DELTA_T=TOPO_ASC_DEC.DELTA_T
			delta_ALPHA=TOPO_ASC_DEC.delta_ALPHA
			;print,ALPHA_T,format='("ALPHA_T : ",G)'
			;print,DELTA_T,format='("DELTA_T : ",G)'
		;��������̫���ྭ����γ(Topocentric Sun Right Ascension, ALPHA_T/Topocentric Sun Declination, DELTA_T,��)

		;�������ط�ʱ��(Topocentric Local Hour Angle, H_T, ��)
			H_T=PTR_NEW((temporary(*H)-temporary(*delta_ALPHA)),/NO_COPY)
			PTR_FREE,delta_ALPHA
			;print,H_T,format='("H_T : ",G)'
		;�������ط�ʱ��(Topocentric Local Hour Angle, H_T, ��)
			PTR_FREE,H

		;��������춥��(Topocentric Zenith Angle, SOLAR_ZENITH,��)
			SOLAR_POSITION0=TZA_CAL(O_LATITUDE,DELTA_T,H_T,PRESSURE,TEMPERATURE)

			;print,SOLAR_ZENITH,format='("SOLAR_ZENITH : ",G)'
		;��������춥��(Topocentric Zenith Angle, SOLAR_ZENITH,��)

		;������淽λ��(Topocentric Azimuth Angle,T_AZIMUTH,��)
			T_AZIMUTH =TAA_CAL(H_T,O_LATITUDE,DELTA_T)

			SOLAR_POSITION1=T_AZIMUTH.S_AZIMUTH

			GAMMA=T_AZIMUTH.GAMMA

			;print,SOLAR_AZIMUTH,format='("SOLAR_AZIMUTH : ",G)'
		;������淽λ��(Topocentric Azimuth Angle,T_AZIMUTH,��)

		;��������������������(Incidence Angle for a Surface Oriented in Any Direction,I,��)

			SOLAR_POSITION2=IASOAD_CAL(SOLAR_POSITION0,SLOPE,GAMMA,ASPECT)
			;;print,I,format='("I : ",G)'
		;��������������������(Incidence Angle for a Surface Oriented in Any Direction,I,��)
			slope_name=merge_temp+'slope_temp'+strtrim(string(i),2)+'_'+strtrim(string(j),2)+'.temp'
			SOLAR_POSITION=SLOPE
			SAVE,SOLAR_POSITION,FILENAME=slope_name
			GC_STR={SOLAR_POSITION:SOLAR_POSITION}
			HEAP_FREE,GC_STR

			zen_name=merge_temp+'zen_temp'+strtrim(string(i),2)+'_'+strtrim(string(j),2)+'.temp'
			SOLAR_POSITION=SOLAR_POSITION0
			SAVE,SOLAR_POSITION,FILENAME=zen_name
			GC_STR={SOLAR_POSITION:SOLAR_POSITION}
			HEAP_FREE,GC_STR

			azi_name=merge_temp+'azi_temp'+strtrim(string(i),2)+'_'+strtrim(string(j),2)+'.temp'
			SOLAR_POSITION=SOLAR_POSITION1
			SAVE,SOLAR_POSITION,FILENAME=azi_name
			GC_STR={SOLAR_POSITION:SOLAR_POSITION}
			HEAP_FREE,GC_STR

			ia_name=merge_temp+'ia_temp'+strtrim(string(i),2)+'_'+strtrim(string(j),2)+'.temp'
			SOLAR_POSITION=SOLAR_POSITION2
			SAVE,SOLAR_POSITION,FILENAME=ia_name
			GC_STR={SOLAR_POSITION:SOLAR_POSITION}
			HEAP_FREE,GC_STR

		endfor

		PROGRESSTIMER->UPDATE, 10+(i*90.0/(DATA_INFO.x-1))
		IF CANCELLED THEN BEGIN
			OK = DIALOG_MESSAGE('�û���ֹ�˲���')
			PROGRESSTIMER->DESTROY ;����������
			RETURN,0
		ENDIF
	endfor
	PTR_FREE,R
;***************���ָ�õ����ݷֱ���д���***************

;***************������������ݽ��кϲ�ƴ��***************
	SLOPE_RESULT=PTR_NEW((DATA_MERGE(DATA_INFO.x,DATA_INFO.y,DATA_INFO.col, $
				DATA_INFO.row,inc,block,merge_temp,'slope_temp',0)),/NO_COPY)
	slope_outputfile=outputfile+'_slope.tif'
	write_tiff, slope_outputfile, *SLOPE_RESULT, GEOTIFF=GEOTIFF,/float
	PTR_FREE,SLOPE_RESULT

	PROGRESSTIMER->UPDATE, 93
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('�û���ֹ�˲���')
		PROGRESSTIMER->DESTROY ;����������
		RETURN,0
	ENDIF

	ZEN_RESULT=PTR_NEW((DATA_MERGE(DATA_INFO.x,DATA_INFO.y,DATA_INFO.col, $
				DATA_INFO.row,inc,block,merge_temp,'zen_temp',0)),/NO_COPY)
	zen_outputfile=outputfile+'_zen.tif'
	write_tiff, zen_outputfile, *ZEN_RESULT, GEOTIFF=GEOTIFF,/float
	PTR_FREE,ZEN_RESULT

	PROGRESSTIMER->UPDATE, 95
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('�û���ֹ�˲���')
		PROGRESSTIMER->DESTROY ;����������
		RETURN,0
	ENDIF

	AZI_RESULT=PTR_NEW((DATA_MERGE(DATA_INFO.x,DATA_INFO.y,DATA_INFO.col, $
				DATA_INFO.row,inc,block,merge_temp,'azi_temp',1)),/NO_COPY)
	azi_outputfile=outputfile+'_azi.tif'
	write_tiff, azi_outputfile, *AZI_RESULT, GEOTIFF=GEOTIFF,/float
	PTR_FREE,AZI_RESULT

	PROGRESSTIMER->UPDATE, 97
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('�û���ֹ�˲���')
		PROGRESSTIMER->DESTROY ;����������
		RETURN,0
	ENDIF

	IA_RESULT =PTR_NEW((DATA_MERGE(DATA_INFO.x,DATA_INFO.y,DATA_INFO.col, $
				DATA_INFO.row,inc,block,merge_temp,'ia_temp',2)),/NO_COPY)
	ia_outputfile=outputfile+'_ia.tif'
	write_tiff, ia_outputfile, *IA_RESULT, GEOTIFF=GEOTIFF,/float
	PTR_FREE,IA_RESULT
;***************������������ݽ��кϲ�ƴ��***************

	PROGRESSTIMER->UPDATE, 100
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('�û���ֹ�˲���')
		PROGRESSTIMER->DESTROY ;����������
		RETURN,0
	ENDIF

	PROGRESSTIMER->DESTROY

	CAL_INFO=dialog_message('������ɣ�',/information)

	return,1

END

PRO SOLAR_POSITION_CALCULATION,event

	on_error,2

	starttime=systime()

	Widget_Control,event.top,get_uvalue=PSTATE

;*********************�û��������*********************
	Widget_Control,(*PSTATE).INPUT_FIELD,get_value=inputfile
	Widget_Control,(*PSTATE).DEM_FIELD,get_value=demfile
	Widget_Control,(*PSTATE).OUTPUT_FIELD,get_value=outputfile

	if FILE_TEST(inputfile) ne 1 then begin
		CAUTION = dialog_message('δָ���������ݻ�����������Ч!',/information)
		return
	endif

	if FILE_TEST(outputfile) ne 1 then begin
		CAUTION = dialog_message('δָ����������ļ��л�·����Ч!',/information)
		return
	endif

	outname=file_basename(inputfile,'.tif',/fold_case)
	outputfile += outname

	YEAR=fix((*PSTATE).YEAR)	;��
	MONTH=fix((*PSTATE).MONTH)		;��
	DAY=fix((*PSTATE).DAY)		;��

	;����ʱ
	Widget_Control,(*PSTATE).HOUR_TEXT,get_value=UT_HOUR	;ʱ
	Widget_Control,(*PSTATE).MIN_TEXT,get_value=UT_MINUTE	;��
	Widget_Control,(*PSTATE).SEC_TEXT,get_value=UT_SECOND	;��
	UT_HOUR=FIX(UT_HOUR[0])
	UT_MINUTE=FIX(UT_MINUTE[0])
	UT_SECOND=FLOAT(UT_SECOND[0])

	Widget_Control,(*PSTATE).DEM_PIXEL,get_value=DEM_PIXEL
	DEM_PIXEL=float(DEM_PIXEL[0])

	Widget_Control,(*PSTATE).PRES_FIELD,get_value=PRESSURE	;��ƽ����ѹ(����,millibars,�൱��1hpa)
	Widget_Control,(*PSTATE).TEMP_FIELD,get_value=TEMPERATURE	;��ƽ������(���϶�,��C)
	PRESSURE=FLOAT(PRESSURE[0])
	TEMPERATURE=FLOAT(TEMPERATURE[0])
;*********************�û��������*********************

;***********************ϵͳ����***********************
	inc=3
	block=512
	temp_path='temp\'
	split_temp=temp_path+'split_temp_'
	merge_temp=temp_path+'merge_temp_'

;***********************ϵͳ����***********************
	data = PTR_NEW(read_tiff(inputfile, geotiff = geotiff),/NO_COPY)	;����Ӱ��

	LAT0 = geotiff.MODELTIEPOINTTAG[4]		;��ʼγ��
	LON0 = geotiff.MODELTIEPOINTTAG[3]		;��ʼ����
	PIXEL = geotiff.MODELPIXELSCALETAG[0]	;��Ԫ��С
	DIMS = size(*data, /DIMENSIONS)			;Ӱ���С


	PTR_FREE,data

	if file_test(demfile) eq 1 then begin
		dem = read_tiff(demfile)	;���ָ߳�ģ��
	endif else begin
		dem = fltarr(2,2,/nozero)
	endelse

	O_ELEVATION=PTR_NEW(temporary(dem),/NO_COPY)

	CAL_STATUS=SOLAR_POS(YEAR,MONTH,DAY,UT_HOUR,UT_MINUTE,UT_SECOND $
						,LON0,LAT0,O_ELEVATION $
						,PRESSURE,TEMPERATURE,DIMS,PIXEL,DEM_PIXEL $
						,inc,block,split_temp,merge_temp,outputfile,geotiff)

	tempfiles=file_search(temp_path+'*.temp',count=tfnum)
	if tfnum ne 0 then $
		FILE_DELETE,tempfiles,/NOEXPAND_PATH

	endtime=systime()

	cost_time=COST_TIME_CAL(starttime,endtime)
	if CAL_STATUS eq 1 then $
		print,cost_time.hour,cost_time.minute,cost_time.second, $
			format='("�ܹ��ķ�ʱ��: ",I3,"Сʱ",I3,"��",I3,"��")'

	common_log,'�������'

END