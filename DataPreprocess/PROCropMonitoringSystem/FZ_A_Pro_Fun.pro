
;˵��:FZ_A_Pro_Fun.pro
;����:���ļ���Ÿ���ָ���������������õĹ��ú��������
;ʱ��:2006.8.18
;����:���¸ղ�ʿ(2004��)
;ע��:(1)������Щ���̻���֮ǰ,Ӧ��֤�ļ��ȱ�����.
;	  (2)����,��ò�Ҫ�ٸĵ�����������̵�˳��,��Ϊ����֮��Ҳ�б����ù�ϵ.

;����B����������A���Ӽ�,��A��B������û���ظ�ֵ,;
;��B�Ĳ���C,��C = A - B
FUNCTION FZ_BuJi,A $  ;���Ԫ�ص�����
			 	,B	   ;A�����������(��ȻB��Aͬ��������)

	Entire = A
	FOR I=0,N_ELEMENTS(B)-1 DO BEGIN
		Temp = WHERE(Entire EQ B[I],COMPLEMENT=NoIs)
		Entire = Entire[NoIs]
	ENDFOR
	C = Entire
	RETURN,C
END

;-------FZ_Create_Composite_Time()�����������г���Create_Composite_Time()----------------
FUNCTION FZ_Create_Composite_Time,StartEndTime $  ;8Ԫ���ַ�����:��ʼ��/��/Ѯ/��,������/��/Ѯ/��
							  ,TimeFre_  $     ;�����ʱ��Ƶ��"Ѯ/��/��"
    						  ,Prompt = Prompt ;���ش�����ʾ��Ϣ
;������ʽ: Result = Create_Composite_Time(StartEndTime,TimeFre_,[Prompt = Prompt])
;����ֵΪ:(1)����������ʼʱ������ʱ��(����:20060409��8λ�ַ�������)
;		  (2)����ֵ--'Error',���������������㲻�ɹ�ʱ.
	StartYear    = StartEndTime[0]
	StartMonth   = StartEndTime[1]
	StartTenday  = StartEndTime[2]
	StartDay     = StartEndTime[3]

	EndYear   = StartEndTime[4]
	EndMonth  = StartEndTime[5]
	EndTenday = StartEndTime[6]
	EndDay    = StartEndTime[7]

	TimeFrequence = TimeFre_

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

		TimeFrequence NE 'MONTH': BEGIN
			IF (FIX(StartYear) EQ FIX(EndYear)) AND (FIX(StartMonth) EQ FIX(EndMonth)) THEN BEGIN
			   IF (FIX(StartTenday) GT FIX(EndTenday)) AND (TimeFrequence EQ 'TENDAY') THEN BEGIN
				  Prompt = '��ʼѮ���ܴ��ڽ���Ѯ��'
				  RETURN,'Error'
			   ENDIF
			   IF (FIX(StartDay) GT FIX(EndDay)) AND (TimeFrequence EQ 'DAY')THEN BEGIN
				  Prompt = '��ʼ�ղ��ܴ��ڽ����գ�'
				  RETURN,'Error'
			   ENDIF
			ENDIF
		END
		ELSE:
	  ENDCASE

	  CASE TimeFrequence OF
	  	 'TENDAY': BEGIN
	  	 		MaxTendayOrDay   = '3'
	  	 		StartTendayOrDay = StartTenday
	  	 		EndTendayOrDay   = EndTenday
	  	 END

	  	 'DAY'   : BEGIN
	  			MaxTendayOrDay = '31'
	  	 		StartTendayOrDay = StartDay
	  	 		EndTendayOrDay   = EndDay
	  	 END

	  	 ELSE:
	  ENDCASE

	  WIDGET_CONTROL,/HOURGLASS
	  CASE TimeFrequence OF     ;��һ����Ϊ������ϵ�ʱ��"��+��[+Ѯ(��)]"
	  	 'MONTH': BEGIN
			AllTime = ''
			IF FIX(StartYear) LT FIX(EndYear) THEN BEGIN
				FOR I=FIX(StartYear),FIX(EndYear) DO BEGIN
						Year = STRTRIM(I,2)
					FOR j=1,12 DO BEGIN
						Month = STRTRIM(j,2)
						IF j LT 10 THEN Month = '0'+STRTRIM(j,2)
						AllTime = [AllTime,Year+Month]
					ENDFOR
				ENDFOR

				IF FIX(StartMonth) GT 1 THEN BEGIN
					Temp1 = ''
					FOR j=1,FIX(StartMonth)-1 DO BEGIN
						Month = STRTRIM(j,2)
						IF j LT 10 THEN Month = '0'+STRTRIM(j,2)
						Temp1 = [Temp1,StartYear+Month]
					ENDFOR
					Temp1 = Temp1[1:*]
					AllTime = FZ_BuJi(AllTime,Temp1)       ;ȥ����ʼʱ��ǰ�Ĳ���.
				ENDIF

				IF FIX(EndMonth) LT 12 THEN BEGIN
					Temp2 = ''
					FOR j=FIX(EndMonth)+1,12 DO BEGIN
						Month = STRTRIM(j,2)
						IF j LT 10 THEN Month = '0'+STRTRIM(j,2)
						Temp2 = [Temp2,EndYear+Month]
					ENDFOR
					Temp2 = Temp2[1:*]
					AllTime = FZ_BuJi(AllTime,Temp2)       ;ȥ������ʱ���Ĳ���.
				ENDIF
			 ENDIF ELSE BEGIN                     ;������
			 	Year = StartYear
				FOR j=FIX(StartMonth),FIX(EndMonth) DO BEGIN
					Month = STRTRIM(j,2)
					IF j LT 10 THEN Month = '0'+STRTRIM(j,2)
					AllTime = [AllTime,Year+Month]
				ENDFOR
			 ENDELSE

			AllTime=AllTime[1:*]
		  END

	  	 ELSE: BEGIN
			AllTime = ''
			IF FIX(StartYear) LT FIX(EndYear) THEN BEGIN
				FOR I=FIX(StartYear),FIX(EndYear) DO BEGIN
						Year = STRTRIM(I,2)
					FOR j=1,12 DO BEGIN
					  Month = STRTRIM(j,2)
					  IF j LT 10 THEN Month = '0'+STRTRIM(j,2)
						FOR H = 1,FIX(MaxTendayOrDay) DO BEGIN
						   TendayOrDay = STRTRIM(H,2)
						   IF H LT 10 THEN TendayOrDay = '0'+STRTRIM(H,2)
							AllTime = [AllTime,Year+Month+TendayOrDay]
						ENDFOR
					ENDFOR
				ENDFOR

				IF FIX(StartMonth) GT 1 THEN BEGIN
					Temp1 = ''
					FOR j=1,FIX(StartMonth)-1 DO BEGIN
						Month = STRTRIM(j,2)
						IF j LT 10 THEN Month = '0'+STRTRIM(j,2)
						FOR H = 1,FIX(MaxTendayOrDay) DO BEGIN
						   TendayOrDay = STRTRIM(H,2)
						   IF H LT 10 THEN TendayOrDay = '0'+STRTRIM(H,2)
						   Temp1 = [Temp1,StartYear+Month+TendayOrDay]
						ENDFOR
					ENDFOR
					Temp1 = Temp1[1:*]

					IF FIX(StartTendayOrDay) GT 1 THEN BEGIN
					   Temp = ''
					   FOR H=1,FIX(StartTendayOrDay)-1 DO BEGIN
						   TendayOrDay = STRTRIM(H,2)
						   IF H LT 10 THEN TendayOrDay = '0'+STRTRIM(H,2)
						   Temp = [Temp,StartYear+StartMonth+TendayOrDay]
					   ENDFOR
					   Temp = Temp[1:*]
					   Temp1 = [Temp1,Temp]
					ENDIF
					AllTime = FZ_BuJi(AllTime,Temp1)         ;ȥ����ʼʱ��ǰ�Ĳ���.
				 ENDIF ELSE BEGIN
					IF FIX(StartTendayOrDay) GT 1 THEN BEGIN
					   Temp1 = ''
					   FOR H=1,FIX(StartTendayOrDay)-1 DO BEGIN
						   TendayOrDay = STRTRIM(H,2)
						   IF H LT 10 THEN TendayOrDay = '0'+STRTRIM(H,2)
						   Temp1 = [Temp1,StartYear+StartMonth+TendayOrDay]
					   ENDFOR
					   Temp1 = Temp1[1:*]
					   AllTime = FZ_BuJi(AllTime,Temp1)         ;ȥ����ʼʱ��ǰ�Ĳ���.
					ENDIF
				 ENDELSE

				IF FIX(EndMonth) LT 12 THEN BEGIN
					Temp2 = ''
					FOR j=FIX(EndMonth)+1,12 DO BEGIN
						Month = STRTRIM(j,2)
						IF j LT 10 THEN Month = '0'+STRTRIM(j,2)
						FOR H = 1,FIX(MaxTendayOrDay) DO BEGIN
						   TendayOrDay = STRTRIM(H,2)
						   IF H LT 10 THEN TendayOrDay = '0'+STRTRIM(H,2)
						   Temp2 = [Temp2,EndYear+Month+TendayOrDay]
						ENDFOR
					ENDFOR
					Temp2 = Temp2[1:*]

					IF FIX(EndTendayOrDay) LT FIX(MaxTendayOrDay) THEN BEGIN
					   Temp = ''
					   FOR H=FIX(EndTendayOrDay)+1,FIX(MaxTendayOrDay) DO BEGIN
						   TendayOrDay = STRTRIM(H,2)
						   IF H LT 10 THEN TendayOrDay = '0'+STRTRIM(H,2)
						   Temp = [Temp,EndYear+EndMonth+TendayOrDay]
					   ENDFOR
					   Temp = Temp[1:*]
					   Temp2 = [Temp,Temp2]        ;Ϊ�ܺõر�ʾʱ��˳��,��Temp����ǰ��.
					ENDIF
					AllTime = FZ_BuJi(AllTime,Temp2)   ;ȥ������ʱ���Ĳ���.
				ENDIF ELSE BEGIN
					IF FIX(EndTendayOrDay) LT FIX(MaxTendayOrDay) THEN BEGIN
					   Temp2 = ''
					   FOR H=FIX(EndTendayOrDay)+1,FIX(MaxTendayOrDay) DO BEGIN
						   TendayOrDay = STRTRIM(H,2)
						   IF H LT 10 THEN TendayOrDay = '0'+STRTRIM(H,2)
						   Temp2 = [Temp2,EndYear+EndMonth+TendayOrDay]
					   ENDFOR
					   Temp2 = Temp2[1:*]
					   AllTime = FZ_BuJi(AllTime,Temp2)   ;ȥ������ʱ���Ĳ���.
					ENDIF
				ENDELSE
			 ENDIF ELSE BEGIN                     ;������
			 	Year = EndYear
				IF FIX(StartMonth) LT FIX(EndMonth) THEN BEGIN
					FOR I=FIX(StartMonth),FIX(EndMonth) DO BEGIN
						Month = STRTRIM(I,2)
						IF I LT 10 THEN Month = '0'+STRTRIM(I,2)
						FOR j=1,FIX(MaxTendayOrDay) DO BEGIN
						  TendayOrDay = STRTRIM(j,2)
						  IF j LT 10 THEN TendayOrDay = '0'+STRTRIM(j,2)

;						  ;===�������޸ģ�20070830========================
;						  AllTime = [AllTime,Year+Month+TendayOrDay]	;ԭ����
						  AllTime = [AllTime,STRTRIM(Year,2)+STRTRIM(Month,2)+STRTRIM(TendayOrDay,2)]
						  ;================================================
						ENDFOR
					ENDFOR

					IF FIX(StartTendayOrDay) GT 1 THEN BEGIN
					   Temp = ''
					   IF FIX(StartMonth) LT 10 THEN Month = '0'+STRTRIM(StartMonth,2) else Month = StartMonth
					   FOR H=1,FIX(StartTendayOrDay)-1 DO BEGIN
						   TendayOrDay = STRTRIM(H,2)
						   IF H LT 10 THEN TendayOrDay = '0'+STRTRIM(H,2)
;                   		  Temp = [Temp,Year+StartMonth+TendayOrDay]	;ԭ����
						  Temp = [Temp,STRTRIM(Year,2)+STRTRIM(Month,2)+STRTRIM(TendayOrDay,2)]
						  ;================================================
					   ENDFOR
					   Temp = Temp[1:*]
					   AllTime = FZ_BuJi(AllTime,Temp)         ;ȥ����ʼʱ��ǰ�Ĳ���.
					ENDIF

					IF FIX(EndTendayOrDay) LT FIX(MaxTendayOrDay) THEN BEGIN
					   Temp = ''
					   IF FIX(EndMonth) LT 10 THEN Month = '0'+STRTRIM(EndMonth,2) else Month = EndMonth
					   FOR H=FIX(EndTendayOrDay)+1,FIX(MaxTendayOrDay) DO BEGIN
						   TendayOrDay = STRTRIM(H,2)
						   IF H LT 10 THEN TendayOrDay = '0'+STRTRIM(H,2)
;						   Temp = [Temp,Year+EndMonth+TendayOrDay]
						  Temp = [Temp,STRTRIM(Year,2)+STRTRIM(Month,2)+STRTRIM(TendayOrDay,2)]
						  ;================================================
					   ENDFOR
					   Temp = Temp[1:*]
					   AllTime = FZ_BuJi(AllTime,Temp)         ;ȥ������ʱ��ǰ�Ĳ���.
					ENDIF
				ENDIF ELSE BEGIN
				    Month = StartMonth
					FOR I=FIX(StartTendayOrDay),FIX(EndTendayOrDay) DO BEGIN
						TendayOrDay = STRTRIM(I,2)
						IF I LT 10 THEN TendayOrDay = '0'+STRTRIM(I,2)
;						 AllTime = [AllTime,Year+Month+TendayOrDay]
						 ;===�������޸ģ�20070830========================
;						  AllTime = [AllTime,Year+Month+TendayOrDay]	;ԭ����
						  AllTime = [AllTime,STRTRIM(Year,2)+STRTRIM(Month,2)+STRTRIM(TendayOrDay,2)]
						  ;================================================
					ENDFOR
				ENDELSE
			ENDELSE
			AllTime=AllTime[1:*]
		 END
	   ENDCASE
;    print,AllTime
	RETURN,AllTime
END

