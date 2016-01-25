
;说明:FZ_A_Pro_Fun.pro
;作用:该文件存放复种指数分析计算中所用的公用函数或过程
;时间:2006.8.18
;作者:徐新刚博士(2004届)
;注意:(1)调用这些过程或函数之前,应保证文件先被编译.
;	  (2)另外,最好不要再改到各函数或过程的顺序,因为它们之间也有被调用关系.

;数组B必须是数组A的子集,且A与B数组中没有重复值,;
;求B的补集C,即C = A - B
FUNCTION FZ_BuJi,A $  ;多个元素的数组
			 	,B	   ;A数组的子数组(当然B与A同数据类型)

	Entire = A
	FOR I=0,N_ELEMENTS(B)-1 DO BEGIN
		Temp = WHERE(Entire EQ B[I],COMPLEMENT=NoIs)
		Entire = Entire[NoIs]
	ENDFOR
	C = Entire
	RETURN,C
END

;-------FZ_Create_Composite_Time()是算生物量中程序Create_Composite_Time()----------------
FUNCTION FZ_Create_Composite_Time,StartEndTime $  ;8元素字符数组:开始年/月/旬/日,结束年/月/旬/日
							  ,TimeFre_  $     ;计算的时间频率"旬/日/月"
    						  ,Prompt = Prompt ;返回错误提示信息
;调用形式: Result = Create_Composite_Time(StartEndTime,TimeFre_,[Prompt = Prompt])
;返回值为:(1)用所给的起始时间生成时间(形如:20060409的8位字符型数组)
;		  (2)返回值--'Error',若不满足条件计算不成功时.
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
			Prompt = '起始年份不能大于结束年份！'
			RETURN,'Error'
		END

		FIX(StartMonth) GT FIX(EndMonth) : BEGIN
			IF FIX(StartYear) EQ FIX(EndYear) THEN BEGIN
				Prompt = '起始月份不能大于结束月份！'
				RETURN,'Error'
			ENDIF
		END

		TimeFrequence NE 'MONTH': BEGIN
			IF (FIX(StartYear) EQ FIX(EndYear)) AND (FIX(StartMonth) EQ FIX(EndMonth)) THEN BEGIN
			   IF (FIX(StartTenday) GT FIX(EndTenday)) AND (TimeFrequence EQ 'TENDAY') THEN BEGIN
				  Prompt = '起始旬不能大于结束旬！'
				  RETURN,'Error'
			   ENDIF
			   IF (FIX(StartDay) GT FIX(EndDay)) AND (TimeFrequence EQ 'DAY')THEN BEGIN
				  Prompt = '起始日不能大于结束日！'
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
	  CASE TimeFrequence OF     ;这一步是为生成组合的时间"年+月[+旬(日)]"
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
					AllTime = FZ_BuJi(AllTime,Temp1)       ;去掉起始时间前的部分.
				ENDIF

				IF FIX(EndMonth) LT 12 THEN BEGIN
					Temp2 = ''
					FOR j=FIX(EndMonth)+1,12 DO BEGIN
						Month = STRTRIM(j,2)
						IF j LT 10 THEN Month = '0'+STRTRIM(j,2)
						Temp2 = [Temp2,EndYear+Month]
					ENDFOR
					Temp2 = Temp2[1:*]
					AllTime = FZ_BuJi(AllTime,Temp2)       ;去掉结束时间后的部分.
				ENDIF
			 ENDIF ELSE BEGIN                     ;年份相等
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
					AllTime = FZ_BuJi(AllTime,Temp1)         ;去掉起始时间前的部分.
				 ENDIF ELSE BEGIN
					IF FIX(StartTendayOrDay) GT 1 THEN BEGIN
					   Temp1 = ''
					   FOR H=1,FIX(StartTendayOrDay)-1 DO BEGIN
						   TendayOrDay = STRTRIM(H,2)
						   IF H LT 10 THEN TendayOrDay = '0'+STRTRIM(H,2)
						   Temp1 = [Temp1,StartYear+StartMonth+TendayOrDay]
					   ENDFOR
					   Temp1 = Temp1[1:*]
					   AllTime = FZ_BuJi(AllTime,Temp1)         ;去掉起始时间前的部分.
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
					   Temp2 = [Temp,Temp2]        ;为很好地表示时间顺序,将Temp放在前面.
					ENDIF
					AllTime = FZ_BuJi(AllTime,Temp2)   ;去掉结束时间后的部分.
				ENDIF ELSE BEGIN
					IF FIX(EndTendayOrDay) LT FIX(MaxTendayOrDay) THEN BEGIN
					   Temp2 = ''
					   FOR H=FIX(EndTendayOrDay)+1,FIX(MaxTendayOrDay) DO BEGIN
						   TendayOrDay = STRTRIM(H,2)
						   IF H LT 10 THEN TendayOrDay = '0'+STRTRIM(H,2)
						   Temp2 = [Temp2,EndYear+EndMonth+TendayOrDay]
					   ENDFOR
					   Temp2 = Temp2[1:*]
					   AllTime = FZ_BuJi(AllTime,Temp2)   ;去掉结束时间后的部分.
					ENDIF
				ENDELSE
			 ENDIF ELSE BEGIN                     ;年份相等
			 	Year = EndYear
				IF FIX(StartMonth) LT FIX(EndMonth) THEN BEGIN
					FOR I=FIX(StartMonth),FIX(EndMonth) DO BEGIN
						Month = STRTRIM(I,2)
						IF I LT 10 THEN Month = '0'+STRTRIM(I,2)
						FOR j=1,FIX(MaxTendayOrDay) DO BEGIN
						  TendayOrDay = STRTRIM(j,2)
						  IF j LT 10 THEN TendayOrDay = '0'+STRTRIM(j,2)

;						  ;===杨绍锷修改，20070830========================
;						  AllTime = [AllTime,Year+Month+TendayOrDay]	;原代码
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
;                   		  Temp = [Temp,Year+StartMonth+TendayOrDay]	;原代码
						  Temp = [Temp,STRTRIM(Year,2)+STRTRIM(Month,2)+STRTRIM(TendayOrDay,2)]
						  ;================================================
					   ENDFOR
					   Temp = Temp[1:*]
					   AllTime = FZ_BuJi(AllTime,Temp)         ;去掉起始时间前的部分.
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
					   AllTime = FZ_BuJi(AllTime,Temp)         ;去掉结束时间前的部分.
					ENDIF
				ENDIF ELSE BEGIN
				    Month = StartMonth
					FOR I=FIX(StartTendayOrDay),FIX(EndTendayOrDay) DO BEGIN
						TendayOrDay = STRTRIM(I,2)
						IF I LT 10 THEN TendayOrDay = '0'+STRTRIM(I,2)
;						 AllTime = [AllTime,Year+Month+TendayOrDay]
						 ;===杨绍锷修改，20070830========================
;						  AllTime = [AllTime,Year+Month+TendayOrDay]	;原代码
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

