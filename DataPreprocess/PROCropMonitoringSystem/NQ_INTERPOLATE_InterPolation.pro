


;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;ET�����ֵ,���ľ������׶��޸�
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;������ʾ��Ϣ
FUNCTION NQ_interpolate_ERROR_MESSAGE, theMessage, Error=error, Informational=information, $

   Traceback=traceback, NoName=noname, Title=title, _Extra=extra

On_Error, 2

   ; Check for presence and type of message.

IF N_Elements(theMessage) EQ 0 THEN theMessage = !Error_State.Msg
s = Size(theMessage)
messageType = s[s[0]+1]
IF messageType NE 7 THEN BEGIN
   Message, "The message parameter must be a string.", _Extra=extra
ENDIF

   ; Get the call stack and the calling routine's name.

Help, Calls=callStack
IF Float(!Version.Release) GE 5.2 THEN $
   callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0] ELSE $
   callingRoutine = (Str_Sep(StrCompress(callStack[1])," "))[0]

   ; Are widgets supported?

widgetsSupported = ((!D.Flags AND 65536L) NE 0)
IF widgetsSupported THEN BEGIN

      ; If this is an error produced with the MESSAGE command, it is a trapped
      ; error and will have the name "IDL_M_USER_ERR".

   IF !ERROR_STATE.NAME EQ "IDL_M_USER_ERR" THEN BEGIN

      IF N_Elements(title) EQ 0 THEN title = 'Trapped Error'

         ; If the message has the name of the calling routine in it,
         ; it should be stripped out. Can you find a colon in the string?

      ; Is the calling routine an object method? If so, special processing
      ; is required. Object methods will have two colons together.

      doublecolon = StrPos(theMessage, "::")
      IF doublecolon NE -1 THEN BEGIN

         prefix = StrMid(theMessage, 0, doublecolon+2)
         submessage = StrMid(theMessage, doublecolon+2)
         colon = StrPos(submessage, ":")
         IF colon NE -1 THEN BEGIN

               ; Extract the text up to the colon. Is this the same as
               ; the callingRoutine? If so, strip it.

            IF StrMid(theMessage, 0, colon+StrLen(prefix)) EQ callingRoutine THEN $
               theMessage = StrMid(theMessage, colon+1+StrLen(prefix))
         ENDIF
      ENDIF ELSE BEGIN

         colon = StrPos(theMessage, ":")
         IF colon NE -1 THEN BEGIN

               ; Extract the text up to the colon. Is this the same as
               ; the callingRoutine? If so, strip it.

            IF StrMid(theMessage, 0, colon) EQ callingRoutine THEN $
               theMessage = StrMid(theMessage, colon+1)
         ENDIF

      ENDELSE


         ; Add the calling routine's name, unless NONAME is set.

      IF Keyword_Set(noname) THEN BEGIN
         answer = Dialog_Message(theMessage, Title=title, _Extra=extra, $
            Error=error, Information=information)
      ENDIF ELSE BEGIN
         answer = Dialog_Message(StrUpCase(callingRoutine) + ": " + $
            theMessage, Title=title, _Extra=extra, $
            Error=error, Information=information)
      ENDELSE

   ENDIF ELSE BEGIN

         ; Otherwise, this is an IDL system error.

      IF N_Elements(title) EQ 0 THEN title = 'System Error'

      IF StrUpCase(callingRoutine) EQ "$MAIN$" THEN $
         answer = Dialog_Message(theMessage, _Extra=extra, Title=title, $
            Error=error, Information=information) ELSE $
      IF Keyword_Set(noname) THEN BEGIN
         answer = Dialog_Message(theMessage, _Extra=extra, Title=title, $
            Error=error, Information=information)
      ENDIF ELSE BEGIN
         answer = Dialog_Message(StrUpCase(callingRoutine) + "--> " + $
            theMessage, _Extra=extra, Title=title, $
            Error=error, Information=information)
      ENDELSE
   ENDELSE
ENDIF ELSE BEGIN
      Message, theMessage, /Continue, /NoPrint, /NoName, /NoPrefix, _Extra=extra
      Print, '%' + callingRoutine + ': ' + theMessage
      answer = 'OK'
ENDELSE

   ; Provide traceback information if requested.

IF Keyword_Set(traceback) THEN BEGIN
   Help, /Last_Message, Output=traceback
   Print,''
   Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
   Print, ''
   FOR j=0,N_Elements(traceback)-1 DO Print, "     " + traceback[j]
ENDIF

RETURN, answer
END

pro NQ_interpolate_closes_event,  event
	common_log,'�ر�ũ����ֵ'
	widget_control,  event.top,  /destroy
end



;���ļ���ֵ��ʼ
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;ϵͳ����˵�������ȵ��õ�INTER_Process������
;************************************************************************************
;/////////////////////////////////////////////////////////////////////////////////////////////////
;///////////////////////////////////////      Albers projection        ///////////////////////////
;/////////////////////////////////////////////////////////////////////////////////////////////////
Function AreaOfTrapezium , O
;****************   ����������������������ӳ������ĳһγ��(O)�����  ******************
	a = 6378245.0D                ;************** ��  Բ  ��  ��  ��  *****************
	b = 6356863.0188D             ;************** ��  Բ  ��  ��  ��  *****************
	                              ;��ĸO,������
	e =  sqrt((a*a - b*b)/(a*a))  ;************** ������Բ��һƫ����  *****************
	temp = e*sin(O)
	F = a*a*(1-e*e)*(sin(O)/(2*(1-temp*temp))+(alog((1+temp)/(1-temp)))/(4*e))
	return,F
end
;---------------------------------------
Function Get_R , O
;***********************    ���γȦ�뾶 (;��ĸO,������)     ********************************
	a = 6378245.0D          ;************** ��  Բ  ��  ��  ��  *****************
	b = 6356863.0188D       ;************** ��  Բ  ��  ��  ��  *****************
	e =  sqrt((a*a - b*b)/(a*a)) ;************** ������Բ��һƫ����  *****************
	r = a*cos(O)/sqrt(1-e*e*sin(O)*sin(O))
	return , r
end

FUNCTION Normalize, range, Position=position
	On_Error, 1
	IF N_Params() EQ 0 THEN Message, 'Please pass range vector as argument.'

	IF (N_Elements(position) EQ 0) THEN position = [0.0, 1.0] ELSE $
	    position=Float(position)
	range = Float(range)
	scale = [((position[0]*range[1])-(position[1]*range[0])) / $
	    (range[1]-range[0]), (position[1]-position[0])/(range[1]-range[0])]
	RETURN, scale
END

Function Project , A,p1,S1,South,Latitude,Longitude,MidLongitude,Offet_North,Offet_East
;********************************     Alberts projection for the Elippse   **********************
;********************** The two standard parallels: Fist_Lat and Second_Lat *********************
;**********************          The centre longitude:Centre_Lon            *********************
;********************** The input Latitude and Longitude:Latitude,Longitude *********************
;*************** The input ps is the min value of latitude of the projection area ***************

	S = AreaOfTrapezium(Latitude)

	p = sqrt(p1*p1+2*(S1-S)/(A))
	ps =  sqrt(p1*p1+2*(S1-South)/(A))
	O = A*(Longitude-MidLongitude)
	y = ps - p*cos(O)+Offet_North
	x = p*sin(O)+Offet_East
	Coordinate = {x : x        ,$       ;����(����)����Albers����
	              y : y         $       ;γ��(����)����Albers����
	              }
	return,Coordinate
end
;----------------------------------------------------
function  Albers110_Project,longi $    ;����,�Ƕ���ֵ,���ǻ���ֵ
						   ,lati $     ;γ��,�Ƕ���ֵ,���ǻ���ֵ
						   ,CenterMeridian105 = CenterMeridian105   ;ʹ��105���뾭��.
;*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*---*
;������ʽ:Result =  Albers110_Project(longi,lati,/CenterMeridian105) �ؼ��������¸ղ�ʿ�����ӵ�.
;������ؼ���,�򷵻ص���Albers105ͶӰ.
    lati_ = double(lati)*!dtor
    longi_ = double(longi)*!dtor
    Standard_par1 = 25.00*!dtor    ;��һγ��.  ������йصľ�תΪ����������.��Ȼ����.
    Standard_par2 = 47.00*!dtor    ;�ڶ�γ��

    r1 = Get_R(Standard_par1)             ;��һγ��(25��)��γȦ�뾶
    r2 = Get_R(Standard_par2)             ;�ڶ�γ��(47��)��γȦ�뾶
    S1 = AreaOfTrapezium(Standard_par1)   ;���������һγ��(25��)�����S1
    S2 = AreaOfTrapezium(Standard_par2)   ;��������ڶ�γ��(47��)�����S2
    A = (r1*r1-r2*r2)/(2*(S2-S1))
    p1 = r1/A
;    p2 = r2/A
    South = AreaOfTrapezium(0.00*!dtor)  ;�����(0��)γ�ߵ����South,

    MidLongitude = 110D*!dtor            ;���뾭��110��
    Offet_East = double(4000000.0)        ;��ƫ4000����.
    Offet_North = double(0.0)             ;��ƫ0����

	IF KEYWORD_SET(CenterMeridian105) THEN BEGIN
	    MidLongitude = 105D*!dtor        ;���뾭��105��
	    Offet_East = double(0.0)         ;��ƫ0����.
	ENDIF

    coordinates = Project(A,p1,S1,South,lati_,longi_,MidLongitude,Offet_North,Offet_East)

    return,coordinates


end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;ͨ�ö�ͷ�ļ�����
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
Function ReadEnviFileInfo , FileName

   On_Error, 2
   CATCH, Error_status

   ;This statement begins the error handler:
IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
      ; Handle the error by extending A:
;      A=FLTARR(12)
      CATCH, /CANCEL
ENDIF

;------------------------------------���դ������ļ�����-------------------------------------------

    if FileName eq '' then begin
       return ,{type:0};��ʾû������
    endif

    index = STRPOS(FileName, '.')

    if index eq -1 then begin
     head=FileName+'.hdr'
    endif else begin
     head = STRMID(FileName,0,index)

     ;print,head
     head=head+'.hdr'
    endelse
;--------------------------------------------------------------------------

    ;print,head
    print,head
    if file_test(head) eq 0 then begin
       a = dialog_message('������'+FileName+'�ļ�!')
       return,{type:0}
    end
    openr,lun,head,/get_lun
    result = FSTAT(lun)
    headdata = bytarr(result.size-1)
    readu,lun,headdata
    free_lun,lun
    headfile=string(headdata)


    index0 = STRPOS(headfile, 'samples = ')
    index1 = STRPOS(headfile, 'lines   = ')
    index2 = STRPOS(headfile, 'bands   = ')
    index3 = STRPOS(headfile, 'header offset = ')
    index4 = STRpos(headfile, 'map info = ')
    index5 = STRpos(headfile, 'projection info = ')
    index6 = STRpos(headfile, 'data type = ')
    index7 = STRpos(headfile, 'interleave')
    samples = STRMID(headfile,index0+10,index1-index0-10-1)
    lines =  STRMID(headfile,index1+10,index2-index1-10-1)
    bands=STRMID(headfile,index2+10,index3-index2-10-1)
    print,STRMID(headfile,index6+11,2)
    type = fix(strtrim(STRMID(headfile, index6+11,2),2))
    ;-----------------------��ȡͼ��Ŀ�ʼ����---------------------------------------
    index = STRpos(headfile, 'map info = ')
    lenth = strlen(headfile)
    headfile = strmid(headfile,index,lenth - index-1)
    index = STRpos(headfile, ',')
    lenth = strlen(headfile)
    headfile = strmid(headfile,index+1,lenth - index-1)
    index = STRpos(headfile, ',')
    ulx = strmid(headfile,0,index)

    lenth = strlen(headfile)
    headfile = strmid(headfile,index+1,lenth - index-1)
    index = STRpos(headfile, ',')
    lenth = strlen(headfile)
    headfile = strmid(headfile,index+1,lenth - index-1)
    index = STRpos(headfile, ',')
    ulx=STRMID(headfile,0,index)


    lenth = strlen(headfile)
    headfile = strmid(headfile,index,lenth - index-1)
    index = STRpos(headfile, ',')
    lenth = strlen(headfile)
    headfile = strmid(headfile,index+1,lenth - index-1)
    index = STRpos(headfile, ',')
    uly = strmid(headfile,0,index)
    ulx = float(ulx)
    uly = float(uly)
    ;--------------------------------------------------------------------------------
    xsize=fix(samples)
    ysize=fix(lines)
    bandnum=fix(bands)
    ;-----------------------define the array for the correct data type -----------
	return,{ xsize      :       xsize  ,$      ;����ͼ�Ŀ��
         ysize      :        ysize ,$       ;����ͼ�ĸ߶�
         startX     :         ulx   ,$         ;����ͼ�����Ͻǵĺ�����
         startY     :         uly   ,$         ;����ͼ�����Ͻǵ�������
         bandnum    :      bandnum ,$       ;����ͼ��ͨ������
         type       :        type  }         ;��������
end

;����������
function readweatherdata,pstate,DataType,state
	On_Error, 2
	CATCH, Error_status
   ;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
		CATCH, /CANCEL
	ENDIF
	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	;help,state
    StartX=state.StartX
    StartY=state.StartY
    EndX=state.EndX
    EndY=state.EndY
case DataType of
		0: begin  ;�ս�ˮ
           qField = '.VAL'
        end
		1: begin  ;����ʱ��
           qField = '.VALUE'
        end
		2: begin  ;���ʪ��
           qField = '.VAL'
        end
        else: begin
        	print,'Error>>�����ս�ˮ������ʱ����Ѯ������ѡ��һ��'
        	state={flagnum:0}
      		RETURN,state
        end
    endcase
;   	qField = '.VALUE'
   	print,'�����ݿ�,DataType=',DataType
	case DataType of
		0: begin  ;�ս�ˮ
           qIndex = 0
        end
		1: begin  ;����ʱ��
           qIndex = 1
        end
		2: begin  ;Ѯ����
           qIndex = 2
        end
        else: begin
        	print,'Error>>�����ս�ˮ������ʱ����Ѯ������ѡ��һ��'
        	state={flagnum:0}
      		RETURN,state
        end
    endcase
   	aveqField = '.VALUE AVG(VALUE)'
	case DataType of
		0: begin  ;�ս�ˮ
           isday = '.tenday'    ; isday���������������ʾ�������ĳ��field��Ϊtenday����day
        end
		1: begin  ;����ʱ��
           isday = '.tenday' ; isday���������������ʾ�������ĳ��field��Ϊtenday����day
        end
		2: begin  ;Ѯ����
           isday = '.tenday' ; isday���������������ʾ�������ĳ��field��Ϊtenday����day
        end
        else: begin
        	print,'Error>>����������¡�������¡����ʪ�ȡ�����ѹ��ƽ�����١�����ʱ����ѡ��һ��'
        	state={flagnum:0}
      		RETURN,state
        end
    endcase
;	TABLES=['METEO_DATA_DAY',$
;       'SUNSHINE_TIME_TENDAY',$
;       'METEO_TENDAY_TEMP_0_ACCUMULATE']
	TABLES=['AGRO_TENDAY_RAINFALL',$
       'SUNSHINE_TIME_TENDAY',$
       'METEO_TENDAY_TEMP_0_ACCUMULATE']
    ;������Ӧ���ֶ�
    ; TYPES=['HU_RA','SUNT','ACCUMULATE'];HU_RA��ʵ��Ӧ�ø�ΪPPT
    if (*pstate).point eq 0 then begin
	    print,"ʵʱ��ֵ"
	    ;���ò�ѯ���
	    year=fix(state.year)
	    month=fix(state.month)
		tenday=fix(state.tenday)
		strQuery = 'SELECT METEO_STATION_INFO.CODE, METEO_STATION_INFO.LONGITUDE,'
		strQuery += 'METEO_STATION_INFO.LATITUDE, METEO_STATION_INFO.ALTITUDE,'
		strQuery += ''+TABLES[qIndex]+''+qField+' as [Value]'
	;	strQuery += TABLES[qIndex] + qField
		strQuery += ' FROM METEO_STATION_INFO,' + TABLES[qIndex]
		strQuery += ' WHERE METEO_STATION_INFO.CODE=' + TABLES[qIndex] + '.STATION_ID'
		strQuery += ' and ' + TABLES[qIndex] + '.YEAR='+STRTRIM(year,2)+' AND ' + TABLES[qIndex] + '.MONTH='+STRTRIM(month,2)+''
		strQuery += ' AND ' + TABLES[qIndex] + ''+ isday +'='+STRTRIM(tenday,2)+''
		strQuery+=' And '+TABLES[qIndex]+''+qField+' >=0 and '+TABLES[qIndex]+''+qField+' <> 9999'
		print,strQuery

;	    year=fix(state.year)
;	    month=fix(state.month)
;		tenday=fix(state.tenday)
;		strQuery = 'SELECT METEO_STATION_INFO.CODE, METEO_STATION_INFO.LONGITUDE,'
;		strQuery += 'METEO_STATION_INFO.LATITUDE, METEO_STATION_INFO.ALTITUDE,'
;		strQuery += 'METEO_DATA_DAY'+qField+' as [Value]'
;	;	strQuery += TABLES[qIndex] + qField
;		strQuery += ' FROM METEO_STATION_INFO,METEO_DATA_DAY'
;		strQuery += ' WHERE METEO_STATION_INFO.CODE=METEO_DATA_DAY.STATION_ID'
;		strQuery += ' and METEO_DATA_DAY.YEAR='+STRTRIM(year,2)+' AND METEO_DATA_DAY.MONTH='+STRTRIM(month,2)+''
;		strQuery += ' AND METEO_DATA_DAY'+ isday +'='+STRTRIM(tenday,2)+''
;		print,strQuery
    ENDIF ELSE BEGIN

	    print,'�����Ƕ���ʷʵʱƽ�����ݽ��в�ֵ'
	    year=fix(state.year)
	    month=fix(state.month)
		tenday=fix(state.tenday)
		nianxian = widget_info((*pstate).drop_nianxian, /COMBOBOX_GETTEXT)
		endyear=year+nianxian-1
		strQuery = 'SELECT METEO_STATION_INFO.CODE, METEO_STATION_INFO.LONGITUDE,'
		strQuery += 'METEO_STATION_INFO.LATITUDE, METEO_STATION_INFO.ALTITUDE,'
	;	strQuery += 'AVG('+TABLES[qIndex]+'.VALUE) as [Value]'
        strQuery += 'AVG('+TABLES[qIndex]+''+qField+') as [Value]'
		strQuery += ' FROM METEO_STATION_INFO,' + TABLES[qIndex]
		strQuery += ' WHERE METEO_STATION_INFO.CODE=' + TABLES[qIndex] + '.STATION_ID'
;		strQuery += ' and ' + TABLES[qIndex] + '.YEAR=''' + string(state.year) + ''' AND ' + TABLES[qIndex] + '.MONTH=''' + string(state.month) + ''''
        strQuery += ' and ' + TABLES[qIndex] + '.YEAR between '+STRTRIM(year,2)+' and '+STRTRIM(endyear,2)+' AND ' + TABLES[qIndex] + '.MONTH='+STRTRIM(month,2)+''
		strQuery += ' AND ' + TABLES[qIndex] + ''+ isday +'=' + STRTRIM(tenday,2)+''
		strQuery +=' group by CODE,LONGITUDE,LATITUDE,ALTITUDE'
		print,strQuery
	endelse
	;�����ݿ�
	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	od = DBobj
	;od=(*pstate).dbco
	;print,'������������'
	oRS = OBJ_NEW('IDLdbRecordset', oD, SQL=strQuery)
	;-----------------------------------------------------------------
	;���ȼ������ݵ�����
	isend=1
	total = long(0)
;-------------------------------------------------------------------------

	IF(oRS->MoveCursor(/FIRST) EQ 1) THEN BEGIN
		while isend do begin
			data = oRS->GetRecord()
			isend = oRS->MoveCursor(/Next)
;			if NOT array_equal(data[0].Value,'',/NO_TYPECONV) and data[0].Value ge 0 then begin
           		total = total + 1
;			endif
		endwhile
        print,"total data: ",total
	Endif else begin
            Text=DIALOG_MESSAGE('���ݿ�����ʱû����Ҫ�����ݡ�',/information,title='��ʾ')
		    state={flagnum:0}
      		RETURN,state
	Endelse

	;return,state
    ;��ֵ������
	index = 0
	a = oRS->MoveCursor(/First)
	CstaId = sindgen(total)    ;վ��
	Cvalue = FLTARR(total)    ;��վת����ֵ
	CvalueOrgi = FLTARR(total) ;��վֵԭʼ��ֵ
	Clanti = FLTARR(total)   ;γ��
	Clongid = FLTARR(total)  ;����
	Ceve = FLTARR(total)    ;�߳�
	Ccountry = sindgen(total)        ;����
	flagnum=0
	;���Խ��������ֵ
	IF(oRS->MoveCursor(/FIRST) EQ 1)THEN BEGIN
	;	if  index lt total and data[0].value ge 0 then begin
	   ;if  index lt total  then begin
	   while  index lt total   do begin
;	       if data[0].value ge 0 then begin
				data = oRS->GetRecord()
				isend = oRS->MoveCursor(/Next)
				CstaId[flagnum] = data[0].CODE
			    Clanti[flagnum] = data[0].LATITUDE
			    Clongid[flagnum] = data[0].LONGITUDE
		    	Ceve[flagnum] = data[0].ALTITUDE
		        Ccountry[flagnum] = 0
				Cvalue[flagnum] = (data[0].Value)
				flagnum=flagnum+1
				index=index+1
;		   Endif
	   endwhile
	ENDIF
	obj_destroy,oRS

	print,'flagnum',flagnum
	;Ϊ���޳��쳣ֵ
	TempCstaId=CstaId
	TempCvalue=Cvalue
	TempCvalueOrgi=CvalueOrgi
	TempClanti=Clanti
	TempClongid =Clongid
	TempCeve =Ceve
	TempCcountry =Ccountry
	;
	CstaId=sindgen(flagnum)    ;վ��
	Cvalue=FLTARR(flagnum)    ;��վת����ֵ
	CvalueOrgi=FLTARR(flagnum) ;��վֵԭʼ��ֵ
	Clanti=FLTARR(flagnum)    ;γ��
	Clongid=FLTARR(flagnum)   ;����
	Ceve=FLTARR(flagnum)     ;�߳�
	Ccountry=sindgen(flagnum);����
	;
	CstaId=TempCstaId[0:flagnum-1]
	Cvalue=TempCvalue[0:flagnum-1]
	CvalueOrgi=TempCvalueOrgi[0:flagnum-1]
	Clanti=TempClanti[0:flagnum-1]
	Clongid=TempClongid[0:flagnum-1]
	Ceve=TempCeve[0:flagnum-1]
	Ccountry=TempCcountry[0:flagnum-1]
	;
	TempCstaId=0
	TempCvalue=0
	TempCvalueOrgi=0
	TempClanti=0
	TempClongid =0
	TempCeve =0
	TempCcountry =0
	;
	state={CstaId:CstaId,Clanti:Clanti,Clongid:Clongid,Ceve:Ceve,Ccountry:Ccountry,CvalueOrgi:CvalueOrgi,Cvalue:Cvalue,flagnum:flagnum}
	RETURN,state
;	HEAP_GC, /VERBOSE
end

;�������ݽ�ϣ�ͬʱ�������ͶӰ�仯������������������������������������
;+++++++++++++++++++++++++++++++++++++++++
function DataTogether,pstate,datatype
	On_Error, 2
	CATCH, Error_status
	;This statement begins the error handler:
   	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
      	PRINT, 'Error message: ', !ERROR_STATE.MSG
      	OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
      	CATCH, /CANCEL
   	ENDIF
	;
	;�õ�������
;	widget_control, (*pstate).SelDate, get_value=SelDate
;WIDGET_CONTROL,Event.top,get_UVALUE = pstate
year = widget_info((*pstate).drop_year,  /COMBOBOX_GETTEXT)
month = widget_info((*pstate).drop_month, /COMBOBOX_GETTEXT)
tenday = widget_info((*pstate).drop_tenday, /COMBOBOX_GETTEXT)
if tenday eq '��' then tenday='1' else $
if tenday eq '��' then tenday='2' else $
if tenday eq '��' then tenday='3'

    startx=4167169
    starty=4624237
    endx=4826169
    endy=3741237
	;��������µķ���ֵ
	state={YEAR:YEAR,MONTH:MONTH,tenDAY:tenDAY,StartX:StartX,StartY:StartY,EndX:EndX,EndY:EndY}
	;print,state
	Cdata=readweatherdata(pstate,datatype,state)
	if Cdata.flagnum eq 0 then begin
		;û�ж�������
		print,'û�ж�������'
		return,{Error:1}
	endif
	;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ;ͳ�ƹ��ж��ٸ�����
	Cnum=Cdata.flagnum
	plusnum=long(Cnum)
	print,"ͳ�ƹ��ж��ٸ�����:",plusnum
	staId3 = sindgen(plusnum)
	lanti3 = FLTARR(plusnum)
	longid3 = FLTARR(plusnum)
	eve3 = FLTARR(plusnum)
	value = FLTARR(plusnum)
	Coun3 = sindgen(plusnum)
	valueOrgi3 = FLTARR(plusnum)
	;
	FOR j=0,Cnum-1 DO BEGIN
		staId3[j] = (Cdata.CstaId)[j]
		lanti3[j] = (Cdata.Clanti)[j]
		longid3[j] = (Cdata.Clongid)[j]
		eve3[j] = (Cdata.Ceve)[j]
		value[j] = (Cdata.CValue)[j]
		valueOrgi3[j] = (Cdata.CvalueOrgi)[j]
		Coun3[j] = (Cdata.Ccountry)[j]
	ENDFOR
	stationNum=N_ELEMENTS(staId3)

	;##########################
	;����������¡�������¡��ʹ���ѹ���и߳�����
	if DataType eq 3 then begin
		print,'����ѹ���ݸ߳�����'
		;print,value
		value /= (1 - eve3/44331.)^(1/0.1903)
		;print,value
	endif
	if DataType eq 6 or DataType eq 7 then begin
		print,'�¶����ݸ߳�����'
		value += 6.5 * eve3/100.;��ʱ�¶ȵĵ�λΪ0.1��
	endif
	;�߳���������

    ;****************************
    ;����������ݽ���ͶӰת��
    ;�Ѿ�γ��ת��Ϊ���� �����ǲ���ͶӰ�Ƿ���ȷ�ļ��㹫ʽ
    ;       lati_i=3.1415926/180* 36.7393583
    ;       longi_i=3.1415926/180*102.009986

    print,'ͶӰת��'
    lanti = lanti3
    longid = longid3
    coor = Albers110_Project(longid,lanti)
    y = (coor.y)  ;γ�� ������Ϊ��λ�����Գ���1000M
    x = (coor.x) ;���ȡ�������Ϊ��λ��
    ;�����γ�������������Բ�ֵ����
    num=N_ELEMENTS(x)
	Xnum=0
	FOR j=0,num-1 DO BEGIN
		if ( x[j] gt 0  ) then begin
        	Xnum=Xnum+1
       	endif
	ENDFOR
    laststaId=sindgen(Xnum)
    lastX=FLTARR(Xnum)
    lastY=FLTARR(Xnum)
    lasteve=FLTARR(Xnum)
    lastvalue=FLTARR(Xnum)
    lastCoun=sindgen(Xnum)
    lastvalueOrgi=FLTARR(Xnum)
    Xnumindex=0
    ;
	FOR j=0,num-1 DO BEGIN
		if ( x[j] gt 0  ) then begin
	        laststaId[Xnumindex]=staId3[j]
    	    lastY[Xnumindex]=y[j]
        	lastX[Xnumindex]=X[j]
	        lasteve[Xnumindex]= eve3[j]
    	    lastvalue[Xnumindex]= value[j]
        	lastvalueOrgi[Xnumindex]= valueOrgi3[j]
        	Coun3[Xnumindex]=Coun3[j]
        	Xnumindex=Xnumindex+1
       	endif
	ENDFOR
	staId3=0
    lanti3=0
    longid3=0
    eve3=0
    value=0
    valueOrgi3=0
    Coun3=0
	return,{ lastX:lastX, lastY:lastY, lastvalue:lastvalue, Error:0 }
END

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;���в�ֵ
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
function InterpolEvent,pstate

  	On_Error, 2
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
		CATCH, /CANCEL
	ENDIF
	;��ֵҪ������
	Datatype = widget_info((*pstate).meteoFeature, /droplist_select)
	;����ʧ������µķ���ֵ
	state = { Error : 1 }
	if (*PSTATE).read_method eq 2 then begin
		;print,'���ݿⷽʽ'
		if (*PSTATE).dbco_id eq 0 then begin
			TEXT=DIALOG_MESSAGE('����û������,�����Ӻ�����',/information)
			return,state
		endif
		print,'test'
	    INterdata=DataTogether(pstate, Datatype)
	    if INterdata.Error eq 1 then begin
			;û�ж�������
			state.Error = 1
			return,state
		endif
		x = INterdata.lastx
		y = INterdata.lasty
		Value = INterdata.lastValue
		print,'�������ת��'
		;print,x,y,Value
	endif
	;��ֵ�߽�Ĵ������

;    ulx=4780500.0000
;    uly=5866500.0000
;    lrx=5849500.0000
;    lry=4840500.0000

	ParaInfo = (*pstate).NQ_ProjectPara    ;�õ�ʡ��Χ�ڵ�ͶӰ����

    ProviceName = (*pstate).province[WHERE((*pstate).proIDlist EQ (*pstate).proID)]

	IF SIZE(ParaInfo,/TYPE) NE 8 THEN BEGIN     ;�����صĲ��ǽṹ��,���ǿ�.
	   PRMPT = DIALOG_MESSAGE('û��'+ProviceName+'�Ļ���������Ϣ,��鿴��Ӧ�Ĳ��������ļ�!',TITLE='����')
 		state.Error = 1
		return,state
	ENDIF
    ulx=float(ParaInfo.ulx)
    uly=float(ParaInfo.uly)
    lrx=float(ParaInfo.lrx)
    lry=float(ParaInfo.lry)


    widget_control,(*PSTATE).reso,get_value=reso

    flreso=float(reso)
    if flreso lt 300 or flreso gt 1000 then begin
        TEXT=DIALOG_MESSAGE('�ֱ��ʱ�����300-1000��!')
		state.Error = 1
		return,state
    endif


    xsize=fix((lrx[0]-ulx[0])/reso)
    ysize=fix((uly[0]-lry[0])/reso)
    ulx=ulx[0]
    uly=uly[0]
    lrx=lrx[0]
    lry=lry[0]
    (*pstate).xsize=xsize
    (*pstate).ysize=ysize

	xbox = [ulx, ulx, lrx, lrx]
	ybox = [uly, lry, uly, lry]
	;
	Result = MIN((x[*]-ulx)^2+(y[*]-uly)^2,index)
	v1 = value[index]
	Result = MIN((x[*]-ulx)^2+(y[*]-lry)^2,index)
	v2 = value[index]
	Result = MIN((x[*]-lrx)^2+(y[*]-uly)^2,index)
	v3 = value[index]
	Result = MIN((x[*]-lrx)^2+(y[*]-uly)^2,index)
	v4 = value[index]
	vbox = [v1,v2,v3,v4]
	;���ɸ���ͼ���������ֵ
    ;ȥ����������ĵ�
	;ȥ��������������ĵ�
	xx = [x[*], xbox]
	yy = [y[*], ybox]
	vv = [Value[*], vbox]
	innerIndex = where(xx[*] ge ulx and xx[*] le lrx and yy[*] ge lry and yy[*] le uly)

    StationX1 = (x - ulx)/xsize
	StationY1 = (y - lry)/ysize
    StationX=StationX1/1000
    StationY=StationY1/1000

	;ȥ�ظ��ĵ�
	GRID_INPUT, xx[innerIndex], yy[innerIndex], vv[innerIndex], xSorted, ySorted, dataSorted
	numbers=N_ELEMENTS(xSorted)
	if numbers eq 4 then begin
	   Text=DIALOG_MESSAGE('���ݿ�����ʱû����Ҫ�����ݡ�',/information,title='��ʾ')
		state.Error = 1
		return,state
	endif
	;�õ���ֵ��ʵ���к��е���
	Xnumbers=xsize
	Ynumbers=ysize
   	;��ֵ����
	gridSize = [Xnumbers, Ynumbers]
	;�õ���ֵ�Ĳ���ͬʱ���в�ֵ
	TRIANGULATE, xSorted, ySorted, tr
	;ͼ����������꣬����ƥ����仯
	;�õ���ֵ����������
	method_index = widget_info((*pstate).method_index, /droplist_select)
	IF method_index EQ '' then begin
		method_index = 0
	ENDIF
	;print,'��ֵ����:',method_index
	case  method_index of
		0 : BEGIN  ;IDW����
			;����������IDW��ֵ�Ĳ���$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
			powerhvalue=(*pstate).IDW_power
			Smoothvalue=(*pstate).IDW_smooth
			IDW_Anisotropy=(*pstate).IDW_Anisotropy
			IDW_Ellipse=(*pstate).IDW_Ellipse
			IDW_ShapeValue=(*pstate).IDW_ShapeValue
			IDW_Minpoints=(*pstate).IDW_Minpoints
			IDW_Maxpoints=(*pstate).IDW_Maxpoints
			;missingdata=(*pstate).Missingvalue
           	;print,"powerhvalue",powerhvalue,"Smoothvalue",Smoothvalue;,missingdata
          	if IDW_Minpoints eq 8 and IDW_Maxpoints eq 15 then begin
				grid = GRIDDATA(xSorted, ySorted, dataSorted, $
               	POWER=powerhvalue,SMOOTHING=Smoothvalue,missing=NAN,$
               	DIMENSION = gridSize, METHOD = 'InverseDistance',$
               	sectors=IDW_ShapeValue,ANISOTROPY=IDW_Anisotropy)
			endif else begin
				grid = GRIDDATA(xSorted, ySorted, dataSorted, $
               	POWER=powerhvalue,SMOOTHING=Smoothvalue,missing=NAN,$
               	DIMENSION = gridSize, METHOD = 'InverseDistance',$
               	sectors=IDW_ShapeValue,ANISOTROPY=IDW_Anisotropy,$
               	MIN_POINTS = IDW_Minpoints ,MAX_PER_SECTOR=IDW_Maxpoints,TRIANGLES = tr,$
               	SEARCH_ELLIPSE=IDW_Ellipse )
            endelse
			print,'IDW Over'
		END
        1 : BEGIN
			grid =MIN_CURVE_SURF(dataSorted,xSorted, ySorted, /TPS, NX=Xnumbers, NY=Ynumbers)
			print,'Spline Over'
		END
        2 : BEGIN
			;WIDGET_CONTROL, (*pstate).AnisotropyAngle, GET_VALUE = AnisotropyAngle
			;WIDGET_CONTROL, (*pstate).AnisotropyX, GET_VALUE = AnisotropyX
			;WIDGET_CONTROL, (*pstate).AnisotropyY, GET_VALUE = AnisotropyY
			;ANISOTROPY = [AnisotropyX, AnisotropyY, AnisotropyAngle]
			print,'klg'
            KShapeValue=(*pstate).Kring_ShapeValue
            KAnisotropy=(*pstate).Kring_Anisotropy
            Kminpoints= (*pstate).Kring_MinpointsID
			;Maxpoints=15
            Kmissingvalue=MIN(dataSorted)
			;VARIOGRAM = [2,6024.0,0.0,1.0]
            KVARIOGRAM =(*pstate).Kring_Variogram
            KMaxpoints=(*pstate).Kring_MinpointsID
			if Kminpoints eq 8 and KMaxpoints eq 15 then begin
				grid = GRIDDATA(xSorted, ySorted, dataSorted, $
             	DIMENSION = gridSize, METHOD = 'Kriging', $
             	sectors=KShapeValue,ANISOTROPY=KAnisotropy,$
             	missing=Kmissingvalue )
             	;VARIOGRAM[0] =2 ,$
            	;VARIOGRAM[2] =KVARIOGRAM[2],$
             	;VARIOGRAM[3] =KVARIOGRAM[3],$
				;MIN_POINTS = Kminpoints ,MAX_PER_SECTOR=KMaxpoints
          	endif else begin
				print,'test'
				grid = GRIDDATA(xSorted, ySorted, dataSorted, $
				DIMENSION = gridSize, METHOD = 'Kriging', $
	            sectors=KShapeValue,ANISOTROPY=KAnisotropy,$
    	        MIN_POINTS = Kminpoints ,MAX_PER_SECTOR=KMaxpoints,TRIANGLES = tr,$
             	missing=Kmissingvalue )
          	endelse
            print,'Kriging over'
			;ANISOTROPY=ANISOTROPY,$
			;missing=NAN,VARIOGRAM =VARIOGRAM)
			;VARIOGRAM =KVARIOGRAM
			END
		3 : BEGIN
			grid = GRIDDATA(xSorted, ySorted, dataSorted, $
			DIMENSION = gridSize, METHOD = 'Linear' ,$
			;POWER=powerhvalue,SMOOTHING=Smoothvalue,$
         	;sectors=(*pstate).ShapeValue,ANISOTROPY=ANISOTROPY,$
			;MIN_POINTS = minpoints ,MAX_PER_SECTOR=Maxpoints,$
			TRIANGLES = tr,$
			missing=NAN)
			print,'Linear over'
        END
		4 : BEGIN
			grid = GRIDDATA(xSorted, ySorted, dataSorted, $
			DIMENSION = gridSize, METHOD = 'NearestNeighbor',$
			;POWER=powerhvalue,SMOOTHING=Smoothvalue,$
			;sectors=(*pstate).ShapeValue,ANISOTROPY=ANISOTROPY,$
			;MIN_POINTS = minpoints ,MAX_PER_SECTOR=Maxpoints,$
			TRIANGLES = tr,$
			missing=NAN)
			print,'NearestNeighbor over'
		END
		5 : BEGIN ;����ʽ�ع��ֵ
			PREGR_power=(*pstate).PREG_power
			;Smoothvalue=(*pstate).IDW_smooth
           	PREGR_Anisotropy=(*pstate).PREG_Anisotropy
           	PREGR_Ellipse=(*pstate).PREG_Ellipse
           	PREGR_ShapeValue=(*pstate).PREG_ShapeValue
           	PREGR_Minpoints=(*pstate).PREG_Minpoints
           	PREGR_Maxpoints=(*pstate).PREG_Maxpoints
			;PREG_Anisotropy :PREG_Anisotropy , $
			;PREG_Anisotropy :PREG_Ellipse   ,$
			;PREG_ShapeValue:1 ,$
			;PREG_Minpoints:8  ,$
			;PREG_Maxpoints:15    ,$
			;PREG_power:2 $
			;
			;Anisotropy=[1.0,1.0,0.0]
			;ShapeValue=1
			;powerhvalue=2
			;Maxpoints=15
			;minpoints=10
         	if PREGR_Minpoints eq 8 and PREGR_Maxpoints eq 15 then begin
				grid = GRIDDATA(xSorted, ySorted, dataSorted, $
				DIMENSION = gridSize, METHOD = 'PolynomialRegression',$
             	POWER=PREGR_power,$
    			;SMOOTHING=Smoothvalue,$
             	sectors=PREGR_ShapeValue,ANISOTROPY=PREGR_Anisotropy,$
              	;MIN_POINTS = minpoints ,MAX_PER_SECTOR=Maxpoints,TRIANGLES = tr,$
             	missing=NAN)
         	endif else begin
				grid = GRIDDATA(xSorted, ySorted, dataSorted, $
             	DIMENSION = gridSize, METHOD = 'PolynomialRegression',$
             	POWER=PREGR_power,$
    			;SMOOTHING=Smoothvalue,$
             	sectors=PREGR_ShapeValue,ANISOTROPY=PREGR_Anisotropy,$
    			;MIN_POINTS = minpoints ,MAX_PER_SECTOR=Maxpoints,TRIANGLES = tr,$
             	missing=NAN,$
             	MIN_POINTS = PREGR_Minpoints ,MAX_PER_SECTOR=PREGR_Maxpoints,TRIANGLES = tr,$
             	SEARCH_ELLIPSE=PREGR_Ellipse )
         	endelse
			print,'NearestNeighbor over'
		END
		6 : BEGIN
			FUN_TYPE=(*pstate).RBFvalue[0]
			;Smoothvalue=(*pstate).IDW_smooth
			RBF_Anisotropy=(*pstate).RBF_Anisotropy
			RBF_Ellipse=(*pstate).RBF_Ellipse
			RBF_ShapeValue=(*pstate).RBF_ShapeValue
			RBF_Minpoints=(*pstate).RBF_Minpoints
			RBF_Maxpoints=(*pstate).RBF_Maxpoints
			if RBF_Minpoints eq 8 and RBF_Maxpoints eq 15 then begin
			 	grid = GRIDDATA(xSorted, ySorted, dataSorted, $
			  	DIMENSION = gridSize, METHOD = 'RadialBasisFunction',$
			  	FUNCTION_TYPE =FUN_TYPE,missing=NAN,$
			  	sectors=RBF_ShapeValue,ANISOTROPY=RBF_Anisotropy )
			endif else begin
			 	grid = GRIDDATA(xSorted, ySorted, dataSorted, $
			  	DIMENSION = gridSize, METHOD = 'RadialBasisFunction',$
			  	FUNCTION_TYPE =FUN_TYPE,missing=NAN,$
			  	sectors=RBF_ShapeValue,ANISOTROPY=RBF_Anisotropy,$
			  	MIN_POINTS = RBF_Minpoints ,MAX_PER_SECTOR=RBF_Maxpoints,TRIANGLES = tr)
			endelse
			print,'RadialBasisFunction Over'
		END
		else : begin
		 	print,'û�����ֲ�ֵ��ʽ'
		 	return,state
		end
	endcase
	print,"grid"
;	;--------------�ӱ߽�------------------------------------------
    widget_control,(*PSTATE).reso,get_value=reso
    ;*************************************************
;    filebianjie='data_grid\province_new'
;    Openr,Lun_file_border,filebianjie,/get_lun
;    file_border     =   bytarr(1069,1026)
;    if reso eq '1000' then begin
;        READU, Lun_file_border  	, file_border
;    endif else begin
;    	READU, Lun_file_border  	, file_border
;   	    file_border=congrid(file_border,xsize,ysize)
;    endelse
;	    grid=temporary(grid)*float(file_border)
;	    free_lun,Lun_file_border
;	    file_border=0B
	;*************************************************

	;--------------�ӱ߽�end------------------------------------------
	;******************************************************************************
	;��ֵ����,����Ӱ��ת
	;
	print,'Ӱ��ת'
	Xnumbers = fix(Xnumbers[0])
	Ynumbers = fix(Ynumbers[0])
    real_value=fltarr(xsize,ysize)


	real_value = reverse(TEMPORARY(grid), 2)
;	(*pstate).data=ptr_new(real_value)
;	*((*pstate).data)=real_value
	print,'test'
	(*pstate).ix = min(xSorted)
	(*pstate).iy = max(ySorted)
	print,'ix, iy: ',(*pstate).ix,(*pstate).iy
	;���ò���
	return,{ OriValue : Value, StationX : StationX, StationY : StationY, Error : 0, real_value : real_value }
 END

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;��ʾͼ��
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;PRO draw_widget_data_event, ev
;	On_Error, 1
;    image=data
;    (*pstate).ImageData = ptr_new(image)
;	WIDGET_CONTROL, (*pstate).DrawID, GET_VALUE=owindow
;  	WIDGET_CONTROL, ev.TOP, GET_UVALUE=stash
;  	IF (TAG_NAMES(ev, /STRUCTURE_NAME) eq 'WIDGET_DRAW') THEN BEGIN
;    	WIDGET_CONTROL, stash.label1, $
;      	SET_VALUE='X position: ' + STRING(ev.X)
;    	WIDGET_CONTROL, stash.label2, $
;      	SET_VALUE='Y position: ' + STRING(ev.Y)
;    	WIDGET_CONTROL, stash.label3, $
;      	SET_VALUE='Hex Value: ' + $
;      	;ע��˴����÷�,�������Ӱ��ֵ.
;      	STRING((*stash.imagePtr)[ev.X, ev.Y], FORMAT='(Z12)')
;		HELP,EV,/STR
;  	ENDIF
;  ; If the event is generated in a button, destroy the widget
;  ; hierarchy. We know we can use this simple test because there
;  ; is only one button in the application.
;
;  IF (TAG_NAMES(ev, /STRUCTURE_NAME) eq 'WIDGET_BUTTON') THEN BEGIN
;    WIDGET_CONTROL, ev.TOP, /DESTROY
;  ENDIF

;END

; Widget creation routine.
;PRO draw_widget_data,image
;
;  ; Define a monochrome image array for use in the application.
;  ;READ_PNG, FILEPATH('mineral.png',SUBDIR=['examples', 'data']), image
;
;  ; Place the image array in a pointer heap variable, so we can
;  ; pass the pointer to the event routine rather than passing the
;  ; entire image array.
;  help,image,/stru
;  imagePtr=PTR_NEW(image, /NO_COPY)  ;��ν���ָ��
;
;  ; Retrieve the size information from the image array.
;  im_size=SIZE(*imagePtr)   ;ע������ʹ��SIZE���������á�
; help,im_size,/STRUCTURE
; print,im_size
;  ; Create a base widget to hold the application.
;  base = WIDGET_BASE(/COLUMN)
;
;  ; Create a draw widget based on the size of the image, and
;  ; set the MOTION_EVENTS keyword so that events are generated
;  ; as the cursor moves across the image. Setting the BUTTON_EVENTS
;  ; keyword rather than MOTION_EVENTS would require the user to
;  ; click on the image before an event is generated.
;  draw = WIDGET_DRAW(base, XSIZE=im_size[1], YSIZE=im_size[2], $
;    /MOTION_EVENTS) ;ע�����/MOTION_EVENTS�ؼ���.
;
;  ; Create 'Done' button.
;  button = WIDGET_BUTTON(base, VALUE='Done')
;
;  ; Create label widgets to hold the cursor position and
;  ; Hexadecimal value of the pixel under the cursor.
;  label1 = WIDGET_LABEL(base, XSIZE=im_size[1]*.9, $
;    VALUE='X position:')
;  label2 = WIDGET_LABEL(base, XSIZE=im_size[1]*.9, $
;    VALUE='Y position:')
;  label3 = WIDGET_LABEL(base, XSIZE=im_size[1]*.9, $
;    VALUE='Hex Value:')
;
;  ; Realize the widget hierarchy.
;  WIDGET_CONTROL, base, /REALIZE
;
;  ; Retrieve the widget ID of the draw widget. Note that the widget
;  ; hierarchy must be realized before you can retrieve this value.
;  WIDGET_CONTROL, draw, GET_VALUE=drawID
;
;  ; Create an anonymous array to hold the image data and widget IDs
;  ; of the label widgets.
;  stash = { imagePtr:imagePtr, label1:label1, label2:label2, $
;            label3:label3 }
;
;  ; Set the user value of the top-level base widget equal to the
;  ; 'stash' array.
;  WIDGET_CONTROL, base, SET_UVALUE=stash
;
;  ; Make the draw widget the current IDL drawable area.
;  WSET, drawID
;
;  ; Draw the image into the draw widget.
;  TVSCL, *imagePtr
;
;  ; Call XMANAGER to manage the widgets.
;  XMANAGER, 'draw_widget_data', base, /NO_BLOCK
;
;END

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;����ͼ��

;��������  ·��(*PSTATE).TEXT_OUTPUT_FILE

function SaveResult,FILE,datatype,SaveData,xsize,ysize,ix,iy;,YEAR,MONTH,DAY

   On_Error, 2
   CATCH, Error_status

   ;This statement begins the error handler:
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)

      CATCH, /CANCEL
   ENDIF

      WIDGET_CONTROL,/HOURGLASS

      print,"FILE",FILE,"fffffffffffffffffffffffffffffffff"
      ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ;��������  ·��(*PSTATE).TEXT_OUTPUT_FILE

        enter=string(byte(13))+string(byte(10))
        time=systime()
        col =STRTRIM(xsize,2)
        line=STRTRIM(ysize,2)
        tulx=STRTRIM(ix,2)
        tuly=STRTRIM(iy,2)



        NDWI_head='ENVI'+enter+$
        'description = {' + enter+ $
        '  Create New File Result ['+time+']}'+enter+$
        'samples = '+col+enter+$
        'lines   = '+line+enter+$
        'bands   = 1'+enter+$
        'header offset = 0'+enter+$
        'file type = ENVI Standard'+enter+$
        'data type = 4'+enter+$
        'interleave = bsq'+enter+$
        'sensor type = Unknown'+enter+$
        'byte order = 0'+enter+$
        'map info = {Albers Conical Equal Area, 1.0000, 1.0000, ' +tulx+', ' +tuly+',1.0000000000e+004, 1.0000000000e+004, Krasovsky, units=Meters}'+enter+$
        'projection info = {9, 6378245.0, 6356863.0, 0.000000, 110.000000, 4000000.0, 0.0, 25.000000, 47.000000, Krasovsky, Albers Conical Equal Area, units=Meters}'+enter+$
        'wavelength units = Unknown'+enter+$
        'band names = {'+enter+$
        'Inter_result}'+enter

         NDWI_result=FILE

         print,"NDWI_result",NDWI_result

         NDWI_headfile=NDWI_result+'.hdr'

           print,"NDWI_headfile",NDWI_headfile

         openw,lun,NDWI_headfile,/get_lun
         writeu,lun,NDWI_head
         free_lun,lun
         openw,lun,NDWI_result
         writeu,lun,float(SaveData)
         free_lun,lun
         SaveData=0
         NDWI_head=0

end

;function DataTEST,pstate,DataType,state,DEMSentive
;	DataFlag=0 ;����ָʾ�ǲ���������
;	readweatherdataindex=readweatherdata_test(pstate,DataType,state,DEMSentive)
;	if readweatherdataindex eq 1 then begin
;		DataFlag=1
;	endif else begin
;		DataFlag=0
;	endelse
;	return,DataFlag
;end

;#####################################################################################################
;��ֵ����,��ֵ����;###################################################################################
;#####################################################################################################
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;�����ļ�
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;FUNCTION INTER_SAVE, EVENT
;	On_Error, 2
;	CATCH, Error_status
;   	;This statement begins the error handler:
;	IF Error_status NE 0 THEN BEGIN
;		PRINT, 'Error index: ', Error_status
;		PRINT, 'Error message: ', !ERROR_STATE.MSG
;		OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
;		;Handle the error by extending A:
;		;A=FLTARR(12)
;		CATCH, /CANCEL
;	ENDIF
;	print,'�����ֵ����ļ�'
;   ;**************************************
;	WIDGET_CONTROL,Event.top,get_UVALUE = pstate
;	Datatype=(*pstate).ClassDROPLIST
;	YEAR=(*pstate).Year_index
;	MONTH=(*pstate).Month_index
;	TENDAY=(*pstate).TENDay_index   ;cwj xie
;	;�õ������յ��ַ������
;	;����·�С��10��ǰ�����
;     if FIX(MONTH) lt 10 then begin
;
;      MONTH='0'+ STRTRIM(MONTH,2)
;     endif
;
;     if FIX(TENDAY) lt 10 then begin             ;ѮС��10��ǰ�����
;
;       TENDAY='0'+ STRTRIM(TENDAY,2)
;
;      endif
;      YMD= STRTRIM(YEAR,2) + STRTRIM(MONTH,2) + STRTRIM(TENDAY,2)      ;cwj xie
;
;      WIDGET_CONTROL,(*pstate).TEXT_OUTPUT_FILE, Get_VALUE=FILE
;
;    	 if FILE ne "" then begin
;
;     FILE_PATH=FILE
;     print,"FILE_PATH",FILE_PATH
;
;    if  N_ELEMENTS((*(*pstate).data)[0,*]) gt 0  then begin
;
;		     c= SaveResult(FILE_PATH,datatype,*((*pstate).data), (*(*pstate).dataBorder)[0],(*(*pstate).dataBorder)[1],(*(*pstate).dataBorder)[2],(*(*pstate).dataBorder)[3])
;
;	     endif else begin
;	         TEXT=DIALOG_MESSAGE('���Ȳ�ֵ��',/information)
;	     endelse
;
;     endif else begin
;           if (*PSTATE).read_method eq 1 then begin
;           TEXT=DIALOG_MESSAGE('�������ñ���·����Ϊ�ļ�����!',/information)
;           endif else begin
;            TEXT=DIALOG_MESSAGE('�������ñ����ļ�·����',/information)
;           endelse
;     endelse
;
;   RETURN, Event ; By Default, return the event.
;
;end
;���ļ���ֵ����
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function INTER_HELP, Event

   On_Error, 2
   CATCH, Error_status

   ;This statement begins the error handler:
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
      ; Handle the error by extending A:
;      A=FLTARR(12)
      CATCH, /CANCEL
   ENDIF

print,"data\��ֵϵͳ����.CHMdata\��ֵϵͳ����.CHM"

if file_test('data\��ֵϵͳ����.CHM') then begin
	ONLINE_HELP, BOOK='data\��ֵϵͳ����.CHM'
endif else begin
	info_help=dialog_message('�Ҳ��������ĵ�',title='����')
endelse

;ONLINE_HELP, BOOK='data\��ֵϵͳ����.CHM'

END

;----------------------------------------------------------------------
;�õ����ݵĿ�ʼ���

function Batch_year_FROM, Event


   On_Error, 2
   CATCH, Error_status

   ;This statement begins the error handler:
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
      ; Handle the error by extending A:
;      A=FLTARR(12)
      CATCH, /CANCEL
   ENDIF


;     Year_index=event.index
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     (*pstate).Batch_FYear_index=event.index+1990
     print,"Year_index=Year_index+1",(*pstate).Batch_FYear_index

     RETURN, Event ; By Default, return the event.
end
;------------------------------------------------------------------------------
;�õ����ݵĿ�ʼ�·�

function Batch_Month_FROM, Event


   On_Error, 2
   CATCH, Error_status

   ;This statement begins the error handler:
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
      ; Handle the error by extending A:
;      A=FLTARR(12)
      CATCH, /CANCEL
   ENDIF


;     Month_index=event.index
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     (*pstate).Batch_FMonth_index=event.index+1
     print,"INTER_DateMonthDR=Month_index+1",(*pstate).Batch_FMonth_index

     RETURN, Event ; By Default, return the event.
end
;------------------------------------------------------------------------------
;�õ����ݵĿ�ʼĳһ��

function Batch_Date_FROM, Event

   On_Error, 2
   CATCH, Error_status

   ;This statement begins the error handler:
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
      CATCH, /CANCEL
   ENDIF


;     Day_index=event.index
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     (*pstate).Batch_FTENDay_index=event.index+1
     print,"INTER_DateDateDR=Day_index+1",(*pstate).Batch_FTENDay_index

     RETURN, Event ; By Default, return the event.
end
;------------------------------------------------------------------------------------
;;----------------------------------------------------------------------

;�õ����ݵĽ������

function Batch_year_TO, Event


   On_Error, 2
   CATCH, Error_status

   ;This statement begins the error handler:
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
      ; Handle the error by extending A:
;      A=FLTARR(12)
      CATCH, /CANCEL
   ENDIF


;     Year_index=event.index
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     (*pstate).Batch_TYear_index=event.index+1990
     print,"Year_index=Year_index+1",(*pstate).Batch_TYear_index

     RETURN, Event ; By Default, return the event.
end
;------------------------------------------------------------------------------
;�õ����ݵĽ����·�

function Batch_Month_TO, Event


   On_Error, 2
   CATCH, Error_status

   ;This statement begins the error handler:
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
      ; Handle the error by extending A:
;      A=FLTARR(12)
      CATCH, /CANCEL
   ENDIF


;     Month_index=event.index
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     (*pstate).Batch_TMonth_index=event.index+1
     print,"INTER_DateMonthDR=Month_index+1",(*pstate).Batch_TMonth_index

     RETURN, Event ; By Default, return the event.
end
;------------------------------------------------------------------------------
;�õ����ݵĽ���ĳһ��

function Batch_Date_TO, Event


   On_Error, 2
   CATCH, Error_status

   ;This statement begins the error handler:
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
      ; Handle the error by extending A:
;      A=FLTARR(12)
      CATCH, /CANCEL
   ENDIF


;     Day_index=event.index
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     (*pstate).Batch_TTENDay_index=event.index+1
     print,"INTER_DateDateDR=Day_index+1",(*pstate).Batch_TTENDay_index

     RETURN, Event ; By Default, return the event.
end
;------------------------------------------------------------------------------
function INTER_BATCH_TABLE_EVENT, Event


   On_Error, 2
   CATCH, Error_status

   ;This statement begins the error handler:
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
      ; Handle the error by extending A:
;      A=FLTARR(12)
      CATCH, /CANCEL
   ENDIF



    widget_control,event.top,get_uvalue = pstate
    help,event,/stru
    if event.type eq 4 and event.sel_top ge 0 and event.sel_left ge 0 then begin
        (*pstate).LEFT = EVENT.SEL_LEFT
        (*pstate).RIGHT = EVENT.SEL_RIGHT
        (*pstate).TOP  = EVENT.SEL_TOP
        (*pstate).BOTTOM = EVENT.SEL_BOTTOM

        print,"(*pstate).LEF",(*pstate).LEFT
        print,"(*pstate).RIGHT",(*pstate).RIGHT
        print,"(*pstate).TOP",(*pstate).TOP
        print,"(*pstate).BOTTOM",(*pstate).BOTTOM

    endif


end


;-------------------------------------------------------------------------

;pro ShowValue,pstate
;
;      On_Error, 2
;      CATCH, Error_status
;
;       ;This statement begins the error handler:
;       IF Error_status NE 0 THEN BEGIN
;          PRINT, 'Error index: ', Error_status
;          PRINT, 'Error message: ', !ERROR_STATE.MSG
;          OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
;          ; Handle the error by extending A:
;    ;      A=FLTARR(12)
;          CATCH, /CANCEL
;       ENDIF
;
;
;    WIDGET_CONTROL, (*pstate).DrawID, GET_VALUE=drawwin
;
;    X = (*(*pstate).new_pointer_array)[0,*]
;    Y = (*(*pstate).new_pointer_array)[1,*]
;
;    value= fix((*(*pstate).old_value))
;
;    scaled = BYTSCL(value, TOP = !D.TABLE_SIZE - 4) + 1B
;
;    value= string(value)
;
;    print,N_ELEMENTS(x)
;
;    FOR i = 0L, (N_ELEMENTS(x) - 1) DO begin
;
;       (*pstate).Text = OBJ_NEW('IDLgrText',value[i],LOCATIONS = [[x[i]-34,y[i]+2]] ,COLOR =[255,255,255],font=(*pstate).default_font)
;
;       (*pstate).theModel->Add,(*pstate).Text
;
;    ENDFOR
;
;    drawwin->Draw, (*PSTATE).theView
;
;End

;-------------------------------------------------------------------------


function antiget_coordinate,coor_x,coor_y,offset_x,offset_y


   On_Error, 2
   CATCH, Error_status

   ;This statement begins the error handler:
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
      CATCH, /CANCEL
   ENDIF

    x = (coor_x-offset_x)
    y = (coor_y-offset_y)
    print,'x',x
      print,'y',y
    return,{x:x,y:y}

end

;-------------------------------------------------------------------------

;���Բ�ѯ����������������������������������

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;-----------------------------------------------------------------
;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;
; Generated on: 11/21/2004 19:09.47


pro InterpET_eventcb,event
      On_Error, 2
      CATCH, Error_status

       ;This statement begins the error handler:
       IF Error_status NE 0 THEN BEGIN
          PRINT, 'Error index: ', Error_status
          PRINT, 'Error message: ', !ERROR_STATE.MSG
          OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
          ; Handle the error by extending A:
    ;      A=FLTARR(12)
          CATCH, /CANCEL
       ENDIF
end
;
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;����ɾ����
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;++++++++++++++++++++++++++++++++++++++++++
;����鿴��
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;�õ����ݵ����

function INTER_DateYearDROPLIST, Event

  On_Error, 2
   CATCH, Error_status

   ;This statement begins the error handler:
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
      ; Handle the error by extending A:
;      A=FLTARR(12)
      CATCH, /CANCEL
   ENDIF


;     Year_index=event.index
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     (*pstate).Year_index=event.index+1990
     print,"Year_index=Year_index+1",(*pstate).Year_index

     RETURN, Event ; By Default, return the event.
end
;------------------------------------------------------------------------------
;����������
function WID_MethodPara, Event

;   On_Error, 2
;    CATCH, Error_status
;
;   ;This statement begins the error handler:
;   IF Error_status NE 0 THEN BEGIN
;      PRINT, 'Error index: ', Error_status
;      PRINT, 'Error message: ', !ERROR_STATE.MSG
;      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
;      ; Handle the error by extending A:
;;      A=FLTARR(12)
;      CATCH, /CANCEL
;   ENDIF
;
;
;    WIDGET_CONTROL,Event.top,get_UVALUE = pstate
;
;    IF (*pstate).method_index EQ '' then begin
;
;         (*pstate).method_index = 0
;
;    ENDIF
;    print,(*pstate).method_index
;
;    case  (*pstate).method_index of
;
;    0:  BEGIN  ;IDW����
;
;    IDW_PARAMETER,PSTATE ;, GROUP_LEADER=(*pstate).WID_BASE_0
;    ;getEllipse,pstate
;    end
;
;    2:  BEGIN  ;kriging����
;
;    Kriging_Parameter,PSTATE ;, GROUP_LEADER=(*pstate).WID_BASE_0
;   ; getEllipse,pstate
;    end
;
;
;    5:  BEGIN  ;����ʽ�ع鷽��
;
;     PolRegr_Parameter,PSTATE ;, GROUP_LEADER=(*pstate).WID_BASE_0
;   ; getEllipse,pstate
;    end
;
;    6:  BEGIN  ;�����������ֵ����
;
;     RBF_Parameter,PSTATE ;, GROUP_LEADER=(*pstate).WID_BASE_0
;   ; getEllipse,pstate
;    end
;
;    else:
;
;    endcase
;
;    return, Event ; By Default, return the event.
end


;�õ����ݵ��·�

function INTER_DateMonthDR, Event

  On_Error, 2
    CATCH, Error_status

   ;This statement begins the error handler:
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
      ; Handle the error by extending A:
;      A=FLTARR(12)
      CATCH, /CANCEL
   ENDIF

;     Month_index=event.index
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     (*pstate).Month_index=event.index+1
     print,"INTER_DateMonthDR=Month_index+1",(*pstate).Month_index


     RETURN, Event ; By Default, return the event.
end
;------------------------------------------------------------------------------
;�õ����ݵ�ĳһ��

function INTER_DateDateDR, Event
    On_Error, 2
    CATCH, Error_status

   ;This statement begins the error handler:
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
      ; Handle the error by extending A:
;      A=FLTARR(12)
      CATCH, /CANCEL
   ENDIF

;     Day_index=event.index
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
    ; (*pstate).Day_index=event.index+1
      (*pstate).TENDay_index=event.index+1
     ;print,"INTER_DateDateDR=Day_index+1",(*pstate).Day_index

       print,"INTER_DateDateDR=TENDay_index+1",(*pstate).TENDay_index
     RETURN, Event ; By Default, return the event.
end
;------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------

;�õ�NCDC����
;---------------------------------------------------------------------------------------
;����ļ�
;����߽��ļ�
;FUNCTION INPUT_Border_FILE, EVENT
;     output_path = ''
;     FILE = DIALOG_PICKFILE(title='ѡ��Ҫ������ļ�',GET_PATH=output_path, /DIRECTORY, DIALOG_PARENT=Event.id)
;     print,file
;     IF (FILE EQ '') THEN RETURN,0
;
;     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
;
;
;     IF (FILE NE '') THEN BEGIN
;          WIDGET_CONTROL, (*PSTATE).TEXT_OUTPUT_FILE, set_VALUE=FILE
;          (*PSTATE).output_path = output_path
;          PRINT,'(*PSTATE).OUTput_path',(*PSTATE).OUTput_path,FILE
;     ENDIF
;     print,file
;     RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.
;
;END
;---------------------------------------------------------------------------------------
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;����DEM����
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

FUNCTION WID_DEM_check, EVENT

  On_Error, 2
    CATCH, Error_status

   ;This statement begins the error handler:
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
      CATCH, /CANCEL
   ENDIF

   WIDGET_CONTROL,Event.top,get_UVALUE = pstate

   if  (*pstate).DEMSentive EQ 0 then begin

      WIDGET_CONTROL,(*pstate).demfile, SENSITIVE  = 1
      WIDGET_CONTROL,(*pstate).bt_opendem, SENSITIVE  = 1
      WIDGET_CONTROL,(*pstate).reso, SENSITIVE  = 0

      (*pstate).DEMSentive=1

      print,"DEM����",(*pstate).DEMSentive

   endif else  begin

      WIDGET_CONTROL,(*pstate).demfile, SENSITIVE  = 0
      WIDGET_CONTROL,(*pstate).bt_opendem, SENSITIVE  = 0
      WIDGET_CONTROL,(*pstate).reso, SENSITIVE  = 1

      (*pstate).DEMSentive=0

    print,"DEM������",(*pstate).DEMSentive
   endelse

END

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;����dem����
;����DEM����
;FUNCTION WID_DEMFILEPATHButton, EVENT
;
;    On_Error, 2
;    CATCH, Error_status
;
;   ;This statement begins the error handler:
;   IF Error_status NE 0 THEN BEGIN
;      PRINT, 'Error index: ', Error_status
;      PRINT, 'Error message: ', !ERROR_STATE.MSG
;      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
;      ; Handle the error by extending A:
;;      A=FLTARR(12)
;      CATCH, /CANCEL
;   ENDIF
;COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
;     input_path = ''
;     FILE = DIALOG_PICKFILE(title='ѡ��Ҫ����dem���ļ�',GET_PATH= input_path,filter='*.*',path=FILE_PATH, DIALOG_PARENT=Event.id)
;
;     IF (FILE EQ '') THEN RETURN,0
;
;     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
;     IF (FILE NE '') THEN BEGIN
;          WIDGET_CONTROL, (*PSTATE).INPUT_DEM_FILE, set_VALUE=FILE
;          (*PSTATE).input_path = input_path
;          PRINT,'(*PSTATE).input_path',(*PSTATE).input_path
;     ENDIF
;     print,file
;     RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.
;
;END
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;FUNCTION WID_Border_check, EVENT
;  On_Error, 2
;    CATCH, Error_status
;
;   ;This statement begins the error handler:
;   IF Error_status NE 0 THEN BEGIN
;      PRINT, 'Error index: ', Error_status
;      PRINT, 'Error message: ', !ERROR_STATE.MSG
;      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
;      ; Handle the error by extending A:
;;      A=FLTARR(12)
;      CATCH, /CANCEL
;   ENDIF
;
;   WIDGET_CONTROL,Event.top,get_UVALUE = pstate
;
;    if  (*pstate).BorderSentive EQ 0 then begin
;
;      WIDGET_CONTROL,(*pstate).INPUT_Border_FILE, SENSITIVE  = 1
;      WIDGET_CONTROL,(*pstate).WID_BorderFILEPATHButton, SENSITIVE  = 1
;
;
;      (*pstate).BorderSentive=1
;
;      print,"Border����",(*pstate).BorderSentive
;
;   endif else  begin
;
;      WIDGET_CONTROL,(*pstate).INPUT_Border_FILE, SENSITIVE  = 0
;      WIDGET_CONTROL,(*pstate).WID_BorderFILEPATHButton, SENSITIVE  = 0
;
;      (*pstate).BorderSentive=0
;    print,"Border������",(*pstate).BorderSentive
;   endelse
;
;END

;---------------------------------------------------------------------------------------
;����DEM����
;FUNCTION WID_DEMFILEPATHButton, EVENT
;
;  On_Error, 2
;    CATCH, Error_status
;
;   ;This statement begins the error handler:
;   IF Error_status NE 0 THEN BEGIN
;      PRINT, 'Error index: ', Error_status
;      PRINT, 'Error message: ', !ERROR_STATE.MSG
;      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
;      ; Handle the error by extending A:
;;      A=FLTARR(12)
;      CATCH, /CANCEL
;   ENDIF
;
;     input_path = ''
;     FILE = DIALOG_PICKFILE(title='ѡ��Ҫ������ļ�',GET_PATH= input_path,filter='*.*', DIALOG_PARENT=Event.id)
;
;     IF (FILE EQ '') THEN RETURN,0
;
;     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
;     IF (FILE NE '') THEN BEGIN
;          WIDGET_CONTROL, (*PSTATE).INPUT_DEM_FILE, set_VALUE=FILE
;          (*PSTATE).input_path = input_path
;          PRINT,'(*PSTATE).input_path',(*PSTATE).input_path
;     ENDIF
;     print,file
;     RETURN, EVENT ; BY DEFAULT, RETURN THE EVENT.
;END
;---------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------

;��ֵ����ѡ���¼�
FUNCTION WID_MethodDR, EVENT

   On_Error, 2
    CATCH, Error_status

   ;This statement begins the error handler:
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
      ; Handle the error by extending A:
;      A=FLTARR(12)
      CATCH, /CANCEL
   ENDIF


     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     ;method_indexĬ��ֵ��0 ��������IDW
     (*pstate).method_index=event.index

     print," (*pstate).WID_MethodDR", (*pstate).method_index

     RETURN, Event ; By Default, return the event.

END



;-------------------------------------------------------------------------------------

pro WID_BASE_0_event, Event
  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)


  wWidget =  Event.top

  case wTarget of
     Widget_Info(wWidget, FIND_BY_UNAME='CMD_PROCESS_DB_CONNECT'): begin
     end
     Widget_Info(wWidget, FIND_BY_UNAME='INTER_ClassDROPLIST'): begin
     end
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BASE_0'): begin

;        WID_BASE_0_MOVE, Event   ;���޸ģ�ԭ�������δע��

     end

    else:
  endcase
end

