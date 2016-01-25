;对比分析模块
pro NQ_classify_CleanAllHeap,tlb
    IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2
    WIDGET_CONTROL,tlb,GET_UVALUE=PA
    HEAP_FREE,PA
end
;*****************************************************

pro CMD_CLASSIFY_NOTATION_HELP_EVENT,event
	 PRINT,'数据异常检测,帮助'

	if file_test('HELP\HELP.chm') then begin
		ONLINE_HELP, '对比分析', BOOK='HELP\HELP.chm', /FULL_PATH
	endif else begin
		info_help=dialog_message('找不到帮助文档',title='警告')
	endelse
end

;---------------------------------------------------------------------------

pro NQ_classify_DisplayTiffImage, event
	;获取窗体绘图对像
	widget_control, event.top, get_uvalue = pstate
	xzoom =float(280)/(*pstate).xsize
	yzoom =float(376)/(*pstate).ysize
	(*pstate).zoomFactor = min([xzoom, yzoom])
	;居于窗口中部
	(*pstate).xpad = 80/(*pstate).zoomFactor-0.5 * ((*pstate).xsize / (*pstate).zoomFactor - (*pstate).xsize)
	(*pstate).ypad = -0.5 * ((*pstate).ysize / (*pstate).zoomFactor - (*pstate).ysize)

	;生成影像对像
    widget_control, (*pstate).GrayData, get_uvalue = GrayData

   ;   Red = [0,0,255] & Green = [255,0,0] & Blue = [0,255,0]
   Red = [0,255,0,0,255,255] & Green = [0,0,0,255,255,255] & Blue = [0,0,255,0,255,255]
   Palette = OBJ_NEW('IDLgrPalette',Red,Green,Blue)
   theImage = Obj_New('IDLgrImage', GrayData, PALETTE = Palette, ORDER=1)
   print, max(GrayData), min(GrayData)
    ;生成影像模型

    theModel = Obj_New('IDLgrModel')
    theModel->Add, theImage
    ;
    zoomFactor = (*pstate).zoomFactor
    ;保证图像不出界
	(*pstate).xpad = (*pstate).xpad < ((*pstate).xsize - 280/zoomFactor)
	(*pstate).xpad = (*pstate).xpad > 0
	(*pstate).ypad = (*pstate).ypad < ((*pstate).ysize - 376/zoomFactor)
	(*pstate).ypad = (*pstate).ypad > 0
	;
	v_rect = [(*pstate).xpad, (*pstate).ypad, 280/zoomFactor, 376/zoomFactor]
	theViewtuli = Obj_New('IDLgrView', Viewplane_Rect = v_rect, color = [255,255,255]) ;为了画图例而作的view



	viewPlane = [0,0,(*pstate).xsize,(*pstate).ysize]
	xsize=(*pstate).xsize
	ysize=(*pstate).ysize
	drawsize=[314,376]
	;view的大小
	Ratios = 1.0
	scale = Min((Double(drawSize*Ratios)/[xSize,ySize])<1)
	IF scale NE 1. THEN BEGIN
		viewDim = [Fix(scale*xSize), Fix(scale*ySize)]
	ENDIF ELSE BEGIN
		IF 1.*xSize/ySize GE 1.*drawSize[0]/drawSize[1] THEN BEGIN
			viewDim = [Fix(Ratios*drawSize[0]),Fix(Ratios*ySize/xSize*drawSize[0])]
		ENDIF ELSE BEGIN
			viewDim = [Fix(Ratios*xSize/ySize*drawSize[1]),Fix(Ratios*drawSize[1])]
		ENDELSE
	ENDELSE

	;view的插入位置
	viewLoc = [86+(drawSize[0]-viewDim[0])/2,(drawSize[1]-viewDim[1])/2]

;	theView = Obj_New('IDLgrView', Viewplane_Rect = v_rect, color = [255,255,255],location=[86,0],dimension=[310,376])
    theView = Obj_New('IDLgrView', Viewplane_Rect = v_rect, color = [255,255,255],location=viewLoc,dimension=[310,376])
    theView->Add, theModel
   ;(2)----------------------------------画图例-------------------------------------------------
   Red1 = [0,0,255] & Green1 = [255,0,0] & Blue1 = [0,255,0]
   LegBoxColor = OBJARR(3)
   FOR i=0,2 DO LegBoxColor[i] = OBJ_NEW('IDLgrPattern')

   OLegTitleFont = OBJ_NEW('IDLgrFont','宋体',SIZE=14)   ;这里的宋体'Song'必须在RSI目录下resource\fonts\tt下拷贝加载并修改即可..
   OLegTitle  = OBJ_NEW('IDLgrText','图 例',FONT=OLegTitleFont)
   OLegend  = OBJ_NEW('IDLgrLegend' ,['偏多','正常','偏少'],TITLE = OLegTitle,ITEM_TYPE=[1,1,1] $
                                    ,GLYPH_WIDTH=2,GAP=0.3 $
                                    ,ITEM_COLOR=TRANSPOSE([[Red1],[Green1],[Blue1]]) $
                                    ,OUTLINE_COLOR=[0,0,255],SHOW_OUTLINE=1 $
                                    ,ITEM_OBJECT =LegBoxColor $   ;可以不要这一项.
                                    ,BORDER_GAP=0.2,FONT=OLegTitleFont)
   theViewtuli->Add,OLegend


   WIDGET_CONTROL, (*pstate).WID_DRAW, GET_VALUE = owindow
    owindow->Draw,theViewtuli
	owindow->Draw, theView

    ;
    obj_destroy, theImage
    obj_destroy, theModel
    obj_destroy, theView
    HEAP_GC, /VERBOSE
end
;---------------------对象图形法ended--------------------------------------

pro NQ_classify_selectdata,event
	WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     DROP_DATATYPE = widget_info((*pstate).DROP_DATATYPE, /droplist_select)
    	case drop_datatype of
	   0:(*pstate).datatype=0
	   1: (*pstate).datatype=1
	   else:(*pstate).datatype=2
    	endcase
end
;-----------------------------------------------------------------------------
function NQ_CMD_CLASSIFY_PARAMETER_CANCEL, Event  ;取消按纽相当于关闭
     CLOSE,/all
     WIDGET_CONTROL, event.top, /destroy
     RETURN, Event ; By Default, return the event.
end
;-----------------------------------------------------------------------------

function NQ_CMD_PARAMETER_OK, Event                  ; 确定按钮
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate

     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_1_MAX,	get_value=LEVEL_1_MAX
     	LEVEL_1_MAX=FLOAT(LEVEL_1_MAX[0])
     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_1_MIN,	get_value=LEVEL_1_MIN
     	LEVEL_1_MIN=FLOAT(LEVEL_1_MIN[0])
     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_2_MAX,	get_value=LEVEL_2_MAX
     	LEVEL_2_MAX=FLOAT(LEVEL_2_MAX[0])
     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_2_MIN,	get_value=LEVEL_2_MIN
     	LEVEL_2_MIN=FLOAT(LEVEL_2_MIN[0])
     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_3_MAX,	get_value=LEVEL_3_MAX
     	LEVEL_3_MAX=FLOAT(LEVEL_3_MAX[0])
     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_3_MIN,	get_value=LEVEL_3_MIN
     	LEVEL_3_MIN=FLOAT(LEVEL_3_MIN[0])

     IF(LEVEL_1_MAX lt LEVEL_1_MIN) THEN BEGIN
 		msg=DIALOG_MESSAGE('请输入合理的参数值!',title='提示')
        CLOSE,/all
        RETURN,0
     ENDIF
     IF(LEVEL_1_MAX NE LEVEL_2_MIN) THEN BEGIN
     	PRINT,LEVEL_1_MAX
     	PRINT,LEVEL_2_MIN
 		msg=DIALOG_MESSAGE('请输入合理的参数值!',title='提示')
        CLOSE,/all
        RETURN,0
     ENDIF
     IF(LEVEL_2_MAX lt LEVEL_2_MIN) THEN BEGIN
 		msg=DIALOG_MESSAGE('请输入合理的参数值!',title='提示')
        CLOSE,/all
        RETURN,0
     ENDIF
     IF(LEVEL_2_MAX NE LEVEL_3_MIN) THEN BEGIN
     	PRINT,LEVEL_2_MAX
     	PRINT,LEVEL_3_MIN
 		msg=DIALOG_MESSAGE('请输入合理的参数值!',title='提示')
        CLOSE,/all
        RETURN,0
     ENDIF
     IF(LEVEL_3_MAX lt LEVEL_3_MIN) THEN BEGIN
     	PRINT,LEVEL_2_MAX
     	PRINT,LEVEL_3_MIN
 		msg=DIALOG_MESSAGE('请输入合理的参数值!',title='提示')
        CLOSE,/all
        RETURN,0
     ENDIF
	(*(*((*pstate).toppstate)).p)[0,0]=LEVEL_1_MIN
	(*(*((*pstate).toppstate)).p)[0,1]=LEVEL_2_MIN
	(*(*((*pstate).toppstate)).p)[0,2]=LEVEL_3_MIN
	(*(*((*pstate).toppstate)).p)[1,0]=LEVEL_1_MAX
	(*(*((*pstate).toppstate)).p)[1,1]=LEVEL_2_MAX
	(*(*((*pstate).toppstate)).p)[1,2]=LEVEL_3_MAX
;----------------------------------------------------------------------------------
       drop_datatype=(*((*pstate).toppstate)).datatype;选择日降水
  	if(drop_datatype eq 0) then begin
  	    (*((*pstate).toppstate)).a[0,0]=LEVEL_1_MIN
  	    (*((*pstate).toppstate)).a[0,1]=LEVEL_2_MIN
  	    (*((*pstate).toppstate)).a[0,2]=LEVEL_3_MIN
  	    (*((*pstate).toppstate)).a[1,0]=LEVEL_1_MAX
  	    (*((*pstate).toppstate)).a[1,1]=LEVEL_2_MAX
  	    (*((*pstate).toppstate)).a[1,2]=LEVEL_3_MAX
  	endif
  	if(drop_datatype eq 1) then begin        ;选择日照时数
   	   	(*((*pstate).toppstate)).b[0,0]=LEVEL_1_MIN
  	    (*((*pstate).toppstate)).b[0,1]=LEVEL_2_MIN
  	    (*((*pstate).toppstate)).b[0,2]=LEVEL_3_MIN
  	    (*((*pstate).toppstate)).b[1,0]=LEVEL_1_MAX
  	    (*((*pstate).toppstate)).b[1,1]=LEVEL_2_MAX
  	    (*((*pstate).toppstate)).b[1,2]=LEVEL_3_MAX
  	endif
  	if(drop_datatype eq 2) then begin   	       ;选择日积温
   	   	(*((*pstate).toppstate)).c[0,0]=LEVEL_1_MIN
  	    (*((*pstate).toppstate)).c[0,1]=LEVEL_2_MIN
  	    (*((*pstate).toppstate)).c[0,2]=LEVEL_3_MIN
  	    (*((*pstate).toppstate)).c[1,0]=LEVEL_1_MAX
  	    (*((*pstate).toppstate)).c[1,1]=LEVEL_2_MAX
  	    (*((*pstate).toppstate)).c[1,2]=LEVEL_3_MAX
  	endif
;----------------------------------------------------------------------------------
     CLOSE,/all
     WIDGET_CONTROL, event.top, /destroy
     RETURN, Event ; By Default, return the event.

end
;----------------------------------------------------------------------------------
pro NQ_CMD_CLASSIFY_CHANGE_PARAMETER, Event
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     DROP_DATATYPE = widget_info((*pstate).DROP_DATATYPE, /droplist_select)
	 if ptr_valid((*pstate).p) eq 1 then ptr_free,(*pstate).p
  	 if(drop_datatype eq 0) then begin
    	(*pstate).p=ptr_new((*pstate).a,/no_copy)                        ;选择日降水
  	 endif else if(drop_datatype eq 1) then begin                              ;选择日照时数
   	   (*pstate).p=ptr_new((*pstate).b,/no_copy)
  	 endif else if(drop_datatype eq 2) then begin   	                         ;选择日积温
   	   (*pstate).p=ptr_new((*pstate).c,/no_copy)
  	 endif
     (*pstate).datatype=drop_datatype
      WIDGET_CONTROL,EVENT.TOP,GET_UVALUE=PSTATE
     NQ_classify_BASE_CLASSIFY_PARAMETER_SETUP,PSTATE

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro NQ_classify_write_HDR_FILE,result_file,datainfo
;;  此进程完成的是将字符串写入到头文件中 ,将数据数组写入裸文件中
;;   输入文件的头文件获取
    index = STRPOS(result_file, '.')
    head1 = ''
    if index eq -1 then begin
      result_head=strtrim(result_file,2)+'.hdr'
    endif else begin
      head1 = STRMID(result_file,0,index)
      result_head=head1+'.hdr'
    endelse
;   定义字符串
    enter=string(byte(13))+string(byte(10))
    samples=strtrim(string(datainfo.XSIZE),2)
    lines=strtrim(string(datainfo.YSIZE),2)                            ;行'4100'
    sulx=strtrim(string(ulong(datainfo.STARTX)),2)+'.0000'             ;起始X'906500.0000'
    suly=strtrim(string(ulong(datainfo.STARTY)),2)+'.0000'             ;起始Y'5906500.0000'
    bands=strtrim(string(datainfo.BANDNUM),2)                          ;波段数
    data_type=strtrim(string(datainfo.DATATYPE),2)                     ;数据类型
    pixel_size=strtrim(string(datainfo.X_PIXELSIZE),2)+'.00000000'       ;像元大小
    bandname=''
    time=systime()                                                     ;时间
    result_headdata='ENVI'+enter+$
    'description = {' + enter+ $
    '  Create New File Result ['+time+']}'+enter+$
    'samples = '+samples+enter+$
    'lines   = '+lines+enter+$
    'bands   = '+bands+enter+$
    'header offset = 0'+enter+$
    'file type = ENVI Standard'+enter+$
    'data type = '+data_type+enter+$
    'interleave = bsq'+enter+$
    'sensor type = Unknown'+enter+$
    'byte order = 0'+enter+$
    'map info = {Albers Conical Equal Area,1.0000, 1.0000, '+	$
    STRTRIM(datainfo.STARTX,2)+ ','+	$
    STRTRIM(datainfo.STARTY,2)+ ','+	$
    STRTRIM(datainfo.X_PIXELSIZE,2)+ ','+	$
    STRTRIM(datainfo.X_PIXELSIZE,2)+ ','+	$
    'Krasovsky, units=Meters}'+enter+$
    'projection info = {9, 6378245.0, 6356863.0, 0.000000, 110.000000, 4000000.0, 0.0, 25.000000, 47.000000, Krasovsky,Albers Conical Equal Area, units=Meters}'+enter+$
    'wavelength units = Unknown'+enter+$
    'pixel size = {'+STRTRIM(datainfo.X_PIXELSIZE,2)+','+STRTRIM(datainfo.Y_PIXELSIZE,2)+', units=Meters}'+enter

;    写入过程
     openw,lun,result_head,/get_lun
     writeu,lun,result_headdata
     free_lun,lun
end
;--------------------------------------------------------------------------------
function NQ_classify_CMD_CLASSIFY_FILE_LAST_YEAR, Event
     COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	COMMON COMMON_SETPATH,ppath
	 v_nq_in_path  = (*ppath).nq_in_path

     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     FILE_LAST_YEAR= dialog_pickfile(dialog_parent=event.top, title='打开影像文件(envi标准格式)', filter=['*.*'],path=v_nq_in_path, /MUST_EXIST)

     IF (FILE_LAST_YEAR NE '') THEN BEGIN
     	  WIDGET_CONTROL,Event.top,get_UVALUE = pstate
          WIDGET_CONTROL, (*pstate).TXT_CLASSIFY_FILE_LAST_YEAR, set_VALUE=FILE_LAST_YEAR
     ENDIF
     RETURN, Event ; By Default, return the event.
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function NQ_classify_CMD_CLASSIFY_FILE_THIS_YEAR, Event
     COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	COMMON COMMON_SETPATH,ppath
	 v_nq_in_path  = (*ppath).nq_in_path

     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
;     FILE_THIS_YEAR=DIALOG_PICKFILE(filter='*.*',path=PathSetting(/READPATH2), DIALOG_PARENT=Event.id)
     FILE_THIS_YEAR = dialog_pickfile(dialog_parent=event.top, title='打开影像文件(envi标准格式)', filter=['*.*'],path=v_nq_in_path, /MUST_EXIST)
     IF (FILE_THIS_YEAR NE '') THEN BEGIN
     	  WIDGET_CONTROL,Event.top,get_UVALUE = pstate
          WIDGET_CONTROL, (*pstate).TXT_CLASSIFY_FILE_THIS_YEAR, set_VALUE=FILE_THIS_YEAR
     ENDIF

     RETURN, Event ; By Default, return the event.

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function NQ_classify_CMD_CLASSIFY_CHOOSE_OUTPUT, Event
     COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	COMMON COMMON_SETPATH,ppath
	 v_nq_out_path  = (*ppath).nq_out_path

     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     FILE_OUTPUT=dialog_pickfile(/write,dialog_parent=event.top, title='保存jpg图像', filter=['*.jpg'],path=v_nq_out_path)

     IF (FILE_OUTPUT NE '') THEN BEGIN
     	  if strmatch(FILE_OUTPUT,'*.jpg') eq 0 then FILE_OUTPUT += '.jpg'
     	  WIDGET_CONTROL,Event.top,get_UVALUE = pstate
          WIDGET_CONTROL, (*pstate).TXT_CLASSIFY_FILE_jpg, set_VALUE=FILE_OUTPUT
     ENDIF

     RETURN, Event ; By Default, return the event.

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function NQ_classify_CMD_OUTPUT, Event
     COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	COMMON COMMON_SETPATH,ppath
	 v_nq_out_path  = (*ppath).nq_out_path

     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     lastpic=dialog_pickfile(/write,dialog_parent=event.top, title='保存影像文件(envi标准格式)', filter=['*.*'],path=v_nq_out_path)

     IF (lastpic NE '') THEN BEGIN
     	  WIDGET_CONTROL,Event.top,get_UVALUE = pstate
          WIDGET_CONTROL, (*pstate).lastpic, set_VALUE=lastpic
     ENDIF
     RETURN, Event ; By Default, return the event.

end
;------------------------------------------------------------jpg&影像图像生成
PRO NQ_classify_output,event

	FORWARD_FUNCTION NQ_CL_Read_ENVIData

     widget_control,/hourglass
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     WWIDGET =  Event.top

     ;进行输入参数完整性检验,
     ;不但检验了是否有输入,
     ;还对输入文件的大小进行了检验
     ;------------------------------------------------------------
     DROP_DATATYPE = widget_info((*pstate).DROP_DATATYPE, /droplist_select)

	 if ptr_valid((*pstate).p) eq 1 then ptr_free,(*pstate).p

     case drop_datatype of
     0: (*pstate).p=ptr_new((*pstate).a)   ;选择日降水
     1:(*pstate).p=ptr_new((*pstate).b)    	;选择日照时数
     else: (*pstate).p=ptr_new((*pstate).c)  ;选择日积温
     endcase
     ;------------------------------------------------------------
 	 WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_FILE_LAST_YEAR,get_value=FILE_LAST_YEAR
     IF(FILE_LAST_YEAR EQ '') THEN BEGIN
         msg=DIALOG_MESSAGE('必须输入实时的气象栅格文件!',title='提示')
         CLOSE,/all
         RETURN
     ENDIF
     FILE_LAST_YEAR=FILE_LAST_YEAR[0]
     RESULT=FILE_INFO(FILE_LAST_YEAR)

      WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_FILE_THIS_YEAR,get_value=FILE_THIS_YEAR
     IF(FILE_THIS_YEAR EQ '') THEN BEGIN
         msg=DIALOG_MESSAGE('必须输入历史实时的气象栅格文件!',title='提示')
         CLOSE,/all
         RETURN
     ENDIF
     FILE_THIS_YEAR=FILE_THIS_YEAR[0]
     RESULT=FILE_INFO(FILE_THIS_YEAR)

     WIDGET_CONTROL,(*pstate).lastpic,get_value=lastpic
     IF(lastpic EQ '') THEN BEGIN
         msg=DIALOG_MESSAGE('请选择输出的分级影像文件!',title='提示')
         CLOSE,/all
         RETURN
     ENDIF
     lastpic=lastpic[0]
     RESULT=FILE_INFO(lastpic)

     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_FILE_jpg,get_value=FILE_jpg
     IF(FILE_jpg EQ '') THEN BEGIN
         msg=DIALOG_MESSAGE('请选择输出的分级jpg文件!',title='提示')
         CLOSE,/all
         RETURN
     ENDIF

     FILE_jpg=FILE_jpg[0]
     RESULT=FILE_INFO(FILE_jpg)

	;-----------------------------------

		 FILE_INFO_TY=NQ_classify_GET_IMAGE_INFO(FILE_LAST_YEAR)
		 if n_tags(FILE_INFO_TY) eq 1 then return
		 FILE_INFO_LY=NQ_classify_GET_IMAGE_INFO(FILE_THIS_YEAR)
		 if n_tags(FILE_INFO_LY) eq 1 then return
         if (FILE_INFO_TY.FILE_RIGHT EQ 0 And FILE_INFO_LY.FILE_RIGHT EQ 0) then begin
            OK = DIALOG_MESSAGE('输入的实时和历史实时的气象栅格文件都有错误!',title='提示')
            RETURN
         endif

    	 IF (FILE_INFO_TY.FILE_RIGHT EQ 0) THEN BEGIN
		 	OK = DIALOG_MESSAGE('输入的实时气象栅格文件有错误!',title='提示')
		 	RETURN
		 ENDIF

		 IF FILE_INFO_LY.FILE_RIGHT EQ 0 THEN BEGIN
		 	OK = DIALOG_MESSAGE('输入的历史实时气象栅格文件有错误!',title='提示')
		 	RETURN
		 ENDIF
;    ------------------------------------
         IF (FILE_INFO_TY.XSIZE NE FILE_INFO_LY.XSIZE) OR $
	 	 	(FILE_INFO_TY.YSIZE NE FILE_INFO_LY.YSIZE) THEN BEGIN $
		  	OK = DIALOG_MESSAGE('输入文件大小不一致!',title='提示')
		 	RETURN
		 ENDIF
;    ------------------------------------
		 IF (FILE_INFO_TY.X_PIXELSIZE NE FILE_INFO_LY.X_PIXELSIZE) OR $
	 	 	(FILE_INFO_TY.Y_PIXELSIZE NE FILE_INFO_LY.Y_PIXELSIZE) $
		 THEN BEGIN
		 	OK = DIALOG_MESSAGE('输入文件的分辩率不一致!',title='提示')
		 	RETURN
		 ENDIF
;    ------------------------------------
		 IF (ABS(FILE_INFO_TY.STARTX-FILE_INFO_LY.STARTX) GT FILE_INFO_TY.X_PIXELSIZE) OR $
		 	(ABS(FILE_INFO_TY.STARTY-FILE_INFO_LY.STARTY) GT FILE_INFO_TY.Y_PIXELSIZE) $
		 THEN BEGIN
		 	OK = DIALOG_MESSAGE('输入文件地理坐标不一致!',title='提示')
		 	RETURN
		 ENDIF

	 progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='数据处理中,请稍候!',TITLE='插值图像分级') ;新建进度条对象
	 progressTimer->START
	 progressTimer->UPDATE, (0.05 * 100.0)  ;更新进度条

	 SAMPLES=FILE_INFO_TY.XSIZE
	 LINES	=FILE_INFO_TY.YSIZE
	 (*pstate).xsize=SAMPLES
	 (*pstate).ysize=LINES
	 ;开始对数据文件的读取
	 INFILE=FILE_LAST_YEAR
	 Openr,Lun_FILE_LAST_YEAR	,file_last_year	,/get_lun
	 Openr,Lun_FILE_THIS_YEAR	,FILE_THIS_YEAR	,/get_lun
	 Openw,Lun_lastpic		,lastpic	,/get_lun
	 Openw,Lun_FILE_jpg		,FILE_jpg	,/get_lun
	 ;定义要用来读取的数据类型
	 ;对程序进行了修改,一次读取一行的数据
	 ;IF(BYTES_OF_DATA EQ 1) THEN BEGIN
	 	FILE_LAST_YEAR	=	fltARR(SAMPLES,LINES)
	 	FILE_THIS_YEAR	=	fltARR(SAMPLES,LINES)
	 	lastpic         =   fltarr(SAMPLES,Lines)
	    DIFF			=	fltARR(SAMPLES,Lines)
        results         =	fltARR(SAMPLES,Lines)
        jpgimg          =   fltarr(SAMPLES,Lines)

	 READU, Lun_FILE_LAST_YEAR	, FILE_LAST_YEAR
	 READU, Lun_FILE_THIS_YEAR	, FILE_THIS_YEAR
	 progressTimer->UPDATE, (0.2 * 100.0)  ;更新进度条
;     READU, Lun_file_border  	, file_border

;     file_border='data_grid\province_new'
;     Openr,Lun_file_border,file_border,/get_lun
;     file_border     =   bytarr(1069,1026)
;     if samples eq '1069' then begin
;     READU, Lun_file_border  	, file_border
;     endif else begin
;     READU, Lun_file_border  	, file_border
;     file_border=congrid(file_border,samples,lines)
;     endelse
;     temp=where(file_border eq 1)
;;     Location = ARRAY_INDICES(file_border,temp)
;     TEMP2=WHERE(file_border ne 1)
;;     Location2 = ARRAY_INDICES(file_border,TEMP2)
;;	 TEST = file_border-file_border
;     ;--------------------以下是对异常数据处理---------------------------
;
;     DROP_DATATYPE = widget_info((*pstate).DROP_DATATYPE, /droplist_select)
;      case drop_datatype of
;	   0:begin
;	     pos1=where(FILE_THIS_YEAR lt 0,count1)
;	     if count1 ne 0 then begin
;	     	FILE_THIS_YEAR[pos1]=0
;	     endif
;	     end
;	   1:begin
;	     pos2=where(FILE_THIS_YEAR lt 10,count2)
;	     if count2 ne 0 then begin
;	     	FILE_THIS_YEAR[pos2]=10
;	     endif
;	     end
;	   2:begin
;
;	     pos3=where(FILE_THIS_YEAR[0] gt 180,count3)
;	     if count3 ne 0 then begin
;	     	FILE_THIS_YEAR[pos3]=180
;	     endif
;         	pos4=where(FILE_THIS_YEAR lt 0,count4)
;	     if count4 ne 0 then begin
;	    	 FILE_THIS_YEAR[pos4]=40
;	     endif
;
; 	   end
;	   else:
;	  endcase
        ParaInfo = (*pstate).NQ_ProjectPara    ;得到省范围内的投影参数
        border_files=ParaInfo.gridpath
        DIFF=(FLOAT(FILE_THIS_YEAR)-FLOAT(FILE_LAST_YEAR))/abs(FLOAT(FILE_THIS_YEAR))*100
	    progressTimer->UPDATE, (0.5 * 100.0)  ;更新进度条
;	    widget_control,(*PSTATE).reso,get_value=Resolution

		border_file = NQ_CL_Read_ENVIData(border_files,SUCCESSSTATUS=Status,DESCRIPTION='耕地层基础文件的')
		border_file=congrid(border_file,samples,lines)
		IF Status EQ 0 THEN begin
            progressTimer->DESTROY ;销毁进度条
			RETURN
	    endif
		ZeroValue=WHERE(border_file EQ 0,COMPLEMENT=NoZero)         ;为取得省耕地边界外的坐标索引
        (*pstate).AddMin = 300-MIN(DIFF[NoZero])
		DIFF[ZeroValue]=0                                     ;将耕地边界外的值赋为0
		lastpic = DIFF										;插值后并去除非耕地后将被保存的数据
        DIFF[NoZero]   = DIFF[NoZero]+(*pstate).AddMin
;        WIDGET_CONTROL,(*pstate).drawid,SET_UVALUE = real_value


;	 DIFF[temp]=(FLOAT(FILE_LAST_YEAR[temp])-FLOAT(FILE_THIS_YEAR[temp]))/abs(FLOAT(FILE_THIS_YEAR[temp]))*100

     ;-----------------分级影像图------------
;	 lastpic=diff
     WRITEU,Lun_lastpic,lastpic
     HDR_INFO=NQ_classify_GET_IMAGE_INFO(INFILE)
     HDR_INFO.DATATYPE=4                    ;CWJ XIE
     WIDGET_CONTROL,(*pstate).lastpic,get_value=fileoutputname
     NQ_classify_write_HDR_FILE,fileoutputname,HDR_INFO
     free_lun,Lun_lastpic
	 progressTimer->UPDATE, (0.75 * 100.0)  ;更新进度条
;     DIFF[TEMP2]=100

	 ;	 lastpic=diff*float(file_border)          ;;;;;;;;;;;分级影像图生成
     ;;;;;;;;;;;生成分了3级的jpg图

	 MaxValue = MAX(DIFF,MIN= MinValue)
	 help,MaxValue,/str

	 PRINT,'分级参数:'
	 LEVEL_1_MIN=(*((*pstate).p))[0,0]+(*pstate).AddMin
	 LEVEL_2_MIN=(*((*pstate).p))[0,1]+(*pstate).AddMin
	 LEVEL_3_MIN=(*((*pstate).p))[0,2]+(*pstate).AddMin
	 LEVEL_1_MAX=(*((*pstate).p))[1,0]+(*pstate).AddMin
	 LEVEL_2_MAX=(*((*pstate).p))[1,1]+(*pstate).AddMin
	 LEVEL_3_MAX=(*((*pstate).p))[1,2]+(*pstate).AddMin

     ClassNew = fltarr(samples,lines)
     Class0 = WHERE(DIFF LT LEVEL_1_MIN and diff ne 0,Num0) 	      ;异常值
	 Class1 = WHERE((DIFF GE LEVEL_1_MIN) AND (DIFF LT LEVEL_1_MAX),Num1)
	 Class2 = WHERE((DIFF GE LEVEL_2_MIN) AND (DIFF LT LEVEL_2_MAX),Num2)
	 Class3 = WHERE((DIFF GE LEVEL_3_MIN) AND (DIFF LT LEVEL_3_MAX),Num3)
	 Class4 = WHERE((DIFF GE LEVEL_3_MAX),Num4)
	 Class5 = WHERE(DIFF eq 0,Num5)

	 IF Num0 NE 0 THEN ClassNew[Class0] = 1.;异常值
	 IF Num1 NE 0 THEN ClassNew[Class1] = 1.
	 IF Num2 NE 0 THEN ClassNew[Class2] = 2.
	 IF Num3 NE 0 THEN ClassNew[Class3] = 3.
	 IF Num4 NE 0 THEN ClassNew[Class4] = 3.  ;异常值
	 IF Num5 NE 0 THEN ClassNew[Class5] = 4.


;;     ClassNew = fltarr(samples,lines)
;     Class0 = WHERE(DIFF LT LEVEL_1_MIN,Num0) 	      ;异常值
;	 Class1 = WHERE((DIFF GE LEVEL_1_MIN) AND (DIFF LT LEVEL_1_MAX),Num1)
;	 Class2 = WHERE((DIFF GE LEVEL_2_MIN) AND (DIFF LT LEVEL_2_MAX),Num2)
;	 Class3 = WHERE((DIFF GE LEVEL_3_MIN) AND (DIFF LT LEVEL_3_MAX),Num3)
;	 Class4 = WHERE((DIFF GE LEVEL_3_MAX) AND (DIFF LT 100),Num4)
;	 Class5 = WHERE(DIFF eq 100,Num5)
;	 IF Num0 NE 0 THEN ClassNew[Class0] = 1.;异常值
;	 IF Num1 NE 0 THEN ClassNew[Class1] = 1.
;	 IF Num2 NE 0 THEN ClassNew[Class2] = 2.
;	 IF Num3 NE 0 THEN ClassNew[Class3] = 3.
;	 IF Num4 NE 0 THEN ClassNew[Class4] = 3.  ;异常值
;	 IF Num5 NE 0 THEN ClassNew[Class5] = 4.

	progressTimer->UPDATE, (0.85 * 100.0)  ;更新进度条
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ;分级jpg图
;     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_FILE_jpg,get_value=jpgfile
;     minData = min(ClassNew, max=maxData)
;     GrayData=byte(ClassNew)
;     widget_control, (*pstate).GrayData, set_uvalue = GrayData
;     NQ_classify_DisplayTiffImage,event
;	 widget_Control,(*pstate).WID_DRAW,GET_VALUE = oWindow
;	 oWindow->GetProperty,IMAGE_DATA =image
;	 jpgfile=jpgfile
;     WRITE_JPEG , jpgfile ,image, ORDER=0,/PROGRESSIVE, QUALITY=100,TRUE=1
	fileinfo = read_file(fileoutputname)
	ARR_DATA=ClassNew;fileinfo.dataarr

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
;==========================================================================
     ;关闭所有打开的文件
   	 free_lun,Lun_FILE_LAST_YEAR
	 free_lun,Lun_FILE_THIS_YEAR
	 free_lun,Lun_lastpic
	 free_lun,lun_file_jpg

	 progressTimer->UPDATE, (1 * 100.0)  ;更新进度条
	 progressTimer->DESTROY;销毁进度条

	 msg=DIALOG_MESSAGE('完成插值图像分级计算!',title='提示' )
	 log, '农气分析-对比分析', 1
;	 ptr_free,(*pstate).p
;	 HEAP_GC, /VERBOSE
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function NQ_classify_close,Event
	common_log,'关闭对比分析'
	 CLOSE,/all
     WIDGET_CONTROL, EVENT.TOP, /destroy
     RETURN, Event ;
END

pro NQ_PARAMETER_SETUP_CleanAllHeap,id
	widget_control, id, get_uvalue=pstate
	ptr_free,pstate
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;分级参数设置界面
pro NQ_classify_BASE_CLASSIFY_PARAMETER_SETUP,toppstate

  NQ_classify_BASE_CLASSIFY_PARAMETER_SETUP = Widget_Base(group_leader=(*toppstate).tlb,UNAME='NQ_classify_BASE_CLASSIFY_PARAMETER_SETUP',$
  		XOFFSET=400 ,YOFFSET=200  $
      ,SCR_XSIZE=279 ,SCR_YSIZE=180 ,TITLE='参数设置' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR=1,/modal)

  BASE_BUTTON_PARAMETER = Widget_Base(NQ_classify_BASE_CLASSIFY_PARAMETER_SETUP,  $
      UNAME='BASE_BUTTON_PARAMETER' ,FRAME=1 ,XOFFSET=7 ,YOFFSET=103  $
      ,SCR_XSIZE=259 ,SCR_YSIZE=39 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  CMD_CLASSIFY_PARAMETER_OK = Widget_Button(BASE_BUTTON_PARAMETER,  $
      UNAME='CMD_CLASSIFY_PARAMETER_OK' ,XOFFSET=20 ,YOFFSET=7  $
      ,SCR_XSIZE=80 ,SCR_YSIZE=23 ,EVENT_func='NQ_CMD_PARAMETER_OK'  $
      ,/ALIGN_CENTER ,VALUE='确定')


  CMD_CLASSIFY_PARAMETER_CANCEL =  $
      Widget_Button(BASE_BUTTON_PARAMETER,  $
      UNAME='CMD_CLASSIFY_PARAMETER_CANCEL' ,XOFFSET=160 ,YOFFSET=7  $
      ,SCR_XSIZE=80 ,SCR_YSIZE=23  $
      ,EVENT_FUNC='NQ_CMD_CLASSIFY_PARAMETER_CANCEL' ,/ALIGN_CENTER  $
      ,VALUE='关闭')


  BASE_CLASSIFY_PARAMETER = Widget_Base(NQ_classify_BASE_CLASSIFY_PARAMETER_SETUP,  $
      UNAME='BASE_CLASSIFY_PARAMETER' ,FRAME=1 ,XOFFSET=7 ,YOFFSET=7  $
      ,SCR_XSIZE=259 ,SCR_YSIZE=89 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  LBL_CLASSIFY_LEVEL_1_MAX = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_1_MAX' ,XOFFSET=140 ,YOFFSET=11  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='一级上限:')



  TXT_CLASSIFY_LEVEL_1_MAX = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_1_MAX' ,XOFFSET=202 ,YOFFSET=7  $
      ,SCR_XSIZE=48 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*(*toppstate).p)[1,0],2)  $
      ,XSIZE=20 ,YSIZE=1)

  TXT_CLASSIFY_LEVEL_2_MAX = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_2_MAX' ,XOFFSET=202 ,YOFFSET=34  $
      ,SCR_XSIZE=48 ,SCR_YSIZE=20 ,/EDITABLE,VALUE=STRTRIM((*(*toppstate).p)[1,1],2)    $
      ,XSIZE=20 ,YSIZE=1)

  LBL_CLASSIFY_LEVEL_2_MAX = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_2_MAX' ,XOFFSET=140 ,YOFFSET=38  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='二级上限:')

  TXT_CLASSIFY_LEVEL_3_MAX = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_3_MAX' ,XOFFSET=202 ,YOFFSET=61  $
      ,SCR_XSIZE=48 ,SCR_YSIZE=20 ,/EDITABLE,VALUE=STRTRIM((*(*toppstate).p)[1,2],2)  $
      ,XSIZE=20 ,YSIZE=1 )

  LBL_CLASSIFY_LEVEL_3_MAX = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_3_MAX' ,XOFFSET=140 ,YOFFSET=65  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='三级上限:')

  TXT_CLASSIFY_LEVEL_3_MIN = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_3_MIN' ,XOFFSET=69 ,YOFFSET=61  $

      ,SCR_XSIZE=48 ,SCR_YSIZE=20 ,/EDITABLE,VALUE=STRTRIM((*(*toppstate).p)[0,2],2)     $
      ,XSIZE=20 ,YSIZE=1)

  LBL_CLASSIFY_LEVEL_3_MIN = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_3_MIN' ,XOFFSET=7 ,YOFFSET=65  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='三级下限:')


  TXT_CLASSIFY_LEVEL_2_MIN = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_2_MIN' ,XOFFSET=69 ,YOFFSET=34  $
      ,SCR_XSIZE=48 ,SCR_YSIZE=20 ,/EDITABLE,VALUE=STRTRIM((*(*toppstate).p)[0,1],2)      $
      ,XSIZE=20 ,YSIZE=1)

  LBL_CLASSIFY_LEVEL_2_MIN = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_2_MIN' ,XOFFSET=7 ,YOFFSET=38  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='二级下限:')


  LBL_CLASSIFY_LEVEL_1_MIN = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_1_MIN' ,XOFFSET=7 ,YOFFSET=11  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='一级下限:')

  TXT_CLASSIFY_LEVEL_1_MIN = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_1_MIN' ,XOFFSET=69 ,YOFFSET=7  $
      ,SCR_XSIZE=48 ,SCR_YSIZE=20 ,/EDITABLE,VALUE=STRTRIM((*(*toppstate).p)[0,0],2)     $
      ,XSIZE=20 ,YSIZE=1)

  STATE={	$
  			toppstate			:	toppstate			,$
  			TXT_CLASSIFY_LEVEL_1_MAX:TXT_CLASSIFY_LEVEL_1_MAX	,$
  			TXT_CLASSIFY_LEVEL_1_MIN:TXT_CLASSIFY_LEVEL_1_MIN	,$
  			TXT_CLASSIFY_LEVEL_2_MAX:TXT_CLASSIFY_LEVEL_2_MAX	,$
  			TXT_CLASSIFY_LEVEL_2_MIN:TXT_CLASSIFY_LEVEL_2_MIN	,$
  			TXT_CLASSIFY_LEVEL_3_MAX:TXT_CLASSIFY_LEVEL_3_MAX	,$
  			TXT_CLASSIFY_LEVEL_3_MIN:TXT_CLASSIFY_LEVEL_3_MIN	 $
  			}

  PSTATE = PTR_NEW(STATE, /NO_COPY)
  WIDGET_CONTROL,NQ_classify_BASE_CLASSIFY_PARAMETER_SETUP, SET_UVALUE=PSTATE
  print,'(*(*((*pstate).toppstate).p))[0,0],(*((*pstate).p))[0,0],(*pstate).p,(*pstate).TXT_CLASSIFY_LEVEL_1_MAX'
  print,(*(*((*pstate).toppstate)).p)[0,0]
  Widget_Control, /REALIZE, NQ_classify_BASE_CLASSIFY_PARAMETER_SETUP

  XManager, 'NQ_classify_BASE_CLASSIFY_PARAMETER_SETUP', NQ_classify_BASE_CLASSIFY_PARAMETER_SETUP, /NO_BLOCK, CLEANUP='NQ_PARAMETER_SETUP_CleanAllHeap'
end

pro NQ_CLASSIFY_CLASSIFYAGRI_EVENT,event
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;主界面程序
PRO NQ_classify_classifyagri,GROUP_LEADER=groupleader
  common_log,'启动对比分析'
  IF ( XREGISTERED('NQ_classify_classifyagri') NE 0 ) THEN RETURN

  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
  TLB = Widget_Base(GROUP_LEADER=BASE_TOP,UNAME='TLB' ,XOFFSET=180  $
      ,YOFFSET=200,TITLE='对比分析' ,SPACE=3 ,XPAD=3 ,YPAD=3,TLB_FRAME_ATTR=1,/row)

  ;-----------------左边的BASE--------------------------------------------
  LEFT_BASE = Widget_Base(TLB, UNAME='LEFT_BASE' ,FRAME=0 ,XOFFSET=7  $
      ,YOFFSET=7 ,SCR_XSIZE=243 ,SCR_YSIZE=403 ,TITLE='IDL' ,SPACE=7  $
      ,XPAD=7 ,YPAD=7)
 ;;;;;;;;;;;;;;;;;;;控制时间;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  WID_time = Widget_base(LEFT_BASE, UNAME='WID_time' ,FRAME=1  $
     ,xoffset=0,yoffset=42,SCR_XSIZE=241 ,SCR_YSIZE=35,SPACE=3 ,XPAD=3  $
      ,YPAD=3,/row)
  label = widget_label(WID_time,value='时间：')
  list_year=['1980','1981','1982','1983','1984','1985','1986',$
  '1987','1988','1989','1990','1991','1992','1993','1994',$
  '1995','1996','1997','1998','1999','2000','2001','2002',$
  '2003','2004','2005','2006','2007','2008','2009','2010',$
  '2011','2012','2013','2014','2015']

  temp=(bin_date())[0]-1980
;=================================================================
  drop_year=widget_combobox(WID_time,uname='drop_year',xoffset=26,yoffset=29,value=list_year) ;原代码
  widget_control,drop_year, SET_COMBOBOX_SELECT=temp
  year=widget_label(WID_time,uname='year',value='年')

  drop_month=widget_combobox(WID_time,uname='drop_month',xoffset=102,yoffset=29,value=['1','2','3','4','5','6','7','8','9','10','11','12'])
  month=widget_label(WID_time,uname='month',value='月')

  drop_tenday=widget_combobox(WID_time,uname='drop_tenday',xoffset=169,yoffset=29,scr_xsize=37,value=['上','中','下'])
  tenday=widget_label(WID_time,uname='tenday',value='旬')
;=================================================================
;  drop_year = Widget_COMBOBOX(WID_time, UNAME='drop_year' ,XOFFSET=63  $	;原代码
;      ,YOFFSET=7,SCR_XSIZE=48 ,SCR_YSIZE=21,VALUE=list_year)
;
;  widget_control,drop_year, SET_COMBOBOX_SELECT=temp
;  label_month = Widget_Label(WID_time,  $
;      UNAME='label_month' ,XOFFSET=116 ,YOFFSET= 10 $
;      ,SCR_XSIZE=17 ,SCR_YSIZE=35 ,/ALIGN_LEFT ,VALUE='月')
;
;  drop_month = Widget_Droplist(WID_time,  $
;      UNAME='drop_month' ,XOFFSET=136 ,YOFFSET=7  $
;      ,SCR_XSIZE=35 ,SCR_YSIZE=21  $
;      ,EVENT_FUNC='' ,VALUE=[ '1', '2', '3',  $
;      '4', '5', '6', '7', '8', '9', '10', '11', '12' ])
;
;  label_day = Widget_Label(WID_time,  $
;      UNAME='label_day' ,XOFFSET=178 ,YOFFSET= 10 $
;      ,SCR_XSIZE=17 ,SCR_YSIZE=35 ,/ALIGN_LEFT ,VALUE='旬')
;
;  drop_day = Widget_Droplist(WID_time,  $
;      UNAME='drop_day' ,XOFFSET=199 ,YOFFSET=7  $
;      ,SCR_XSIZE=35 ,SCR_YSIZE=21  $
;      ,EVENT_FUNC='' ,VALUE=[ '1', '2', '3'])
;;;;;;;;;;;;
  wid_datatype = Widget_base(LEFT_BASE, UNAME='wid_datatype' ,FRAME=1  $
     ,xoffset=0,yoffset=0,SCR_XSIZE=241 ,SCR_YSIZE=35,SPACE=7 ,XPAD=7  $
      ,YPAD=7)
  label_datatype = Widget_Label(wid_datatype,  $
      UNAME='label_datatype',XOFFSET=20,YOFFSET=13 $
      ,SCR_YSIZE=35 ,/ALIGN_LEFT ,VALUE='气象要素：')
  drop_datatype = Widget_Droplist(wid_datatype,  $
      UNAME='drop_datatype' ,XOFFSET=86 ,YOFFSET=7  $
      ,SCR_YSIZE=21  $
      ,EVENT_pro='NQ_classify_selectdata' ,VALUE=[ '日降水量', '日照时数', '旬积温'])
;;;;;;;;;;;;;;;;;;;;;;;;;选择图像;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  wid_choose = Widget_base(LEFT_BASE, UNAME='wid_choose' ,FRAME=1  $
     ,xoffset=0,yoffset=84,SCR_XSIZE=241 ,SCR_YSIZE=250,SPACE=7 ,XPAD=7  $
      ,YPAD=7)

  label_shishi = Widget_Label(wid_choose,  $
      UNAME='label_shishi' ,XOFFSET=7 ,YOFFSET= 12 $
      ,SCR_XSIZE=120 ,SCR_YSIZE=23 ,/ALIGN_LEFT ,VALUE='实时气象栅格数据')

  CMD_shishi = Widget_Button(wid_choose,  $
      UNAME='CMD_shishi' ,XOFFSET=204 ,YOFFSET=7  $
      ,SCR_XSIZE=28 ,SCR_YSIZE=23  $
      ,EVENT_FUNC='NQ_classify_CMD_CLASSIFY_FILE_LAST_YEAR' ,/ALIGN_CENTER  $
      ,value='Image\open.bmp',/bitmap)

 TXT_CLASSIFY_FILE_LAST_YEAR = Widget_Text(wid_choose,  $
      UNAME='TXT_CLASSIFY_FILE_LAST_YEAR' ,XOFFSET=7 ,YOFFSET=37  $
      ,SCR_XSIZE=225 ,SCR_YSIZE=23  ,XSIZE=20 ,YSIZE=1,value='D:\WorkStation\省级系统_修改\代码\省级系统_20070903\data_test\040701'); $,/EDITABLE
      ;,value='D:\classify\实验数据\20060101AveTemp')

  label_lishi = Widget_Label(wid_choose,  $
      UNAME='label_lishi' ,XOFFSET=7 ,YOFFSET= 72 $
      ,SCR_XSIZE=144 ,SCR_YSIZE=23 ,/ALIGN_LEFT ,VALUE='历史实时气象栅格数据')

  CMD_lishi = Widget_Button(wid_choose,  $
      UNAME='CMD_lishi' ,XOFFSET=204 ,YOFFSET=67  $
      ,SCR_XSIZE=28 ,SCR_YSIZE=23  $
      ,EVENT_FUNC='NQ_classify_CMD_CLASSIFY_FILE_THIS_YEAR' ,/ALIGN_CENTER  $
      ,value='Image\open.bmp',/bitmap)

 TXT_CLASSIFY_FILE_THIS_YEAR = Widget_Text(wid_choose,  $
      UNAME='TXT_CLASSIFY_FILE_THIS_YEAR' ,XOFFSET=7 ,YOFFSET=97  $
      ,SCR_XSIZE=225 ,SCR_YSIZE=23 ,XSIZE=20 ,YSIZE=1,value='D:\WorkStation\省级系统_修改\代码\省级系统_20070903\data_test\050701'); $,/EDITABLE
      ;,value='D:\classify\实验数据\20060601AveTemp')

  label_jpg = Widget_Label(wid_choose,  $
      UNAME='label_jpg' ,XOFFSET=7 ,YOFFSET= 132 $
      ,SCR_XSIZE=144 ,SCR_YSIZE=23 ,/ALIGN_LEFT ,VALUE='输出的分级jpg图像')

  CMD_jpg = Widget_Button(wid_choose,  $
      UNAME='CMD_jpg' ,XOFFSET=204 ,YOFFSET=127  $
      ,SCR_XSIZE=28 ,SCR_YSIZE=23  $
      ,EVENT_FUNC='NQ_classify_CMD_CLASSIFY_CHOOSE_OUTPUT' ,/ALIGN_CENTER  $
      ,value='Image\open.bmp',/bitmap)

 TXT_CLASSIFY_FILE_jpg = Widget_Text(wid_choose,  $
      UNAME='TXT_CLASSIFY_FILE_jpg' ,XOFFSET=7 ,YOFFSET=157  $
      ,SCR_XSIZE=225 ,SCR_YSIZE=23  ,XSIZE=20 ,YSIZE=1,value='data_test\result_jpg');$,/EDITABLE
      ;,value='D:\classify\实验结果\d1')

  label_fenji = Widget_Label(wid_choose,  $
      UNAME='label_jpg' ,XOFFSET=7 ,YOFFSET= 192 $
      ,SCR_XSIZE=144 ,SCR_YSIZE=23 ,/ALIGN_LEFT ,VALUE='输出的分级影像图像')

  CMD_fenji = Widget_Button(wid_choose,  $
      UNAME='CMD_jpg' ,XOFFSET=204 ,YOFFSET=187  $
      ,SCR_XSIZE=28 ,SCR_YSIZE=23  $
      ,EVENT_FUNC='NQ_classify_CMD_OUTPUT' ,/ALIGN_CENTER  $
      ,value='Image\open.bmp',/bitmap)

 lastpic = Widget_Text(wid_choose,  $
      UNAME='TXT_CLASSIFY_FILE_THIS_YEAR' ,XOFFSET=7 ,YOFFSET=217  $
      ,SCR_XSIZE=225 ,SCR_YSIZE=23  ,XSIZE=20 ,YSIZE=1,value='data_test\result_img');$,/EDITABLE
      ;,value='D:\classify\实验结果\d2')

 CMD_CLASSIFY_CHANGE_PARAMETER = Widget_Button(LEFT_BASE,  $
      UNAME='CMD_classify_CHANGE_PARAMETER' ,XOFFSET=0 ,YOFFSET=344  $
      ,SCR_XSIZE=241 ,SCR_YSIZE=23 ,EVENT_pro='NQ_CMD_classify_CHANGE_PARAMETER'  $
      ,/ALIGN_CENTER ,VALUE='进行分级参数设定')

  CMD_CLASSIFY_OK = Widget_Button(LEFT_BASE,  $
      UNAME='CMD_CLASSIFY_OK' ,XOFFSET=0 ,YOFFSET=378+1 ,SCR_XSIZE=76  $
      ,SCR_YSIZE=23 ,EVENT_pro='NQ_classify_output' ,/ALIGN_CENTER  $
      ,VALUE='运行')

  CMD_CLASSIFY_NOTATION_HELP = Widget_Button(LEFT_BASE,  $
      UNAME='CMD_CLASSIFY_NOTATION_HELP' ,XOFFSET=89-6 ,YOFFSET=378+1 ,SCR_XSIZE=76  $
      ,SCR_YSIZE=23 ,EVENT_FUNC='' ,/ALIGN_CENTER  $
      ,VALUE='帮助',EVENT_PRO='CMD_CLASSIFY_NOTATION_HELP_EVENT')

  CMD_CLASSIFY_CANCEL = Widget_Button(LEFT_BASE,  $
      UNAME='CMD_CLASSIFY_CANCEL' ,XOFFSET=172-6 ,YOFFSET=378+1  $
      ,SCR_XSIZE=75 ,SCR_YSIZE=23 ,EVENT_FUNC='NQ_classify_close'  $
      ,/ALIGN_CENTER ,VALUE='关闭')

 ;;;;;;;;;;;;;;;;;;;右边图像;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  right_BASE = Widget_Base(TLB, UNAME='right_BASE' ,FRAME=1 ,XOFFSET=254  $
      ,YOFFSET=7 ,SPACE=3  $
      ,XPAD=1 ,YPAD=3,/column)
  label_show=Widget_Label(right_BASE,  $
      UNAME='label_show' ,YOFFSET= 7,xoffset=188 $
      ,SCR_XSIZE=50 ,SCR_YSIZE=11 ,/ALIGN_center ,VALUE='分级结果')
;  WID_DRAW = Widget_Draw(right_BASE, UNAME='WID_DRAW' ,FRAME=0 $
;      ,YOFFSET=25 ,xoffset=0,SCR_XSIZE=400 ,SCR_YSIZE=376 $
;      ,GRAPHICS_LEVEL=2,retain=2)
base_id = Widget_Base(right_BASE,SCR_XSIZE=400 ,SCR_YSIZE=376,/col,xpad=0,ypad=0,space=0,/frame)

	colorLevel = $
	[[255B,	255B,	255B],$;1级0
	 [255B,	0B,		0B],$;2级1
	 [0B,	0B,	  255B],$;3级2
	 [0B,	255B,	0B],$;4级3
	 [255B, 255B,	255B]];5级4
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    a=fltarr(2,3)
    b=fltarr(2,3)
    c=fltarr(2,3)
    Province = ['北京市','天津市','河北','山西','内蒙古','辽宁','吉林','黑龙江','上海市','江苏' $
						,'浙江','安微','福建','江西','山东','河南','湖北','湖南','广东','广西','海南' $
						,'重庆市','四川','贵州','云南','西藏','陕西','甘肃','青海','宁夏','新疆']
	ProIDList = ['11','12','13','14','15','21','22','23','31','32','33','34','35','36','37',$
						'41','42','43','44','45','46','50','51','52','53','54','61','62','63','64','65']

    ProCode = STRMID(PROVINCE_CODE,0,2)      ;这三量设在这里是为接受系统设置所预设的参数

    NQ_ProjectPara = NQ_readparameter(Province[WHERE(ProIDList EQ ProCode)])



   ;定义图像大小
   xsize = 659L      ;初始值 ;后面的程序中可以改变
   ysize = 883L      ;初始值 后面的程序中可以改变

    Red = [255,0,0,255] & Green = [255,255,0,0] & Blue = [255,0,255,0]
    thePalette = OBJ_NEW('IDLgrPalette',Red,Green,Blue)
    STATE = { $
			;---画图组件------------------------
		    GrayData:right_BASE,$
 		   thePalette	: thePalette, $	;调色板对像
           xpad		:	0.0,	$	;横向平移系数
	       ypad		:	0.0,	$	;纵向平移系数
		   zoomFactor	:	1.0,	$	;缩放系数
           NQ_ProjectPara:  NQ_ProjectPara		,$
           Province		 :  Province			,$
           ProID		 :  ProCode			    ,$
           ProIDList	 :  ProIDList			,$
;           WID_DRAW                    :  WID_DRAW,  		$
           DROP_DATATYPE               :  DROP_DATATYPE,   $
           datatype                    :  0,$
           xsize:xsize,$
           ysize:ysize,$                 ;            记录选择什么类型数据进行分级，以便得到分级参数
		   TXT_CLASSIFY_FILE_THIS_YEAR	:TXT_CLASSIFY_FILE_THIS_YEAR	,$
		   TXT_CLASSIFY_FILE_LAST_YEAR :TXT_CLASSIFY_FILE_LAST_YEAR	,$
           TXT_CLASSIFY_FILE_jpg:TXT_CLASSIFY_FILE_jpg	,$
           AddMin:0.0,$
           a:[[-40,-25],[-25,25],[25,100]],$
           b:[[-100,-30],[-30,30],[30,100]],$
           c:[[-100,-20],[-20,20],[20,100]],$
           lastpic:lastpic,$
           drop_year:drop_year,$
           drop_month:drop_month,$
           drop_tenday:drop_tenday,$
           tlb			:tlb				,$
           p           :ptr_new()          ,$;创建一个指针，指向分级参数
           Muple_isx_y:FLTARR(2)           ,   $
                   ;标识哪一个Tab被选择.以明确是计算par还是apar.
           pstaff_display:pstaff_display $
            }

  PA = PTR_NEW(STATE, /NO_COPY)
  WIDGET_CONTROL, tlb, SET_UVALUE=PA
  widget_control,tlb,/realize

  WIDGET_CONTROL,CMD_CLASSIFY_CANCEL,/INPUT_FOCUS
;  widget_control,WID_DRAW,get_value=test
;  test->ERASE
;  theView = Obj_New('IDLgrView', Viewplane_Rect =[0,0,400,376], color = [255,255,255])
;  test->DRAW,theView
  WIDGET_CONTROL,(*PA).drop_month, SET_droplist_SELECT=6

    WIDGET_CONTROL,(*PA).drop_month, SET_COMBOBOX_SELECT=(bin_date())[1]-1
	tenday_index = (bin_date())[2]/10
	tenday_index = (tenday_index gt 2)? 2 : tenday_index
	WIDGET_CONTROL,(*PA).drop_tenday, SET_COMBOBOX_SELECT=tenday_index

	COMMON COMMON_SETPATH,ppath
	v_nq_in_path  = (*ppath).nq_in_path
	v_nq_out_path = (*ppath).nq_out_path

	WIDGET_CONTROL,(*PA).TXT_CLASSIFY_FILE_THIS_YEAR, set_value=v_nq_in_path
	WIDGET_CONTROL,(*PA).TXT_CLASSIFY_FILE_LAST_YEAR, set_value=v_nq_in_path
	WIDGET_CONTROL,(*PA).TXT_CLASSIFY_FILE_jpg, set_value=v_nq_out_path+'result.jpg'
	WIDGET_CONTROL,(*PA).lastpic, set_value=v_nq_out_path+'result_img'

  XMANAGER,'NQ_classify_classifyagri', CLEANUP='NQ_classify_CleanAllHeap',TLB,/NO_BLOCK
end




