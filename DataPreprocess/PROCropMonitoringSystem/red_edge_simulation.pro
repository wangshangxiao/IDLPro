FUNCTION RE_SIM,EVENT

;	CATCH, Error_status
;	;This statement begins the error handler:
;	IF Error_status NE 0 THEN BEGIN
;		PRINT, 'Error index: ', Error_status
;		PRINT, 'Error message: ', !ERROR_STATE.MSG
;		if obj_valid(progressTimer) ne 0 then $
;			OBJ_DESTROY,progressTimer ;销毁进度条
;		help, /last_message, output=errtext
;		common_log,'计算出错' + errtext
;		Result = DIALOG_MESSAGE(errtext, /CENTER)
;		CATCH, /CANCEL
;		return,0
;	ENDIF

	Widget_Control,Event.top,get_uvalue=PSTATE
	Widget_Control,(*PSTATE).INPUT_FIELD,get_value=inputfile
	Widget_Control,(*PSTATE).OUTPUT_FIELD,get_value=outputfile

	;检测输入、输出文件
	if file_test(inputfile) eq 0 then begin
		CAUTION = dialog_message('输入的文件不存在!',title='警告')
		return,0
	endif

	if strcompress(outputfile,/remove_all) eq '' then begin
		CAUTION = dialog_message('输出的文件未设置!',title='警告')
		return,0
	endif

	;确保输出文件类型的正确性
	tempfile=strsplit(outputfile,'.',/extract)
	outputfile=tempfile[0]+'.tif'

	queryStatus = query_image(inputfile,imginfo)
	channels = imginfo.channels

;根据传感器类型确定相应的参数文件
	case 1 of
		Widget_Info((*PSTATE).Hyperion_Butt,/BUTTON_SET) : begin
			bandnum = 242
		   wavfile='.\PARA\Hyperion_WV.txt'
		   wavelength=fltarr(bandnum)
   		openr,lun,wavfile,/get_lun
   		readf,lun,wavelength
   		free_lun,lun
		end
		Widget_Info((*PSTATE).HSI_Butt,/BUTTON_SET) : begin
			bandnum = 115
		   wavfile='.\PARA\HSI_WV.txt'
		   wavelength=fltarr(bandnum)
   		openr,lun,wavfile,/get_lun
   		readf,lun,wavelength
   		free_lun,lun
		end
		Widget_Info((*PSTATE).MERIS_Butt,/BUTTON_SET) : begin
			bandnum = 15
		   wavfile='.\PARA\MERIS_WV.txt'
		   wavelength=fltarr(bandnum)
   		openr,lun,wavfile,/get_lun
   		readf,lun,wavelength
   		free_lun,lun
		end
		else :
	endcase

	if channels ne bandnum then begin
		CAUTION = dialog_message('输入影像的波段数与选择的传感器不匹配！',title='警告')
		return,0
	endif

	progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='数据处理中,请稍候!',TITLE='红边参数模拟')
	progressTimer->START
	CANCELLED = PROGRESSTIMER->CHECKCANCEL()
	progressTimer->UPDATE, 10
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF

;读入数据并记录数据维数
	data=read_tiff(inputfile,geotiff=geoinfo,interleave=2)
   img_size=[imginfo.dimensions[0],imginfo.dimensions[1]]

;确定模拟的起始位置和终止位置
	inital_index=where(abs(wavelength-670) eq min(abs(wavelength-670)))
	inital=inital_index[0]

	endwv_index=where(abs(wavelength-800) eq min(abs(wavelength-800)))
	endwv=endwv_index[0]

;确定红肩和红谷的位置
	R0_index=where(abs(wavelength-685) eq min(abs(wavelength-685)))
	Rs_index=where(abs(wavelength-780) eq min(abs(wavelength-780)))

;计算模拟参数用到的波段数
	sim_num=endwv-inital+1

   sim=indgen(sim_num)+inital

   out_coeff=fltarr(img_size[0],img_size[1],6)

;	count=0

	for nx=0,img_size[0]-1,1 do begin
	   for ny=0,img_size[1]-1,1 do begin

			R0 = min(data[nx,ny,inital:R0_index])
			Rs = max(data[nx,ny,Rs_index-1:endwv-1])

			B = fltarr(sim_num)

			B = sqrt(-alog((Rs-data[nx,ny,inital:(inital+sim_num-1)])/(Rs-R0)))

			good = WHERE( FINITE(B) )
			if (good[0] eq -1) or (n_elements(good) le 1) then continue
			a1 = REGRESS(wavelength[sim[good]],B[good],const=a0)

			;红谷波长位置
			out_coeff[nx,ny,0]=-a0/a1

			if out_coeff[nx,ny,0] lt wavelength[inital_index] then begin
				out_coeff[nx,ny,*] = 0.0
				continue
			endif

			;红边吸收谷宽度
			out_coeff[nx,ny,1]=1/(sqrt(2)*a1)

			yfit = Rs-(Rs-R0)*exp((-(out_coeff[nx,ny,0]-wavelength[sim])^2)/(2*out_coeff[nx,ny,1]^2))

			;红边波长位置
			out_coeff[nx,ny,2]=out_coeff[nx,ny,0]+out_coeff[nx,ny,1]

			;红边斜率
			index=where(abs(wavelength-out_coeff[nx,ny,2]) eq min(abs(wavelength-out_coeff[nx,ny,2])))

			if n_elements(index) gt 1 then index=index[0]
			if (index le 0) or (index ge bandnum-1) or (index-inital le 0) or (index-inital ge n_elements(yfit)-1) then begin
				out_coeff[nx,ny,3] = 0
			endif else begin
				der = deriv(wavelength[index-1:index+1],yfit[(index-inital-1):(index-inital+1)])
				out_coeff[nx,ny,3]=der[1]
			endelse

			;红边振幅（红肩与红谷反射率之差）
			out_coeff[nx,ny,4]=Rs-R0

			;模拟的红边与原光谱曲线的相关性，用于检验模拟精度
			y = data[nx,ny,inital:(inital+sim_num-1)]

			out_coeff[nx,ny,5]=CORRELATE(y,yfit)

;			count++
;
;			if count eq 780 then begin
;				x1=indgen(bandnum)
;				y1=fltarr(bandnum)
;				y1[0:*]=data[nx,ny,*]
;				PLOT, wavelength[x1], y1, TITLE='IG Simulation'+strtrim(string(1),2)
;				OPLOT, wavelength[sim], yfit, THICK=2,color=255
;			endif
      endfor

		progressTimer->UPDATE, 10+(nx*80/(img_size[0]-1))
		IF CANCELLED THEN BEGIN
			OK = DIALOG_MESSAGE('用户终止了操作')
			PROGRESSTIMER->DESTROY ;结束进度条
			RETURN,0
		ENDIF
   endfor

	write_tiff,outputfile,out_coeff,/float,PLANARCONFIG=2,geotiff=geoinfo

	PROGRESSTIMER->UPDATE, 100

	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('用户终止了操作')
		PROGRESSTIMER->DESTROY ;结束进度条
		RETURN,0
	ENDIF

	PROGRESSTIMER->DESTROY

	return,1
END

PRO RED_EDGE_SIMULATION_EVENT,EVENT

	on_error,2

	Widget_Control,Event.top,get_uvalue=PSTATE

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
			widget_info(Event.id, /tree_root) : event.id)

	wWidget =  Event.top

	case wTarget of
		;打开输入文件的控件
		Widget_Info(wWidget, FIND_BY_UNAME='INPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'打开输入影像'
		end

		;打开输出文件的控件
		Widget_Info(wWidget, FIND_BY_UNAME='OUTPUT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
      		PICK_FILE,event
      		common_log,'打开输出影像'
		end

		;指令控制区域的响应
		Widget_Info(wWidget, FIND_BY_UNAME='CAL_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				;运行程序
      		STATUS=RE_SIM(event)

				if STATUS eq 1 then $
					CAL_INFO=dialog_message('计算完成！',/information,title='提示')
      		common_log,'进行运算'
		end

		Widget_Info(wWidget, FIND_BY_UNAME='HELP_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then begin
				common_log,'打开帮助文档'
      		if file_test('HELP\HELP.chm') then begin
					ONLINE_HELP, '指数计算模块', BOOK='HELP\HELP.chm', /FULL_PATH
				endif else begin
					info_help=dialog_message('找不到帮助文档',title='警告')
				endelse
			endif
		end

		Widget_Info(wWidget, FIND_BY_UNAME='EXIT_BUTTON'): begin
      	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
				common_log,'退出程序'
		  		PTR_FREE,PSTATE
				HEAP_GC, /VERBOSE
				widget_control,event.top,/DESTROY
		end
		else :
	endcase

	IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
		common_log,'退出程序'
  		PTR_FREE,PSTATE
		HEAP_GC, /VERBOSE
		widget_control,event.top,/DESTROY
	ENDIF

END

PRO RED_EDGE_SIMULATION,GROUP_LEADER=wGroup, _EXTRA=_VWBExtra

	common_log,'红边参数模拟'

;设置程序初始位置
	scr_dims = GET_SCREEN_SIZE()
	w_xoffset=(scr_dims[0]-350)/2
	w_yoffset=(scr_dims[1]-350)/2

;设置主框架
	TLB_BASE = WIDGET_BASE(GROUP_LEADER=wGroup, UNAME='TLB_BASE' $
		,XOFFSET=w_xoffset ,YOFFSET=w_yoffset ,/TLB_MOVE_EVENTS  $
      ,TITLE='植被红边参数模拟' ,SPACE=3 ,XPAD=3 ,YPAD=1  $
      ,COLUMN=1 $ ;设定字组件按列或行排列
      ,/TLB_KILL_REQUEST_EVENTS,TLB_FRAME_ATTR=1) ;设置关闭组件事件

;设置子框架
	FILE_BASE = Widget_Base(TLB_BASE,/COLUMN,UNAME='FILE_BASE',/FRAME)

;选择文件组件
	INPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='INPUT_BASE',/align_right)
		INPUT_FIELD = CW_FIELD(INPUT_BASE,UNAME='INPUT_FIELD',TITLE='输入影像',XSIZE=35 ,YSIZE=1,NOEDIT=1)
		INPUT_BUTTON = Widget_Button(INPUT_BASE,UNAME='INPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
				,SCR_XSIZE=36 ,SCR_YSIZE=15)

	pfinfo = {field_id:INPUT_FIELD, filter:'*.tif', title:'输入影像文件'}
		widget_control,INPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

	OUTPUT_BASE = Widget_Base(FILE_BASE,/ROW,UNAME='OUTPUT_BASE',/align_right)
		OUTPUT_FIELD = CW_FIELD(OUTPUT_BASE,UNAME='OUTPUT_FIELD',TITLE='输出影像',XSIZE=35 ,YSIZE=1,NOEDIT=1)
		OUTPUT_BUTTON = Widget_Button(OUTPUT_BASE,UNAME='OUTPUT_BUTTON',VALUE='.\Image\Open.bmp',/BITMAP $
				,SCR_XSIZE=36 ,SCR_YSIZE=15)

	pfinfo = {field_id:OUTPUT_FIELD, filter:'*.tif', title:'输出影像文件'}
		widget_control,OUTPUT_BUTTON, set_uvalue=pfinfo,/NO_COPY

	TYPE_BASE = Widget_Base(TLB_BASE,UNAME='TYPE_BASE',/COLUMN,/FRAME,XPAD=2)
		TYPE_LABEL = Widget_Label(TYPE_BASE,UNAME='TYPE_LABEL',VALUE='传感器类型：',/ALIGN_LEFT)
	SENSOR_TYPE_BASE = Widget_Base(TYPE_BASE,UNAME='SENSOR_TYPE_BASE',/ROW,/EXCLUSIVE)
		Hyperion_Butt = Widget_Button(SENSOR_TYPE_BASE,UNAME='Hyperion_Butt',value='Hyperion数据')
		HSI_Butt = Widget_Button(SENSOR_TYPE_BASE,UNAME='HSI_Butt',value='环境星HSI数据')
		MERIS_Butt = Widget_Button(SENSOR_TYPE_BASE,UNAME='MERIS_Butt',value='MERIS数据')

	Widget_Control,Hyperion_Butt,/set_button

	;指令控制区域
	CMD_BASE = Widget_Base(TLB_BASE,UNAME='CMD_BASE',/FRAME,SCR_XSIZE=328,SCR_YSIZE=32,/row,space=53,XPAD=15)
		CAL_BUTTON = Widget_Button(CMD_BASE,VALUE='计算',UNAME='CAL_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		HELP_BUTTON = Widget_Button(CMD_BASE,VALUE='帮助',UNAME='HELP_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)
		EXIT_BUTTON = Widget_Button(CMD_BASE,VALUE='退出',UNAME='EXIT_BUTTON',SCR_XSIZE=60,SCR_YSIZE=25)

	Widget_Control,TLB_BASE,/REALIZE

	winfo = {	INPUT_FIELD	:	INPUT_FIELD , $
					OUTPUT_FIELD	:	OUTPUT_FIELD , $
					Hyperion_Butt	:	Hyperion_Butt , $
					HSI_Butt	:	HSI_Butt , $
					MERIS_Butt	:	MERIS_Butt $
				}

	PSTATE = PTR_NEW(winfo,/NO_COPY)

	Widget_Control,TLB_BASE,set_uvalue=PSTATE

	Xmanager,'RED_EDGE_SIMULATION',TLB_BASE,/NO_BLOCK

END