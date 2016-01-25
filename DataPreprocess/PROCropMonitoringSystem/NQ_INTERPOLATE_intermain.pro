;气象插值模块
;---------------------------------------------------------------------------
pro NQ_INTERPOLATEMAIN_HELP_EVENT,event
	 PRINT,'数据异常检测,帮助'

	if file_test('HELP\HELP.chm') then begin
		ONLINE_HELP, '气象插值', BOOK='HELP\HELP.chm', /FULL_PATH
	endif else begin
		info_help=dialog_message('找不到帮助文档',title='警告')
	endelse
end
;------------------------------------------------------------------
;用于选择是否要用到dem数据
FUNCTION WID_DEM_check, EVENT

	FORWARD_FUNCTION NQ_interpolate_ERROR_MESSAGE

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

   if  (*pstate).DEMSentive EQ 0 then begin

;      WIDGET_CONTROL,(*pstate).demfile, SENSITIVE  = 1
      WIDGET_CONTROL,(*pstate).bt_opendem, SENSITIVE  = 1
      (*pstate).DEMSentive=1

   endif else  begin

;      WIDGET_CONTROL,(*pstate).demfile, SENSITIVE  = 0
      WIDGET_CONTROL,(*pstate).bt_opendem, SENSITIVE  = 0
      (*pstate).DEMSentive=0
   endelse
END

;---------------------------------------------------------------------------------------
PRO CURSOR,EVENT                                  ;   指针活动事件
; 		 wWidget =  Event.top
;        WIDGET_CONTROL, WWIDGET,GET_UVALUE=PA
;        WIDGET_CONTROL, EVENT.ID,GET_UVALUE = Image
;		IF ARRAY_EQUAL(Image,0.0) THEN BEGIN    ;
;		  Widget_Control,(*PA).ValueBox,SET_VALUE = ''
;		  RETURN
;		ENDIF
;
;        Imagedata = rotate(Image,2)    ;之所以按行反转,是因为DRAW组件坐标是从左下角开始的.
;      ;  Imagedata=rotate(Image,2)
;	  	Datasize=SIZE(Imagedata,/DIMENSIONS)
;	    ImageMatchWidth = Datasize[0]  & ImageMatchHeight = Datasize[1]
;
;		CorX = FLOOR((event.x) * (*PA).Muple_isx_y[0])
;		CorY = FLOOR(event.y * (*PA).Muple_isx_y[0])
;
;        IF  Event.type EQ 2 THEN BEGIN
;			IF (*PA).Muple_isx_y[1] EQ 0.0 THEN BEGIN
;			   IF event.y GT FLOOR(ImageMatchHeight/(*PA).Muple_isx_y[0]) THEN BEGIN
;			      showdata = ''
;			   ENDIF ELSE showdata = STRTRIM((Imagedata[CorX,CorY]-300),2)
;			ENDIF ELSE BEGIN
;			   IF event.x GT FLOOR(ImageMatchWidth/(*PA).Muple_isx_y[0]) THEN BEGIN
;			      showdata = ''
;			   ENDIF ELSE showdata = STRTRIM((Imagedata[CorX,CorY]-300),2)
;
;			ENDELSE
;		print,showdata
;        Widget_Control,(*PA).ValueBox,SET_VALUE = showdata
;        ENDIF
;        HEAP_GC, /VERBOSE

 		 wWidget =  Event.top
         WIDGET_CONTROL, WWIDGET,GET_UVALUE=pstate

        IF NOT OBJ_VALID((*pstate).oView) THEN RETURN   ;如果是空对象则返回.

;		ParaInfo = (*state).ProjectPara    			;得到省范围内的投影参数
;        IF SIZE(ParaInfo,/TYPE) NE 8 THEN BEGIN     ;即返回的不是结构体,而是空.
;        	PRMPT = DIALOG_MESSAGE('没有'+ProviceName+'的基础参数信息,请查看相应的参数设置文件!')
;        	RETURN
;        ENDIF

		IF Event.type EQ 0 OR Event.type EQ 2 THEN BEGIN

		    (*pstate).oView->GetProperty,Location = viewLoc,Dimensions = viewDim
			xSize = (*pstate).xsize & ySize = (*pstate).ysize ;影像宽高
			Zoom = 1.*viewDim[0]/xSize
			nPos = ([event.x,event.y]-viewLoc)/zoom

			IF (nPos[0] LT -1) OR (nPos[0] GT xSize+2) THEN Return
			IF (nPos[1] LT -1) OR (nPos[1] GT ySize+2) THEN Return

			pos = [xSize, ySize]<nPos>0

			IF Max(pos) LE 5 THEN Return

            widget_Control,(*pstate).drawid,GET_UVALUE = Imagedata
        	Imagedata = REVERSE(TEMPORARY(Imagedata),2)    ;之所以按行反转,是因为DRAW组件坐标是从左下角开始的.
;        	if pos[0] ge 1069 then begin
;        	   pos[0]=1068
;        	endif
;        	if pos[1] ge 1026 then begin
;        	   pos[1]=1026
;        	endif
            returnsize=size(Imagedata)

            pos[0]<=returnsize[1]-1

            pos[1]<=returnsize[2]-1
		    dataValue = Imagedata[fix(pos[0]),fix(pos[1])]

   ;         print,dataValue
		    IF dataValue EQ 0.0 THEN showdata='' ELSE showdata=STRTRIM(dataValue-(*pstate).AddMin,2)
	    	Widget_Control,(*pstate).ValueBox,SET_VALUE = showdata
		;	WIDGET_CONTROL,EVENT.ID,TOOLTIP=ProText
	      	ENDIF
END
;--------------------------------------------------------------------------------------------
PRO NQ_Draw_Resultimage,ImageData  $                 ;为指针类型.要绘图的数据,
				    ,WID_DRAW	$                  ;DRAW组件.
					,MUPLE_ISX_Y = muple_isx_y    ;返回的参数为两元素的数据,一个为倍数,一个标识是哪个方向.

    WIDGET_CONTROL,/HOURGLASS
    WIDGET_CONTROL,WID_DRAW,GET_VALUE = drawID
    ERASE

	white=!D.N_COLORS-1
	WSET,drawID
	drawID=INDGEN(2)
	PLOT,drawID,BACKGROUND=WHITE

    r=widget_info(WID_DRAW,/GEOMETRY)     ;&&&&&&&&&&组件
  	data = *ImageData
  	Datasize=SIZE(data,/DIMENSIONS)
    ImageMatchWidth = Datasize[0]  & ImageMatchHeight = Datasize[1]
    Muple = (FLOAT(ImageMatchWidth)/r.SCR_XSIZE) > (FLOAT(ImageMatchHeight)/r.SCR_YSIZE)  ;;如果不用浮点型,两个整型相除,
    dataRe=CONGRID(data,ROUND(ImageMatchWidth/Muple),ROUND(ImageMatchHeight/Muple))  ;将图放缩相同倍数,以便能显示在窗口中,同时保持图形原始性.
	print,min(dataRe),max(dataRe)
	;dataDis = BYTSCL(dataRe,TOP = !D.TABLE_SIZE-1,min=1)  ;使用BYTSCL后,一般使用TV,而不是TVSCL
    dataDis=  dataRe
    ;-----------------------
    DEVICE,GET_DECOMPOSED=old_color                               ;获取当前DECOMPOSED值
    DEVICE, RETAIN=2, DECOMPOSED=0      ;用IDL提供后备存储,使用颜色查询表(停用颜色分解功能),
    !P.BACKGROUND=0        ;"!P":The main plotting system variable structure
    LOADCT,39
    TVLCT,255,255,255    ;使0号颜色索引号颜色为白色
   ;-------------------------
    TV,dataDis,/ORDER
;    TVSCL,dataDis,/ORDER
;    下面是为指针取值作准备.下面的"0.0"表示X轴方向匹配显示影像,"1.0"则表示Y轴方向匹配显示影像。
	IsX_Y = ((ImageMatchWidth/r.SCR_XSIZE) GE (ImageMatchHeight/r.SCR_YSIZE) ? 0.0 : 1.0)
	muple_isx_y = [Muple,IsX_Y]
	IF SIZE(*ImageData,/TYPE) EQ 1 THEN BEGIN        ;如果是字节型则转成整型,为画图所用
	  WIDGET_CONTROL,WID_DRAW,SET_UVALUE =FIX(*ImageData)  ;不然显示指针值时用"字符相关函数"会出现问题.
	ENDIF ELSE	WIDGET_CONTROL,WID_DRAW,SET_UVALUE = *ImageData

	PTR_free,ImageData
    DEVICE,DECOMPOSED=old_color                                     ;返回原来的DECOMPOSED值,,须还原.
END
;-----------------------------------------------------------------------------------
pro pointhis,event             ;点击历史平均数据插值
;    print,'ttt'
	WIDGET_CONTROL,event.top,GET_UVALUE=Pstate

	IF EVENT.SELECT EQ 1 THEN begin
    widget_control, (*pstate).drop_nianxian, SENSITIVE=1
    (*pstate).point=1
    endif
end

pro pointsshi,event             ;点击实时数据
	WIDGET_CONTROL,event.top,GET_UVALUE=Pstate
	IF EVENT.SELECT EQ 1 THEN begin
    widget_control, (*pstate).drop_nianxian, SENSITIVE=0
        (*pstate).point=0
    endif
end

;------------------------------------------------------------------------
function opendem_event, event
     COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     demfile= dialog_pickfile(dialog_parent=event.top, title='打开影像文件(Envi标准格式)', filter=['*.*'],path=FILE_PATH,/MUST_EXIST)

     IF (demfile NE '') THEN BEGIN
     	  WIDGET_CONTROL,Event.top,get_UVALUE = pstate
          WIDGET_CONTROL, (*pstate).demfile, set_VALUE=demfile
     ENDIF

     RETURN, Event ; By Default, return the event.
end
;------------------------------------------------------------------------------------
  function nq_inter_cmd_output, Event
     COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	 COMMON COMMON_SETPATH,ppath
	 v_nq_in_path  = (*ppath).nq_in_path

     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     outputfile=dialog_pickfile(/write,dialog_parent=event.top, title='保存影像文件(Envi标准格式)', filter=['*.*'],path=v_nq_in_path)

     IF (outputfile NE '') THEN BEGIN
     	  WIDGET_CONTROL,Event.top,get_UVALUE = pstate
          WIDGET_CONTROL, (*pstate).outputfile, set_VALUE=outputfile
     ENDIF

     RETURN, Event ; By Default, return the event.

end
;-------------------------------------------------------------------------------------

pro RadialBasisFunction_para,event
	wTLB = WIDGET_BASE(group_leader = event.top, /modal, /row, XOFFSET=300, YOFFSET=200, /BASE_ALIGN_TOP,$
		TITLE='(RadialBasisFunction)插值参数', TLB_FRAME_ATTR=1)

       wTab= WIDGET_BASE(wTLB,/COLUMN)
       Anisotropy=[1.0,1.0,0.0];各向异性值

       wT2 = WIDGET_BASE(wTab, TITLE='选项',space=5,xsize=500,ysize=420)

       base = Widget_Base( wT2, Column =2, Frame = 1,xsize=245,xoffset=10 ,yoffset=5,/base_align_center)

       AniBase1 = Widget_Base(base, /Column)
       label = Widget_Label(AniBase1, value = '各向异性', /align_Center)
       AnisotropyID = Widget_Draw(AniBase1, xSize = 100, ySize =100, EVENT_PRO='RBF_refresh_event')

       AniBase2 = Widget_Base(base ,/Column, /base_align_center)
       AniBase21 = Widget_Base(AniBase2 ,Column=2, /base_align_center,space=10)
       label = Widget_Label(AniBase21, value = '角度')

       AnisotropyAngle= widget_text(AniBase21, value=strtrim(Anisotropy[2],2),XSIZE=10,ysize=0.5 ,/EDITABLE, EVENT_PRO='RBF_refresh_event')
       AniBase22 = Widget_Base(AniBase2,Column=2, /base_align_center,space=10)
       label = Widget_Label(AniBase22, value = 'X  ')

       AnisotropyX = widget_text(AniBase22, value=strtrim(Anisotropy[0],2),XSIZE=10,ysize=0.5 ,/EDITABLE, EVENT_PRO='RBF_refresh_event')
       AniBase23 = Widget_Base(AniBase2 ,Column=2, /base_align_center,space=10)
       label = Widget_Label(AniBase23, value = 'Y  ')

       AnisotropyY = widget_text(AniBase23,value=strtrim(Anisotropy[1],2),XSIZE=10,ysize=0.8 ,/EDITABLE, EVENT_PRO='RBF_refresh_event')
;       AniBase24 = Widget_Base(AniBase2 ,Column=2, /base_align_center,space=10)
       AnisotropyLabel= widget_label(AniBase2, value ='各向异性值:'+ strtrim(Anisotropy[0]/Anisotropy[1],2),XSIZE=100 ,ysize=15)
;       Anisotropy = widget_text(AniBase24, value ='',XSIZE=100 ,ysize=15)

       RefreshID =  WIDGET_label(AniBase2, VALUE='#角度的单位是弧度',ysize=17)

     ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
       SearchBaseTOP=Widget_Base(wT2 ,row=4,/frame,yoffset=155,xoffset=10,xsize=245)

           Searchlabel = Widget_Label(SearchBaseTOP, value = '搜索方向参数: ')

           SearchBase=Widget_Base(SearchBaseTOP,Column=2)

           Searchlabel = Widget_Label( SearchBase, value = '    形状: ')
           SearchBase1=Widget_Base( SearchBase ,Column=5, space=5,event_pro='RBF_searchshape',/EXCLUSIVE)

           shape1 = widget_button(SearchBase1, $
                        value='image\search1.bmp',/bitmap)
           shape2 = widget_button(SearchBase1, $
             value='image\search2.bmp',/bitmap)
           shape3 = widget_button(SearchBase1, $
           value='image\search3.bmp',/bitmap)
           shape4 = widget_button(SearchBase1, $
           value='image\search4.bmp',/bitmap)

        SearchBase2=Widget_Base(SearchBaseTOP,Column=2)
        Searchlabel = Widget_Label( SearchBase2, value = '　　每个方向最多包含的点数　: ')
        MaxpointsID =Widget_Slider(SearchBase2, UNAME='MaxpointsID'  $
                     ,SCR_XSIZE=40 ,SCR_YSIZE=20 ,value=15,/VERTICA)

        SearchBase3=Widget_Base(SearchBaseTOP,Column=2)
        Searchlabel = Widget_Label( SearchBase3, value = '　　最少应包含的点数: ')

        MinpointsID =Widget_Slider(SearchBase3, UNAME='MinpointsID'  $
                     ,SCR_XSIZE=40 ,SCR_YSIZE=20 ,value=8,/VERTICA )


     ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

       base = Widget_Base( wT2, Column =2, Frame = 1,xsize=245,xoffset=10 ,yoffset=280,/base_align_center)

       Ellipse = [0.0,0.0,0.0];各向异性值
       ElliBase1 = Widget_Base(base, /Column)
       label = Widget_Label(ElliBase1, value = '设定搜索椭圆', /align_Center)
       EllipseID = Widget_Draw(ElliBase1, xSize = 100, ySize =100, EVENT_PRO='RBF_EllipseEvent')

       ElliBase2 = Widget_Base(base ,/Column, /base_align_center)
       ElliBase21 = Widget_Base(ElliBase2 ,Column=2, /base_align_center,space=10)
       label = Widget_Label(ElliBase21, value = '角度')

       EllipseAngle= widget_text(ElliBase21, value=strtrim(Ellipse[2],2),XSIZE=8,ysize=0.5 ,/EDITABLE, EVENT_PRO='RBF_EllipseEvent')
       ElliBase22 = Widget_Base(ElliBase2,Column=2, /base_align_center,space=10)
       label = Widget_Label(ElliBase22, value = 'X  ')

       EllipseX = widget_text(ElliBase22, value=strtrim(Ellipse[0],2),XSIZE=8,ysize=0.5 ,/EDITABLE, EVENT_PRO='RBF_EllipseEvent')
       ElliBase23 = Widget_Base(ElliBase2 ,Column=2, /base_align_center,space=10)
       label = Widget_Label(ElliBase23, value = 'Y  ')

       EllipseY = widget_text(ElliBase23,value=strtrim(Ellipse[1],2),XSIZE=8,ysize=0.8 ,/EDITABLE, EVENT_PRO='RBF_EllipseEvent')

;       AniBase24 = Widget_Base(AniBase2 ,Column=2, /base_align_center,space=10)
;       AniBase24 = Widget
     ;存储半变异函数的参数,分别为模型选择、Range、Nugget、sill


         RBFvalue = [0,7.98199]

         wT3 = WIDGET_BASE(wT2, TITLE='径向基函数',space=6,xsize=245,xoffset=245 ,yoffset=0)

          VariogramBase=WIDGET_BASE(wT3,/frame,xoffset=30,yoffset=5,xsize=210,ysize=230)
          label = Widget_Label(VariogramBase, value = '径向基函数形状', yoffset=10,xoffset=60)
          ExtraID = Widget_Draw(VariogramBase, xsize = 130, ysize = 130,yoffset=30,xoffset=40)


          label = Widget_Label(VariogramBase, value = '(上图显示的X值为实际值/100000',XSIZE=180,yoffset=170,xoffset=20)
          label = Widget_Label(VariogramBase, value = '           Y值为实际值*10000)',XSIZE=180,yoffset=182,xoffset=20)
          TypeBase=WIDGET_BASE(VariogramBase,xoffset=10,yoffset=190,xsize=210,ysize=30)

          List = ['0-反高次曲面函数', '1-高次对数', '2-高次曲面函数', $
           '3-自然多项式样条函数', '4-平面样条函数']
;          TypeID = FSC_DropList(VariogramBase, value = list, title = '半变异函数模型:', $
;              Index =Variogram[0]-1,name='Type',$
;              Event_pro = 'VariogramType',xoffset=10,XSIZE=180,yoffset=200,ysize=20)
          TypeID = WIDGET_DROPLIST(TypeBase, $   ; This list belongs to 'base'.
          value = list, title = '径向基函数模型:', $   ; Put 'listitems' in the list.
          UVALUE = 'TypeID', $ ; 'LIST' is this widgets User Value.
          XSIZE=190,yoffset=10,event_pro='RBFType_event')

;          VariogramBase1=WIDGET_BASE(VariogramBase,column=2,yoffset=230,xoffset=10)
           SmoothingBase = WIDGET_BASE(wT3,yoffset=260,xoffset=30,/frame,xsize=210,ysize=50)
          label = Widget_Label(SmoothingBase, value = '光滑度: ',xoffset=10,yoffset=15,xsize=50)

          SmoothingID=widget_text(SmoothingBase, value = strtrim(RBFvalue[1],2),$
                    xsize = 8,/EDITABLE,Event_pro = 'RBFType_event',xoffset=60,yoffset=10)


       bINterRBFParOK = WIDGET_BUTTON(wT2, VALUE='确定',event_pro='bINterRBFParOK',$
                                          xsize = 50, ysize = 25,yoffset=360,xoffset=300)

       bINterRBFParCancle = WIDGET_BUTTON(wT2, VALUE='取消',event_pro='NQ_interpolate_closes_event',$
                                          xsize = 50, ysize = 25,yoffset=360,xoffset=400)

	widget_control, wTLB, /realize
end

pro PolynomialRegression_para,event

	wTLB = WIDGET_BASE(group_leader = event.top, /modal, /row,XOFFSET=300,YOFFSET=200, /BASE_ALIGN_TOP,$
       TITLE='多项式回归(PolynomialRegression)插值参数', TLB_FRAME_ATTR=1)

       wTab= WIDGET_BASE(wTLB,/COLUMN)
       Anisotropy=[1.0,1.0,0.0];各向异性值

       wT2 = WIDGET_BASE(wTab, TITLE='选项',space=5,xsize=500,ysize=280)

       base = Widget_Base( wT2, Column =2, Frame = 1,xsize=245,xoffset=10 ,yoffset=5,/base_align_center)

       AniBase1 = Widget_Base(base, /Column)
       label = Widget_Label(AniBase1, value = '各向异性', /align_Center)
       AnisotropyID = Widget_Draw(AniBase1, xSize = 100, ySize =100, EVENT_PRO='PREG_refresh_event')

       AniBase2 = Widget_Base(base ,/Column, /base_align_center)
       AniBase21 = Widget_Base(AniBase2 ,Column=2, /base_align_center,space=10)
       label = Widget_Label(AniBase21, value = '角度')

       AnisotropyAngle= widget_text(AniBase21, value=strtrim(Anisotropy[2],2),XSIZE=10,ysize=0.5 ,/EDITABLE, EVENT_PRO='PREG_refresh_event')
       AniBase22 = Widget_Base(AniBase2,Column=2, /base_align_center,space=10)
       label = Widget_Label(AniBase22, value = 'X  ')

       AnisotropyX = widget_text(AniBase22, value=strtrim(Anisotropy[0],2),XSIZE=10,ysize=0.5 ,/EDITABLE, EVENT_PRO='PREG_refresh_event')
       AniBase23 = Widget_Base(AniBase2 ,Column=2, /base_align_center,space=10)
       label = Widget_Label(AniBase23, value = 'Y  ')

       AnisotropyY = widget_text(AniBase23,value=strtrim(Anisotropy[1],2),XSIZE=10,ysize=0.8 ,/EDITABLE, EVENT_PRO='PREG_refresh_event')
;       AniBase24 = Widget_Base(AniBase2 ,Column=2, /base_align_center,space=10)
       AnisotropyLabel= widget_label(AniBase2, value ='各向异性值:'+ strtrim(Anisotropy[0]/Anisotropy[1],2),XSIZE=100 ,ysize=15)
;       Anisotropy = widget_text(AniBase24, value ='',XSIZE=100 ,ysize=15)

       RefreshID =  WIDGET_label(AniBase2, VALUE='#角度的单位是弧度',ysize=17)

     ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
       SearchBaseTOP=Widget_Base(wT2 ,row=4,/frame,yoffset=155,xoffset=10,xsize=245)

           Searchlabel = Widget_Label(SearchBaseTOP, value = '搜索方向参数: ')

           SearchBase=Widget_Base(SearchBaseTOP,Column=2)

           Searchlabel = Widget_Label( SearchBase, value = '    形状: ')
           SearchBase1=Widget_Base( SearchBase ,Column=5, space=5,event_pro='PREG_searchshape',/EXCLUSIVE)

           shape1 = widget_button(SearchBase1, $
                        value='image\search1.bmp',/bitmap)
           shape2 = widget_button(SearchBase1, $
             value='image\search2.bmp',/bitmap)
           shape3 = widget_button(SearchBase1, $
           value='image\search3.bmp',/bitmap)
           shape4 = widget_button(SearchBase1, $
           value='image\search4.bmp',/bitmap)

        SearchBase2=Widget_Base(SearchBaseTOP,Column=2)
        Searchlabel = Widget_Label( SearchBase2, value = '　　每个方向最多包含的点数　: ')
        MaxpointsID =Widget_Slider(SearchBase2, UNAME='MaxpointsID'  $
                     ,SCR_XSIZE=40 ,SCR_YSIZE=20 ,value=15,/VERTICA)

        SearchBase3=Widget_Base(SearchBaseTOP,Column=2)
        Searchlabel = Widget_Label( SearchBase3, value = '　　最少应包含的点数: ')

        MinpointsID =Widget_Slider(SearchBase3, UNAME='MinpointsID'  $
                     ,SCR_XSIZE=40 ,SCR_YSIZE=20 ,value=8,/VERTICA )


     ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

       base = Widget_Base( wT2, Column =2, Frame = 1,xsize=220,xoffset=270 ,yoffset=5,/base_align_center)

       Ellipse = [0.0,0.0,0.0];各向异性值
       ElliBase1 = Widget_Base(base, /Column)
       label = Widget_Label(ElliBase1, value = '设定搜索椭圆', /align_Center)
       EllipseID = Widget_Draw(ElliBase1, xSize = 100, ySize =100, EVENT_PRO='PREG_EllipseEvent')

       ElliBase2 = Widget_Base(base ,/Column, /base_align_center)
       ElliBase21 = Widget_Base(ElliBase2 ,Column=2, /base_align_center,space=10)
       label = Widget_Label(ElliBase21, value = '角度')

       EllipseAngle= widget_text(ElliBase21, value=strtrim(Ellipse[2],2),XSIZE=8,ysize=0.5 ,/EDITABLE, EVENT_PRO='PREG_EllipseEvent')
       ElliBase22 = Widget_Base(ElliBase2,Column=2, /base_align_center,space=10)
       label = Widget_Label(ElliBase22, value = 'X  ')

       EllipseX = widget_text(ElliBase22, value=strtrim(Ellipse[0],2),XSIZE=8,ysize=0.5 ,/EDITABLE, EVENT_PRO='PREG_EllipseEvent')
       ElliBase23 = Widget_Base(ElliBase2 ,Column=2, /base_align_center,space=10)
       label = Widget_Label(ElliBase23, value = 'Y  ')

       EllipseY = widget_text(ElliBase23,value=strtrim(Ellipse[1],2),XSIZE=8,ysize=0.8 ,/EDITABLE, EVENT_PRO='PREG_EllipseEvent')

;       AniBase24 = Widget_Base(AniBase2 ,Column=2, /base_align_center,space=10)
;       AniBase24 = Widget
     ;存储半变异函数的参数,分别为模型选择、Range、Nugget、sill

       base=Widget_Base(wT2 ,yoffset=155,xoffset=270,/frame,xsize=220,ysize=50)
       PowerBase = Widget_Base(base ,Column=2,yoffset=10,xoffset=10)
       label = Widget_Label(PowerBase, yoffset=10,xoffset=0, SCR_XSIZE=40 ,SCR_YSIZE=20,value = '权重值:  ')
       Powervalue=['2','1','3']
       PowerID =widget_droplist(PowerBase, value = Powervalue, $
                   xsize = 70, Event_Pro = 'Powerselect')

       bINterPREGParOK = WIDGET_BUTTON(wT2, VALUE='确定',event_pro='bINterPREGParOK',$
                                          xsize = 50, ysize = 25,yoffset=230,xoffset=300)

       bINterPREGParCancle = WIDGET_BUTTON(wT2, VALUE='取消',event_pro='NQ_interpolate_closes_event',$
                                          xsize = 50, ysize = 25,yoffset=230,xoffset=400)


	widget_control, wTLB, /realize
end

pro Kriging_para,event
	wTLB = WIDGET_BASE(group_leader = event.top, /modal, /row,XOFFSET=300,YOFFSET=200, /BASE_ALIGN_TOP,$
       TITLE='克里格(Kriging)插值参数', TLB_FRAME_ATTR=1)

       wTab= WIDGET_BASE(wTLB,/COLUMN)
       Anisotropy=[1.0,1.0,0.0];各向异性值

       wT2 = WIDGET_BASE(wTab, TITLE='选项',space=5,xsize=500,ysize=350)

       base = Widget_Base( wT2, Column =2, Frame = 1,xsize=245,xoffset=230 ,yoffset=5,/base_align_center)

       AniBase1 = Widget_Base(base, /Column)
       label = Widget_Label(AniBase1, value = '各向异性', /align_Center)
       AnisotropyID = Widget_Draw(AniBase1, xSize = 100, ySize =100, EVENT_PRO='refresh_event')

       AniBase2 = Widget_Base(base ,/Column, /base_align_center)
       AniBase21 = Widget_Base(AniBase2 ,Column=2, /base_align_center,space=10)
       label = Widget_Label(AniBase21, value = '角度')

       AnisotropyAngle= widget_text(AniBase21, value=strtrim(Anisotropy[2],2),XSIZE=10,ysize=0.5 ,/EDITABLE, EVENT_PRO='refresh_event')
       AniBase22 = Widget_Base(AniBase2,Column=2, /base_align_center,space=10)
       label = Widget_Label(AniBase22, value = 'X  ')

       AnisotropyX = widget_text(AniBase22, value=strtrim(Anisotropy[0],2),XSIZE=10,ysize=0.5 ,/EDITABLE, EVENT_PRO='refresh_event')
       AniBase23 = Widget_Base(AniBase2 ,Column=2, /base_align_center,space=10)
       label = Widget_Label(AniBase23, value = 'Y  ')

       AnisotropyY = widget_text(AniBase23,value=strtrim(Anisotropy[1],2),XSIZE=10,ysize=0.8 ,/EDITABLE, EVENT_PRO='refresh_event')
;       AniBase24 = Widget_Base(AniBase2 ,Column=2, /base_align_center,space=10)
       AnisotropyLabel= widget_label(AniBase2, value ='各向异性值:'+ strtrim(Anisotropy[0]/Anisotropy[1],2),XSIZE=100 ,ysize=15)
;       Anisotropy = widget_text(AniBase24, value ='',XSIZE=100 ,ysize=15)

       RefreshID =  WIDGET_label(AniBase2, VALUE='#角度的单位是弧度',ysize=17)

     ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
       SearchBaseTOP=Widget_Base(wT2 ,row=4,/frame,yoffset=155,xoffset=230,xsize=245)

           Searchlabel = Widget_Label(SearchBaseTOP, value = '搜索方向参数: ')

           SearchBase=Widget_Base(SearchBaseTOP,Column=2)

           Searchlabel = Widget_Label( SearchBase, value = '    形状: ')
           SearchBase1=Widget_Base( SearchBase ,Column=5, space=5,event_pro='searchshape',/EXCLUSIVE)

           shape1 = widget_button(SearchBase1, $
                        value='image\search1.bmp',/bitmap)
           shape2 = widget_button(SearchBase1, $
             value='image\search2.bmp',/bitmap)
           shape3 = widget_button(SearchBase1, $
           value='image\search3.bmp',/bitmap)
           shape4 = widget_button(SearchBase1, $
           value='image\search4.bmp',/bitmap)

        SearchBase2=Widget_Base(SearchBaseTOP,Column=2)
        Searchlabel = Widget_Label( SearchBase2, value = '　　每个方向最多包含的点数　: ')
        MaxpointsID =Widget_Slider(SearchBase2, UNAME='MaxpointsID'  $
                     ,SCR_XSIZE=40 ,SCR_YSIZE=20 ,value=15,/VERTICA)

        SearchBase3=Widget_Base(SearchBaseTOP,Column=2)
        Searchlabel = Widget_Label( SearchBase3, value = '　　最少应包含的点数: ')

        MinpointsID =Widget_Slider(SearchBase3, UNAME='MinpointsID'  $
                     ,SCR_XSIZE=40 ,SCR_YSIZE=20 ,value=8,/VERTICA )


     ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     ;存储半变异函数的参数,分别为模型选择、Range、Nugget、sill
      Variogram = [2,63.8559,0.0,1.0]
;      wT3 = WIDGET_BASE(wTab, TITLE='半变异函数',space=6)

          VariogramBase=WIDGET_BASE(wT2,/frame,xoffset=10,yoffset=5,xsize=200)
          label = Widget_Label(VariogramBase, value = '半变异参数设置', yoffset=10,xoffset=60)
          ExtraID = Widget_Draw(VariogramBase, xsize = 130, ysize = 130,yoffset=30,xoffset=40)


          label = Widget_Label(VariogramBase, value = '(上图显示的X值为Rang/1000)',XSIZE=180,yoffset=170,xoffset=20)
          TypeBase=WIDGET_BASE(VariogramBase,xoffset=10,yoffset=190,xsize=210,ysize=30)
          list = ['1-线性模型', '2-指数模型', '3-高斯模型', '4-球状模型']
;          TypeID = FSC_DropList(VariogramBase, value = list, title = '半变异函数模型:', $
;              Index =Variogram[0]-1,name='Type',$
;              Event_pro = 'VariogramType',xoffset=10,XSIZE=180,yoffset=200,ysize=20)
          TypeID = WIDGET_DROPLIST(TypeBase, $   ; This list belongs to 'base'.
          value = list, title = '半变异函数模型:', $   ; Put 'listitems' in the list.
          UVALUE = 'TypeID', $ ; 'LIST' is this widgets User Value.
          XSIZE=180,yoffset=10,event_pro='VariogramType')

          VariogramBase1=WIDGET_BASE(VariogramBase,column=2,yoffset=230,xoffset=10)
          label = Widget_Label(VariogramBase1, value = '自相关阈值(Range):')
          RangeID=widget_text(VariogramBase1, value = strtrim(Variogram[1],2),$
                    xsize = 9,/EDITABLE,Event_pro = 'VariogramType')
          VariogramBase2=WIDGET_BASE(VariogramBase,column=2,yoffset=260,xoffset=10)
          label = Widget_Label(VariogramBase2, value = '块金方差(nugget): ')
          NuggetID=widget_text(VariogramBase2, value =  strtrim(Variogram[2],2),$
                    xsize = 9,/EDITABLE,Event_pro = 'VariogramType')
          VariogramBase3=WIDGET_BASE(VariogramBase,column=2,yoffset=290,xoffset=10)
          label = Widget_Label(VariogramBase3, value = '基台值(Sill):     ')
          SillID=widget_text(VariogramBase3, value =  strtrim(Variogram[3],2),$
                    xsize = 9,/EDITABLE,Event_pro = 'VariogramType')
	;
	bINterKringParOK = WIDGET_BUTTON(wT2, VALUE='确定',event_pro='bINterKringParOK',$
                       xsize = 50, ysize = 25,yoffset=300,xoffset=260)
	bINterKringParCancle = WIDGET_BUTTON(wT2, VALUE='取消',event_pro='NQ_interpolate_closes_event',$
                       xsize = 50, ysize = 25,yoffset=300,xoffset=360)
	widget_control, wTLB, /realize
end

;设置反距离插值参数
pro IDW_para, event
	wTLB = WIDGET_BASE(group_leader = event.top,  /modal, /row, XOFFSET=300,YOFFSET=200, /BASE_ALIGN_TOP,$
	TITLE='反距离权重(IDW)插值参数', TLB_FRAME_ATTR=1)
	wTab= WIDGET_BASE(wTLB,/COLUMN)
	Anisotropy=[1.0,1.0,0.0];各向异性值

       wT2 = WIDGET_BASE(wTab, TITLE='选项',space=5,xsize=500,ysize=280)

       base = Widget_Base( wT2, Column =2, Frame = 1,xsize=245,xoffset=10 ,yoffset=5,/base_align_center)

       AniBase1 = Widget_Base(base, /Column)
       label = Widget_Label(AniBase1, value = '各向异性', /align_Center)
       AnisotropyID = Widget_Draw(AniBase1, xSize = 100, ySize =100, EVENT_PRO='refresh_event')

       AniBase2 = Widget_Base(base ,/Column, /base_align_center)
       AniBase21 = Widget_Base(AniBase2 ,Column=2, /base_align_center,space=10)
       label = Widget_Label(AniBase21, value = '角度')

       AnisotropyAngle= widget_text(AniBase21, value=strtrim(Anisotropy[2],2),XSIZE=10,ysize=0.5 ,/EDITABLE, EVENT_PRO='refresh_event')
       AniBase22 = Widget_Base(AniBase2,Column=2, /base_align_center,space=10)
       label = Widget_Label(AniBase22, value = 'X  ')

       AnisotropyX = widget_text(AniBase22, value=strtrim(Anisotropy[0],2),XSIZE=10,ysize=0.5 ,/EDITABLE, EVENT_PRO='refresh_event')
       AniBase23 = Widget_Base(AniBase2 ,Column=2, /base_align_center,space=10)
       label = Widget_Label(AniBase23, value = 'Y  ')

       AnisotropyY = widget_text(AniBase23,value=strtrim(Anisotropy[1],2),XSIZE=10,ysize=0.8 ,/EDITABLE, EVENT_PRO='refresh_event')
;       AniBase24 = Widget_Base(AniBase2 ,Column=2, /base_align_center,space=10)
       AnisotropyLabel= widget_label(AniBase2, value ='各向异性值:'+ strtrim(Anisotropy[0]/Anisotropy[1],2),XSIZE=100 ,ysize=15)
;       Anisotropy = widget_text(AniBase24, value ='',XSIZE=100 ,ysize=15)

       RefreshID =  WIDGET_label(AniBase2, VALUE='#角度的单位是弧度',ysize=17)

     ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
       SearchBaseTOP=Widget_Base(wT2 ,row=4,/frame,yoffset=155,xoffset=10,xsize=245)

           Searchlabel = Widget_Label(SearchBaseTOP, value = '搜索方向参数: ')

           SearchBase=Widget_Base(SearchBaseTOP,Column=2)

           Searchlabel = Widget_Label( SearchBase, value = '    形状: ')
           SearchBase1=Widget_Base( SearchBase ,Column=5, space=5,event_pro='searchshape',/EXCLUSIVE)

           shape1 = widget_button(SearchBase1, $
                        value='image\search1.bmp',/bitmap)
           shape2 = widget_button(SearchBase1, $
             value='image\search2.bmp',/bitmap)
           shape3 = widget_button(SearchBase1, $
             value='image\search3.bmp',/bitmap)
           shape4 = widget_button(SearchBase1, $
             value='image\search4.bmp',/bitmap)

        SearchBase2=Widget_Base(SearchBaseTOP,Column=2)
        Searchlabel = Widget_Label( SearchBase2, value = '　　每个方向最多包含的点数　: ')
        MaxpointsID =Widget_Slider(SearchBase2, UNAME='MaxpointsID'  $
                     ,SCR_XSIZE=40 ,SCR_YSIZE=20 ,value=15,/VERTICA)

        SearchBase3=Widget_Base(SearchBaseTOP,Column=2)
        Searchlabel = Widget_Label( SearchBase3, value = '　　最少应包含的点数: ')

        MinpointsID =Widget_Slider(SearchBase3, UNAME='MinpointsID'  $
                     ,SCR_XSIZE=40 ,SCR_YSIZE=20 ,value=8,/VERTICA )


     ;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

       base = Widget_Base( wT2, Column =2, Frame = 1,xsize=220,xoffset=270 ,yoffset=5,/base_align_center)

       Ellipse = [0.0,0.0,0.0];各向异性值
       ElliBase1 = Widget_Base(base, /Column)
       label = Widget_Label(ElliBase1, value = '设定搜索椭圆', /align_Center)
       EllipseID = Widget_Draw(ElliBase1, xSize = 100, ySize =100, EVENT_PRO='EllipseEvent')

       ElliBase2 = Widget_Base(base ,/Column, /base_align_center)
       ElliBase21 = Widget_Base(ElliBase2 ,Column=2, /base_align_center,space=10)
       label = Widget_Label(ElliBase21, value = '角度')

       EllipseAngle= widget_text(ElliBase21, value=strtrim(Ellipse[2],2),XSIZE=8,ysize=0.5 ,/EDITABLE, EVENT_PRO='EllipseEvent')
       ElliBase22 = Widget_Base(ElliBase2,Column=2, /base_align_center,space=10)
       label = Widget_Label(ElliBase22, value = 'X  ')

       EllipseX = widget_text(ElliBase22, value=strtrim(Ellipse[0],2),XSIZE=8,ysize=0.5 ,/EDITABLE, EVENT_PRO='EllipseEvent')
       ElliBase23 = Widget_Base(ElliBase2 ,Column=2, /base_align_center,space=10)
       label = Widget_Label(ElliBase23, value = 'Y  ')

       EllipseY = widget_text(ElliBase23,value=strtrim(Ellipse[1],2),XSIZE=8,ysize=0.8 ,/EDITABLE, EVENT_PRO='EllipseEvent')

;       AniBase24 = Widget_Base(AniBase2 ,Column=2, /base_align_center,space=10)
;       AniBase24 = Widget
     ;存储半变异函数的参数,分别为模型选择、Range、Nugget、sill

       base=Widget_Base(wT2 ,yoffset=155,xoffset=270,/frame,xsize=220,ysize=50)
       PowerBase = Widget_Base(base ,Column=2,yoffset=10,xoffset=10)
       label = Widget_Label(PowerBase, yoffset=10,xoffset=0, SCR_XSIZE=40 ,SCR_YSIZE=20,value = '权重值:  ')
       Powervalue=['2','1','3']
       PowerID =widget_text(PowerBase, value = '2.0', yoffset=0,$
               xsize = 10,SCR_XSIZE=50, Event_Pro = 'Powerselect',/EDITABLE)

	SmoothBase = Widget_Base(base ,Column=2, yoffset=10,xoffset=110)
	label = Widget_Label(SmoothBase, yoffset=10,xoffset=0, SCR_XSIZE=40 ,SCR_YSIZE=20,value = '光滑度:  ')
	SmoothID =widget_text(SmoothBase, yoffset=0,xoffset=5,value = '0.0', xsize = 10, SCR_XSIZE=50 ,SCR_YSIZE=20,/EDITABLE)
	;
	btnOK = WIDGET_BUTTON(wT2, VALUE='确定',event_pro='bINterIDWParOK', xsize = 50, ysize = 25,yoffset=230,xoffset=300)
	btnCancel = WIDGET_BUTTON(wT2, VALUE='取消',event_pro='NQ_interpolate_closes_event',xsize = 50, ysize = 25,yoffset=230,xoffset=400)
	widget_control, wTLB, /realize
	;xmanager,  'IDWPara_TLB', wTLB,  /no_block
end

;处理d_inter里的事件
pro NQ_INTERPOLATE_intermain_event, event
	widget_control, event.handler, get_uvalue=wid
	if event.id eq (*wid).para then begin
		sel=widget_info((*wid).method_index, /DROPLIST_SELECT)
		widget_control, (*wid).method_index, get_value=LIST
		index=where(['IDW','Kriging','PolynomialRegression','RadialBasisFunction'] eq list[sel],count)
		if count gt 0 then begin
			prog=list[sel]+'_para'
			call_procedure, prog,event
		endif
	endif

end

;处理关闭事件,清空内存
pro NQ_interpolate_widget_close,event
	widget_control, event.top, get_uvalue=pstate
;	ptr_free, pstate
	NQ_interpolate_closes_event,event
end

FUNCTION DB_RS2Arr,Rsobj
	nrow=n_records(Rsobj)
	if nrow eq 0 then return,-1
	ncol=RSobj -> NFields()
	IF nrow GT 0 AND ncol GT 0 THEN BEGIN
		arr=FLTARR(ncol,nrow)
		Result = RSobj -> MoveCursor(/FIRST)
		FOR i=0,nrow-1 DO BEGIN
			FOR j=0,ncol-1 DO BEGIN
				arr[j,i]=RSobj -> GetField(J)
			ENDFOR
			Result = RSobj -> MoveCursor(/NEXT)
		ENDFOR
	END
	RETURN,ARR
END

;处理绘图窗口上的鼠标点击事件
pro image_draw_event,event
end
;显示图像

;function showimage,pstate,StationValue   ;显示图像
;	On_Error, 2
;   	CATCH, Error_status
;   	;This statement begins the error handler:
;   	IF Error_status NE 0 THEN BEGIN
;      	PRINT, 'Error index: ', Error_status
;      	PRINT, 'Error message: ', !ERROR_STATE.MSG
;      	OK = NQ_interpolate_ERROR_MESSAGE(Traceback = debug)
;      	CATCH, /CANCEL
;   	ENDIF
;   	;
;   	print, 'show image start'
;   	On_Error, 1
;   	;
;    WIDGET_CONTROL, (*pstate).DrawID, GET_VALUE=owindow
;    ;
;    OBJ_DESTROY, (*pstate).theImage
;    OBJ_DESTROY, (*pstate).theColorBar
;    OBJ_DESTROY, (*pstate).theModel
;    OBJ_DESTROY, (*pstate).theView
;  	xsize=(*pstate).xsize
;   	ysize=(*pstate).ysize
;
;;	(*pstate).theImage = Obj_New('IDLgrImage', (*pstate).pixel_value, Palette=(*pstate).thePalette, ORDER=1)
;;	(*pstate).theImage = Obj_New('IDLgrImage', pixel_value, Palette=(*pstate).thePalette, ORDER=1)
;	(*pstate).theImage = Obj_New('IDLgrImage', *(((*pstate).pixeldata)), Palette=(*pstate).thePalette, ORDER=1)
;	(*pstate).theImage->SetProperty, XCoord_Conv=Normalize([0,(*pstate).xsize]), YCoord_Conv=Normalize([0,(*pstate).ysize])
;    (*pstate).theModel = Obj_New('IDLgrModel')
;    ;添加图像数据
;    (*pstate).theModel->Add, (*pstate).theImage
;    ;添加站点原始数据
;    StationNumbers = n_elements(StationValue.stationx)
;    for i=0, StationNumbers-1 do begin
;    	(*pstate).theModel->Add,OBJ_NEW('IDLgrText', String(StationValue.OriValue[i]), Locations=[StationValue.stationX[i]-0.01, StationValue.stationY[i]], COLOR=[150,100,150])
;    	;(*pstate).theModel->Add,OBJ_NEW('IDLgrText', String(StationValue.OriValue[i]), Locations=[StationValue.stationX[i]-0.01, StationValue.stationY[i]], COLOR=[150,100,150])
;	(*pstate).theModel->Add,OBJ_NEW('IDLgrPolyline', [StationValue.stationX[i]-0.01, StationValue.stationY[i]], COLOR=[80,70,80])
;	endfor
;	;添加颜色条
;    (*pstate).theColorBar = OBJ_NEW('IDLgrColorbar', PALETTE = (*pstate).thePalette, DIMENSIONS = [0.256, 0.016])
;    (*pstate).theModel->Add, (*pstate).theColorBar
;
; (*pstate).theView = Obj_New('IDLgrView', Viewplane_Rect=[0,0,1,1],color=[255,255,255])
;(*pstate).theView->Add, (*pstate).theModel
;owindow->Draw, (*PSTATE).theView
;print,'show image over'
;return, (*pstate).theImage
;HEAP_GC, /VERBOSE
;end

;响应插值操作,显示参数
pro runInterpolation,event

	forward_function INTERPOLEVENT,read_file

   widget_control,/hourglass
;	print,'执行插值命令'
	WIDGET_CONTROL,Event.top, get_UVALUE = pstate
	widget_control,(*PSTATE).reso,get_value=reso
    widget_control, (*pstate).outputfile, get_value=outputfile
;         IF (FILE_TEST(outputfile) EQ 0) THEN BEGIN
;         	PRINT,'没有该路径:'+outputfile+',请检查路径名!'
;         	msg=DIALOG_MESSAGE('请输入正确的保存路径!',/INFORMATION)
;        	CLOSE,/all
;         RETURN
;         ENDIF
    flreso=float(reso)
    if flreso lt 300 or flreso gt 1000 then begin
        TEXT=DIALOG_MESSAGE('分辨率必须在300-1000米!',title='提示')
        RETURN
    endif
	;取得dem文件
    DEMSentive=(*pstate).DEMSentive

    if DEMSentive eq 1 then begin
     WIDGET_CONTROL,(*pstate).demfile,get_value=demfile
     widget_control,(*pstate).reso,get_value=reso
	     if demfile eq '' then begin
	     	OK = DIALOG_MESSAGE('dem文件为空,请重新选择!',title='提示')
	     	RETURN
	     endif else begin

;-------------------------------------------------------------
		 demtest=GET_IMAGE_INFO(demfile)
		 IF demtest.FILE_RIGHT EQ 0 THEN BEGIN
		 	OK = DIALOG_MESSAGE('输入的dem文件有错误',title='提示')
		 	RETURN
		 ENDIF

;-------------------------------------------------------------

	     Openr,Lun_demfile	,demfile	,/get_lun
	     demfile=demfile[0]
	     RESULT=FILE_INFO(demfile)
	     FILE_INFO_TY=NQ_classify_GET_IMAGE_INFO(demfile)
;	     IF (FILE_INFO_TY.XSIZE NE reso) then begin		;原代码
	     IF (FILE_INFO_TY.X_PIXELSIZE NE reso) then begin	;杨绍锷修改，20070904
		  	OK = DIALOG_MESSAGE('输入的dem文件与现在所选的分辨率大小不一致,请重新检查!',title='提示')
		 	RETURN
		 ENDIF
	     	 SAMPLES=FILE_INFO_TY.XSIZE
		     LINES	=FILE_INFO_TY.YSIZE
		 	 demdata	=	bytARR(SAMPLES,LINES)
		 READU, Lun_demfile	, demdata
	     print,'demdata'
	    endelse
   endif
	progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='数据处理中,请稍候!',TITLE='空间插值') ;新建进度条对象
	progressTimer->START
	progressTimer->UPDATE, (0.05 * 100.0)  ;更新进度条
	Intestru=InterpolEvent(pstate)

	if Intestru.Error eq 1 then begin

		progressTimer->DESTROY ;销毁进度条
		return
	endif
	progressTimer->UPDATE, (0.2 * 100.0)  ;更新进度条

	;保存站点值

    ;-----------------------------------------------------
      ;widget_control,/hourglass
	;对于大气压，使用DEM数据和多年平均温度数据
	Datatype = widget_info((*pstate).meteoFeature, /droplist_select)
	xsize=(*pstate).xsize
	ysize=(*pstate).ysize
	real_value=Intestru.real_value
	;对于气温，使用DEM数据
   if DEMSentive eq 1 then begin
		if Datatype eq 2  then begin
			print,'温度迭加DEM数据'
	        real_value -= 0.065*demdata
		endif
   endif else begin
   endelse

    if outputfile ne '' then begin
	    samples=(*pstate).xsize
	    lines=(*pstate).ysize
	    DataType=4
	    sensortype='Unknown'
;	    ULX=4780500.0000
;	    ULY=5866500.0000
;	    ULX=4024807.1903
;	    ULY=4394885.0378
	ParaInfo = (*pstate).NQ_ProjectPara    ;得到省范围内的投影参数
;	IF SIZE(ParaInfo,/TYPE) NE 8 THEN BEGIN     ;即返回的不是结构体,而是空.
;	   PRMPT = DIALOG_MESSAGE('没有'+ProviceName+'的基础参数信息,请查看相应的参数设置文件!',TITLE='警告')
;	   return,0
;	ENDIF
    ulx=float(ParaInfo.ulx)
    uly=float(ParaInfo.uly)
    lrx=float(ParaInfo.lrx)
    lry=float(ParaInfo.lry)
       border_files=ParaInfo.gridpath

       shapefile=ParaInfo.shppath


	    widget_control,(*PSTATE).reso,get_value=Resolution
	;	border_files='data_grid\shanxi'                         ;如果不是水田,则用旱地百分比文件.

    ;    border_files='data_grid\province_new'
		border_file = NQ_CL_Read_ENVIData(border_files,SUCCESSSTATUS = Status,DESCRIPTION='耕地层基础文件的')
		border_file=congrid(border_file,samples,lines)
		IF Status EQ 0 THEN begin
		    progressTimer -> DESTROY
			RETURN
		endif
		ZeroValue=WHERE(border_file EQ 0,COMPLEMENT=NoZero)         ;为取得省耕地边界外的坐标索引
        (*pstate).AddMin = 300-MIN(real_value[NoZero])
		real_value[ZeroValue]=0                                     ;将耕地边界外的值赋为0
		SavedImage = real_value										;插值后并去除非耕地后将被保存的数据
        real_value[NoZero]   = real_value[NoZero]+(*pstate).AddMin
;        WIDGET_CONTROL,(*pstate).drawid,SET_UVALUE = real_value

		pathnum=strpos(outputfile,'\',/reverse_search)
		if pathnum ne -1 then $
			path=strmid(outputfile,0,pathnum+1)
		if file_test(path,/DIRECTORY)	ne 1 then begin
			info = DIALOG_MESSAGE('所指定的路径不存在！')
			progressTimer->DESTROY
			RETURN
		endif
		if file_test(outputfile) eq 1 then begin
			info = DIALOG_MESSAGE('同名文件已存在，确认要覆盖吗？', DIALOG_PARENT=Event.top, /Question,TITLE='提示')
			if info eq 'No' then begin
				progressTimer->DESTROY
				RETURN
			endif
		endif

    	p_SavedImage =  ptr_new(float(SavedImage)) ;注意ImageData是指针类型.
	    NQ_INTERPOLATE_dataSaveImageFile,outputfile, p_SavedImage $
			  ,samples,lines,DataType,sensortype $
			  ,ULX,ULY,Resolution
		ptr_free, p_SavedImage
    endif

	progressTimer->UPDATE, (0.75 * 100.0)  ;更新进度条
;    NQ_Draw_image,outputfile,(*pstate).drawid,OView=oView,MINVALUE=299.9,MUPLE_ISX_Y=muple_isx_y,shapefile=shapefile,/WHITE
;    (*pstate).Muple_isx_y = Muple_isx_y
;	HEAP_FREE,(*pstate).oView
;	(*pstate).oView = oView
	fileinfo = read_file(outputfile)
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

	progressTimer->UPDATE, (1 * 100.0)  ;更新进度条
	progressTimer->DESTROY ;销毁进度条
	msg=DIALOG_MESSAGE('完成插值!',title='提示')
	log, '农气分析-气象插值', 1
end

pro featurechange, event
	WIDGET_CONTROL,Event.top, get_UVALUE=pstate
	Datatype = widget_info(event.id, /droplist_select)
	if Datatype eq 2 then begin
		WIDGET_CONTROL,(*pstate).b_dem, sensitive=1
	endif else begin
		WIDGET_CONTROL,(*pstate).b_dem, sensitive=0
	endelse
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;主界面程序&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
pro NQ_INTERPOLATE_intermain,GROUP_LEADER=groupleader

	common_log,'启动气象插值'

	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	;数据库连接设置
 IF ( XREGISTERED('NQ_INTERPOLATE_intermain') NE 0 ) THEN RETURN

  TLB = Widget_Base(GROUP_LEADER=groupleader,UNAME='TLB' ,XOFFSET=180  $
      ,YOFFSET=200 ,SCR_XSIZE=678 ,SCR_YSIZE=413  $
      ,TITLE='气象插值' ,SPACE=7 ,XPAD=7 ,YPAD=7,TLB_FRAME_ATTR=1)
;-------------------------主界面的left边---------------------------------------------
  LEFT_BASE = Widget_Base(TLB, UNAME='LEFT_BASE' ,FRAME=0 ,XOFFSET=7  $
      ,YOFFSET=7 ,SCR_XSIZE=243 ,SCR_YSIZE=371 ,TITLE='IDL' ,SPACE=7  $
      ,XPAD=7 ,YPAD=7)
  datatype=widget_base(LEFT_BASE,uname='datatype',XPAD=7 ,YPAD=7,XOFFSET=0,YOFFSET=0 $
  ,scr_xsize=241,scr_ysize=36,FRAME=1 )
  list = ['日降水量','日照时数','旬积温']
;  插值的气象要素类型
  type=widget_label(datatype,uname='type',XOFFSET=20,YOFFSET=13,value='气象要素：',scr_ysize=23)
  meteoFeature = widget_droplist(datatype, xoffset=80,yoffset=8,uname='meteoFeature', value = list, EVENT_PRO='featurechange')

  time=widget_base(LEFT_BASE,uname='time',xoffset=0,yoffset=43,scr_xsize=241,scr_ysize=92,frame=1,/column)
;  shishi=WIDGET_COMBOBOX(time,xoffset=7,yoffset=7,uname='shishi')
  Frequenc_BASE=widget_base(time,uname='Frequenc_BASE',/EXCLUSIVE,row=1,space=10,xpad=5,ypad=1)
   shishi = Widget_Button(Frequenc_BASE, UNAME='shishi'  $
      ,SCR_XSIZE=90 ,SCR_YSIZE=18 ,xoffset=25,yoffset=0,VALUE='实时数据',event_pro='pointsshi')
  WIDGET_CONTROL,shishi,SET_BUTTON = 1
  lishi=Widget_Button(Frequenc_BASE, UNAME='lishi'  $
      ,XOFFSET=135 ,SCR_XSIZE=150 ,SCR_YSIZE=18 ,yoffset=0  $
      ,VALUE='历史平均数据',event_pro='pointhis')

 time_1=widget_base(time,/row,space=3,xpad=3,ypad=1)
 time_2=widget_base(time,/row,space=3,xpad=3,ypad=1)
 label = widget_label(time_1, value='时间：')
 listyear=['1980','1981','1982','1983','1984','1985','1986',$
  '1987','1988','1989','1990','1991','1992','1993','1994',$
  '1995','1996','1997','1998','1999','2000','2001','2002',$
  '2003','2004','2005','2006','2007','2008','2009','2010',$
  '2011','2012','2013','2014','2015']
 temp=(bin_date())[0]-1980

  drop_year=widget_combobox(time_1,uname='drop_year',xoffset=26,yoffset=29,value=listyear) ;原代码
  widget_control,drop_year, SET_COMBOBOX_SELECT=temp

  year=widget_label(time_1,uname='year',value='年')

;=================================================================
  ;  drop_month=widget_combobox(time,uname='drop_month',xoffset=102,yoffset=29,value=['1','2','3','4','5','6','7','8','9','10','11','12'])
  drop_month=widget_combobox(time_1,uname='drop_month',xoffset=102,yoffset=29,value=['1','2','3','4','5','6','7','8','9','10','11','12'])
  month=widget_label(time_1,uname='month',value='月')

  ;  drop_tenday=widget_combobox(time,uname='drop_tenday',xoffset=169,yoffset=29,scr_xsize=37,value=['1','2','3','23'])
  drop_tenday=widget_combobox(time_1,uname='drop_tenday',xoffset=169,yoffset=29,scr_xsize=37,value=['上','中','下'])
  tenday=widget_label(time_1,uname='tenday',value='旬')

  l_nianxian=widget_label(time_2,uname='l_nianxian',value='历史插值年限：')
  drop_nianxian=widget_combobox(time_2,uname='drop_nianxian',xsize=40, value=['3','2','1'],sensitive=0)

  l_yeary=widget_label(time,uname='l_yeary',xoffset=147,yoffset=62,value='年')
  inter=widget_base(LEFT_BASE,uname='inter',xoffset=0,yoffset=134,scr_xsize=241,scr_ysize=65,frame=1)
  l_method=widget_label(inter,uname='l_method',xoffset=7,yoffset=10,value='插值方法：')
  method_index = widget_droplist(inter,xoffset=70,yoffset=7,value = ['反距离加权插值','样条插值','克里格插值'],scr_ysize=23)
  para = widget_button(inter,yoffset=7,xoffset=240+100,xsize=0,ysize=0, value = 'image\detail.bmp',/bitmap,tooltip='设置插值参数')
  l_resolution=widget_label(inter,uname='l_resolution',xoffset=7,yoffset=39,value='插值分辨率(米)：')
  reso = widget_text(inter,uname='reso',xoffset=104,yoffset=34,value ='1000',xsize=11,/editable)
  b_dem=widget_base(left_base,uname='b_dem',xoffset=0,yoffset=206,frame=1,scr_xsize=241,scr_ysize=57,sensitive=0)
  WID_BASE_11 = Widget_Base(b_dem, UNAME='WID_BASE_11'  $
      ,XOFFSET=5 ,YOFFSET=1 ,SCR_XSIZE=250,TITLE='IDL' ,COLUMN=1 ,/NONEXCLUSIVE)
  l_dem=widget_button(WID_BASE_11,uname='l_dem',xoffset=70,yoffset=7,scr_xsize=148,scr_ysize=18,value='dem文件(仅用于积温，可选)',$
  event_func='WID_DEM_check')
  demfile=widget_text(b_dem,uname='demfile',xoffset=7,yoffset=26,scr_xsize=195,$
      SENSITIVE=0 )
  bt_opendem=widget_button(b_dem,uname='bt_opendem',xoffset=210,yoffset=26,value='image\open.bmp',/bitmap,scr_ysize=23,event_func='opendem_event',$
      SENSITIVE=0 )
  b_output=widget_base(left_base,uname='b_output',xoffset=0,yoffset=270,frame=1,scr_xsize=241,scr_ysize=57)
  l_output=widget_label(b_output,uname='l_output',xoffset=7,yoffset=7,scr_xsize=148,scr_ysize=18,value='插值结果输出')
  outputfile=widget_text(b_output,uname='outputfile',xoffset=7,yoffset=26,scr_xsize=195,VALUE='data_test\INTERPOLATE_TEST1')
  bt_openoutput=widget_button(b_output,uname='bt_openoutput',xoffset=210,yoffset=26,value='image\open.bmp',/bitmap,scr_ysize=23,event_func='nq_inter_cmd_output')

	button_base=widget_base(left_base,xoffset=0,yoffset=334,scr_xsize=241, ysize=33, frame=1)

  bt_inter=widget_button(button_base,xoffset=7,yoffset=5,uname='bt_inter',value='运行',xsize = 45,  ysize = 23, event_pro = 'runInterpolation', SENSITIVE = SENSITIVE)
  bt_help=widget_button(button_base,value = '帮助', yoffset=5,xoffset=98,xsize = 45,  ysize = 23,EVENT_PRO='NQ_INTERPOLATEMAIN_HELP_EVENT')
  bt_close=widget_button(button_base,value = '关闭',yoffset=5,xoffset=188, xsize = 45,  ysize = 23,  event_pro = 'NQ_interpolate_widget_close')
;-------------------------主界面的右边---------------------------------------------
  right_base = Widget_Base(TLB, UNAME='right_base' ,FRAME=0 ,XOFFSET=253  $
      ,YOFFSET=7 ,SCR_XSIZE=412 ,SCR_YSIZE=369 ,TITLE='IDL' ,SPACE=7  $
      ,XPAD=7 ,YPAD=7)
  base_id = Widget_Base(right_base,SCR_XSIZE=412 ,SCR_YSIZE=369,/col,xpad=0,ypad=0,space=0,/frame)

	class=0
	staff_display = {$
	base_id  :base_id,$
	image    :ptr_new(/ALLOCATE_HEAP,/no_copy),$
	startx	 :0.0    , $
	starty	 :0.0    , $
	xsize	 :0.0    , $
	ysize	 :0.0    , $
	pixelsize:0.0    , $
	palette	 :39, $
	shapefile:'',$
	legend   :'',$
	class	 :class ,$
	title    :''}
	pstaff_display = ptr_new(staff_display, /NO_COPY)
	widget_control, base_id, set_uvalue=pstaff_display
	display,pstaff_display

;  l_result=widget_label(right_base,uname='right_base',xoffset=180,yoffset=9,value='插值结果',ysize=12)

  ;-------------DRAW---------------
;  drawid = Widget_Draw(right_base, UNAME='drawid' ,FRAME=1  $
;  ,RETAIN=2,/BUTTON_EVENTS,/MOTION_EVENTS,UVALUE=FLTARR(400,293) $  ;之所以给初始值,是因为没有影像时,指针移动时不出错.
;  ,XOFFSET=7 ,YOFFSET=26 ,SCR_XSIZE=400 ,SCR_YSIZE=293,EVENT_PRO='CURSOR',GRAPHICS_LEVEL=2)
;**************8
;  drawid = Widget_Draw(right_base, UNAME='drawid' ,FRAME=1  $
;  ,RETAIN=2,/BUTTON_EVENTS,/MOTION_EVENTS,UVALUE=FLTARR(400,293) $  ;之所以给初始值,是因为没有影像时,指针移动时不出错.
;  ,XOFFSET=7 ,YOFFSET=26 ,SCR_XSIZE=400 ,SCR_YSIZE=293,EVENT_PRO='CURSOR')
;****************
;  drawid = Widget_Draw(right_base, UNAME='drawid' ,FRAME=0  $
;  ,RETAIN=2,/BUTTON_EVENTS,/MOTION_EVENTS,GRAPHICS_LEVEL=2  $  ;之所以给初始值,是因为没有影像时,指针移动时不出错.
;  ,XOFFSET=5 ,YOFFSET=26 ,SCR_XSIZE=400 ,SCR_YSIZE=293,UVALUE=FLTARR(400,293),EVENT_PRO='CURSOR')
;  LABEL_cursor = Widget_Label(right_BASE, UNAME='LABEL_cursor'  $
;      ,XOFFSET=110 ,YOFFSET=339 ,SCR_XSIZE=78 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
;      ,VALUE='指针处数值：')
;
;  ValueBox = Widget_Text(right_BASE, UNAME='ValueBox'  $
;      ,XOFFSET=195 ,YOFFSET=332 ,SCR_XSIZE=89 ,SCR_YSIZE=22 ,XSIZE=20  $
;      ,YSIZE=1)

  ProCode = STRMID(PROVINCE_CODE,0,2)      ;这三量设在这里是为接受系统设置所预设的参数,将来系统集成时应作修改
	print,'插值参数设置'
	;
	INPUTBORDER_PATH=''
	output_path='' ;文件输出路径
	;input_path=''  ;DEM输入路径
	;DEMSentive = 1   ;激活插值方法,是否技持DEM数据
	BorderSentive=0   ;激活边界方法
;	default_font = obj_new('IDLgrFont', '宋体', size=7.0)
    TVLCT, r, g, b, /Get
    listvalue = intarr(3)
    Kring_Anisotropy=[1.0,1.0,0.0];各向异性值
    Kring_Variogram = [2,63.8559,0.0,1.0]
    IDW_Anisotropy=[1.0,1.0,0.0]
    IDW_Ellipse=[0.0,0.0,0.0]
    PREG_Anisotropy=[1.0,1.0,0.0]
    PREG_Ellipse=[0.0,0.0,0.0]
    RBF_ANISOTROPY=[1.0,1.0,0.0]
    RBF_Ellipse=[0.0,0.0,0.0]
    RBFvalue=[0.0,7.98199]
    DEMSentive=0   ;激活dem
    ;设置调色板
;    thePalette = Obj_New('IDLgrPalette')
;    thePalette->LoadCT, 39

    ;定义图像大小
    xsize = 659L      ;初始值 ;后面的程序中可以改变
    ysize = 883L      ;初始值 后面的程序中可以改变

    COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD
	Widget_Control, /REALIZE, tlb
	WIDGET_CONTROL,bt_close,/INPUT_FOCUS
	;**************
;	white=!D.N_COLORS-1
;	WIDGET_CONTROL,drawid,GET_VALUE=TMEP
;	WSET,TMEP
;	TEMP=INDGEN(2)
;	PLOT,TEMP,BACKGROUND=WHITE
;****************
;	Widget_Control,drawid,GET_VALUE=Owindow
;	Owindow->ERASE,COLOR=255

    ProCode = STRMID(PROVINCE_CODE,0,2)      ;这三量设在这里是为接受系统设置所预设的参数

    Province = ['北京市','天津市','河北','山西','内蒙古','辽宁','吉林','黑龙江','上海市','江苏' $
						,'浙江','安微','福建','江西','山东','河南','湖北','湖南','广东','广西','海南' $
						,'重庆市','四川','贵州','云南','西藏','陕西','甘肃','青海','宁夏','新疆']
	ProIDList = ['11','12','13','14','15','21','22','23','31','32','33','34','35','36','37',$
						'41','42','43','44','45','46','50','51','52','53','54','61','62','63','64','65']


    NQ_ProjectPara = NQ_readparameter(Province[WHERE(ProIDList EQ ProCode)])


    state = { $
;        theWindow	: DrawID,$
; 		thePalette	: thePalette, $	;调色板对像
;        xpad		:	0.0,	$	;横向平移系数
;		ypad		:	0.0,	$	;纵向平移系数
;		zoomFactor	:	1.0,	$	;缩放系数
		NQ_ProjectPara		   :  NQ_ProjectPara		,$		;用于定义省相应范围内的投影参数值(见"text\nq_parametersetting.txt")
		Province			   :  Province			,$    ;省名列表
        ProID				   :  ProCode			,$	  ;被选省ID,2位字符型
		ProIDList			   :  ProIDList			,$	  ;省ID列表
        bt_opendem:bt_opendem,$
        demfile:demfile,$
        DEMSentive:DEMSentive,$        ;dem是否被激活
    	xsize : xsize, $               ; The x size of the image window and of the image.
		ysize : ysize, $               ; The y size of the image window and of the image.
        reso:reso,$
     ;   data:ptr_new(), $
      ;  pixeldata:ptr_new(),$
        AddMin				   :  0.0				,$    ;将省内的插值结果进行识别的参数
        ix	:	0.,	$
        iy	:	0.,	$
        point:0,$            ;用来表示是选中了实时还是历史
	;	default_font:default_font, $
		BATHINDEX:1           , $    ;用来代表是否是批处理插值
;		drawid:drawid,$               ;
; 		theModel 	: obj_new(), $
; 		theImage 	: Obj_New(), $
;        oView       : OBJ_NEW()			,$		;视图对象
;		text 		: OBJ_NEW(), $
;		theColorbar	: OBJ_NEW(), $
;		zoomfactor : 2, $              ; The initial zoom factor.
		flagnum:0,$
		drop_nianxian:drop_nianxian,$;年限
        drop_year:drop_year,$
        drop_month:drop_month,$
        drop_tenday:drop_tenday,$
        tenday:tenday,$
		meteoFeature : meteoFeature,$
		;气象要素
		para : para, $
		method_index	: method_index  ,$        ;插值方法序列数
		OUTPUTFILE       	: OUTPUTFILE,$
        InputBorder_path    :	InputBorder_path,$
        WID_resolution      :	reso,$     	;插值精度
;        ValueBox			:	ValueBox,$	;显示鼠标所在点的数据值
        dbco_id 	: 1 ,	$ ;数据库链接标志
;        dbco		:  obj_new('IDLDBdatabase'),$   ;数据库链接
        Muple_isx_y     :  FLTARR(2)  ,$               ;用于画图用.
        TABLE_YSIZE_NUM:0  , $
        BatchAvtive:0 , $
        POINTDELELTE:0,$  ;删点标志
        POINTView:0,$     ;浏览点标志
        Powerdata:2,$
        Smoothvalue:0.0,$
        read_method : 2                     , $ ;数据库链接方式
	;	NewfilePtr:ptr_new(/allocate_heap) ,$;以文件方式进行读取时保存数据
		;
		;接下来设置Kring插值的参数$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
		Kring_Anisotropy :Kring_Anisotropy , $
		Kring_Variogram :Kring_Variogram   ,$
        Kring_ShapeValue:1 ,$
        Kring_MinpointsID:8 ,$
		;接下来设置IDW插值的参数$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
		IDW_Anisotropy :IDW_Anisotropy , $
		IDW_Ellipse :IDW_Ellipse   ,$
        IDW_ShapeValue:1 ,$
        IDW_Minpoints:8  ,$
        IDW_Maxpoints:15    ,$
        IDW_smooth:0.0     ,$
        IDW_power:2 ,$
        ;接下来设置多项式回归插值的参数$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
        PREG_Anisotropy :PREG_Anisotropy , $
        PREG_Ellipse :PREG_Ellipse   ,$
        PREG_ShapeValue:1 ,$
        PREG_Minpoints:8  ,$
        PREG_Maxpoints:15    ,$
        PREG_power:2, $
        ;接下来设置径向基插值的参数$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
        RBF_Anisotropy :RBF_Anisotropy , $
        RBF_Ellipse :RBF_Ellipse   ,$
        RBF_ShapeValue:1 ,$
        RBF_Minpoints:8  ,$
        RBF_Maxpoints:15    ,$
        RBFvalue:RBFvalue,  $
        b_dem : b_dem ,$
        pstaff_display:pstaff_display $
	}
  	pstate = PTR_NEW(state, /no_copy)
;  	(*PSTATE).DBCO=DBobj

	widget_control, tlb, set_uvalue=pstate
	WIDGET_CONTROL,(*pstate).drop_month, SET_COMBOBOX_SELECT=(bin_date())[1]-1
	tenday_index = (bin_date())[2]/10
	tenday_index = (tenday_index gt 2)? 2 : tenday_index
	WIDGET_CONTROL,(*pstate).drop_tenday, SET_COMBOBOX_SELECT=tenday_index

	COMMON COMMON_SETPATH,ppath
	v_nq_in_path  = (*ppath).nq_in_path
	v_nq_out_path = (*ppath).nq_out_path
	v_nq_dem_file = (*ppath).nq_dem_file

	WIDGET_CONTROL,(*pstate).demfile, set_value=v_nq_dem_file
	WIDGET_CONTROL,(*pstate).OUTPUTFILE, set_value=v_nq_in_path+'INTERPOLATE'

	xmanager, 'NQ_INTERPOLATE_intermain', tlb,/NO_BLOCK, CLEANUP='NQ_INTERPOLATE_CleanAllHeap'
end

;pro NQ_INTERPOLATE_intermain_interface,GROUP_LEADER=wGroup
;
;    NQ_INTERPOLATE_intermain,GROUP_LEADER=wGroup
;
;end
pro NQ_INTERPOLATE_CleanAllHeap, id
	Widget_Control, id, get_uvalue=pstate
	heap_free, pstate
end