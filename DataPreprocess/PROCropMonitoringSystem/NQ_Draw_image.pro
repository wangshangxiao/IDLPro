;PRO NQ_Draw_image,SAVE_FILE,WID_DRAW,oView=oView,WHITE=WHITE
;
;	widget_Control,WID_DRAW,GET_VALUE = oWindow,GET_UVALUE=data
;	oWindow->ERASE,COLOR=255
;
;	ShapeFileName='data_vector\province.shp'
;	r=widget_info(WID_DRAW,/geometry)     ;&&&&&&&&&&组件
;
;	;(1)------------读影像图-----------------------------------------
;;	oColor = [128,128,128]
;	IF KEYWORD_SET(WHITE) THEN 	oColor = [255,255,255] ELSE oColor = [0,0,0]
;
;
;
;	oScene = Obj_New('IDLgrScene', Color = oColor)
;	oView  = Obj_New('IDLgrView',Color = oColor,Eye = 101,ZClip = [100,-1]) 	;创建视图
;	Image_Layer   = Obj_New('IDLgrModel')	  ;影像图层
;	Polygon_Layer = obj_new('IDLgrModel')     ;矢量图层
;
;	oWindow->SetProperty, Graphics_Tree = oScene
;	oWindow->SetCurrentCursor, 'ARROW'    	   ;鼠标指针为箭头,不是十字叉.
;
;	oScene->Add, oView
;;	oView ->Add, Polygon_Layer
;	oView ->Add, Image_Layer
;	oView ->Add, Polygon_Layer
;
;	;将数值拉伸到256色显示范围内
;	dataDis = BYTSCL(data,MIN=299.99)
;
;	Palette = obj_new('IDLgrPalette')
;	Palette->LoadCt,40
;;;	TVLCT,0,0,0    ;使0号颜色索引号颜色为白色(这里其实可以看成是背景色,默认是黑色),
;
;	oImage = Obj_New('IDLgrImage',Depth_Test_Disable = 2,Location=[0,0,0],ORDER=1 ,PALETTE=Palette $
;			               ,dataDis)   ;这里dataDis是拉伸数据
;
;	Image_Layer->Add,oImage
;
;	;----将矢量数据进行缩放的参数---------------------------------------
;	Para = DC_ReadHead_file(SAVE_FILE)
;	UlX     = Double(Para.UlX)     & UlY   = Double(Para.UlY)  ;左上角
;	samples = Double(Para.samples) & Lines = Double(Para.Lines)  ;
;	Resolution = float(Para.Resolution)
;	uvRange = [UlX,UlY-Lines*Resolution,UlX+samples*Resolution,UlY]
;	uRange  = uvRange[2]-uvRange[0]
;	vRange  = uvRange[3]-uvRange[1]
;
;	Datasize = SIZE(data,/DIMENSIONS)
;	xSize = Datasize[0]
;	ySize = Datasize[1]
;	drawSize = [r.scr_xsize,r.scr_ysize]
;
;	;----读矢量数据图层----------------------------------------
;	IF file_test(ShapeFileName) EQ 0  THEN return
;	myshape=OBJ_NEW('IDLffShape', ShapeFileName)
;	myshape -> IDLffShape::GetProperty, N_ENTITIES = num_ent,ENTITY_TYPE  = type
;
;	IF (type EQ 5 OR type EQ 3) THEN BEGIN
;	 FOR i=0,num_ent-1 DO BEGIN
;	    ent = myshape -> IDLffShape::GetEntity(i)
;	    NumPoints = ent.N_VERTICES-1
;	    x = (*ent.vertices)[0,0:NumPoints]
;	    y = (*ent.vertices)[1,0:NumPoints]
;	    x = (TEMPORARY(x)-uvRange[0])*xSize/uRange     ;这里将X,Y坐标进行缩放
;		y = (TEMPORARY(y)-uvRange[1])*ySize/vRange
;
;	    oPolyline = obj_new('IDLgrPolyline',x,y,color=[255,255,255],thick = 2,LINESTYLE = 0) ;[245,122,182]
;	    Polygon_Layer->Add,oPolyline
;	    myshape -> IDLffShape::DestroyEntity, ent
;	 ENDFOR
;	ENDIF
;	OBJ_DESTROY, myshape
;
;	;view的坐标体系
;	viewPlane = [0,0,xSize,ySize]
;	;view的大小
;	Ratio = 1.0
;	scale = Min((Double(drawSize*Ratio)/[xSize,ySize])<1)
;	IF scale NE 1. THEN BEGIN
;		viewDim = [Fix(scale*xSize), Fix(scale*ySize)]
;	ENDIF ELSE BEGIN
;		IF 1.*xSize/ySize GE 1.*drawSize[0]/drawSize[1] THEN BEGIN
;			viewDim = [Fix(Ratio*drawSize[0]),Fix(Ratio*ySize/xSize*drawSize[0])]
;		ENDIF ELSE BEGIN
;			viewDim = [Fix(Ratio*xSize/ySize*drawSize[1]),Fix(Ratio*drawSize[1])]
;		ENDELSE
;	ENDELSE
;
;	;view的插入位置
;	viewLoc = [(drawSize[0]-viewDim[0])/2,(drawSize[1]-viewDim[1])/2]
;
;	oView->Setproperty, ViewPlane_Rect = viewPlane, Dimensions = viewDim,Location = viewLoc
;	oWindow->Draw,oScene
;
;	HEAP_GC
;
;END

;**************读ENVI标准文件的数据*********************************************
FUNCTION NQ_CL_Read_ENVIData,inputfile_ $  				;输入的文件名(字符型)
					  ,SUCCESSSTATUS = SuccessStatus $	;读取是否成功的状态.
					  ,DESCRIPTION  = Description		;用户输入的提示语,字符型
	;调用形式:Resutl = DC_Read_ENVIData(inputfile_,[SUCCESSSTATUS = var],[DESCRIPTION=Decscription])
	;返回值为:(1)SuccessStatus=0 ,读取不成功为0
	;		  (2)SuccessStatus=1 ,读取成功,返回为数据
    ;注意该程序只限用于读标准ENVI文件(数据文件+头文件)
    ;,且只限读取单波段数据(改造后也许可读多波段的)
 	IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2						;return to caller

    inputfile = inputfile_

	IF NOT KEYWORD_SET(Decscription) THEN Decscription=''

	DotPosition = STRPOS(inputfile, '.',/REVERSE_SEARCH)
    IF DotPosition EQ -1 THEN BEGIN
       HeadFile = STRTRIM(inputfile,2)+'.hdr'
       DataFile = STRTRIM(inputfile,2)
    ENDIF ELSE BEGIN
       HeadFile = STRMID(inputfile,0,DotPosition)+'.hdr'
       DataFile = STRMID(inputfile,0,DotPosition)
    ENDELSE

    CASE 1 OF
        ~FILE_TEST(HeadFile) : BEGIN
	       prompt = DIALOG_MESSAGE(Decscription+'数据文件不存在!',title='提示')
		   IF ARG_PRESENT(SuccessStatus) THEN SuccessStatus = 0
	       RETURN,0
        END

        ~FILE_TEST(DataFile) : BEGIN
	       prompt = DIALOG_MESSAGE(Decscription+'头文件不存在!',title='提示')
		   IF ARG_PRESENT(SuccessStatus) THEN SuccessStatus = 0
	       RETURN,0
        END
        ELSE:
    ENDCASE

    DataFileInfo = FILE_INFO(DataFile)
    HeadFileInfo = DC_ReadHead_file(HeadFile)

    ;因为程序中文件大小均是以字节计算长度,而数据类型不同,计算行列数时也会有所不同.
    IF ROUND((DataFileInfo.SIZE)/FIX(HeadFileInfo.ByteNum),/L64) NE $
       ULONG(HeadFileInfo.samples)*(HeadFileInfo.lines) THEN BEGIN
       prompt = DIALOG_MESSAGE(Decscription+'数据文件与头文件信息行列数不一致!',title='提示')
	   IF ARG_PRESENT(SuccessStatus) THEN SuccessStatus = 0
       RETURN,0
    ENDIF

    Type_code = FIX(HeadFileInfo.datatype)
    Data = MAKE_ARRAY(LONG(HeadFileInfo.samples),LONG(HeadFileInfo.lines),TYPE=Type_code)

    OPENR,Lun,DataFile,/GET_LUN
    READU,Lun,Data
    FREE_LUN,Lun


	IF ARG_PRESENT(SuccessStatus) THEN SuccessStatus = 1

   RETURN,Data

END

;***********************画插值图**************************************
;=================在"对象图形窗口"中打开影像图和矢量图=============================
PRO NQ_Draw_image,INputFile $	;被显示的影像文件名,字符串
				  ,WID_DRAW $	;Draw组件,如果其有用户值data,则data维大小与INputFile文件影像维数据必须相同
				  ,OView=View $ ;返回的要得到的视图对象
				  ,MINVALUE=minvalue $
				  ,MUPLE_ISX_Y = muple_isx_y $  ;用于指定被BYTSCL()进行提伸的最小值
				  ,shapefile=shapefile $
				  ,WHITE=WHITE	 ;指定背景色为白色.
; 调用:DC_Draw_image,INputFile,WID_DRAW,[OView=View,MINVALUE=minvalue,/WHITE]
	widget_Control,WID_DRAW,GET_VALUE = oWindow,GET_UVALUE=data
	oWindow->ERASE,COLOR=255
	oWindow->GETPROPERTY,GRAPHICS_TREE = TreeOBJ
	IF OBJ_VALID(TreeOBJ) THEN OBJ_DESTROY,TreeOBJ
;    ShapeFileName='data_vector\province.shp'
;    widget_control, event.top, get_uvalue =state
;	ParaInfo = (*state).NQ_ProjectPara    ;得到省范围内的投影参数
;
;    ShapeFileName=ParaInfo.shppath

	;ShapeFileName='data_vector\shanxi.shp'
	r=widget_info(WID_DRAW,/geometry)     ;&&&&&&&&&&组件

	;(1)------------读影像图-----------------------------------------
;	oColor = [128,128,128]
	IF (N_ELEMENTS(data) EQ 0) THEN data= DC_Read_ENVIData(INputFile) ;如果没有用户值,则用输入影像
	IF KEYWORD_SET(WHITE)      THEN oColor = [255,255,255] ELSE oColor = [0,0,0]
	IF ~KEYWORD_SET(MINVALUE)  THEN minvalue = MIN(data)

	oScene = Obj_New('IDLgrScene', Color = oColor)
	oView  = Obj_New('IDLgrView',Color = oColor,Eye = 101,ZClip = [100,-1]) 	;创建视图
  ;  oView  = Obj_New('IDLgrView',Eye = 101,ZClip = [100,-1],color = [56,255,126]) 	;创建视图
	Image_Layer   = Obj_New('IDLgrModel')	  ;影像图层
	Polygon_Layer = obj_new('IDLgrModel')     ;矢量图层

	oWindow->SetProperty, Graphics_Tree = oScene
	;oWindow->SetCurrentCursor, 'ARROW'    	   ;鼠标指针为箭头,不是十字叉.

	oScene->Add, oView

	oView ->Add, Image_Layer
    oView ->Add, Polygon_Layer
	;将数值拉伸到256色显示范围内

	dataDis = BYTSCL(data,MIN=minvalue)

	Palette = obj_new('IDLgrPalette')
;	LoadCt,23
;	TVLCT,200,200,200   ;使0号颜色索引号颜色为白色(这里其实可以看成是背景色,默认是黑色)
	Palette->LoadCt,20
;	TVLCT,56,255,126   ;使0号颜色索引号颜色为白色(这里其实可以看成是背景色,默认是黑色),

	oImage = Obj_New('IDLgrImage',Depth_Test_Disable = 2,Location=[0,0,0],ORDER=1 ,PALETTE=Palette $
			               ,dataDis)   ;这里dataDis是拉伸数据

	Image_Layer->Add,oImage

	;----将矢量数据进行缩放的参数---------------------------------------
	Para = DC_ReadHead_file(INputFile)
	UlX     = Double(Para.UlX)     & UlY   = Double(Para.UlY)  ;左上角
	samples = Double(Para.samples) & Lines = Double(Para.Lines)  ;
;;	Resolution = FIX(Para.Resolution)	用FIx时,若变量为"1.0000000000e+003",则不能被正确识别.被看成1
	Resolution = FLOAT(Para.Resolution)
	uvRange = [UlX,UlY-Lines*Resolution,UlX+samples*Resolution,UlY]
	uRange  = uvRange[2]-uvRange[0]
	vRange  = uvRange[3]-uvRange[1]

	Datasize = SIZE(data,/DIMENSIONS)
	xSize = Datasize[0]
	ySize = Datasize[1]
	drawSize = [r.scr_xsize,r.scr_ysize]

    ImageMatchWidth = Datasize[0]  & ImageMatchHeight = Datasize[1]
    Muple = (FLOAT(ImageMatchWidth)/r.SCR_XSIZE) > (FLOAT(ImageMatchHeight)/r.SCR_YSIZE)
    IsX_Y = ((ImageMatchWidth/r.SCR_XSIZE) GE (ImageMatchHeight/r.SCR_YSIZE) ? 0.0 : 1.0)
	muple_isx_y = [Muple,IsX_Y]

	;----读矢量数据图层----------------------------------------
	IF file_test(shapefile) EQ 0  THEN return
	myshape=OBJ_NEW('IDLffShape', shapefile)
	myshape -> IDLffShape::GetProperty, N_ENTITIES = num_ent,ENTITY_TYPE  = type

	IF (type EQ 5 OR type EQ 3) THEN BEGIN
	 FOR i=0,num_ent-1 DO BEGIN
	    ent = myshape -> IDLffShape::GetEntity(i)
	    NumPoints = ent.N_VERTICES-1
	    x = (*ent.vertices)[0,0:NumPoints]
	    y = (*ent.vertices)[1,0:NumPoints]
	    x = (TEMPORARY(x)-uvRange[0])*xSize/uRange     ;这里将X,Y坐标进行缩放
		y = (TEMPORARY(y)-uvRange[1])*ySize/vRange

	    oPolyline = obj_new('IDLgrPolyline',x,y,color=[0,0,255],thick = 2,LINESTYLE = 0) ;[245,122,182]
	    Polygon_Layer->Add,oPolyline
	    myshape -> IDLffShape::DestroyEntity, ent
	 ENDFOR
	ENDIF
	OBJ_DESTROY, myshape

	;view的坐标体系
	viewPlane = [0,0,xSize,ySize]
	;view的大小
	Ratio = 1.0
	scale = Min((Double(drawSize*Ratio)/[xSize,ySize])<1)
	IF scale NE 1. THEN BEGIN
		viewDim = [Fix(scale*xSize), Fix(scale*ySize)]
	ENDIF ELSE BEGIN
		IF 1.*xSize/ySize GE 1.*drawSize[0]/drawSize[1] THEN BEGIN
			viewDim = [Fix(Ratio*drawSize[0]),Fix(Ratio*ySize/xSize*drawSize[0])]
		ENDIF ELSE BEGIN
			viewDim = [Fix(Ratio*xSize/ySize*drawSize[1]),Fix(Ratio*drawSize[1])]
		ENDELSE
	ENDELSE

	;view的插入位置
	viewLoc = [(drawSize[0]-viewDim[0])/2,(drawSize[1]-viewDim[1])/2]

	oView->Setproperty, ViewPlane_Rect = viewPlane, Dimensions = viewDim,Location = viewLoc
	oWindow->Draw,oScene

	OBJ_DESTROY,Palette

	IF ARG_PRESENT(View) THEN View=oView
END