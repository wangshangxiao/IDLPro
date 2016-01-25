;PRO NQ_Draw_image,SAVE_FILE,WID_DRAW,oView=oView,WHITE=WHITE
;
;	widget_Control,WID_DRAW,GET_VALUE = oWindow,GET_UVALUE=data
;	oWindow->ERASE,COLOR=255
;
;	ShapeFileName='data_vector\province.shp'
;	r=widget_info(WID_DRAW,/geometry)     ;&&&&&&&&&&���
;
;	;(1)------------��Ӱ��ͼ-----------------------------------------
;;	oColor = [128,128,128]
;	IF KEYWORD_SET(WHITE) THEN 	oColor = [255,255,255] ELSE oColor = [0,0,0]
;
;
;
;	oScene = Obj_New('IDLgrScene', Color = oColor)
;	oView  = Obj_New('IDLgrView',Color = oColor,Eye = 101,ZClip = [100,-1]) 	;������ͼ
;	Image_Layer   = Obj_New('IDLgrModel')	  ;Ӱ��ͼ��
;	Polygon_Layer = obj_new('IDLgrModel')     ;ʸ��ͼ��
;
;	oWindow->SetProperty, Graphics_Tree = oScene
;	oWindow->SetCurrentCursor, 'ARROW'    	   ;���ָ��Ϊ��ͷ,����ʮ�ֲ�.
;
;	oScene->Add, oView
;;	oView ->Add, Polygon_Layer
;	oView ->Add, Image_Layer
;	oView ->Add, Polygon_Layer
;
;	;����ֵ���쵽256ɫ��ʾ��Χ��
;	dataDis = BYTSCL(data,MIN=299.99)
;
;	Palette = obj_new('IDLgrPalette')
;	Palette->LoadCt,40
;;;	TVLCT,0,0,0    ;ʹ0����ɫ��������ɫΪ��ɫ(������ʵ���Կ����Ǳ���ɫ,Ĭ���Ǻ�ɫ),
;
;	oImage = Obj_New('IDLgrImage',Depth_Test_Disable = 2,Location=[0,0,0],ORDER=1 ,PALETTE=Palette $
;			               ,dataDis)   ;����dataDis����������
;
;	Image_Layer->Add,oImage
;
;	;----��ʸ�����ݽ������ŵĲ���---------------------------------------
;	Para = DC_ReadHead_file(SAVE_FILE)
;	UlX     = Double(Para.UlX)     & UlY   = Double(Para.UlY)  ;���Ͻ�
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
;	;----��ʸ������ͼ��----------------------------------------
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
;	    x = (TEMPORARY(x)-uvRange[0])*xSize/uRange     ;���ｫX,Y�����������
;		y = (TEMPORARY(y)-uvRange[1])*ySize/vRange
;
;	    oPolyline = obj_new('IDLgrPolyline',x,y,color=[255,255,255],thick = 2,LINESTYLE = 0) ;[245,122,182]
;	    Polygon_Layer->Add,oPolyline
;	    myshape -> IDLffShape::DestroyEntity, ent
;	 ENDFOR
;	ENDIF
;	OBJ_DESTROY, myshape
;
;	;view��������ϵ
;	viewPlane = [0,0,xSize,ySize]
;	;view�Ĵ�С
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
;	;view�Ĳ���λ��
;	viewLoc = [(drawSize[0]-viewDim[0])/2,(drawSize[1]-viewDim[1])/2]
;
;	oView->Setproperty, ViewPlane_Rect = viewPlane, Dimensions = viewDim,Location = viewLoc
;	oWindow->Draw,oScene
;
;	HEAP_GC
;
;END

;**************��ENVI��׼�ļ�������*********************************************
FUNCTION NQ_CL_Read_ENVIData,inputfile_ $  				;������ļ���(�ַ���)
					  ,SUCCESSSTATUS = SuccessStatus $	;��ȡ�Ƿ�ɹ���״̬.
					  ,DESCRIPTION  = Description		;�û��������ʾ��,�ַ���
	;������ʽ:Resutl = DC_Read_ENVIData(inputfile_,[SUCCESSSTATUS = var],[DESCRIPTION=Decscription])
	;����ֵΪ:(1)SuccessStatus=0 ,��ȡ���ɹ�Ϊ0
	;		  (2)SuccessStatus=1 ,��ȡ�ɹ�,����Ϊ����
    ;ע��ó���ֻ�����ڶ���׼ENVI�ļ�(�����ļ�+ͷ�ļ�)
    ;,��ֻ�޶�ȡ����������(�����Ҳ��ɶ��ನ�ε�)
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
	       prompt = DIALOG_MESSAGE(Decscription+'�����ļ�������!',title='��ʾ')
		   IF ARG_PRESENT(SuccessStatus) THEN SuccessStatus = 0
	       RETURN,0
        END

        ~FILE_TEST(DataFile) : BEGIN
	       prompt = DIALOG_MESSAGE(Decscription+'ͷ�ļ�������!',title='��ʾ')
		   IF ARG_PRESENT(SuccessStatus) THEN SuccessStatus = 0
	       RETURN,0
        END
        ELSE:
    ENDCASE

    DataFileInfo = FILE_INFO(DataFile)
    HeadFileInfo = DC_ReadHead_file(HeadFile)

    ;��Ϊ�������ļ���С�������ֽڼ��㳤��,���������Ͳ�ͬ,����������ʱҲ��������ͬ.
    IF ROUND((DataFileInfo.SIZE)/FIX(HeadFileInfo.ByteNum),/L64) NE $
       ULONG(HeadFileInfo.samples)*(HeadFileInfo.lines) THEN BEGIN
       prompt = DIALOG_MESSAGE(Decscription+'�����ļ���ͷ�ļ���Ϣ��������һ��!',title='��ʾ')
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

;***********************����ֵͼ**************************************
;=================��"����ͼ�δ���"�д�Ӱ��ͼ��ʸ��ͼ=============================
PRO NQ_Draw_image,INputFile $	;����ʾ��Ӱ���ļ���,�ַ���
				  ,WID_DRAW $	;Draw���,��������û�ֵdata,��dataά��С��INputFile�ļ�Ӱ��ά���ݱ�����ͬ
				  ,OView=View $ ;���ص�Ҫ�õ�����ͼ����
				  ,MINVALUE=minvalue $
				  ,MUPLE_ISX_Y = muple_isx_y $  ;����ָ����BYTSCL()�����������Сֵ
				  ,shapefile=shapefile $
				  ,WHITE=WHITE	 ;ָ������ɫΪ��ɫ.
; ����:DC_Draw_image,INputFile,WID_DRAW,[OView=View,MINVALUE=minvalue,/WHITE]
	widget_Control,WID_DRAW,GET_VALUE = oWindow,GET_UVALUE=data
	oWindow->ERASE,COLOR=255
	oWindow->GETPROPERTY,GRAPHICS_TREE = TreeOBJ
	IF OBJ_VALID(TreeOBJ) THEN OBJ_DESTROY,TreeOBJ
;    ShapeFileName='data_vector\province.shp'
;    widget_control, event.top, get_uvalue =state
;	ParaInfo = (*state).NQ_ProjectPara    ;�õ�ʡ��Χ�ڵ�ͶӰ����
;
;    ShapeFileName=ParaInfo.shppath

	;ShapeFileName='data_vector\shanxi.shp'
	r=widget_info(WID_DRAW,/geometry)     ;&&&&&&&&&&���

	;(1)------------��Ӱ��ͼ-----------------------------------------
;	oColor = [128,128,128]
	IF (N_ELEMENTS(data) EQ 0) THEN data= DC_Read_ENVIData(INputFile) ;���û���û�ֵ,��������Ӱ��
	IF KEYWORD_SET(WHITE)      THEN oColor = [255,255,255] ELSE oColor = [0,0,0]
	IF ~KEYWORD_SET(MINVALUE)  THEN minvalue = MIN(data)

	oScene = Obj_New('IDLgrScene', Color = oColor)
	oView  = Obj_New('IDLgrView',Color = oColor,Eye = 101,ZClip = [100,-1]) 	;������ͼ
  ;  oView  = Obj_New('IDLgrView',Eye = 101,ZClip = [100,-1],color = [56,255,126]) 	;������ͼ
	Image_Layer   = Obj_New('IDLgrModel')	  ;Ӱ��ͼ��
	Polygon_Layer = obj_new('IDLgrModel')     ;ʸ��ͼ��

	oWindow->SetProperty, Graphics_Tree = oScene
	;oWindow->SetCurrentCursor, 'ARROW'    	   ;���ָ��Ϊ��ͷ,����ʮ�ֲ�.

	oScene->Add, oView

	oView ->Add, Image_Layer
    oView ->Add, Polygon_Layer
	;����ֵ���쵽256ɫ��ʾ��Χ��

	dataDis = BYTSCL(data,MIN=minvalue)

	Palette = obj_new('IDLgrPalette')
;	LoadCt,23
;	TVLCT,200,200,200   ;ʹ0����ɫ��������ɫΪ��ɫ(������ʵ���Կ����Ǳ���ɫ,Ĭ���Ǻ�ɫ)
	Palette->LoadCt,20
;	TVLCT,56,255,126   ;ʹ0����ɫ��������ɫΪ��ɫ(������ʵ���Կ����Ǳ���ɫ,Ĭ���Ǻ�ɫ),

	oImage = Obj_New('IDLgrImage',Depth_Test_Disable = 2,Location=[0,0,0],ORDER=1 ,PALETTE=Palette $
			               ,dataDis)   ;����dataDis����������

	Image_Layer->Add,oImage

	;----��ʸ�����ݽ������ŵĲ���---------------------------------------
	Para = DC_ReadHead_file(INputFile)
	UlX     = Double(Para.UlX)     & UlY   = Double(Para.UlY)  ;���Ͻ�
	samples = Double(Para.samples) & Lines = Double(Para.Lines)  ;
;;	Resolution = FIX(Para.Resolution)	��FIxʱ,������Ϊ"1.0000000000e+003",���ܱ���ȷʶ��.������1
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

	;----��ʸ������ͼ��----------------------------------------
	IF file_test(shapefile) EQ 0  THEN return
	myshape=OBJ_NEW('IDLffShape', shapefile)
	myshape -> IDLffShape::GetProperty, N_ENTITIES = num_ent,ENTITY_TYPE  = type

	IF (type EQ 5 OR type EQ 3) THEN BEGIN
	 FOR i=0,num_ent-1 DO BEGIN
	    ent = myshape -> IDLffShape::GetEntity(i)
	    NumPoints = ent.N_VERTICES-1
	    x = (*ent.vertices)[0,0:NumPoints]
	    y = (*ent.vertices)[1,0:NumPoints]
	    x = (TEMPORARY(x)-uvRange[0])*xSize/uRange     ;���ｫX,Y�����������
		y = (TEMPORARY(y)-uvRange[1])*ySize/vRange

	    oPolyline = obj_new('IDLgrPolyline',x,y,color=[0,0,255],thick = 2,LINESTYLE = 0) ;[245,122,182]
	    Polygon_Layer->Add,oPolyline
	    myshape -> IDLffShape::DestroyEntity, ent
	 ENDFOR
	ENDIF
	OBJ_DESTROY, myshape

	;view��������ϵ
	viewPlane = [0,0,xSize,ySize]
	;view�Ĵ�С
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

	;view�Ĳ���λ��
	viewLoc = [(drawSize[0]-viewDim[0])/2,(drawSize[1]-viewDim[1])/2]

	oView->Setproperty, ViewPlane_Rect = viewPlane, Dimensions = viewDim,Location = viewLoc
	oWindow->Draw,oScene

	OBJ_DESTROY,Palette

	IF ARG_PRESENT(View) THEN View=oView
END