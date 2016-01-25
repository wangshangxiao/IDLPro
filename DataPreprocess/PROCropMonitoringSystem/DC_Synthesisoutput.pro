;单产结果显示输出
;***********清空数据********************************************
PRO DC_ClearUp,EventTop

   Widget_Control,EventTop,GET_UVALUE=state
     Widget_Control,(*state).Yield_TABLE,GET_VALUE=Yield & Yield[*,*]=''
     Widget_Control,(*state).Yield_TABLE,SET_VALUE=Yield,SET_TABLE_SELECT=[-1,-1],COLUMN_LABELS='',ROW_LABELS=''
     Widget_Control,(*state).wholeYield_LABEL,SET_VALUE='模拟的产量'
     IF (*state).DisplayYesOrNo EQ 1 THEN BEGIN
        DisplayDraw = Widget_Info(EventTop,FIND_BY_UNAME='DisplayDraw')
        Widget_Control,DisplayDraw,SET_BUTTON = 0
        (*state).DisplayYesOrNo = 0
        Widget_Control,(*state).Draw_shape,GET_VALUE=Owindow
        Owindow->ERASE,COLOR=255
     ENDIF

END

;*******自定义函数:画矢量图形函数*******************************************
FUNCTION DC_DrawShapeMape,ShapeFileName,DistrictCode,WID_DRAW

	forward_function getShapeBounds
   IF file_test(ShapeFileName) EQ 0  THEN return,0

;   max_x = -10.0e10
;   min_x = 10.0e10
;   max_y = -10.0e10
;   min_y = 10.0e10
;   scale = 0.0D

   widget_Control,WID_DRAW,get_value = oWindow
   oWindow->ERASE,COLOR=255
   oWindow->GETPROPERTY,GRAPHICS_TREE=TreeOBJ
   IF OBJ_VALID(TreeOBJ) THEN OBJ_DESTROY,TreeOBJ   ;及时释放变量

   r=widget_info(WID_DRAW,/geometry)         ;画图组件尺寸大小

   PolyLin_Layer = OBJ_NEW('IDLgrModel')     ;line矢量图层
   Polygon_Layer = OBJ_NEW('IDLgrModel')     ;POLYGEN矢量图层
   oView = OBJ_NEW('IDLgrView',LOCATION=[0,0],DIMENSION = [0,0],VIEWPLANE_RECT = [0,0,435,350] $
                           ,UNITS=0,color = [255,255,255],eye = 8,projection = 2)
   oView->add,PolyLin_Layer          ;先加的在上层.
   oView->add,Polygon_Layer
   oWindow->SetProperty, Graphics_Tree = oView     ;为释放变量的目的

   myshape=OBJ_NEW('IDLffShape',ShapeFileName)
   myshape -> GetProperty, N_ENTITIES = num_ent,ENTITY_TYPE  = type
;   DistrictCode=['230000','420000','220000']  ;测试用%%
   IF type EQ 5 THEN BEGIN
     FOR i=0,num_ent-1 DO BEGIN
        ent = myshape -> GetEntity(i,/ATTRIBUTES)
        x = (*ent.vertices)[0,*]
        y = (*ent.vertices)[1,*]
;        max_x = max_x > max(x)
;        min_x = min_x < min(x)
;        max_y = max_y > max(y)
;        min_y = min_y < min(y)                           ;为得到整个shpe图中最大和最小的x,y坐标.

         oTess = OBJ_NEW('IDLgrTessellator')
           oTess->AddPolygon,TRANSPOSE(x),TRANSPOSE(y)    ;其作用是将凹多形转成凸多边形.x,y须是一行值.
           result = oTess->Tessellate(v, polygons)
           oPolygon = OBJ_NEW('IDLgrPolygon',v,POLYGONS = polygons,STYLE=1,COLOR=[255,255,255],THICK=2,LINESTYLE=0)
           OBJ_DESTROY,oTess

        IF WHERE(DistrictCode EQ (*ent.ATTRIBUTES).(0)) NE [-1] THEN BEGIN
           oPolygon->SetProperty,color=[0,255,255];,STYLE=2
        ENDIF

        oPolyline = OBJ_NEW('IDLgrPolyline',x,y,color=[0,0,0],thick=1,LINESTYLE=0)

        PolyLin_Layer->Add,oPolyline
        Polygon_Layer->Add,oPolygon

        myshape -> DestroyEntity, ent
     ENDFOR
   ENDIF
   OBJ_DESTROY, myshape
;;;*********************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;读取shp文件的范围,并设置缩放比例
	rect =  getShapeBounds(ShapeFileName)
	if size(rect, /type) ne 7 then begin
		wf = rect[2] / double(r.SCR_XSIZE)
		hf = rect[3] / double(r.SCR_YSIZE)
		maxf = max([wf, hf])
		origin_rect = rect
		image_rect = [(r.SCR_XSIZE-rect[2]/maxf)/2.0,(r.SCR_YSIZE-rect[3]/maxf)/2.0,rect[2]/maxf, rect[3]/maxf]
		scale = maxf
	endif

	;坐标变换
	;缩放比例
	L = 1.0 / scale

	;平移与缩放变换
	Polygon_Layer->reset
	Polygon_Layer->Translate, -origin_rect[0], -origin_rect[1], 0
	Polygon_Layer->Scale, L, L, 1
	Polygon_Layer->Translate, image_rect[0], image_rect[1], 0

	PolyLin_Layer->reset
	PolyLin_Layer->Translate, -origin_rect[0], -origin_rect[1], 0
	PolyLin_Layer->Scale, L, L, 1
	PolyLin_Layer->Translate, image_rect[0], image_rect[1], 0

	;绘制图像
    oWindow->Draw, oView

;	min_x = -origin_rect[0] * L + image_rect[0]
;	min_y = -origin_rect[1] * L + image_rect[1]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;   scalex =r.scr_xsize/(max_x-min_x)
;   scaley =r.scr_ysize/(max_y-min_y)
;   scale = scalex < scaley
;  Polygon_Layer->Reset
;  Polygon_Layer->Translate,-min_x,-min_y,0       ;理解这两句的意思:平移/比例
;  Polygon_Layer->Scale,scale,scale,1
;
;  PolyLin_Layer->Reset
;  PolyLin_Layer->Translate,-min_x,-min_y,0
;  PolyLin_Layer->Scale,scale,scale,1
;
;  oWindow->Draw,oView

  Map_paraeter= {origin_rect   :  origin_rect ,$
  				 image_rect    :  image_rect  ,$
  				 scale         :  scale, $
                 oWindow           :  oWindow ,$
                 oView             :  oView ,$
                 Polygon_Layer     :  Polygon_Layer ,$
                 PolyLin_Layer     :  PolyLin_Layer}

  RETURN,Map_paraeter

END

;***********是否显示矢量图**************************************
PRO DC_DisplayShapeEV,EVENT

   Widget_Control,Event.Top,GET_UVALUE=state
   (*state).DisplayYesOrNo = EVENT.SELECT
   IF EVENT.SELECT EQ 0 THEN BEGIN
     Widget_Control,EVENT.TOP,SCR_XSIZE=433,SCR_YSIZE=390,XOFFSET=320 ,YOFFSET=200
     RETURN
   ENDIF ELSE BEGIN

;;********杨绍锷修改，20070824**********************************************
;	IF (*state).ShapeFileName NE 'data_vector\'+(*state).ProID+'county.shp' THEN BEGIN
;		Info = DIALOG_MESSAGE('基础数据目录中没有相应省的县矢量数据!',/INFO)
;		RETURN
;	ENDIF
	;以上为原代码=================================

	IF (*state).ShapeFileName NE 'data_vector\county.shp' THEN BEGIN
		Info = DIALOG_MESSAGE('基础数据目录中没有相应省的县矢量数据!',title='警告')
		RETURN
	ENDIF


;************************************************************************************

   ENDELSE

    Widget_Control,/HOURGLASS
    Widget_Control,(*state).Yield_TABLE,SET_VALUE=Yield,SET_TABLE_SELECT=[-1,-1] $
                  ,SET_TABLE_VIEW=[0,0]
    Widget_Control,EVENT.TOP,SCR_XSIZE=875,SCR_YSIZE=390,XOFFSET=100 ,YOFFSET=200
    Widget_Control,(*state).Yield_TABLE,GET_VALUE=Yield
    DistrictCode=Yield[0,*]                            ;第一列值为行政区域代码

   Draw_shape = (*state).Draw_shape
   ShapeFileName = (*state).ShapeFileName

   Map_para=DC_DrawShapeMape(ShapeFileName,DistrictCode,Draw_shape)

   Widget_Control,Draw_shape,SET_UVALUE=Map_para       ;将相关参数作为DRAW的用户值.

END

;**************Draw中的事件***********************************************
PRO DC_DrawWidget_EV,EVENT

     CATCH, Error_status               ;截取错误.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['运算中止,原因如下:',[!ERROR_STATE.MSG]],TITLE='错误',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;如果不加上这句,则会继续执行下面的语句,仍会出现错误.
     ENDIF

   Widget_Control,/HOURGLASS

   Widget_Control,Event.Top,GET_UVALUE=state
   Widget_Control,(*state).Draw_shape,GET_UVALUE=Map_para
   Widget_Control,(*state).Yield_TABLE,GET_VALUE=Yield
   DistrictCode=Yield[0,*]                            ;第一列值为行政区域代码
   IF ARRAY_EQUAL(DistrictCode,'') THEN RETURN
;   DistrictCode=['230000','420000','220000']          ;测试用%%%%%%%%%%%%%%%%%%%%%%%%%%%

;   startx  = Map_para.Coordinate_para[0]
;   starty  = Map_para.Coordinate_para[1]
;   scale   = Map_para.Coordinate_para[2]
   origin_rect = Map_para.origin_rect
   image_rect = Map_para.image_rect
   L = Map_para.scale
   oWindow = Map_para.oWindow
   oView   = Map_para.oView
   Polygon_Layer = Map_para.Polygon_Layer
;   PolyLin_Layer = Map_para.PolyLin_Layer

    IF EVENT.TYPE EQ 1 THEN BEGIN
;        point_x = double(event.x)/scale + startx     ;如何获得指针处的坐标值.
;        point_y = double(event.y)/scale + starty
		point_x = (double(event.x)-image_rect[0])*L + origin_rect[0]
		point_y = (double(event.y)-image_rect[1])*L + origin_rect[1]
        SelectEDCode = ''       ;初始化
       myshape=OBJ_NEW('IDLffShape',(*state).ShapeFileName)
       myshape->GetProperty, N_ENTITIES=num_ent

         AllPoly = Polygon_Layer->Get(/ALL,COUNT=Num_poly)            ;看副类帮助
           FOR j=0,Num_poly-1 DO BEGIN                    ;事实上,AllPoly的中各对象对应于myshape中的各实体,当然数量也相等num_ent=Num_poly.
              AllPoly[j]->GetProperty,Color=YNyellow
              SelectArrt = myshape -> GetAttributes(j)
              IF ARRAY_EQUAL(YNyellow,[255,255,0]) THEN BEGIN
                 IF WHERE(DistrictCode EQ SelectArrt.(0)) NE [-1] THEN BEGIN
                    AllPoly[j]->SetProperty,COLOR=[0,255,255]
                  ENDIF ELSE BEGIN
                    AllPoly[j]->SetProperty,COLOR=[255,255,255]
                  ENDELSE
              ENDIF
           ENDFOR                                  ;这一循环的目的是取消被选中的黄色,还原成最初状态.

          FOR i=0,num_ent-1 DO BEGIN
               attr = myshape -> GetAttributes(i)
               ent  = myshape -> GetEntity(i)
               x = (*ent.vertices)[0,*]
               y = (*ent.vertices)[1,*]
               myshape -> DestroyEntity, ent
               temp_anAOI = OBJ_NEW( 'IDLanROI',x,y)
               r = temp_anAOI->ContainsPoints(point_x,point_y)
              IF r EQ 1 THEN BEGIN
;                 info = DIALOG_MESSAGE(attr.(4),/INFORMATION)
                 AllPoly[i]->SetProperty,COLOR=[255,255,0]
                 SelectEDCode = attr.(0)
                 OBJ_DESTROY, temp_anAOI
                 BREAK
              ENDIF
               OBJ_DESTROY, temp_anAOI
           ENDFOR

       OBJ_DESTROY, myshape
       oWindow->Draw,oView

       SelectEDIndex = WHERE(DistrictCode EQ SelectEDCode,Num_selct)
       IF Num_selct NE 0 THEN BEGIN                          ;这两层循环可能不好理解.是如何表达表格的行不连续选择.
           SelectGrid = INTARR(2,Num_selct*5)                ;实际这可以用数据串通技巧轻易解决,
           FOR i=0,Num_selct-1 DO BEGIN
               FOR j=i,i+4 DO BEGIN
                  SelectGrid[0,4*i+j] =j-i
                  SelectGrid[1,4*i+j] =SelectEDIndex[i]
               ENDFOR
           ENDFOR
           Widget_Control,(*state).Yield_TABLE,SET_TABLE_SELECT=SelectGrid
;;           Widget_Control,(*state).Yield_TABLE,SET_TABLE_VIEW =SelectGrid[*,0]   ;将被选择的自动滚到表视图面
       ENDIF ELSE BEGIN
           Widget_Control,(*state).Yield_TABLE,SET_TABLE_SELECT=[-1,-1]
       ENDELSE

    ENDIF

END
;***************************由表选择到图显示的事件(产量查图)********************
PRO DC_TableToMapEV,EVENT

   IF (EVENT.TYPE EQ 7) THEN RETURN
   IF (EVENT.TYPE EQ 4) AND (EVENT.SEL_LEFT NE -1) THEN BEGIN     ;也可以用IF (TAG_NAMES(EVENT, /STRUCTURE_NAME) EQ 'WIDGET_TABLE_CELL_SEL') THEN BEGIN
      Widget_Control,Event.Top,GET_UVALUE=state                   ;注意每次选择新单元格时,生成两次"选择单元格事件",所以加"(EVENT.SEL_LEFT NE -1)",看帮助
      Widget_Control,(*state).Yield_TABLE,GET_VALUE=Yield
      Widget_Control,/HOURGLASS

      IF Yield[0,0] EQ '' THEN RETURN
	 NumColumn = (SIZE(Yield,/DIMENSIONS))[0]
;;	 NumColumn = Datasize[0]

      DistrictCode = Yield[0,*]

      SelectEDGrid  = Widget_Info((*state).Yield_TABLE,/TABLE_SELECT)
      SelectEDGrid_row = SelectEDGrid[1,*]                                                ;取第二列行值.
      SelectEDRowIndex = SelectEDGrid_row[UNIQ(SelectEDGrid_row,SORT(SelectEDGrid_row))]  ;得到唯一的行值,且按从小到大排序.
        Num_selct = N_ELEMENTS(SelectEDRowIndex)
        SelectGrid = INTARR(2,Num_selct*NumColumn)   ;此处 5 指表列数
        FOR i=0,Num_selct-1 DO BEGIN
            FOR j=i,i+NumColumn-1 DO BEGIN
               SelectGrid[0,(NumColumn-1)*i+j] =j-i
               SelectGrid[1,(NumColumn-1)*i+j] =SelectEDRowIndex[i]
            ENDFOR
        ENDFOR

       Widget_Control,(*state).Yield_TABLE,SET_TABLE_SELECT=SelectGrid ;$;,SET_TABLE_VIEW =SelectGrid[*,0]
;					 ,BACKGROUND_COLOR=[0,255,0]
       IF (*state).DisplayYesOrNo EQ 1 THEN BEGIN
          Widget_Control,(*state).Draw_shape,GET_UVALUE=Map_para

          SelectEDCode = DistrictCode[SelectEDRowIndex]        ;得到被选择行的区域代码
;          startx  = Map_para.Coordinate_para[0]
;          starty  = Map_para.Coordinate_para[1]
;          scale   = Map_para.Coordinate_para[2]
          oWindow = Map_para.oWindow
          oView   = Map_para.oView
          Polygon_Layer = Map_para.Polygon_Layer

          myshape=OBJ_NEW('IDLffShape',(*state).ShapeFileName)
          AllPoly = Polygon_Layer->Get(/ALL,COUNT=Num_poly)     ;看副类帮助"IDLcontainer"
           FOR j=0,Num_poly-1 DO BEGIN                          ;AllPoly的中各对象对应于myshape中的各实体
              AllPoly[j]->GetProperty,Color=YNyellow
              SelectArrt = myshape -> GetAttributes(j)
              IF ARRAY_EQUAL(YNyellow,[255,255,0]) THEN BEGIN
                 IF WHERE(DistrictCode EQ SelectArrt.(0)) NE [-1] THEN BEGIN
                    AllPoly[j]->SetProperty,COLOR=[0,255,255];将列表中返回的区县显示成表色
                  ENDIF ELSE BEGIN
                    AllPoly[j]->SetProperty,COLOR=[255,255,255]
                  ENDELSE
              ENDIF
           ENDFOR                                  ;这一循环的目的是取消被选中的黄色,还原成最初状态.

          FOR j=0,Num_selct-1 DO BEGIN
             FOR i=0,Num_poly-1 DO BEGIN
                attr = myshape -> GetAttributes(i)
               IF SelectEDCode[j] EQ attr.(0) THEN BEGIN
                 AllPoly[i]->SetProperty,COLOR=[255,255,0]   ;设成黄色
                 BREAK
               ENDIF
             ENDFOR
          ENDFOR

          OBJ_DESTROY, myshape
          oWindow->Draw,oView
       ENDIF

   ENDIF

END
;**************保存数据******************************************
PRO DC_SaveYieldEV,EVENT

   Widget_Control,Event.Top,GET_UVALUE=state
     Widget_Control,(*state).Yield_TABLE,GET_VALUE=Yield,GET_UVALUE=TableHead

   IF Yield[0,0] EQ '' THEN BEGIN
      Prompt=DIALOG_MESSAGE('您还没有提取计算数据,请先提取!',TITLE='提示',/INFORMATION)
      RETURN
   ENDIF

   SaveData=[[TableHead],[Yield]]

    IF WHERE(SaveData EQ '') NE [-1] THEN BEGIN    ;说明有空格.注意用-1与[-1]的区别.
        SaveData[WHERE(SaveData EQ '')]='---'
    ENDIF

   Filename=DIALOG_PICKFILE(TITLE='另存为：',DEFAULT_EXTENSION='txt',FILTER=['*.txt']  $
          ,/OVERWRITE_PROMPT,/WRITE,PATH=DC_PathSetting(), DIALOG_PARENT=Event.id)

   IF Filename EQ '' THEN RETURN

   OPENW,LUN,Filename,/GET_LUN,WIDTH=MAX(STRLEN(SaveData))*5
   PRINTF,LUN,SaveData
   FREE_LUN,LUN

   INFO = DIALOG_MESSAGE('保存完成',/INFOR)

END
;**************提取最终估算产量数据************************************
PRO DC_TakeYeildEV,EVENT

    Widget_Control,Event.top,GET_UVALUE=state
;;    Widget_Control,(*state).Yield_TABLE,SET_VALUE=Yield;,SET_TABLE_SELECT=[-1,-1]
    WIDGET_CONTROL, /HOURGLASS

    Cropid= (*state).CropID
    CalcYear = (*state).CalcYear
    CropName=STRCOMPRESS((*state).CropNameList[WHERE((*state).CropIDlist EQ Cropid)],/REMOVE_ALL)
	ProName = (*state).PronameList[WHERE((*state).ProIDList EQ (*state).ProID)]

    ColumnName=['省/县码','省/县名','模拟单产','作物名称','年份']

    Sqlstr="select County_Code,Yield from COUNTY_ESTIMATED_YIELD where Crop_ID='"+Cropid $
    		+"' and year="+CalcYear+' and Model_type_id=0 and LEFT(County_Code,2)='+ (*state).ProID
    Sqlstr='select County_Code,name,Yield from COUNTY_CODE a,('+Sqlstr+') b where a.code=b.county_code'+ $
            ' order by County_code'

    EstimationYield = DC_GetdataFromDB_Str(3,Sqlstr,N_RECORDS = RowsNum)
    IF RowsNum EQ 0 THEN BEGIN
       Prompt=DIALOG_MESSAGE('数据库中没有'+ProName+CalcYear+'年'+CropName+'各县估算产量,请先模拟计算!',TITLE='提示')
       RETURN
    ENDIF
	EstimationYield = [EstimationYield,STRARR(1,RowsNum)+CropName[0],STRARR(1,RowsNum)+CalcYear]

	;========杨绍锷修改，20070903============================================
;    SQLstr="select province_Code,Yield from Province_ESTIMATED_YIELD where Crop_ID='"+Cropid $
;    		+"' and year="+CalcYear+' and Model_type_id=0 and LEFT(province_Code,2)='+ (*state).ProID
 ;(省的单产数据没有进行融合，所以没有Model_type_id=0的数据)已修改，20070908
	SQLstr="select province_Code,Yield/15.0 from Province_ESTIMATED_YIELD where Crop_ID='"+Cropid $
    		+"' and year="+CalcYear+' and Model_type_id=0 and LEFT(province_Code,2)='+ (*state).ProID
	;=========================================================================

    ProYield = DC_GetdataFromDB_Str(2,SQLstr,N_RECORDS = Num)
    IF Num EQ 0 THEN BEGIN
       Prompt=DIALOG_MESSAGE('数据库中没有'+ProName+CalcYear+'年'+CropName+'"省级"估算单产,请进行产量融合分析!',TITLE='提示',/INFO)
    	ProYield[0,0] = (*state).ProID+'0000'
    ENDIF
	ProYield=[ProYield[0,0],ProName,ProYield[1,0],CropName[0],CalcYear]
	EstimationYield = [[ProYield],[EstimationYield]]

    Widget_Control,(*state).Yield_TABLE,TABLE_XSIZE=5,TABLE_YSIZE=RowsNum+1,ALIGNMENT=2  $
                  ,SET_VALUE=EstimationYield  ,SET_UVALUE= ColumnName $  ;这里的用户值用于传递表头
                  ,ROW_LABELS=STRTRIM(INDGEN(RowsNum+1)+1,2),COLUMN_WIDTHS=66 $
                  ,COLUMN_LABELS=ColumnName
	WIDGET_CONTROL,(*state).Yield_TABLE,USE_TABLE_SELECT=[INDGEN(1,5),INTARR(1,5)] $
					,FOREGROUND_COLOR  = [0,0,255]


    Widget_Control,(*state).wholeYield_LABEL,SET_VALUE='模拟的'+ProName[0]+CalcYear+'年'+CropName[0]+'各县单产'

    IF (*state).DisplayYesOrNo EQ 1 THEN BEGIN

       Widget_Control,(*state).Yield_TABLE,GET_VALUE=Yield
       DistrictCode=Yield[0,*]                            ;第一列值为行政区域代码

       Draw_shape = (*state).Draw_shape
;       ShapeFileName='data_vector\county.shp'
       ShapeFileName = (*state).ShapeFileName

       Map_para=DC_DrawShapeMape(ShapeFileName,DistrictCode,Draw_shape)

       Widget_Control,Draw_shape,SET_UVALUE=Map_para       ;将相关参数作为DRAW的用户值.
    ENDIF

END
;*******************************************************************
PRO DC_SynthesisOutput_event,Event
   wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

     CATCH, Error_status               ;截取错误.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['运算中止,原因如下:',[!ERROR_STATE.MSG]],TITLE='错误',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;如果不加上这句,则会继续执行下面的语句,仍会出现错误.
     ENDIF

	wWidget =  Event.top
	WIDGET_CONTROL,Event.top,GET_UVALUE=state
	WIDGET_CONTROL,/HOURGLASS

  CASE wTarget OF
    Widget_Info(wWidget, FIND_BY_UNAME='CropDroplist'): BEGIN
         DC_ClearUp,Event.top
        (*state).CropID=(*state).CropIDList[Event.index]

    END

    Widget_Info(wWidget, FIND_BY_UNAME='ProDroplist'): BEGIN
         DC_ClearUp,Event.top
         (*state).ProID=(*state).ProIDList[Event.index]

    END

    Widget_Info(wWidget, FIND_BY_UNAME='YearDroplist'): BEGIN
       DC_ClearUp,Event.top
      (*state).CalcYear=(*state).ARRAY_YEAR[Event.index]

    END

    Widget_Info(wWidget, FIND_BY_UNAME='Help_bu'):begin
			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '结果显示输出', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('找不到帮助文档',title='警告')
			endelse
		end
;    ONLINE_HELP,BOOK='HELP\HELP.chm', /FULL_PATH,'结果显示输出'
    Widget_Info(wWidget, FIND_BY_UNAME='Quit_bu'):begin
    	common_log,'关闭结果显示输出'
    	widget_control,Event.top,/DESTROY
    end
    ELSE:
  ENDCASE

END
;***********结果显示输出模块"界面************************
PRO DC_SynthesisOutput,GROUP_LEADER=groupleader

	common_log,'启动结果显示输出'
   IF ( XREGISTERED('DC_SynthesisOutput') NE 0 ) THEN RETURN  ;如果该窗口已弹出,则不重复弹出.

   TLB = Widget_Base(UNAME='TLB'   $
      ,XOFFSET=320 ,YOFFSET=200,SCR_XSIZE=434,SCR_YSIZE=389   $
      ,TITLE='结果显示输出' ,SPACE=2 ,XPAD=1 ,YPAD=1 ,COLUMN=2 $
      ,/BASE_ALIGN_LEFT,TLB_FRAME_ATTR=1,GROUP_LEADER=groupleader) ;XOFFSET=320
   ;------------左边BASE------------------------------------;
   SynthesisOutputBase = Widget_Base(TLB,UNAME='SynthesisOutputBase'   $
      ,SPACE=2 ,XPAD=0 ,YPAD=0 ,COLUMN=1,/BASE_ALIGN_LEFT)
   ;------------------条件BASE即四个DROPlist所在的BASE-----------------
	  ConditionB = Widget_Base(SynthesisOutputBase,UNAME='ConditionB' ,FRAME=1 $
	      ,SCR_XSIZE=420,SCR_YSIZE=32,SPACE=10,XPAD=1,YPAD=1 ,ROW=1,/BASE_ALIGN_TOP)

	D_WIDTH=100
;----------------------------------------------------------------
		Province = ['北京市','天津市','河北','山西','内蒙古','辽宁','吉林','黑龙江','上海市','江苏' $
					,'浙江','安微','福建','江西','山东','河南','湖北','湖南','广东','广西','海南' $
					,'重庆市','四川','贵州','云南','西藏','陕西','甘肃','青海','宁夏','新疆']
		ProIDList = ['11','12','13','14','15','21','22','23','31','32','33','34','35','36','37',$
					'41','42','43','44','45','46','50','51','52','53','54','61','62','63','64','65']

		Crop = ['春小麦','冬小麦','早  稻','中  稻','晚  稻','春玉米','夏玉米','大  豆']
	    CropIDList = ['11','12','21','22','23','31','32','41']				;Crop与CropIDList应对应
	    ARRAY_YEAR = STRTRIM(INDGEN(36)+1980,2)         ;这样随年份变化,系统下列表中的年份也会变化.
		YearNum = N_ELEMENTS(ARRAY_YEAR)
;		ProDroplist  = Widget_Droplist(ConditionB,UNAME='ProDroplist',TITLE='省名:',SCR_XSIZE=D_WIDTH)   ;用/frame与frame=1是相同的
		CropDroplist = Widget_Droplist(ConditionB,UNAME='CropDroplist',TITLE='作物:',SCR_XSIZE=D_WIDTH)
		YearDroplist = Widget_Droplist(ConditionB,UNAME='YearDroplist',TITLE='计算年份:',SCR_XSIZE=D_WIDTH+15)

		Take_bu = Widget_Button(ConditionB,SCR_XSIZE=60 ,SCR_YSIZE=22 ,/ALIGN_CENTER  $
		  		,VALUE='提取',EVENT_PRO='DC_TakeYeildEV')

;----------------界面中间的表格部分---------------------------------------
  YieldTable_BASE = Widget_Base(SynthesisOutputBase,SCR_XSIZE=420 , $
      UNAME='YieldTable_BASE' ,FRAME=1,SPACE=1 ,XPAD=0,YPAD=1 $
      ,SCR_YSIZE=275,/COLUMN,/BASE_ALIGN_LEFT)

	  wholeYield_LABEL = Widget_Label(YieldTable_BASE,  $
	      UNAME='wholeYield_LABEL' ,XOFFSET=156 ,YOFFSET=3 ,SCR_XSIZE=228  $
	      ,SCR_YSIZE=18 ,/ALIGN_CENTER ,VALUE='模拟的产量')

;====杨绍锷添加，20070905=========================================================

Unit_LABEL = Widget_Label(YieldTable_BASE,  $
      UNAME='Unit_LABEL' ,XOFFSET=356 ,YOFFSET=0 ,SCR_XSIZE=158  $
      ,SCR_YSIZE=12 ,/ALIGN_CENTER ,VALUE='单位:公斤/亩')
;==============================================================


	  Yield_TABLE = Widget_Table(YieldTable_BASE, UNAME='Yield_TABLE',/ALL_EVENTS $
	      ,XOFFSET=3 ,YOFFSET=24 ,SCR_XSIZE=418 ,SCR_YSIZE=235 ,XSIZE=7 $
	      ,YSIZE=19,/FRAME, COLUMN_WIDTHS=66,/DISJOINT_SELECTION,EVENT_PRO='DC_TableToMapEV')
;---------------------------界面中的按钮部分-----------------------
  button_BASE = Widget_Base(SynthesisOutputBase, UNAME='button_BASE'  $
      ,FRAME=1 ,SCR_XSIZE=420,SCR_YSIZE=35  $
      ,SPACE=40 ,XPAD=5,YPAD=1 ,ROW=1)
 ;-------------------------是否显示矢量图-------------
	  Draw_BASE= Widget_Base(button_BASE, UNAME='Draw_BASE'   $
	     ,SCR_XSIZE=85 ,SCR_YSIZE=20,XPAD=0 ,YPAD=0 ,ROW=1,/NONEXCLUSIVE $
	     ,/BASE_ALIGN_TOP)
		  DisplayDraw = Widget_Button(Draw_BASE, UNAME='DisplayDraw'  $
		      ,SCR_XSIZE=103 ,SCR_YSIZE=20 ,VALUE='显示空间图' $
		      ,EVENT_PRO='DC_DisplayShapeEV')
  ;-----------------------------------------------------------------
	  Width_s = 60
	  Save_bu = Widget_Button(button_BASE, UNAME='Save_bu' ,XOFFSET=175  $
	      ,YOFFSET=4 ,SCR_XSIZE=Width_s ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,TOOLTIP='将提取的数据保存到本地磁盘' ,VALUE='保存' $
	      ,EVENT_PRO='DC_SaveYieldEV')

	  Help_bu = Widget_Button(button_BASE, UNAME='Help_bu' ,XOFFSET=330  $
	      ,YOFFSET=4 ,SCR_XSIZE=Width_s ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,VALUE='帮助')

	  Quit_bu = Widget_Button(button_BASE, UNAME='Quit_bu' ,XOFFSET=330  $
	      ,YOFFSET=4 ,SCR_XSIZE=Width_s ,SCR_YSIZE=24 ,/ALIGN_CENTER  $
	      ,VALUE='关闭',TOOLTIP='退出预测综合模块')

;---------------------右边的DRAW_base部分---------------------------
  DrawBase = Widget_Base(TLB,UNAME='SynthesisOutputBase'   $
      ,SCR_XSIZE=437,SCR_YSIZE=350,SPACE=3 ,XPAD=0 ,YPAD=0 ,COLUMN=1 $
      ,/BASE_ALIGN_LEFT,/FRAME)
	  Draw_shape = Widget_Draw(DrawBase, UNAME='Draw_shape'  $
	      ,SCR_XSIZE=435 ,SCR_YSIZE=350,GRAPHICS_LEVEL=2,RETAIN=2,/BUTTON_EVENTS $
	      ,/FRAME,EVENT_PRO='DC_DrawWidget_EV')
;------------------------------------------------------------
	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
	PRO_COMMON_DC
	COMMON DC_BLOCK,NewCropID
	ProCode = STRMID(PROVINCE_CODE,0,2)      ;这三量设在这里是为接受系统设置所预设的参数,将来系统集成时应作修改
	CropID = NewCropID
	CalcYear = strmid(systime(),3,4,/REVERSE_OFFSET)

	temp = WHERE(ARRAY_YEAR EQ CalcYear,Count)
	IF Count NE 0 THEN BEGIN
		WIDGET_CONTROL,YearDroplist,SET_VALUE =ARRAY_YEAR,SET_DROPLIST_SELECT=WHERE(ARRAY_YEAR EQ CalcYear)
	ENDIF ELSE BEGIN
		WIDGET_CONTROL,YearDroplist,SET_VALUE =ARRAY_YEAR,SET_DROPLIST_SELECT=YearNum-1
		CalcYear = ARRAY_YEAR[YearNum-1]
	ENDELSE

;	WIDGET_CONTROL,ProDroplist,SET_VALUE =Province,SET_DROPLIST_SELECT=WHERE(ProIDList EQ ProCode)
	WIDGET_CONTROL,CropDroplist,SET_VALUE =Crop,SET_DROPLIST_SELECT=WHERE(CropIDList EQ CropID)


  Widget_Control,Yield_TABLE,SET_TABLE_SELECT=[-1,-1]
  Widget_Control, /REALIZE, TLB
  WIDGET_CONTROL,Quit_bu,/INPUT_FOCUS

  state={   ProNameList			:	Province		,$				;省名列表
		    ProIDList			:	ProIDList		,$				;省ID列表
		    ProID				:	ProCode			,$				;被选省ID
		    CropIDList			:	CropIDList		,$				;作物ID列表
		    CropNameList		:	Crop			,$      		;作物名列表
		    CropID				:	CropID			,$
		    CalcYear			:	CalcYear		,$
			ARRAY_YEAR	 		:	ARRAY_YEAR		,$
         	wholeYield_LABEL  : wholeYield_LABEL 	,$
         	Yield_TABLE       : Yield_TABLE		 	,$
         	Draw_shape        : Draw_shape		 	,$
         	DisplayYesOrNo    : 0 				 	,$       ;是否显示了查询图:1--显示;0--没有显示.
         	ShapeFileName     : 'data_vector\county.shp'}

 Widget_Control,TLB,SET_UVALUE=PTR_NEW(state,/NO_COPY)

  XManager, 'DC_SynthesisOutput',TLB,CLEANUP='DC_CleanAllHeap', /NO_BLOCK

END
