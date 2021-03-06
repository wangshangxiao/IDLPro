;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;
; Generated on:	02/01/2007 14:10.02
;
pro WID_BASE_0_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)


  wWidget =  Event.top

  case wTarget of

    else:
  endcase

end
pro MJ_TJ_todb,event

        COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
     	WIDGET_CONTROL,/hourglass
     	WIDGET_CONTROL,Event.top,get_UVALUE = pstate
    	WWIDGET =  Event.top
    	countynum=(*pstate).countynum
 if countynum ne 5000 then begin    ;countynum 是用来表示是否已经完成面积统计，如果完成了，才能入库，为5000就没有完成面积统计
        warning=DIALOG_MESSAGE('你确认要入库吗？',title='警告', /QUESTION)
     if warning eq 'Yes' then begin
	    progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='数据处理中,请稍候!',TITLE='导入数据库')
	    progressTimer->START
	    WIDGET_CONTROL,/hourglass
     	WIDGET_CONTROL,Event.top,get_UVALUE = pstate
    	WWIDGET =  Event.top
    	countynum=(*pstate).countynum
      ;  widget_control, (*pstate).drop_year, get_value=drop_year
        drop_year = widget_info((*pstate).drop_year, /COMBOBOX_GETTEXT)
       ; drop_year = widget_info((*pstate).drop_year, /droplist_select)
        Drop_type = widget_info((*pstate).Drop_type, /COMBOBOX_GETTEXT)
        if drop_type eq '夏粮' then begin
           e=0
        endif else begin
           e=1
        endelse
        b=fix(drop_year)
        nameofcounty=(*pstate).p_nameofcounty
        coverarea=(*pstate).p_coverarea
        perplanted=(*pstate).p_perplanted
        print,nameofcounty
        if Not ptr_valid(nameofcounty) then begin
    	   progressTimer->UPDATE, 1* 100.0  ;更新进度条
     	   obj_destroy,progresstimer
      	   warning=DIALOG_MESSAGE('请统计后再进行入库！',title='提示')
           ptr_free,coverarea
           ptr_free,nameofcounty
           ptr_free,perplanted
    	   return
        endif

		         for i=0,countynum-1 do begin	;杨绍锷修改，20070911，有待改正　91原为countynum
		      		CATCH, Error_status               ;截取错误.
		    		 IF Error_status NE 0 THEN BEGIN
		    		 	print, !ERROR_STATE.MSG
		       			 CATCH, /CANCEL
		        		 Goto,nexts
		             ENDIF
		             	;以下代码有杨绍锷添加-------------------------------------
		             	;在插入新数据前删除原有的数据
		             	sql_del='delete from crop_plant_proportion_ORI where (' $
		             	+'county_code='+"'"+strtrim((*nameofcounty)[i],2)+"'"+' and ' $
		             	+'year='+strtrim(b,2)+' and SU_OR_AU='+strtrim(e,2)+')'

		             	print,'sql_del:',sql_del
						DBobj->ExecuteSQL,sql_del

		             	;---------------------------------------------------------

                     	 sql1='insert into crop_plant_proportion_ORI values ('''+''+strtrim((*nameofcounty)[i],2)+''+''','+strtrim(b,2)+','+strtrim((*perplanted)[i],2)+','+strtrim((*coverarea)[i],2)+','+strtrim(e,2)+''+')'

                         print,sql1
		                 DBobj->ExecuteSQL,SQL1
		                 print,'i=',i
		                 nexts:
	                     progressTimer->UPDATE, (float(i)/countynum* 100.0)  ;更新进度条
            	endfor
		             progressTimer->UPDATE, 1* 100.0  ;更新进度条
		             obj_destroy,progresstimer
		             warning=DIALOG_MESSAGE('完成入库！',title='提示')
		             log, '种植成数-空间统计', 1
		             ptr_free,coverarea
		             ptr_free,nameofcounty
		             ptr_free,perplanted
	endif else begin
	    return
	endelse
endif else begin
        warning=DIALOG_MESSAGE('你还没有进行面积统计！',title='警告')
        log, '种植成数-空间统计', -1
        return
endelse

end
;----------------------------------------------------------
pro MJ_TJ_openrecode,event
     COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     Recodeimg = dialog_pickfile(dialog_parent=event.top, title='打开重编码影像文件(tif格式)', filter=['*.tif'],path=FILE_PATH,/MUST_EXIST)

     Result = QUERY_TIFF ( Recodeimg,GEOTIFF=variable)

     IF (Recodeimg NE '') THEN BEGIN
     	  IF RESULT EQ 0 THEN BEGIN
	        infors=DIALOG_MESSAGE('你必须选择tif格式的数据!',title='提示' )
	        (*pstate).countynum=5000	;5000表示没有完成面积统计，杨绍锷添加，20070911
	        return
     	  ENDIF
     	  WIDGET_CONTROL,Event.top,get_UVALUE = pstate
        WIDGET_CONTROL, (*pstate).WID_recode, set_VALUE=Recodeimg
     ENDIF

     RETURN ; By Default, return the event.
end
;----------------------------------------------------------

pro MJ_Tongji_CleanAllHeap,tlb
    IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
 	ON_ERROR, 2
    WIDGET_CONTROL,tlb,GET_UVALUE=PA
    HEAP_FREE,PA
end
;----------------------------------------------------------
pro MJ_TJhelp_event,event
	 PRINT,'面积统计,帮助'

	if file_test('HELP\HELP.chm') then begin
		ONLINE_HELP, '种植成数计算', BOOK='HELP\HELP.chm', /FULL_PATH
	endif else begin
		info_help=dialog_message('找不到帮助文档',title='警告')
	endelse

;	 ONLINE_HELP,'统计模块',  BOOK='HELP\HELP.chm'
end
;----------------------------------------------------------

;--------------------------------------------------------------------------------------------
pro MJ_TJ_Close,Event
	common_log,'关闭面积统计'
	 CLOSE,/all
     WIDGET_CONTROL, EVENT.TOP, /destroy
     RETURN
END
;-------------------------------------------以下是主程序
pro MJ_Tongji,event
     COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD
     progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='数据处理中,请稍候!',TITLE='面积统计')
     progressTimer->START
	;----------第一步：判断影像与矢量是否有交集，若无，则退出程序
     WIDGET_CONTROL,/hourglass
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     WWIDGET =  Event.top

	 progressTimer->UPDATE, (0.1 * 100.0)  ;更新进度条
 	 ;读取recode数据信息,主要包括大小信息、左上角坐标信息等等、投影信息等
   	 WIDGET_CONTROL,(*pstate).WID_recode,get_value=Recodeimg
    ; Recodeimg='E:\Provincial_Crop_System20070106\data_test\recode.tif'
	 Resultrecode = QUERY_TIFF(Recodeimg,Info,GEOTIFF=recodehead)

	;===杨绍锷添加，20070911========================================
	if Resultrecode ne 1 then begin
		temp_y=dialog_message('不能打开recode文件',title='提示')
		OBJ_DESTROY,progressTimer;销毁进度条
		(*pstate).countynum=5000	;5000表示没有完成面积统计，杨绍锷添加，20070911
		return
	endif

	tname_y=tag_names(recodehead)
	index_y_0=where(tname_y eq 'MODELTIEPOINTTAG',count0)
	index_y_1=where(tname_y eq 'MODELPIXELSCALETAG',count1)
	index_y_2=where(tname_y eq 'PROJCENTERLONGGEOKEY',count2)
	if (count0 lt 1) or (count1 lt 1) or (count2 lt 1) then begin
		temp_y = DIALOG_MESSAGE('输入耕地层文件和重编码文件的投影不一致！',title='提示')
		OBJ_DESTROY,progressTimer;销毁进度条
		(*pstate).countynum=5000	;5000表示没有完成面积统计，杨绍锷添加，20070911
		return
	endif
	;===============================================================

     recodesize=Info.dimensions
	 gridy_max=recodehead.MODELTIEPOINTTAG[4]            ;栅格顶点中y值最小值
	 gridy_min=recodehead.MODELTIEPOINTTAG[4] - recodesize[1]*recodehead.MODELPIXELSCALETAG[1]
     gridx_min=recodehead.MODELTIEPOINTTAG[3]            ;栅格顶点中x值最小值
	 gridx_max=recodehead.MODELTIEPOINTTAG[3] + recodesize[0]*recodehead.MODELPIXELSCALETAG[1]

     ;读取县界数据
    ; countyshape='E:\Provincial_Crop_System20070106\data_vector\county105.shp'
    countyshape='data_vector\county105.shp'
     shpobj = obj_new('IDLffShape',countyshape)
     shpobj->GetProperty, N_ENTITIES=num_ent, ENTITY_TYPE=ShapeType

     ;-----------以下保证重编码的投影要和矢量数据一致(Albers105)才能进行统计

     if (float(recodehead.PROJCENTERLONGGEOKEY[0]) ne float(105)) then begin

        OK = DIALOG_MESSAGE('输入耕地层文件和重编码文件的投影不一致！',title='提示')
        OBJ_DESTROY,progressTimer;销毁进度条	;杨绍锷添加，20070911
        (*pstate).countynum=5000	;5000表示没有完成面积统计，杨绍锷添加，20070911
        return
     endif
 	roiObj = objarr(num_ent)
	boundlist = fltarr(num_ent, 2)
    flag=0                                              ;flag=0 默认栅格影像与矢量数据没有交集

	min_x = fltarr(num_ent)
	min_y = fltarr(num_ent)
	max_x = fltarr(num_ent)
	max_y = fltarr(num_ent)

    for i=0, num_ent-1 do begin
		ent = shpobj->GetEntity(i, /ATTRIBUTES)
        plx=(*ent.vertices)[0,*]
        ply=(*ent.vertices)[1,*]
		min_x[i] =min(plx)
		max_x[i]=max(plx)
		min_y[i] = min(ply)
		max_y[i]=max(ply)
		roiObj[i] = obj_new('IDLanROI', (*ent.vertices)[0,*], (*ent.vertices)[1,*])
		shpy_min=min((*ent.vertices)[1,*])               ;矢量顶点中y值最小值
		shpy_max=max((*ent.vertices)[1,*])               ;矢量顶点中y值最大值
		shpx_min=min((*ent.vertices)[0,*])               ;矢量顶点中x值最小值
		shpx_max=max((*ent.vertices)[0,*])       		 ;矢量顶点中x值最大值

;----------------------------------------最开始判断顶点
        Result0 = roiObj[i]->ContainsPoints(gridx_min,gridy_min)             ;判断左上顶点是否在矢量内
        Result1 = roiObj[i]->ContainsPoints(gridx_max,gridy_min)             ;判断右上顶点是否在矢量内
        Result2= roiObj[i]->ContainsPoints(gridx_min,gridy_max)
        Result3= roiObj[i]->ContainsPoints(gridx_max,gridy_max)               ;判断右下顶点是否在矢量内
;----------------------------------------
        roiObj[i]->GetProperty ,data=data5
        a=where((data5[1,*] lt gridy_max),counta)
        aa=where((data5[1,*] gt gridy_max),countaa)
        if counta eq 0 then begin
           flag=flag+1
        endif
       ;---------------左边切割; ;        if ((shpx_min le gridx_min) and (gridx_min le shpy_max)) then begin
        roiObj[i]->GetProperty ,data=data0
        roiObj[i]->GetProperty ,data=data00
        b=where((data00[0,*] gt gridx_min),countb)
        bb=where((data00[0,*] lt gridx_min),countbb)

        if flag ne i+1 then begin
	        if countb eq 0 then begin
	           flag=flag+1
	        endif
	    endif
        ;---------------下边切割; ;
        roiObj[i]->GetProperty ,data=data1
        roiObj[i]->GetProperty ,data=data11
        c=where((data11[1,*] gt gridy_min),countc)
        cc=where((data11[1,*] lt gridy_min),countcc)
        if flag ne i+1 then begin
	        if countc eq 0 then begin
	           flag=flag+1
	        endif
	    endif
       ;---------------右边切割; ;
        roiObj[i]->GetProperty ,data=data2
        roiObj[i]->GetProperty ,data=data22
        d=where((data22[0,*] lt gridx_max),countd)
        dd=where((data22[0,*] gt gridx_max),countdd)
        if flag ne i+1 then begin
	        if countd eq 0 then begin
        	   flag=flag+1
	        endif
	    endif
	    shpobj->DestroyEntity, ent
	    OBJ_DESTROY,roiObj[i]
  endfor

  if flag eq num_ent then begin            ;flag就是没有与栅格相交的矢量的个数
	 progressTimer->UPDATE, (1 * 100.0)  ;更新进度条
	 OBJ_DESTROY,progressTimer;销毁进度条
     temp=dialog_message('耕地层数据与矢量数据没有交集！',title='信息')
     return

  endif


;-------------------------第二步，若矢量与栅格有交集，则可以开始统计了
;既然确定重编码数据与耕地层数据和县界数据有交集,则需要进行下面操作求出重编码数据与耕地层数据的交集&
;重编码数据与县界数据交集
;栅格1与栅格2相交集区域是这么计算的：两者的上边y最小值、下边y最大值、左边x最大值、右边x最小值

;重编码数据与耕地层数据的交集
;先读耕地层数据
;     countyimg='E:\Provincial_Crop_System20070106\data_grid\county105.tif'
;     landimg='E:\Provincial_Crop_System20070106\data_grid\landuse.tif'
     countyimg='data_grid\county105.tif'
     landimg='data_grid\landuse.tif'
	 Resultland = QUERY_TIFF(landimg,info,GEOTIFF=landhead)

	;===杨绍锷添加，20070911========================================
	if Resultland ne 1 then begin
		temp_y=dialog_message('不能打开recode文件',title='提示')
		OBJ_DESTROY,progressTimer;销毁进度条
		(*pstate).countynum=5000	;5000表示没有完成面积统计，杨绍锷添加，20070911
		return
	endif

	tname_y=tag_names(landhead)
	index_y_0=where(tname_y eq 'MODELTIEPOINTTAG',count0)
	index_y_1=where(tname_y eq 'MODELPIXELSCALETAG',count1)
	index_y_2=where(tname_y eq 'PROJCENTERLONGGEOKEY',count2)
	if (count0 lt 1) or (count1 lt 1) or (count2 lt 1) then begin
		temp_y = DIALOG_MESSAGE('输入耕地层文件和重编码文件的投影不一致！',title='提示')
		OBJ_DESTROY,progressTimer;销毁进度条
		(*pstate).countynum=5000	;5000表示没有完成面积统计，杨绍锷添加，20070911
		return
	endif
	;===============================================================

	 landsize=info.dimensions

	 gridy_max=recodehead.MODELTIEPOINTTAG[4]            ;栅格顶点中y值最小值
	 gridy_min=recodehead.MODELTIEPOINTTAG[4] - recodesize[1]*recodehead.MODELPIXELSCALETAG[1]
     gridx_min=recodehead.MODELTIEPOINTTAG[3]            ;栅格顶点中x值最小值
	 gridx_max=recodehead.MODELTIEPOINTTAG[3] + recodesize[0]*recodehead.MODELPIXELSCALETAG[1]


     ;两者上边y的最小值,即交集的左上角顶点y值
     minyu_landreco=min([landhead.MODELTIEPOINTTAG[4],gridy_max])
     ;下边y的最大值
     maxyu_landreco=max([landhead.MODELTIEPOINTTAG[4]-landsize[1]*landhead.MODELPIXELSCALETAG[1],gridy_min])
     ;左边x的最大值,即交集的左上角顶点x值
     maxxu_landreco=max([landhead.MODELTIEPOINTTAG[3],gridx_min])
     ;右边x的最小值
     minxu_landreco=min([landhead.MODELTIEPOINTTAG[3]+landsize[0]*landhead.MODELPIXELSCALETAG[1],gridx_max])


     ;因此交集左上角的顶点相对于原始land图像的左上角顶点为:
     XDX_landreco=(maxxu_landreco-landhead.MODELTIEPOINTTAG[3])/landhead.MODELPIXELSCALETAG[1]
     XDY_landreco=(landhead.MODELTIEPOINTTAG[4]-minyu_landreco)/landhead.MODELPIXELSCALETAG[1]

     ;交集的列数
     landreco_Column=(minxu_landreco-maxxu_landreco)/landhead.MODELPIXELSCALETAG[1]
     ;交集的行数
     landreco_line=(minyu_landreco-maxyu_landreco)/landhead.MODELPIXELSCALETAG[1]
     ;因此从land数据中读取两者的相交部分则是:
     Intersect_land=read_tiff(landimg, geotiff=landgeotiff,SUB_RECT=[XDX_landreco,XDY_landreco,landreco_Column,landreco_line])

	 landgeotiff.MODELPIXELSCALETAG[0] = landgeotiff.MODELPIXELSCALETAG[0]
	 landgeotiff.MODELPIXELSCALETAG[1] = landgeotiff.MODELPIXELSCALETAG[1]
	 landgeotiff.MODELTIEPOINTTAG[3] =maxxu_landreco                    ;-1000：是为了让图像大些
	 landgeotiff.MODELTIEPOINTTAG[4] =minyu_landreco                  ;-1000：是为了让图像大些

 ;    write_tiff, 'd:\temp\s0.tif', Intersect_land, geotiff=landgeotiff ;s0：代表是一幅范畴包括了耕地层和矢量层所跨的范畴的一幅分辨率为1000米的栅格图

	 progressTimer->UPDATE, (0.2 * 100.0)  ;更新进度条


     ;因此交集左上角的顶点相对于原始重编码图像的左上角顶点为:
     XDX_reco=(maxxu_landreco-gridx_min)/recodehead.MODELPIXELSCALETAG[1]
     XDY_reco=(gridy_max-minyu_landreco)/recodehead.MODELPIXELSCALETAG[1]
     ;交集的列数
     reco_Column=(minxu_landreco-maxxu_landreco)/recodehead.MODELPIXELSCALETAG[1]
     ;交集的行数
     reco_line=(minyu_landreco-maxyu_landreco)/recodehead.MODELPIXELSCALETAG[1]


     ;从重编码数据中提取两者的相交部分为:
     Intersect_reland= read_tiff(Recodeimg, geotiff=recodehead,SUB_RECT=[XDX_reco,XDY_reco,reco_Column,reco_line])

	 recodehead.MODELPIXELSCALETAG[0] = recodehead.MODELPIXELSCALETAG[0]
	 recodehead.MODELPIXELSCALETAG[1] = recodehead.MODELPIXELSCALETAG[1]
	 recodehead.MODELTIEPOINTTAG[3] =maxxu_landreco                    ;-1000：是为了让图像大些
	 recodehead.MODELTIEPOINTTAG[4] =minyu_landreco                  ;-1000：是为了让图像大些

   ;  write_tiff, 'd:\temp\s1.tif', Intersect_reland, geotiff=recodehead ;s0：代表是一幅范畴包括了耕地层和矢量层所跨的范畴的一幅分辨率为1000米的栅格图
	 progressTimer->UPDATE, (0.3 * 100.0)  ;更新进度条
     ;由于县界数据和耕地层数据在系统中是同样的大小和位置，因此县界数据与重编码数据的交集部分从县界数据中提取为:
     Intersect_county=read_tiff(countyimg, geotiff=countygeotiff,SUB_RECT=[XDX_landreco,XDY_landreco,landreco_Column,landreco_line])
     ;从重编码数据中提取两者的相交部分为Intersect_reland


  ;   write_tiff, 'd:\temp\s2.tif', Intersect_county, geotiff=landgeotiff ;s0：代表是一幅范畴包括了耕地层和矢量层所跨的范畴的一幅分辨率为1000米的栅格图
     ;用congrid函数使得三幅影像大小、分辨率全一样
     ;如果重编码数据分辨率大于60meters（例如为100米），则将从县界和耕地层图像中提取的重合部分congrid成100米的

	 progressTimer->UPDATE, (0.4 * 100.0)  ;更新进度条

     ;如果重编码数据分辨率小于60米（例如30米),则将重编码数据congrid为60米的
      congridpixel=max([recodehead.MODELPIXELSCALETAG[1],landgeotiff.MODELPIXELSCALETAG[0]])
      If recodehead.MODELPIXELSCALETAG[1] lt landgeotiff.MODELPIXELSCALETAG[0] then begin
         congridrecode=CONGRID(Intersect_reland, landreco_Column, landreco_line)
         congridland=Intersect_land
         congridcounty=Intersect_county
      endif else begin
         congridland=CONGRID(Intersect_land, landreco_Column,landreco_line)
         congridcounty=CONGRID(Intersect_county, landreco_Column,landreco_line)
         congridrecode=Intersect_reland
      endelse
	  progressTimer->UPDATE, (0.55 * 100.0)  ;更新进度条
      Intersect_land=0B
      Intersect_county=0B
      Intersect_reland=0B
     ;-------------从县界数据中读出的重合部分数据可分析出每个县监测的面积

		;-----原代码－－－－－－－－－－－－－
;      uniqvaluenum=N_elements(UNIQ(congridcounty,sort(congridcounty)))   ;uniqvaluenum:指耕地层影像所跨了几个县
;      index1=UNIQ(congridcounty,sort(congridcounty))                     ;index1:把耕地层图像从矢量栅格化后
;      unioncounty=where(congridcounty[index1] ne 0,countynum)
		;-----以上为原代码－－－－－－－－－－－－－
		;----以下代码为杨绍锷修改，20070511------------------
		temp=histogram(congridcounty)
		index1=where(temp ne 0,count)
		uniqvaluenum=count
		unioncounty=where(index1 ne 0,countynum)
;		;----以上代码为杨绍锷修改，20070511------------------



     ;-------------将从耕地层中读取的耕地层与重编码数据的重合部分数据与从县界数据中读出的重合部分数据相乘�;
     ;可分析出每个县内有多少耕地面积

     dd=congridland
     dryindex=where(dd eq 2,numdry)                ;找出有旱地的地方，随后把有旱地的地方的值改为0;
                                                   ;几个相乘后可以找出种植了作物的地方(有值的地方)
     if numdry gt 0 then begin                     ;把有旱地的地方的值改为0;
     	dd[dryindex]=1
    	mm=congridcounty*dd                       ;mm中的值大于0(或者说等于congridcounty[index1[i]])的地方就是

        progressTimer->UPDATE, (0.75 * 100.0)  ;更新进度条                                             ;为耕地的地方(包括水田和旱地)
     endif else begin
        progressTimer->UPDATE, (0.75 * 100.0)  ;更新进度条
    	mm=congridcounty*dd
     endelse

	 progressTimer->UPDATE, (0.8 * 100.0)  ;更新进度条
     ;-------------将从县界中读出的重合部分数据乘以从重编码数据中读出的重合部分数据可分析出每个县内有多少是种了作物的面积
    nn=congridrecode

    ;---该部分代码杨雷东进行修改，20070511-----------------------
    cloundindex=where(nn eq 2,numclound);杨雷东注销           ;找出有云的地方，随后把有云的地方的值改为0;
;    ;----杨雷东添加，20070511－－－－－－
;	    b = histogram(nn)                                               ;几个相乘后可以找出种植了作物的地方(有值的地方)
;	    nncount = n_elements(nn)
;	    numclound = b[2]
;	    for i=0L,nncount-1 do begin
;			if nn[i] eq 2 then nn[i] = 0
;	    endfor
	 ;------------------------------
    if numclound gt 0 then begin                   ;把有云的地方的值改为0;
     	nn[cloundindex]=0    ;杨雷东注销
     	hasclound=1
        ll=nn*congridcounty                        ;ll中的值大于0(或者说等于getdata[index1[i]])的地方就是
                                                   ;种植了作物的地方
    endif else begin
        ll=nn*congridcounty
    endelse
;-------以上代码杨雷东进行修改----------------------------------
    print,'d'
     ;-------------之后可以计算每个县的种植成数

     perplanted=fltarr(num_ent)                ;种植成数
     coverarea=fltarr(num_ent)                ;监测范围
     nameofcounty=strarr(num_ent)

     his=histogram(mm)
   ;  covercounty=where(congridcounty eq index1[i],covercountynum)
    hiscovercounty=histogram(congridcounty)
    hisll=histogram(ll)
;    ss=where(ll eq index1[i],countplanted)
    for i=unioncounty[0],countynum do begin	;原代码
       ;rr=where(mm eq congridcounty[index1[i]],countplowland) ;原代码
       ;rr=where(mm eq index1[i],countplowland)
        if index1[i] ge n_elements(his) then $
        countplowland=0 else countplowland=his[index1[i]]
;    ---杨绍锷修改，20070511------------------
;	for i=unioncounty[1],countynum-1 do begin
;		countplowland=0
;		for k=0L,n_elements(mm)-1 do begin
;			if mm[k] eq congridcounty[index1[i]] then countplowland=countplowland+1
;		endfor
	;---------------------------------------

       ;  ent = shpobj->GetEntity(congridcounty[index1[i]]-1, /ATTRIBUTES)
         ent = shpobj->GetEntity(index1[i]-1, /ATTRIBUTES)
         nameofcounty[i-1]=(*ent.attributes).Attribute_0
         shpobj->DestroyEntity, ent
          ;----原代码-----
       ;  covercounty=where(congridcounty eq congridcounty[index1[i]],covercountynum) ;原代码
        if index1[i] ge n_elements(hiscovercounty) then $
        covercountynum=0 else covercountynum=hiscovercounty[index1[i]]

;       covercounty=where(congridcounty eq index1[i],covercountynum)
;       ---杨绍锷修改，20070511------------------
;			covercountynum=0
;		for k=0L,n_elements(congridcounty)-1 do begin
;			if congridcounty[k] eq congridcounty[index1[i]] then covercountynum=covercountynum+1
;		endfor
	;---------------------------------------


         coverarea[i-1]=congridpixel*covercountynum
         if countplowland gt 0 then begin

			;----原代码-----
	       ;  ss=where(ll eq congridcounty[index1[i]],countplanted);原代码
	     ;    ss=where(ll eq index1[i],countplanted)
         if index1[i] ge n_elements(hisll) then $
         countplanted=0 else countplanted=hisll[index1[i]]
			;---杨绍锷修改，20070511------------------
;			countplanted=0
;			for k=0L,n_elements(ll)-1 do begin
;				if ll[k] eq congridcounty[index1[i]] then countplanted=countplanted+1
;			endfor
			;---------------------------------------

	         perplanted[i-1]=countplanted/float(countplowland)
	         if perplanted[i-1] lt 0 or perplanted[i-1] gt 1 then perplanted[i-1]=abs(1/perplanted[i-1]-0.2)

         endif else begin
         	 perplanted[i-1]=0
         endelse

         print,nameofcounty[i-1]
         print,coverarea[i-1]
;;         ;=====杨绍锷测试添加，20070911===========================
;         	if strcmp(strtrim(nameofcounty[i-1],2), string('360101')) then aa____
;;         ;=======================================================
	     print,perplanted[i-1]
    endfor
print,nameofcounty
	progressTimer->UPDATE, (0.95 * 100.0)  ;更新进度条
         nn=1B
         dd=1B
         mm=1B
         ll=1B
         congridcounty=1B
         congridland=1B
         congridrecode=1B
         print,'d'

         b=2004
         e=1
         PTR_FREE,(*pstate).p_nameofcounty
         PTR_FREE,(*pstate).p_coverarea
         PTR_FREE,(*pstate).p_perplanted
         (*pstate).p_nameofcounty=ptr_new(nameofcounty)
         (*pstate).p_coverarea=ptr_new(coverarea)
         (*pstate).p_perplanted=ptr_new(perplanted)
         (*pstate).countynum=countynum
	    progressTimer->UPDATE, (1 * 100.0)  ;更新进度条

        OBJ_DESTROY,progressTimer;销毁进度条
        OBJ_DESTROY,shpobj
        OK = DIALOG_MESSAGE('完成统计！',title='提示')
        log, '种植成数-空间统计', 0
;-------------显示在widget_table表中

end


pro MJ_sta_event,event
end
;----------------------------------------------------------
pro MJ_sta, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

	IF ( XREGISTERED('MJ_sta') NE 0 ) THEN RETURN

  Resolve_Routine, 'MJ_statistical_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines
 ; Resolve_Routine, 'MJ_statistical_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP

  WID_BASE_0 = Widget_Base( GROUP_LEADER=BASE_TOP, UNAME='WID_BASE_0'  $
      ,XOFFSET=355 ,YOFFSET=200 ,SCR_XSIZE=286 ,SCR_YSIZE=244 ,TAB_MODE=2  $
      ,TITLE='种植成数计算' ,SPACE=3 ,XPAD=3 ,YPAD=3, TLB_FRAME_ATTR =1)


  WID_BASE_10 = Widget_Base(WID_BASE_0, UNAME='WID_BASE_10' ,FRAME=0  $
      ,XOFFSET=6 ,YOFFSET=7 ,SCR_XSIZE=269 ,SCR_YSIZE=201  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BASE_11 = Widget_Base(WID_BASE_10, UNAME='WID_BASE_11' ,FRAME=1  $
      ,XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=267 ,SCR_YSIZE=67 ,TITLE='IDL'  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3)

  year=['1980','1981','1982','1983','1984','1985','1986',$
  '1987','1988','1989','1990','1991','1992','1993','1994',$
  '1995','1996','1997','1998','1999','2000','2001','2002',$
  '2003','2004','2005','2006','2007','2008','2009','2010',$
  '2011','2012','2013','2014','2015']

  LabelMJ_year = Widget_Label(WID_BASE_11, UNAME='LabelMJ_year'  $
      ,XOFFSET=11 ,YOFFSET=10 ,SCR_XSIZE=29 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='年份')



;====杨绍锷修改，20070906======================================

;drop_year = Widget_combobox(WID_BASE_11,  $		;原代码
;      UNAME='drop_year' ,XOFFSET=48 ,YOFFSET=7 ,SCR_XSIZE=63  $
;      ,SCR_YSIZE=20,value=year)

	temp=(bin_date())[0]-1980

  drop_year = Widget_combobox(WID_BASE_11,  $
      UNAME='drop_year' ,XOFFSET=48 ,YOFFSET=7 ,SCR_XSIZE=63  $
      ,SCR_YSIZE=20,value=year)

widget_control,drop_year, SET_COMBOBOX_SELECT=temp
 ;=================================================================


  labeltype = Widget_Label(WID_BASE_11, UNAME='labeltype'  $
      ,XOFFSET=127 ,YOFFSET=10 ,SCR_XSIZE=58 ,SCR_YSIZE=19  $
      ,/ALIGN_LEFT ,VALUE='夏粮/秋粮')

  season=['夏粮','秋粮']
  Drop_type = Widget_combobox(WID_BASE_11,  $
      UNAME='Drop_type' ,XOFFSET=194 ,YOFFSET=7 ,SCR_XSIZE=64  $
      ,SCR_YSIZE=18,value=season)


  label_landuse = Widget_Label(WID_BASE_11, UNAME='label_landuse'  $
      ,XOFFSET=11 ,YOFFSET=42 ,SCR_XSIZE=100 ,SCR_YSIZE=20  $
      ,/ALIGN_LEFT ,VALUE='土地利用数据类型')

  landtype=['栅格']
  drop_landuse = Widget_Droplist(WID_BASE_11,  $
      UNAME='drop_landuse' ,XOFFSET=193 ,YOFFSET=40 ,SCR_XSIZE=65  $
      ,SCR_YSIZE=18,value=landtype)


  WID_BASE_12 = Widget_Base(WID_BASE_10, UNAME='WID_BASE_12' ,FRAME=1  $
      ,XOFFSET=0 ,YOFFSET=72 ,SCR_XSIZE=267 ,SCR_YSIZE=83  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_LABEL_12 = Widget_Label(WID_BASE_12, UNAME='WID_LABEL_12'  $
      ,XOFFSET=8 ,YOFFSET=14 ,SCR_XSIZE=103 ,SCR_YSIZE=19  $
      ,/ALIGN_LEFT ,VALUE='选择重编码数据')


  WID_BUTTON_8 = Widget_Button(WID_BASE_12, UNAME='WID_BUTTON_8'  $
      ,XOFFSET=234 ,YOFFSET=44   $
      ,/ALIGN_CENTER ,value='\open.bmp',/bitmap,event_pro='MJ_TJ_openrecode')


  WID_recode = Widget_Text(WID_BASE_12, UNAME='WID_recode' ,XOFFSET=7  $
      ,YOFFSET=44 ,SCR_XSIZE=224 ,SCR_YSIZE=23 ,XSIZE=20 ,YSIZE=1,value='data_grid\recode.tif')


  WID_BASE_13 = Widget_Base(WID_BASE_10, UNAME='WID_BASE_13' ,FRAME=1  $
      ,XOFFSET=0 ,YOFFSET=160 ,SCR_XSIZE=267 ,SCR_YSIZE=39  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BUTTON_9 = Widget_Button(WID_BASE_13, UNAME='WID_BUTTON_9'  $
      ,XOFFSET=14 ,YOFFSET=7 ,SCR_XSIZE=46 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='计算',event_pro='MJ_Tongji')


  WID_BUTTON_10 = Widget_Button(WID_BASE_13, UNAME='WID_BUTTON_10'  $
      ,XOFFSET=77 ,YOFFSET=7 ,SCR_XSIZE=46 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='入库',event_pro='MJ_TJ_todb')


  WID_BUTTON_11 = Widget_Button(WID_BASE_13, UNAME='WID_BUTTON_11'  $
      ,XOFFSET=140 ,YOFFSET=7 ,SCR_XSIZE=46 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='帮助',event_pro='MJ_TJhelp_event')


  WID_BUTTON_12 = Widget_Button(WID_BASE_13, UNAME='WID_BUTTON_12'  $
      ,XOFFSET=205 ,YOFFSET=7 ,SCR_XSIZE=46 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='关闭',event_pro='MJ_TJ_Close')

;
;  WID_TABLE_1 = Widget_Table(WID_BASE_0, UNAME='WID_TABLE_1' ,FRAME=1  $
;      ,XOFFSET=296 ,YOFFSET=7 ,SCR_XSIZE=268 ,SCR_YSIZE=243 ,XSIZE=5  $
;      ,YSIZE=6)
   state = { $
        Drop_type	: Drop_type,$
        drop_year   : drop_year,$
        p_nameofcounty:ptr_new(),$;创建一个指针，指向分级参数
        p_perplanted:ptr_new(),$
        countynum:5000,$
        WID_recode:WID_recode,$
        p_coverarea:ptr_new() $
	}
	  PA = PTR_NEW(STATE, /NO_COPY)
	  WIDGET_CONTROL, WID_BASE_0, SET_UVALUE=PA

  Widget_Control, /REALIZE, WID_BASE_0
  WIDGET_CONTROL,WID_BUTTON_12,/INPUT_FOCUS

XManager, 'MJ_sta', WID_BASE_0,CLEANUP='MJ_Tongji_CleanAllHeap', /NO_BLOCK

end
;
; Empty stub procedure used for autoloading.
;
pro MJ_statistical, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  common_log,'启动面积统计'
  MJ_sta, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end
