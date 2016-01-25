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
 if countynum ne 5000 then begin    ;countynum ��������ʾ�Ƿ��Ѿ�������ͳ�ƣ��������ˣ�������⣬Ϊ5000��û��������ͳ��
        warning=DIALOG_MESSAGE('��ȷ��Ҫ�����',title='����', /QUESTION)
     if warning eq 'Yes' then begin
	    progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='���ݴ�����,���Ժ�!',TITLE='�������ݿ�')
	    progressTimer->START
	    WIDGET_CONTROL,/hourglass
     	WIDGET_CONTROL,Event.top,get_UVALUE = pstate
    	WWIDGET =  Event.top
    	countynum=(*pstate).countynum
      ;  widget_control, (*pstate).drop_year, get_value=drop_year
        drop_year = widget_info((*pstate).drop_year, /COMBOBOX_GETTEXT)
       ; drop_year = widget_info((*pstate).drop_year, /droplist_select)
        Drop_type = widget_info((*pstate).Drop_type, /COMBOBOX_GETTEXT)
        if drop_type eq '����' then begin
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
    	   progressTimer->UPDATE, 1* 100.0  ;���½�����
     	   obj_destroy,progresstimer
      	   warning=DIALOG_MESSAGE('��ͳ�ƺ��ٽ�����⣡',title='��ʾ')
           ptr_free,coverarea
           ptr_free,nameofcounty
           ptr_free,perplanted
    	   return
        endif

		         for i=0,countynum-1 do begin	;�������޸ģ�20070911���д�������91ԭΪcountynum
		      		CATCH, Error_status               ;��ȡ����.
		    		 IF Error_status NE 0 THEN BEGIN
		    		 	print, !ERROR_STATE.MSG
		       			 CATCH, /CANCEL
		        		 Goto,nexts
		             ENDIF
		             	;���´��������������-------------------------------------
		             	;�ڲ���������ǰɾ��ԭ�е�����
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
	                     progressTimer->UPDATE, (float(i)/countynum* 100.0)  ;���½�����
            	endfor
		             progressTimer->UPDATE, 1* 100.0  ;���½�����
		             obj_destroy,progresstimer
		             warning=DIALOG_MESSAGE('�����⣡',title='��ʾ')
		             log, '��ֲ����-�ռ�ͳ��', 1
		             ptr_free,coverarea
		             ptr_free,nameofcounty
		             ptr_free,perplanted
	endif else begin
	    return
	endelse
endif else begin
        warning=DIALOG_MESSAGE('�㻹û�н������ͳ�ƣ�',title='����')
        log, '��ֲ����-�ռ�ͳ��', -1
        return
endelse

end
;----------------------------------------------------------
pro MJ_TJ_openrecode,event
     COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     Recodeimg = dialog_pickfile(dialog_parent=event.top, title='���ر���Ӱ���ļ�(tif��ʽ)', filter=['*.tif'],path=FILE_PATH,/MUST_EXIST)

     Result = QUERY_TIFF ( Recodeimg,GEOTIFF=variable)

     IF (Recodeimg NE '') THEN BEGIN
     	  IF RESULT EQ 0 THEN BEGIN
	        infors=DIALOG_MESSAGE('�����ѡ��tif��ʽ������!',title='��ʾ' )
	        (*pstate).countynum=5000	;5000��ʾû��������ͳ�ƣ���������ӣ�20070911
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
	 PRINT,'���ͳ��,����'

	if file_test('HELP\HELP.chm') then begin
		ONLINE_HELP, '��ֲ��������', BOOK='HELP\HELP.chm', /FULL_PATH
	endif else begin
		info_help=dialog_message('�Ҳ��������ĵ�',title='����')
	endelse

;	 ONLINE_HELP,'ͳ��ģ��',  BOOK='HELP\HELP.chm'
end
;----------------------------------------------------------

;--------------------------------------------------------------------------------------------
pro MJ_TJ_Close,Event
	common_log,'�ر����ͳ��'
	 CLOSE,/all
     WIDGET_CONTROL, EVENT.TOP, /destroy
     RETURN
END
;-------------------------------------------������������
pro MJ_Tongji,event
     COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD
     progressTimer = Obj_New("ShowProgress", tlb,MESSAGE='���ݴ�����,���Ժ�!',TITLE='���ͳ��')
     progressTimer->START
	;----------��һ�����ж�Ӱ����ʸ���Ƿ��н��������ޣ����˳�����
     WIDGET_CONTROL,/hourglass
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     WWIDGET =  Event.top

	 progressTimer->UPDATE, (0.1 * 100.0)  ;���½�����
 	 ;��ȡrecode������Ϣ,��Ҫ������С��Ϣ�����Ͻ�������Ϣ�ȵȡ�ͶӰ��Ϣ��
   	 WIDGET_CONTROL,(*pstate).WID_recode,get_value=Recodeimg
    ; Recodeimg='E:\Provincial_Crop_System20070106\data_test\recode.tif'
	 Resultrecode = QUERY_TIFF(Recodeimg,Info,GEOTIFF=recodehead)

	;===��������ӣ�20070911========================================
	if Resultrecode ne 1 then begin
		temp_y=dialog_message('���ܴ�recode�ļ�',title='��ʾ')
		OBJ_DESTROY,progressTimer;���ٽ�����
		(*pstate).countynum=5000	;5000��ʾû��������ͳ�ƣ���������ӣ�20070911
		return
	endif

	tname_y=tag_names(recodehead)
	index_y_0=where(tname_y eq 'MODELTIEPOINTTAG',count0)
	index_y_1=where(tname_y eq 'MODELPIXELSCALETAG',count1)
	index_y_2=where(tname_y eq 'PROJCENTERLONGGEOKEY',count2)
	if (count0 lt 1) or (count1 lt 1) or (count2 lt 1) then begin
		temp_y = DIALOG_MESSAGE('������ز��ļ����ر����ļ���ͶӰ��һ�£�',title='��ʾ')
		OBJ_DESTROY,progressTimer;���ٽ�����
		(*pstate).countynum=5000	;5000��ʾû��������ͳ�ƣ���������ӣ�20070911
		return
	endif
	;===============================================================

     recodesize=Info.dimensions
	 gridy_max=recodehead.MODELTIEPOINTTAG[4]            ;դ�񶥵���yֵ��Сֵ
	 gridy_min=recodehead.MODELTIEPOINTTAG[4] - recodesize[1]*recodehead.MODELPIXELSCALETAG[1]
     gridx_min=recodehead.MODELTIEPOINTTAG[3]            ;դ�񶥵���xֵ��Сֵ
	 gridx_max=recodehead.MODELTIEPOINTTAG[3] + recodesize[0]*recodehead.MODELPIXELSCALETAG[1]

     ;��ȡ�ؽ�����
    ; countyshape='E:\Provincial_Crop_System20070106\data_vector\county105.shp'
    countyshape='data_vector\county105.shp'
     shpobj = obj_new('IDLffShape',countyshape)
     shpobj->GetProperty, N_ENTITIES=num_ent, ENTITY_TYPE=ShapeType

     ;-----------���±�֤�ر����ͶӰҪ��ʸ������һ��(Albers105)���ܽ���ͳ��

     if (float(recodehead.PROJCENTERLONGGEOKEY[0]) ne float(105)) then begin

        OK = DIALOG_MESSAGE('������ز��ļ����ر����ļ���ͶӰ��һ�£�',title='��ʾ')
        OBJ_DESTROY,progressTimer;���ٽ�����	;��������ӣ�20070911
        (*pstate).countynum=5000	;5000��ʾû��������ͳ�ƣ���������ӣ�20070911
        return
     endif
 	roiObj = objarr(num_ent)
	boundlist = fltarr(num_ent, 2)
    flag=0                                              ;flag=0 Ĭ��դ��Ӱ����ʸ������û�н���

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
		shpy_min=min((*ent.vertices)[1,*])               ;ʸ��������yֵ��Сֵ
		shpy_max=max((*ent.vertices)[1,*])               ;ʸ��������yֵ���ֵ
		shpx_min=min((*ent.vertices)[0,*])               ;ʸ��������xֵ��Сֵ
		shpx_max=max((*ent.vertices)[0,*])       		 ;ʸ��������xֵ���ֵ

;----------------------------------------�ʼ�ж϶���
        Result0 = roiObj[i]->ContainsPoints(gridx_min,gridy_min)             ;�ж����϶����Ƿ���ʸ����
        Result1 = roiObj[i]->ContainsPoints(gridx_max,gridy_min)             ;�ж����϶����Ƿ���ʸ����
        Result2= roiObj[i]->ContainsPoints(gridx_min,gridy_max)
        Result3= roiObj[i]->ContainsPoints(gridx_max,gridy_max)               ;�ж����¶����Ƿ���ʸ����
;----------------------------------------
        roiObj[i]->GetProperty ,data=data5
        a=where((data5[1,*] lt gridy_max),counta)
        aa=where((data5[1,*] gt gridy_max),countaa)
        if counta eq 0 then begin
           flag=flag+1
        endif
       ;---------------����и�; ;        if ((shpx_min le gridx_min) and (gridx_min le shpy_max)) then begin
        roiObj[i]->GetProperty ,data=data0
        roiObj[i]->GetProperty ,data=data00
        b=where((data00[0,*] gt gridx_min),countb)
        bb=where((data00[0,*] lt gridx_min),countbb)

        if flag ne i+1 then begin
	        if countb eq 0 then begin
	           flag=flag+1
	        endif
	    endif
        ;---------------�±��и�; ;
        roiObj[i]->GetProperty ,data=data1
        roiObj[i]->GetProperty ,data=data11
        c=where((data11[1,*] gt gridy_min),countc)
        cc=where((data11[1,*] lt gridy_min),countcc)
        if flag ne i+1 then begin
	        if countc eq 0 then begin
	           flag=flag+1
	        endif
	    endif
       ;---------------�ұ��и�; ;
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

  if flag eq num_ent then begin            ;flag����û����դ���ཻ��ʸ���ĸ���
	 progressTimer->UPDATE, (1 * 100.0)  ;���½�����
	 OBJ_DESTROY,progressTimer;���ٽ�����
     temp=dialog_message('���ز�������ʸ������û�н�����',title='��Ϣ')
     return

  endif


;-------------------------�ڶ�������ʸ����դ���н���������Կ�ʼͳ����
;��Ȼȷ���ر�����������ز����ݺ��ؽ������н���,����Ҫ���������������ر�����������ز����ݵĽ���&
;�ر����������ؽ����ݽ���
;դ��1��դ��2�ཻ����������ô����ģ����ߵ��ϱ�y��Сֵ���±�y���ֵ�����x���ֵ���ұ�x��Сֵ

;�ر�����������ز����ݵĽ���
;�ȶ����ز�����
;     countyimg='E:\Provincial_Crop_System20070106\data_grid\county105.tif'
;     landimg='E:\Provincial_Crop_System20070106\data_grid\landuse.tif'
     countyimg='data_grid\county105.tif'
     landimg='data_grid\landuse.tif'
	 Resultland = QUERY_TIFF(landimg,info,GEOTIFF=landhead)

	;===��������ӣ�20070911========================================
	if Resultland ne 1 then begin
		temp_y=dialog_message('���ܴ�recode�ļ�',title='��ʾ')
		OBJ_DESTROY,progressTimer;���ٽ�����
		(*pstate).countynum=5000	;5000��ʾû��������ͳ�ƣ���������ӣ�20070911
		return
	endif

	tname_y=tag_names(landhead)
	index_y_0=where(tname_y eq 'MODELTIEPOINTTAG',count0)
	index_y_1=where(tname_y eq 'MODELPIXELSCALETAG',count1)
	index_y_2=where(tname_y eq 'PROJCENTERLONGGEOKEY',count2)
	if (count0 lt 1) or (count1 lt 1) or (count2 lt 1) then begin
		temp_y = DIALOG_MESSAGE('������ز��ļ����ر����ļ���ͶӰ��һ�£�',title='��ʾ')
		OBJ_DESTROY,progressTimer;���ٽ�����
		(*pstate).countynum=5000	;5000��ʾû��������ͳ�ƣ���������ӣ�20070911
		return
	endif
	;===============================================================

	 landsize=info.dimensions

	 gridy_max=recodehead.MODELTIEPOINTTAG[4]            ;դ�񶥵���yֵ��Сֵ
	 gridy_min=recodehead.MODELTIEPOINTTAG[4] - recodesize[1]*recodehead.MODELPIXELSCALETAG[1]
     gridx_min=recodehead.MODELTIEPOINTTAG[3]            ;դ�񶥵���xֵ��Сֵ
	 gridx_max=recodehead.MODELTIEPOINTTAG[3] + recodesize[0]*recodehead.MODELPIXELSCALETAG[1]


     ;�����ϱ�y����Сֵ,�����������ϽǶ���yֵ
     minyu_landreco=min([landhead.MODELTIEPOINTTAG[4],gridy_max])
     ;�±�y�����ֵ
     maxyu_landreco=max([landhead.MODELTIEPOINTTAG[4]-landsize[1]*landhead.MODELPIXELSCALETAG[1],gridy_min])
     ;���x�����ֵ,�����������ϽǶ���xֵ
     maxxu_landreco=max([landhead.MODELTIEPOINTTAG[3],gridx_min])
     ;�ұ�x����Сֵ
     minxu_landreco=min([landhead.MODELTIEPOINTTAG[3]+landsize[0]*landhead.MODELPIXELSCALETAG[1],gridx_max])


     ;��˽������ϽǵĶ��������ԭʼlandͼ������ϽǶ���Ϊ:
     XDX_landreco=(maxxu_landreco-landhead.MODELTIEPOINTTAG[3])/landhead.MODELPIXELSCALETAG[1]
     XDY_landreco=(landhead.MODELTIEPOINTTAG[4]-minyu_landreco)/landhead.MODELPIXELSCALETAG[1]

     ;����������
     landreco_Column=(minxu_landreco-maxxu_landreco)/landhead.MODELPIXELSCALETAG[1]
     ;����������
     landreco_line=(minyu_landreco-maxyu_landreco)/landhead.MODELPIXELSCALETAG[1]
     ;��˴�land�����ж�ȡ���ߵ��ཻ��������:
     Intersect_land=read_tiff(landimg, geotiff=landgeotiff,SUB_RECT=[XDX_landreco,XDY_landreco,landreco_Column,landreco_line])

	 landgeotiff.MODELPIXELSCALETAG[0] = landgeotiff.MODELPIXELSCALETAG[0]
	 landgeotiff.MODELPIXELSCALETAG[1] = landgeotiff.MODELPIXELSCALETAG[1]
	 landgeotiff.MODELTIEPOINTTAG[3] =maxxu_landreco                    ;-1000����Ϊ����ͼ���Щ
	 landgeotiff.MODELTIEPOINTTAG[4] =minyu_landreco                  ;-1000����Ϊ����ͼ���Щ

 ;    write_tiff, 'd:\temp\s0.tif', Intersect_land, geotiff=landgeotiff ;s0��������һ����������˸��ز��ʸ��������ķ����һ���ֱ���Ϊ1000�׵�դ��ͼ

	 progressTimer->UPDATE, (0.2 * 100.0)  ;���½�����


     ;��˽������ϽǵĶ��������ԭʼ�ر���ͼ������ϽǶ���Ϊ:
     XDX_reco=(maxxu_landreco-gridx_min)/recodehead.MODELPIXELSCALETAG[1]
     XDY_reco=(gridy_max-minyu_landreco)/recodehead.MODELPIXELSCALETAG[1]
     ;����������
     reco_Column=(minxu_landreco-maxxu_landreco)/recodehead.MODELPIXELSCALETAG[1]
     ;����������
     reco_line=(minyu_landreco-maxyu_landreco)/recodehead.MODELPIXELSCALETAG[1]


     ;���ر�����������ȡ���ߵ��ཻ����Ϊ:
     Intersect_reland= read_tiff(Recodeimg, geotiff=recodehead,SUB_RECT=[XDX_reco,XDY_reco,reco_Column,reco_line])

	 recodehead.MODELPIXELSCALETAG[0] = recodehead.MODELPIXELSCALETAG[0]
	 recodehead.MODELPIXELSCALETAG[1] = recodehead.MODELPIXELSCALETAG[1]
	 recodehead.MODELTIEPOINTTAG[3] =maxxu_landreco                    ;-1000����Ϊ����ͼ���Щ
	 recodehead.MODELTIEPOINTTAG[4] =minyu_landreco                  ;-1000����Ϊ����ͼ���Щ

   ;  write_tiff, 'd:\temp\s1.tif', Intersect_reland, geotiff=recodehead ;s0��������һ����������˸��ز��ʸ��������ķ����һ���ֱ���Ϊ1000�׵�դ��ͼ
	 progressTimer->UPDATE, (0.3 * 100.0)  ;���½�����
     ;�����ؽ����ݺ͸��ز�������ϵͳ����ͬ���Ĵ�С��λ�ã�����ؽ��������ر������ݵĽ������ִ��ؽ���������ȡΪ:
     Intersect_county=read_tiff(countyimg, geotiff=countygeotiff,SUB_RECT=[XDX_landreco,XDY_landreco,landreco_Column,landreco_line])
     ;���ر�����������ȡ���ߵ��ཻ����ΪIntersect_reland


  ;   write_tiff, 'd:\temp\s2.tif', Intersect_county, geotiff=landgeotiff ;s0��������һ����������˸��ز��ʸ��������ķ����һ���ֱ���Ϊ1000�׵�դ��ͼ
     ;��congrid����ʹ������Ӱ���С���ֱ���ȫһ��
     ;����ر������ݷֱ��ʴ���60meters������Ϊ100�ף����򽫴��ؽ�͸��ز�ͼ������ȡ���غϲ���congrid��100�׵�

	 progressTimer->UPDATE, (0.4 * 100.0)  ;���½�����

     ;����ر������ݷֱ���С��60�ף�����30��),���ر�������congridΪ60�׵�
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
	  progressTimer->UPDATE, (0.55 * 100.0)  ;���½�����
      Intersect_land=0B
      Intersect_county=0B
      Intersect_reland=0B
     ;-------------���ؽ������ж������غϲ������ݿɷ�����ÿ���ؼ������

		;-----ԭ���룭������������������������
;      uniqvaluenum=N_elements(UNIQ(congridcounty,sort(congridcounty)))   ;uniqvaluenum:ָ���ز�Ӱ�������˼�����
;      index1=UNIQ(congridcounty,sort(congridcounty))                     ;index1:�Ѹ��ز�ͼ���ʸ��դ�񻯺�
;      unioncounty=where(congridcounty[index1] ne 0,countynum)
		;-----����Ϊԭ���룭������������������������
		;----���´���Ϊ�������޸ģ�20070511------------------
		temp=histogram(congridcounty)
		index1=where(temp ne 0,count)
		uniqvaluenum=count
		unioncounty=where(index1 ne 0,countynum)
;		;----���ϴ���Ϊ�������޸ģ�20070511------------------



     ;-------------���Ӹ��ز��ж�ȡ�ĸ��ز����ر������ݵ��غϲ�����������ؽ������ж������غϲ���������ˣ;
     ;�ɷ�����ÿ�������ж��ٸ������

     dd=congridland
     dryindex=where(dd eq 2,numdry)                ;�ҳ��к��صĵط��������к��صĵط���ֵ��Ϊ0;
                                                   ;������˺�����ҳ���ֲ������ĵط�(��ֵ�ĵط�)
     if numdry gt 0 then begin                     ;���к��صĵط���ֵ��Ϊ0;
     	dd[dryindex]=1
    	mm=congridcounty*dd                       ;mm�е�ֵ����0(����˵����congridcounty[index1[i]])�ĵط�����

        progressTimer->UPDATE, (0.75 * 100.0)  ;���½�����                                             ;Ϊ���صĵط�(����ˮ��ͺ���)
     endif else begin
        progressTimer->UPDATE, (0.75 * 100.0)  ;���½�����
    	mm=congridcounty*dd
     endelse

	 progressTimer->UPDATE, (0.8 * 100.0)  ;���½�����
     ;-------------�����ؽ��ж������غϲ������ݳ��Դ��ر��������ж������غϲ������ݿɷ�����ÿ�������ж�����������������
    nn=congridrecode

    ;---�ò��ִ������׶������޸ģ�20070511-----------------------
    cloundindex=where(nn eq 2,numclound);���׶�ע��           ;�ҳ����Ƶĵط����������Ƶĵط���ֵ��Ϊ0;
;    ;----���׶���ӣ�20070511������������
;	    b = histogram(nn)                                               ;������˺�����ҳ���ֲ������ĵط�(��ֵ�ĵط�)
;	    nncount = n_elements(nn)
;	    numclound = b[2]
;	    for i=0L,nncount-1 do begin
;			if nn[i] eq 2 then nn[i] = 0
;	    endfor
	 ;------------------------------
    if numclound gt 0 then begin                   ;�����Ƶĵط���ֵ��Ϊ0;
     	nn[cloundindex]=0    ;���׶�ע��
     	hasclound=1
        ll=nn*congridcounty                        ;ll�е�ֵ����0(����˵����getdata[index1[i]])�ĵط�����
                                                   ;��ֲ������ĵط�
    endif else begin
        ll=nn*congridcounty
    endelse
;-------���ϴ������׶������޸�----------------------------------
    print,'d'
     ;-------------֮����Լ���ÿ���ص���ֲ����

     perplanted=fltarr(num_ent)                ;��ֲ����
     coverarea=fltarr(num_ent)                ;��ⷶΧ
     nameofcounty=strarr(num_ent)

     his=histogram(mm)
   ;  covercounty=where(congridcounty eq index1[i],covercountynum)
    hiscovercounty=histogram(congridcounty)
    hisll=histogram(ll)
;    ss=where(ll eq index1[i],countplanted)
    for i=unioncounty[0],countynum do begin	;ԭ����
       ;rr=where(mm eq congridcounty[index1[i]],countplowland) ;ԭ����
       ;rr=where(mm eq index1[i],countplowland)
        if index1[i] ge n_elements(his) then $
        countplowland=0 else countplowland=his[index1[i]]
;    ---�������޸ģ�20070511------------------
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
          ;----ԭ����-----
       ;  covercounty=where(congridcounty eq congridcounty[index1[i]],covercountynum) ;ԭ����
        if index1[i] ge n_elements(hiscovercounty) then $
        covercountynum=0 else covercountynum=hiscovercounty[index1[i]]

;       covercounty=where(congridcounty eq index1[i],covercountynum)
;       ---�������޸ģ�20070511------------------
;			covercountynum=0
;		for k=0L,n_elements(congridcounty)-1 do begin
;			if congridcounty[k] eq congridcounty[index1[i]] then covercountynum=covercountynum+1
;		endfor
	;---------------------------------------


         coverarea[i-1]=congridpixel*covercountynum
         if countplowland gt 0 then begin

			;----ԭ����-----
	       ;  ss=where(ll eq congridcounty[index1[i]],countplanted);ԭ����
	     ;    ss=where(ll eq index1[i],countplanted)
         if index1[i] ge n_elements(hisll) then $
         countplanted=0 else countplanted=hisll[index1[i]]
			;---�������޸ģ�20070511------------------
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
;;         ;=====�����ɲ�����ӣ�20070911===========================
;         	if strcmp(strtrim(nameofcounty[i-1],2), string('360101')) then aa____
;;         ;=======================================================
	     print,perplanted[i-1]
    endfor
print,nameofcounty
	progressTimer->UPDATE, (0.95 * 100.0)  ;���½�����
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
	    progressTimer->UPDATE, (1 * 100.0)  ;���½�����

        OBJ_DESTROY,progressTimer;���ٽ�����
        OBJ_DESTROY,shpobj
        OK = DIALOG_MESSAGE('���ͳ�ƣ�',title='��ʾ')
        log, '��ֲ����-�ռ�ͳ��', 0
;-------------��ʾ��widget_table����

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
      ,TITLE='��ֲ��������' ,SPACE=3 ,XPAD=3 ,YPAD=3, TLB_FRAME_ATTR =1)


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
      ,/ALIGN_LEFT ,VALUE='���')



;====�������޸ģ�20070906======================================

;drop_year = Widget_combobox(WID_BASE_11,  $		;ԭ����
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
      ,/ALIGN_LEFT ,VALUE='����/����')

  season=['����','����']
  Drop_type = Widget_combobox(WID_BASE_11,  $
      UNAME='Drop_type' ,XOFFSET=194 ,YOFFSET=7 ,SCR_XSIZE=64  $
      ,SCR_YSIZE=18,value=season)


  label_landuse = Widget_Label(WID_BASE_11, UNAME='label_landuse'  $
      ,XOFFSET=11 ,YOFFSET=42 ,SCR_XSIZE=100 ,SCR_YSIZE=20  $
      ,/ALIGN_LEFT ,VALUE='����������������')

  landtype=['դ��']
  drop_landuse = Widget_Droplist(WID_BASE_11,  $
      UNAME='drop_landuse' ,XOFFSET=193 ,YOFFSET=40 ,SCR_XSIZE=65  $
      ,SCR_YSIZE=18,value=landtype)


  WID_BASE_12 = Widget_Base(WID_BASE_10, UNAME='WID_BASE_12' ,FRAME=1  $
      ,XOFFSET=0 ,YOFFSET=72 ,SCR_XSIZE=267 ,SCR_YSIZE=83  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_LABEL_12 = Widget_Label(WID_BASE_12, UNAME='WID_LABEL_12'  $
      ,XOFFSET=8 ,YOFFSET=14 ,SCR_XSIZE=103 ,SCR_YSIZE=19  $
      ,/ALIGN_LEFT ,VALUE='ѡ���ر�������')


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
      ,/ALIGN_CENTER ,VALUE='����',event_pro='MJ_Tongji')


  WID_BUTTON_10 = Widget_Button(WID_BASE_13, UNAME='WID_BUTTON_10'  $
      ,XOFFSET=77 ,YOFFSET=7 ,SCR_XSIZE=46 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='���',event_pro='MJ_TJ_todb')


  WID_BUTTON_11 = Widget_Button(WID_BASE_13, UNAME='WID_BUTTON_11'  $
      ,XOFFSET=140 ,YOFFSET=7 ,SCR_XSIZE=46 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='����',event_pro='MJ_TJhelp_event')


  WID_BUTTON_12 = Widget_Button(WID_BASE_13, UNAME='WID_BUTTON_12'  $
      ,XOFFSET=205 ,YOFFSET=7 ,SCR_XSIZE=46 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='�ر�',event_pro='MJ_TJ_Close')

;
;  WID_TABLE_1 = Widget_Table(WID_BASE_0, UNAME='WID_TABLE_1' ,FRAME=1  $
;      ,XOFFSET=296 ,YOFFSET=7 ,SCR_XSIZE=268 ,SCR_YSIZE=243 ,XSIZE=5  $
;      ,YSIZE=6)
   state = { $
        Drop_type	: Drop_type,$
        drop_year   : drop_year,$
        p_nameofcounty:ptr_new(),$;����һ��ָ�룬ָ��ּ�����
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
  common_log,'�������ͳ��'
  MJ_sta, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end
