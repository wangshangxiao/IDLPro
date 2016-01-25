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
 if countynum ne 5000 then begin    ;countynum  «”√¿¥±Ì æ «∑Ò“—æ≠ÕÍ≥…√Êª˝Õ≥º∆£¨»Áπ˚ÕÍ≥…¡À£¨≤≈ƒ‹»Îø‚£¨Œ™5000æÕ√ª”–ÕÍ≥…√Êª˝Õ≥º∆
        warning=DIALOG_MESSAGE('ƒ„»∑»œ“™»Îø‚¬£ø',title='æØ∏Ê', /QUESTION)
     if warning eq 'Yes' then begin
	    progressTimer = Obj_New("ShowProgress", tlb,MESSAGE=' ˝æ›¥¶¿Ì÷–,«Î…‘∫Ú!',TITLE='µº»Î ˝æ›ø‚')
	    progressTimer->START
	    WIDGET_CONTROL,/hourglass
     	WIDGET_CONTROL,Event.top,get_UVALUE = pstate
    	WWIDGET =  Event.top
    	countynum=(*pstate).countynum
      ;  widget_control, (*pstate).drop_year, get_value=drop_year
        drop_year = widget_info((*pstate).drop_year, /COMBOBOX_GETTEXT)
       ; drop_year = widget_info((*pstate).drop_year, /droplist_select)
        Drop_type = widget_info((*pstate).Drop_type, /COMBOBOX_GETTEXT)
        if drop_type eq 'œƒ¡∏' then begin
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
    	   progressTimer->UPDATE, 1* 100.0  ;∏¸–¬Ω¯∂»Ãı
     	   obj_destroy,progresstimer
      	   warning=DIALOG_MESSAGE('«ÎÕ≥º∆∫Û‘ŸΩ¯––»Îø‚£°',title='Ã· æ')
           ptr_free,coverarea
           ptr_free,nameofcounty
           ptr_free,perplanted
    	   return
        endif

		         for i=0,countynum-1 do begin	;—Ó…‹Ô…–ﬁ∏ƒ£¨20070911£¨”–¥˝∏ƒ’˝°°91‘≠Œ™countynum
		      		CATCH, Error_status               ;Ωÿ»°¥ÌŒÛ.
		    		 IF Error_status NE 0 THEN BEGIN
		    		 	print, !ERROR_STATE.MSG
		       			 CATCH, /CANCEL
		        		 Goto,nexts
		             ENDIF
		             	;“‘œ¬¥˙¬Î”–—Ó…‹Ô…ÃÌº”-------------------------------------
		             	;‘⁄≤Â»Î–¬ ˝æ›«∞…æ≥˝‘≠”–µƒ ˝æ›
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
	                     progressTimer->UPDATE, (float(i)/countynum* 100.0)  ;∏¸–¬Ω¯∂»Ãı
            	endfor
		             progressTimer->UPDATE, 1* 100.0  ;∏¸–¬Ω¯∂»Ãı
		             obj_destroy,progresstimer
		             warning=DIALOG_MESSAGE('ÕÍ≥…»Îø‚£°',title='Ã· æ')
		             log, '÷÷÷≤≥… ˝-ø’º‰Õ≥º∆', 1
		             ptr_free,coverarea
		             ptr_free,nameofcounty
		             ptr_free,perplanted
	endif else begin
	    return
	endelse
endif else begin
        warning=DIALOG_MESSAGE('ƒ„ªπ√ª”–Ω¯––√Êª˝Õ≥º∆£°',title='æØ∏Ê')
        log, '÷÷÷≤≥… ˝-ø’º‰Õ≥º∆', -1
        return
endelse

end
;----------------------------------------------------------
pro MJ_TJ_openrecode,event
     COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     Recodeimg = dialog_pickfile(dialog_parent=event.top, title='¥Úø™÷ÿ±‡¬Î”∞œÒŒƒº˛(tif∏Ò Ω)', filter=['*.tif'],path=FILE_PATH,/MUST_EXIST)

     Result = QUERY_TIFF ( Recodeimg,GEOTIFF=variable)

     IF (Recodeimg NE '') THEN BEGIN
     	  IF RESULT EQ 0 THEN BEGIN
	        infors=DIALOG_MESSAGE('ƒ„±ÿ–Î—°‘Òtif∏Ò Ωµƒ ˝æ›!',title='Ã· æ' )
	        (*pstate).countynum=5000	;5000±Ì æ√ª”–ÕÍ≥…√Êª˝Õ≥º∆£¨—Ó…‹Ô…ÃÌº”£¨20070911
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
	 PRINT,'√Êª˝Õ≥º∆,∞Ô÷˙'

	if file_test('HELP\HELP.chm') then begin
		ONLINE_HELP, '÷÷÷≤≥… ˝º∆À„', BOOK='HELP\HELP.chm', /FULL_PATH
	endif else begin
		info_help=dialog_message('’“≤ªµΩ∞Ô÷˙Œƒµµ',title='æØ∏Ê')
	endelse

;	 ONLINE_HELP,'Õ≥º∆ƒ£øÈ',  BOOK='HELP\HELP.chm'
end
;----------------------------------------------------------

;--------------------------------------------------------------------------------------------
pro MJ_TJ_Close,Event
	common_log,'πÿ±’√Êª˝Õ≥º∆'
	 CLOSE,/all
     WIDGET_CONTROL, EVENT.TOP, /destroy
     RETURN
END
;-------------------------------------------“‘œ¬ «÷˜≥Ã–Ú
pro MJ_Tongji,event
     COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD
     progressTimer = Obj_New("ShowProgress", tlb,MESSAGE=' ˝æ›¥¶¿Ì÷–,«Î…‘∫Ú!',TITLE='√Êª˝Õ≥º∆')
     progressTimer->START
	;----------µ⁄“ª≤Ω£∫≈–∂œ”∞œÒ”Î ∏¡ø «∑Ò”–ΩªºØ£¨»ÙŒﬁ£¨‘ÚÕÀ≥ˆ≥Ã–Ú
     WIDGET_CONTROL,/hourglass
     WIDGET_CONTROL,Event.top,get_UVALUE = pstate
     WWIDGET =  Event.top

	 progressTimer->UPDATE, (0.1 * 100.0)  ;∏¸–¬Ω¯∂»Ãı
 	 ;∂¡»°recode ˝æ›–≈œ¢,÷˜“™∞¸¿®¥Û–°–≈œ¢°¢◊Û…œΩ«◊¯±Í–≈œ¢µ»µ»°¢Õ∂”∞–≈œ¢µ»
   	 WIDGET_CONTROL,(*pstate).WID_recode,get_value=Recodeimg
    ; Recodeimg='E:\Provincial_Crop_System20070106\data_test\recode.tif'
	 Resultrecode = QUERY_TIFF(Recodeimg,Info,GEOTIFF=recodehead)

	;===—Ó…‹Ô…ÃÌº”£¨20070911========================================
	if Resultrecode ne 1 then begin
		temp_y=dialog_message('≤ªƒ‹¥Úø™recodeŒƒº˛',title='Ã· æ')
		OBJ_DESTROY,progressTimer;œ˙ªŸΩ¯∂»Ãı
		(*pstate).countynum=5000	;5000±Ì æ√ª”–ÕÍ≥…√Êª˝Õ≥º∆£¨—Ó…‹Ô…ÃÌº”£¨20070911
		return
	endif

	tname_y=tag_names(recodehead)
	index_y_0=where(tname_y eq 'MODELTIEPOINTTAG',count0)
	index_y_1=where(tname_y eq 'MODELPIXELSCALETAG',count1)
	index_y_2=where(tname_y eq 'PROJCENTERLONGGEOKEY',count2)
	if (count0 lt 1) or (count1 lt 1) or (count2 lt 1) then begin
		temp_y = DIALOG_MESSAGE(' ‰»Î∏˚µÿ≤„Œƒº˛∫Õ÷ÿ±‡¬ÎŒƒº˛µƒÕ∂”∞≤ª“ª÷¬£°',title='Ã· æ')
		OBJ_DESTROY,progressTimer;œ˙ªŸΩ¯∂»Ãı
		(*pstate).countynum=5000	;5000±Ì æ√ª”–ÕÍ≥…√Êª˝Õ≥º∆£¨—Ó…‹Ô…ÃÌº”£¨20070911
		return
	endif
	;===============================================================

     recodesize=Info.dimensions
	 gridy_max=recodehead.MODELTIEPOINTTAG[4]            ;’§∏Ò∂•µ„÷–y÷µ◊Ó–°÷µ
	 gridy_min=recodehead.MODELTIEPOINTTAG[4] - recodesize[1]*recodehead.MODELPIXELSCALETAG[1]
     gridx_min=recodehead.MODELTIEPOINTTAG[3]            ;’§∏Ò∂•µ„÷–x÷µ◊Ó–°÷µ
	 gridx_max=recodehead.MODELTIEPOINTTAG[3] + recodesize[0]*recodehead.MODELPIXELSCALETAG[1]

     ;∂¡»°œÿΩÁ ˝æ›
    ; countyshape='E:\Provincial_Crop_System20070106\data_vector\county105.shp'
    countyshape='data_vector\county105.shp'
     shpobj = obj_new('IDLffShape',countyshape)
     shpobj->GetProperty, N_ENTITIES=num_ent, ENTITY_TYPE=ShapeType

     ;-----------“‘œ¬±£÷§÷ÿ±‡¬ÎµƒÕ∂”∞“™∫Õ ∏¡ø ˝æ›“ª÷¬(Albers105)≤≈ƒ‹Ω¯––Õ≥º∆

     if (float(recodehead.PROJCENTERLONGGEOKEY[0]) ne float(105)) then begin

        OK = DIALOG_MESSAGE(' ‰»Î∏˚µÿ≤„Œƒº˛∫Õ÷ÿ±‡¬ÎŒƒº˛µƒÕ∂”∞≤ª“ª÷¬£°',title='Ã· æ')
        OBJ_DESTROY,progressTimer;œ˙ªŸΩ¯∂»Ãı	;—Ó…‹Ô…ÃÌº”£¨20070911
        (*pstate).countynum=5000	;5000±Ì æ√ª”–ÕÍ≥…√Êª˝Õ≥º∆£¨—Ó…‹Ô…ÃÌº”£¨20070911
        return
     endif
 	roiObj = objarr(num_ent)
	boundlist = fltarr(num_ent, 2)
    flag=0                                              ;flag=0 ƒ¨»œ’§∏Ò”∞œÒ”Î ∏¡ø ˝æ›√ª”–ΩªºØ

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
		shpy_min=min((*ent.vertices)[1,*])               ; ∏¡ø∂•µ„÷–y÷µ◊Ó–°÷µ
		shpy_max=max((*ent.vertices)[1,*])               ; ∏¡ø∂•µ„÷–y÷µ◊Ó¥Û÷µ
		shpx_min=min((*ent.vertices)[0,*])               ; ∏¡ø∂•µ„÷–x÷µ◊Ó–°÷µ
		shpx_max=max((*ent.vertices)[0,*])       		 ; ∏¡ø∂•µ„÷–x÷µ◊Ó¥Û÷µ

;----------------------------------------◊Óø™ º≈–∂œ∂•µ„
        Result0 = roiObj[i]->ContainsPoints(gridx_min,gridy_min)             ;≈–∂œ◊Û…œ∂•µ„ «∑Ò‘⁄ ∏¡øƒ⁄
        Result1 = roiObj[i]->ContainsPoints(gridx_max,gridy_min)             ;≈–∂œ”“…œ∂•µ„ «∑Ò‘⁄ ∏¡øƒ⁄
        Result2= roiObj[i]->ContainsPoints(gridx_min,gridy_max)
        Result3= roiObj[i]->ContainsPoints(gridx_max,gridy_max)               ;≈–∂œ”“œ¬∂•µ„ «∑Ò‘⁄ ∏¡øƒ⁄
;----------------------------------------
        roiObj[i]->GetProperty ,data=data5
        a=where((data5[1,*] lt gridy_max),counta)
        aa=where((data5[1,*] gt gridy_max),countaa)
        if counta eq 0 then begin
           flag=flag+1
        endif
       ;---------------◊Û±ﬂ«–∏Ó; ;        if ((shpx_min le gridx_min) and (gridx_min le shpy_max)) then begin
        roiObj[i]->GetProperty ,data=data0
        roiObj[i]->GetProperty ,data=data00
        b=where((data00[0,*] gt gridx_min),countb)
        bb=where((data00[0,*] lt gridx_min),countbb)

        if flag ne i+1 then begin
	        if countb eq 0 then begin
	           flag=flag+1
	        endif
	    endif
        ;---------------œ¬±ﬂ«–∏Ó; ;
        roiObj[i]->GetProperty ,data=data1
        roiObj[i]->GetProperty ,data=data11
        c=where((data11[1,*] gt gridy_min),countc)
        cc=where((data11[1,*] lt gridy_min),countcc)
        if flag ne i+1 then begin
	        if countc eq 0 then begin
	           flag=flag+1
	        endif
	    endif
       ;---------------”“±ﬂ«–∏Ó; ;
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

  if flag eq num_ent then begin            ;flagæÕ «√ª”–”Î’§∏Òœ‡Ωªµƒ ∏¡øµƒ∏ˆ ˝
	 progressTimer->UPDATE, (1 * 100.0)  ;∏¸–¬Ω¯∂»Ãı
	 OBJ_DESTROY,progressTimer;œ˙ªŸΩ¯∂»Ãı
     temp=dialog_message('∏˚µÿ≤„ ˝æ›”Î ∏¡ø ˝æ›√ª”–ΩªºØ£°',title='–≈œ¢')
     return

  endif


;-------------------------µ⁄∂˛≤Ω£¨»Ù ∏¡ø”Î’§∏Ò”–ΩªºØ£¨‘Úø…“‘ø™ ºÕ≥º∆¡À
;º»»ª»∑∂®÷ÿ±‡¬Î ˝æ›”Î∏˚µÿ≤„ ˝æ›∫ÕœÿΩÁ ˝æ›”–ΩªºØ,‘Ú–Ë“™Ω¯––œ¬√Ê≤Ÿ◊˜«Û≥ˆ÷ÿ±‡¬Î ˝æ›”Î∏˚µÿ≤„ ˝æ›µƒΩªºØ&
;÷ÿ±‡¬Î ˝æ›”ÎœÿΩÁ ˝æ›ΩªºØ
;’§∏Ò1”Î’§∏Ò2œ‡ΩªºØ«¯”Ú «’‚√¥º∆À„µƒ£∫¡Ω’ﬂµƒ…œ±ﬂy◊Ó–°÷µ°¢œ¬±ﬂy◊Ó¥Û÷µ°¢◊Û±ﬂx◊Ó¥Û÷µ°¢”“±ﬂx◊Ó–°÷µ

;÷ÿ±‡¬Î ˝æ›”Î∏˚µÿ≤„ ˝æ›µƒΩªºØ
;œ»∂¡∏˚µÿ≤„ ˝æ›
;     countyimg='E:\Provincial_Crop_System20070106\data_grid\county105.tif'
;     landimg='E:\Provincial_Crop_System20070106\data_grid\landuse.tif'
     countyimg='data_grid\county105.tif'
     landimg='data_grid\landuse.tif'
	 Resultland = QUERY_TIFF(landimg,info,GEOTIFF=landhead)

	;===—Ó…‹Ô…ÃÌº”£¨20070911========================================
	if Resultland ne 1 then begin
		temp_y=dialog_message('≤ªƒ‹¥Úø™recodeŒƒº˛',title='Ã· æ')
		OBJ_DESTROY,progressTimer;œ˙ªŸΩ¯∂»Ãı
		(*pstate).countynum=5000	;5000±Ì æ√ª”–ÕÍ≥…√Êª˝Õ≥º∆£¨—Ó…‹Ô…ÃÌº”£¨20070911
		return
	endif

	tname_y=tag_names(landhead)
	index_y_0=where(tname_y eq 'MODELTIEPOINTTAG',count0)
	index_y_1=where(tname_y eq 'MODELPIXELSCALETAG',count1)
	index_y_2=where(tname_y eq 'PROJCENTERLONGGEOKEY',count2)
	if (count0 lt 1) or (count1 lt 1) or (count2 lt 1) then begin
		temp_y = DIALOG_MESSAGE(' ‰»Î∏˚µÿ≤„Œƒº˛∫Õ÷ÿ±‡¬ÎŒƒº˛µƒÕ∂”∞≤ª“ª÷¬£°',title='Ã· æ')
		OBJ_DESTROY,progressTimer;œ˙ªŸΩ¯∂»Ãı
		(*pstate).countynum=5000	;5000±Ì æ√ª”–ÕÍ≥…√Êª˝Õ≥º∆£¨—Ó…‹Ô…ÃÌº”£¨20070911
		return
	endif
	;===============================================================

	 landsize=info.dimensions

	 gridy_max=recodehead.MODELTIEPOINTTAG[4]            ;’§∏Ò∂•µ„÷–y÷µ◊Ó–°÷µ
	 gridy_min=recodehead.MODELTIEPOINTTAG[4] - recodesize[1]*recodehead.MODELPIXELSCALETAG[1]
     gridx_min=recodehead.MODELTIEPOINTTAG[3]            ;’§∏Ò∂•µ„÷–x÷µ◊Ó–°÷µ
	 gridx_max=recodehead.MODELTIEPOINTTAG[3] + recodesize[0]*recodehead.MODELPIXELSCALETAG[1]


     ;¡Ω’ﬂ…œ±ﬂyµƒ◊Ó–°÷µ,º¥ΩªºØµƒ◊Û…œΩ«∂•µ„y÷µ
     minyu_landreco=min([landhead.MODELTIEPOINTTAG[4],gridy_max])
     ;œ¬±ﬂyµƒ◊Ó¥Û÷µ
     maxyu_landreco=max([landhead.MODELTIEPOINTTAG[4]-landsize[1]*landhead.MODELPIXELSCALETAG[1],gridy_min])
     ;◊Û±ﬂxµƒ◊Ó¥Û÷µ,º¥ΩªºØµƒ◊Û…œΩ«∂•µ„x÷µ
     maxxu_landreco=max([landhead.MODELTIEPOINTTAG[3],gridx_min])
     ;”“±ﬂxµƒ◊Ó–°÷µ
     minxu_landreco=min([landhead.MODELTIEPOINTTAG[3]+landsize[0]*landhead.MODELPIXELSCALETAG[1],gridx_max])


     ;“Ú¥ÀΩªºØ◊Û…œΩ«µƒ∂•µ„œ‡∂‘”⁄‘≠ ºlandÕºœÒµƒ◊Û…œΩ«∂•µ„Œ™:
     XDX_landreco=(maxxu_landreco-landhead.MODELTIEPOINTTAG[3])/landhead.MODELPIXELSCALETAG[1]
     XDY_landreco=(landhead.MODELTIEPOINTTAG[4]-minyu_landreco)/landhead.MODELPIXELSCALETAG[1]

     ;ΩªºØµƒ¡– ˝
     landreco_Column=(minxu_landreco-maxxu_landreco)/landhead.MODELPIXELSCALETAG[1]
     ;ΩªºØµƒ–– ˝
     landreco_line=(minyu_landreco-maxyu_landreco)/landhead.MODELPIXELSCALETAG[1]
     ;“Ú¥À¥”land ˝æ›÷–∂¡»°¡Ω’ﬂµƒœ‡Ωª≤ø∑÷‘Ú «:
     Intersect_land=read_tiff(landimg, geotiff=landgeotiff,SUB_RECT=[XDX_landreco,XDY_landreco,landreco_Column,landreco_line])

	 landgeotiff.MODELPIXELSCALETAG[0] = landgeotiff.MODELPIXELSCALETAG[0]
	 landgeotiff.MODELPIXELSCALETAG[1] = landgeotiff.MODELPIXELSCALETAG[1]
	 landgeotiff.MODELTIEPOINTTAG[3] =maxxu_landreco                    ;-1000£∫ «Œ™¡À»√ÕºœÒ¥Û–©
	 landgeotiff.MODELTIEPOINTTAG[4] =minyu_landreco                  ;-1000£∫ «Œ™¡À»√ÕºœÒ¥Û–©

 ;    write_tiff, 'd:\temp\s0.tif', Intersect_land, geotiff=landgeotiff ;s0£∫¥˙±Ì «“ª∑˘∑∂≥Î∞¸¿®¡À∏˚µÿ≤„∫Õ ∏¡ø≤„À˘øÁµƒ∑∂≥Îµƒ“ª∑˘∑÷±Ê¬ Œ™1000√◊µƒ’§∏ÒÕº

	 progressTimer->UPDATE, (0.2 * 100.0)  ;∏¸–¬Ω¯∂»Ãı


     ;“Ú¥ÀΩªºØ◊Û…œΩ«µƒ∂•µ„œ‡∂‘”⁄‘≠ º÷ÿ±‡¬ÎÕºœÒµƒ◊Û…œΩ«∂•µ„Œ™:
     XDX_reco=(maxxu_landreco-gridx_min)/recodehead.MODELPIXELSCALETAG[1]
     XDY_reco=(gridy_max-minyu_landreco)/recodehead.MODELPIXELSCALETAG[1]
     ;ΩªºØµƒ¡– ˝
     reco_Column=(minxu_landreco-maxxu_landreco)/recodehead.MODELPIXELSCALETAG[1]
     ;ΩªºØµƒ–– ˝
     reco_line=(minyu_landreco-maxyu_landreco)/recodehead.MODELPIXELSCALETAG[1]


     ;¥”÷ÿ±‡¬Î ˝æ›÷–Ã·»°¡Ω’ﬂµƒœ‡Ωª≤ø∑÷Œ™:
     Intersect_reland= read_tiff(Recodeimg, geotiff=recodehead,SUB_RECT=[XDX_reco,XDY_reco,reco_Column,reco_line])

	 recodehead.MODELPIXELSCALETAG[0] = recodehead.MODELPIXELSCALETAG[0]
	 recodehead.MODELPIXELSCALETAG[1] = recodehead.MODELPIXELSCALETAG[1]
	 recodehead.MODELTIEPOINTTAG[3] =maxxu_landreco                    ;-1000£∫ «Œ™¡À»√ÕºœÒ¥Û–©
	 recodehead.MODELTIEPOINTTAG[4] =minyu_landreco                  ;-1000£∫ «Œ™¡À»√ÕºœÒ¥Û–©

   ;  write_tiff, 'd:\temp\s1.tif', Intersect_reland, geotiff=recodehead ;s0£∫¥˙±Ì «“ª∑˘∑∂≥Î∞¸¿®¡À∏˚µÿ≤„∫Õ ∏¡ø≤„À˘øÁµƒ∑∂≥Îµƒ“ª∑˘∑÷±Ê¬ Œ™1000√◊µƒ’§∏ÒÕº
	 progressTimer->UPDATE, (0.3 * 100.0)  ;∏¸–¬Ω¯∂»Ãı
     ;”…”⁄œÿΩÁ ˝æ›∫Õ∏˚µÿ≤„ ˝æ›‘⁄œµÕ≥÷– «Õ¨—˘µƒ¥Û–°∫ÕŒª÷√£¨“Ú¥ÀœÿΩÁ ˝æ›”Î÷ÿ±‡¬Î ˝æ›µƒΩªºØ≤ø∑÷¥”œÿΩÁ ˝æ›÷–Ã·»°Œ™:
     Intersect_county=read_tiff(countyimg, geotiff=countygeotiff,SUB_RECT=[XDX_landreco,XDY_landreco,landreco_Column,landreco_line])
     ;¥”÷ÿ±‡¬Î ˝æ›÷–Ã·»°¡Ω’ﬂµƒœ‡Ωª≤ø∑÷Œ™Intersect_reland


  ;   write_tiff, 'd:\temp\s2.tif', Intersect_county, geotiff=landgeotiff ;s0£∫¥˙±Ì «“ª∑˘∑∂≥Î∞¸¿®¡À∏˚µÿ≤„∫Õ ∏¡ø≤„À˘øÁµƒ∑∂≥Îµƒ“ª∑˘∑÷±Ê¬ Œ™1000√◊µƒ’§∏ÒÕº
     ;”√congrid∫Ø ˝ πµ√»˝∑˘”∞œÒ¥Û–°°¢∑÷±Ê¬ »´“ª—˘
     ;»Áπ˚÷ÿ±‡¬Î ˝æ›∑÷±Ê¬ ¥Û”⁄60meters£®¿˝»ÁŒ™100√◊£©£¨‘ÚΩ´¥”œÿΩÁ∫Õ∏˚µÿ≤„ÕºœÒ÷–Ã·»°µƒ÷ÿ∫œ≤ø∑÷congrid≥…100√◊µƒ

	 progressTimer->UPDATE, (0.4 * 100.0)  ;∏¸–¬Ω¯∂»Ãı

     ;»Áπ˚÷ÿ±‡¬Î ˝æ›∑÷±Ê¬ –°”⁄60√◊£®¿˝»Á30√◊),‘ÚΩ´÷ÿ±‡¬Î ˝æ›congridŒ™60√◊µƒ
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
	  progressTimer->UPDATE, (0.55 * 100.0)  ;∏¸–¬Ω¯∂»Ãı
      Intersect_land=0B
      Intersect_county=0B
      Intersect_reland=0B
     ;-------------¥”œÿΩÁ ˝æ›÷–∂¡≥ˆµƒ÷ÿ∫œ≤ø∑÷ ˝æ›ø…∑÷Œˆ≥ˆ√ø∏ˆœÿº‡≤‚µƒ√Êª˝

		;-----‘≠¥˙¬Î£≠£≠£≠£≠£≠£≠£≠£≠£≠£≠£≠£≠£≠
;      uniqvaluenum=N_elements(UNIQ(congridcounty,sort(congridcounty)))   ;uniqvaluenum:÷∏∏˚µÿ≤„”∞œÒÀ˘øÁ¡Àº∏∏ˆœÿ
;      index1=UNIQ(congridcounty,sort(congridcounty))                     ;index1:∞—∏˚µÿ≤„ÕºœÒ¥” ∏¡ø’§∏ÒªØ∫Û
;      unioncounty=where(congridcounty[index1] ne 0,countynum)
		;-----“‘…œŒ™‘≠¥˙¬Î£≠£≠£≠£≠£≠£≠£≠£≠£≠£≠£≠£≠£≠
		;----“‘œ¬¥˙¬ÎŒ™—Ó…‹Ô…–ﬁ∏ƒ£¨20070511------------------
		temp=histogram(congridcounty)
		index1=where(temp ne 0,count)
		uniqvaluenum=count
		unioncounty=where(index1 ne 0,countynum)
;		;----“‘…œ¥˙¬ÎŒ™—Ó…‹Ô…–ﬁ∏ƒ£¨20070511------------------



     ;-------------Ω´¥”∏˚µÿ≤„÷–∂¡»°µƒ∏˚µÿ≤„”Î÷ÿ±‡¬Î ˝æ›µƒ÷ÿ∫œ≤ø∑÷ ˝æ›”Î¥”œÿΩÁ ˝æ›÷–∂¡≥ˆµƒ÷ÿ∫œ≤ø∑÷ ˝æ›œ‡≥À£;
     ;ø…∑÷Œˆ≥ˆ√ø∏ˆœÿƒ⁄”–∂‡…Ÿ∏˚µÿ√Êª˝

     dd=congridland
     dryindex=where(dd eq 2,numdry)                ;’“≥ˆ”–∫µµÿµƒµÿ∑Ω£¨ÀÊ∫Û∞—”–∫µµÿµƒµÿ∑Ωµƒ÷µ∏ƒŒ™0;
                                                   ;º∏∏ˆœ‡≥À∫Ûø…“‘’“≥ˆ÷÷÷≤¡À◊˜ŒÔµƒµÿ∑Ω(”–÷µµƒµÿ∑Ω)
     if numdry gt 0 then begin                     ;∞—”–∫µµÿµƒµÿ∑Ωµƒ÷µ∏ƒŒ™0;
     	dd[dryindex]=1
    	mm=congridcounty*dd                       ;mm÷–µƒ÷µ¥Û”⁄0(ªÚ’ﬂÀµµ»”⁄congridcounty[index1[i]])µƒµÿ∑ΩæÕ «

        progressTimer->UPDATE, (0.75 * 100.0)  ;∏¸–¬Ω¯∂»Ãı                                             ;Œ™∏˚µÿµƒµÿ∑Ω(∞¸¿®ÀÆÃÔ∫Õ∫µµÿ)
     endif else begin
        progressTimer->UPDATE, (0.75 * 100.0)  ;∏¸–¬Ω¯∂»Ãı
    	mm=congridcounty*dd
     endelse

	 progressTimer->UPDATE, (0.8 * 100.0)  ;∏¸–¬Ω¯∂»Ãı
     ;-------------Ω´¥”œÿΩÁ÷–∂¡≥ˆµƒ÷ÿ∫œ≤ø∑÷ ˝æ›≥À“‘¥”÷ÿ±‡¬Î ˝æ›÷–∂¡≥ˆµƒ÷ÿ∫œ≤ø∑÷ ˝æ›ø…∑÷Œˆ≥ˆ√ø∏ˆœÿƒ⁄”–∂‡…Ÿ «÷÷¡À◊˜ŒÔµƒ√Êª˝
    nn=congridrecode

    ;---∏√≤ø∑÷¥˙¬Î—Ó¿◊∂´Ω¯–––ﬁ∏ƒ£¨20070511-----------------------
    cloundindex=where(nn eq 2,numclound);—Ó¿◊∂´◊¢œ˙           ;’“≥ˆ”–‘∆µƒµÿ∑Ω£¨ÀÊ∫Û∞—”–‘∆µƒµÿ∑Ωµƒ÷µ∏ƒŒ™0;
;    ;----—Ó¿◊∂´ÃÌº”£¨20070511£≠£≠£≠£≠£≠£≠
;	    b = histogram(nn)                                               ;º∏∏ˆœ‡≥À∫Ûø…“‘’“≥ˆ÷÷÷≤¡À◊˜ŒÔµƒµÿ∑Ω(”–÷µµƒµÿ∑Ω)
;	    nncount = n_elements(nn)
;	    numclound = b[2]
;	    for i=0L,nncount-1 do begin
;			if nn[i] eq 2 then nn[i] = 0
;	    endfor
	 ;------------------------------
    if numclound gt 0 then begin                   ;∞—”–‘∆µƒµÿ∑Ωµƒ÷µ∏ƒŒ™0;
     	nn[cloundindex]=0    ;—Ó¿◊∂´◊¢œ˙
     	hasclound=1
        ll=nn*congridcounty                        ;ll÷–µƒ÷µ¥Û”⁄0(ªÚ’ﬂÀµµ»”⁄getdata[index1[i]])µƒµÿ∑ΩæÕ «
                                                   ;÷÷÷≤¡À◊˜ŒÔµƒµÿ∑Ω
    endif else begin
        ll=nn*congridcounty
    endelse
;-------“‘…œ¥˙¬Î—Ó¿◊∂´Ω¯–––ﬁ∏ƒ----------------------------------
    print,'d'
     ;-------------÷Æ∫Ûø…“‘º∆À„√ø∏ˆœÿµƒ÷÷÷≤≥… ˝

     perplanted=fltarr(num_ent)                ;÷÷÷≤≥… ˝
     coverarea=fltarr(num_ent)                ;º‡≤‚∑∂Œß
     nameofcounty=strarr(num_ent)

     his=histogram(mm)
   ;  covercounty=where(congridcounty eq index1[i],covercountynum)
    hiscovercounty=histogram(congridcounty)
    hisll=histogram(ll)
;    ss=where(ll eq index1[i],countplanted)
    for i=unioncounty[0],countynum do begin	;‘≠¥˙¬Î
       ;rr=where(mm eq congridcounty[index1[i]],countplowland) ;‘≠¥˙¬Î
       ;rr=where(mm eq index1[i],countplowland)
        if index1[i] ge n_elements(his) then $
        countplowland=0 else countplowland=his[index1[i]]
;    ---—Ó…‹Ô…–ﬁ∏ƒ£¨20070511------------------
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
          ;----‘≠¥˙¬Î-----
       ;  covercounty=where(congridcounty eq congridcounty[index1[i]],covercountynum) ;‘≠¥˙¬Î
        if index1[i] ge n_elements(hiscovercounty) then $
        covercountynum=0 else covercountynum=hiscovercounty[index1[i]]

;       covercounty=where(congridcounty eq index1[i],covercountynum)
;       ---—Ó…‹Ô…–ﬁ∏ƒ£¨20070511------------------
;			covercountynum=0
;		for k=0L,n_elements(congridcounty)-1 do begin
;			if congridcounty[k] eq congridcounty[index1[i]] then covercountynum=covercountynum+1
;		endfor
	;---------------------------------------


         coverarea[i-1]=congridpixel*covercountynum
         if countplowland gt 0 then begin

			;----‘≠¥˙¬Î-----
	       ;  ss=where(ll eq congridcounty[index1[i]],countplanted);‘≠¥˙¬Î
	     ;    ss=where(ll eq index1[i],countplanted)
         if index1[i] ge n_elements(hisll) then $
         countplanted=0 else countplanted=hisll[index1[i]]
			;---—Ó…‹Ô…–ﬁ∏ƒ£¨20070511------------------
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
;;         ;=====—Ó…‹Ô…≤‚ ‘ÃÌº”£¨20070911===========================
;         	if strcmp(strtrim(nameofcounty[i-1],2), string('360101')) then aa____
;;         ;=======================================================
	     print,perplanted[i-1]
    endfor
print,nameofcounty
	progressTimer->UPDATE, (0.95 * 100.0)  ;∏¸–¬Ω¯∂»Ãı
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
	    progressTimer->UPDATE, (1 * 100.0)  ;∏¸–¬Ω¯∂»Ãı

        OBJ_DESTROY,progressTimer;œ˙ªŸΩ¯∂»Ãı
        OBJ_DESTROY,shpobj
        OK = DIALOG_MESSAGE('ÕÍ≥…Õ≥º∆£°',title='Ã· æ')
        log, '÷÷÷≤≥… ˝-ø’º‰Õ≥º∆', 0
;-------------œ‘ æ‘⁄widget_table±Ì÷–

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
      ,TITLE='÷÷÷≤≥… ˝º∆À„' ,SPACE=3 ,XPAD=3 ,YPAD=3, TLB_FRAME_ATTR =1)


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
      ,/ALIGN_LEFT ,VALUE='ƒÍ∑›')



;====—Ó…‹Ô…–ﬁ∏ƒ£¨20070906======================================

;drop_year = Widget_combobox(WID_BASE_11,  $		;‘≠¥˙¬Î
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
      ,/ALIGN_LEFT ,VALUE='œƒ¡∏/«Ô¡∏')

  season=['œƒ¡∏','«Ô¡∏']
  Drop_type = Widget_combobox(WID_BASE_11,  $
      UNAME='Drop_type' ,XOFFSET=194 ,YOFFSET=7 ,SCR_XSIZE=64  $
      ,SCR_YSIZE=18,value=season)


  label_landuse = Widget_Label(WID_BASE_11, UNAME='label_landuse'  $
      ,XOFFSET=11 ,YOFFSET=42 ,SCR_XSIZE=100 ,SCR_YSIZE=20  $
      ,/ALIGN_LEFT ,VALUE='Õ¡µÿ¿˚”√ ˝æ›¿‡–Õ')

  landtype=['’§∏Ò']
  drop_landuse = Widget_Droplist(WID_BASE_11,  $
      UNAME='drop_landuse' ,XOFFSET=193 ,YOFFSET=40 ,SCR_XSIZE=65  $
      ,SCR_YSIZE=18,value=landtype)


  WID_BASE_12 = Widget_Base(WID_BASE_10, UNAME='WID_BASE_12' ,FRAME=1  $
      ,XOFFSET=0 ,YOFFSET=72 ,SCR_XSIZE=267 ,SCR_YSIZE=83  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_LABEL_12 = Widget_Label(WID_BASE_12, UNAME='WID_LABEL_12'  $
      ,XOFFSET=8 ,YOFFSET=14 ,SCR_XSIZE=103 ,SCR_YSIZE=19  $
      ,/ALIGN_LEFT ,VALUE='—°‘Ò÷ÿ±‡¬Î ˝æ›')


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
      ,/ALIGN_CENTER ,VALUE='º∆À„',event_pro='MJ_Tongji')


  WID_BUTTON_10 = Widget_Button(WID_BASE_13, UNAME='WID_BUTTON_10'  $
      ,XOFFSET=77 ,YOFFSET=7 ,SCR_XSIZE=46 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='»Îø‚',event_pro='MJ_TJ_todb')


  WID_BUTTON_11 = Widget_Button(WID_BASE_13, UNAME='WID_BUTTON_11'  $
      ,XOFFSET=140 ,YOFFSET=7 ,SCR_XSIZE=46 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='∞Ô÷˙',event_pro='MJ_TJhelp_event')


  WID_BUTTON_12 = Widget_Button(WID_BASE_13, UNAME='WID_BUTTON_12'  $
      ,XOFFSET=205 ,YOFFSET=7 ,SCR_XSIZE=46 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,VALUE='πÿ±’',event_pro='MJ_TJ_Close')

;
;  WID_TABLE_1 = Widget_Table(WID_BASE_0, UNAME='WID_TABLE_1' ,FRAME=1  $
;      ,XOFFSET=296 ,YOFFSET=7 ,SCR_XSIZE=268 ,SCR_YSIZE=243 ,XSIZE=5  $
;      ,YSIZE=6)
   state = { $
        Drop_type	: Drop_type,$
        drop_year   : drop_year,$
        p_nameofcounty:ptr_new(),$;¥¥Ω®“ª∏ˆ÷∏’Î£¨÷∏œÚ∑÷º∂≤Œ ˝
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
  common_log,'∆Ù∂Ø√Êª˝Õ≥º∆'
  MJ_sta, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end
