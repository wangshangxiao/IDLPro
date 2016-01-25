;钮立明修改于2009年3月13日
PRO GL_xianshi_EVENT,EVENT

	CATCH, Error_status               ;截取错误.
	IF Error_status NE 0 THEN BEGIN
		infomation=DIALOG_MESSAGE(['出现以下错误：',[!ERROR_STATE.MSG],'请检查基础数据是否齐全.'],TITLE='错误',/ERROR)
		CATCH, /CANCEL
		RETURN                    ;如果不加上这句,则会继续执行下面的语句,仍会出现错误.
	ENDIF

	widget_control,event.top,get_uvalue=pState
	widget_control,event.id,get_value=buttonvalue

	wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
	              widget_info(Event.id, /tree_root) : event.id)
	wWidget=event.top
	;help,event,/structure

	;选择查询时间段
	if(wTarget eq Widget_Info(wWidget,FIND_BY_UNAME=('time1')) or $
	   wTarget eq Widget_Info(wWidget,FIND_BY_UNAME=('time2')) or $
	   wTarget eq Widget_Info(wWidget,FIND_BY_UNAME=('time3'))) then begin

	   case buttonvalue of
	   '当前时间':begin
	      widget_control,(*pState).select_time_base1,map=1
	      widget_control,(*pState).select_time_base2,map=0
	      widget_control,(*pState).select_time_base3,map=0
	      end
	   '指定时间段':begin
	      widget_control,(*pState).select_time_base1,map=0
	      widget_control,(*pState).select_time_base2,map=1
	      widget_control,(*pState).select_time_base3,map=0
	      end
	   '所有任务':begin
	      widget_control,(*pState).select_time_base1,map=0
	      widget_control,(*pState).select_time_base2,map=0
	      widget_control,(*pState).select_time_base3,map=1
	      end
	   else:
	   endcase

	endif

	y=(bin_date())[0]

	if (y mod 100 ne 0 and y mod 4 eq 0) or (y mod 400 eq 0) then begin
	   flag=1	;1表示为闰年
	endif else begin
	   flag=0
	endelse

	start_day_31=strtrim(indgen(31)+1,2)
	start_day_30=strtrim(indgen(30)+1,2)
	start_day_29=strtrim(indgen(29)+1,2)
	start_day_28=strtrim(indgen(28)+1,2)

	if(widget_info((*pState).buttopt1,/button_set) eq 1) then begin
	   CASE wTarget OF

	      Widget_Info(wWidget,FIND_BY_UNAME='Day_droplist'):	BEGIN
	          (*PSTATE).Day = (*PSTATE).DayList[event.index]
	          end

	      Widget_Info(wWidget,FIND_BY_UNAME='search_butt'):	BEGIN
	      	;(1)获取数据库链接
	      	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

			DBCO=DBobj

	        STime=(bin_date())[1]*31+(bin_date())[2]
	        ETime=(bin_date())[1]*31+(bin_date())[2]+(*PSTATE).Day

	      	;(2)读取数据
	      	SQL='SELECT business_name,month_start,day_start,kuadu,note,period FROM business_time_table'
	        SQL=SQL+' where '
	        SQL=SQL+'(((month_start*31+day_start) >='+'('+ STRTRIM(STime,2)+ '))'
	        SQL=SQL+' AND ((month_start*31+day_start) <='+'('+ STRTRIM(ETime,2)+'))'
	        SQL=SQL+' OR period=1)'
	      	SQL=SQL+' order by month_start,day_start'

	      	;***************************************************************************
	      	;获取数据长度
	      	;获取数据记录的个数
			RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+Sql+')')
			RecordNum = RecordNumOBJ->GETFIELD(0)
			NUM=RecordNum

			Obj_Destroy,RecordNumOBJ

			;***************************************************************************

	        IF NUM LE 0 then begin
	            temp=dialog_message('未发现符合条件的任务',/information,title='提示')
	            temp[*,*]=''
	            widget_control,(*PSTATE).mission_table,set_value=temp[*,*]
	            return
	        endif

			arr_data=strarr(6,NUM)
			ORS = Obj_New('IDLdbRecordset', DBCO, SQL=SQL)
			count=0
			IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
				REPEAT BEGIN
					arr_data[0,count]=ORS -> GETFIELD(0)

	                if ORS -> GETFIELD(1) eq 0 then begin
					   arr_data[1,count]=''
					endif else begin
					   arr_data[1,count]=ORS -> GETFIELD(1)
					endelse

					if ORS -> GETFIELD(2) eq 0 then begin
					   arr_data[2,count]=''
					endif else begin
					   arr_data[2,count]=ORS -> GETFIELD(2)
					endelse

					arr_data[3,count]=string(ORS -> GETFIELD(3),format='(f5.1)')
					arr_data[4,count]=ORS -> GETFIELD(4)
					arr_data[5,count]=ORS -> GETFIELD(5)


					COUNT=COUNT+1
				ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
			ENDIF

			Obj_Destroy,ORS


	      	;(3)结果显示

			widget_control,(*PSTATE).mission_table,table_ysize=NUM
			ROW_LABELS_new=strtrim(indgen(NUM),2)
			widget_control,(*PSTATE).mission_table,ROW_LABELS=ROW_LABELS_new
	      	widget_control,(*PSTATE).mission_table,set_value=arr_data

			;endif

	      END
	      else:
	   endcase
	endif
;=====================================================================================
	IF(WIDGET_INFO((*pState).buttopt2,/BUTTON_SET) EQ 1) THEN BEGIN
	   CASE wTarget OF
	      WIDGET_INFO(wWidget,FIND_BY_UNAME='start_month'): BEGIN
	            (*PSTATE).SMonth = (*PSTATE).SMonth_list[event.index]

				IF ((*PSTATE).SMonth EQ 2) THEN BEGIN
	               IF flag EQ 1 THEN BEGIN
	                  widget_control,(*PSTATE).start_day_droplist,SET_VALUE=start_day_29, $
	              				SET_DROPLIST_SELECT=0
	               ENDIF ELSE BEGIN
	                  widget_control,(*PSTATE).start_day_droplist,SET_VALUE=start_day_28, $
	              				SET_DROPLIST_SELECT=0
	               ENDELSE
	            ENDIF ELSE BEGIN
	               IF ((*PSTATE).SMonth EQ 4) OR ((*PSTATE).SMonth EQ 6)      $
	                  OR ((*PSTATE).SMonth EQ 9) OR ((*PSTATE).SMonth EQ 11) THEN BEGIN
	                     WIDGET_CONTROL,(*PSTATE).start_day_droplist,SET_VALUE=start_day_30, $
	              				SET_DROPLIST_SELECT=0
	               ENDIF ELSE BEGIN
	               		 WIDGET_CONTROL,(*PSTATE).start_day_droplist,SET_VALUE=start_day_31, $
	              				SET_DROPLIST_SELECT=0
	               ENDELSE
	            ENDELSE

				(*PSTATE).SDay=1

				WIDGET_CONTROL,(*PSTATE).start_day_droplist,GET_VALUE=temp
				IF PTR_VALID((*PSTATE).ptr_SDay_list) EQ 1 THEN PTR_FREE,(*PSTATE).ptr_SDay_list
				(*PSTATE).ptr_SDay_list=PTR_NEW(temp)

	      END

	;;-----------------------------------------------------------------------------
	      WIDGET_INFO(wWidget,FIND_BY_UNAME='start_day'):  BEGIN
	            (*PSTATE).SDay = (*((*PSTATE).ptr_SDay_list))[event.index]

	      END

	;-----------------------------------------------------------------------------
	      WIDGET_INFO(wWidget,FIND_BY_UNAME='end_month'):  BEGIN
	            (*PSTATE).EMonth = (*PSTATE).EMonth_list[event.index]

				IF ((*PSTATE).EMonth EQ 2) THEN BEGIN
	               IF flag EQ 1 THEN BEGIN
	                  widget_control,(*PSTATE).end_day_droplist,SET_VALUE=start_day_29, $
	              				SET_DROPLIST_SELECT=0
	               ENDIF ELSE BEGIN
	                 widget_control,(*PSTATE).end_day_droplist,SET_VALUE=start_day_28, $
	              				SET_DROPLIST_SELECT=0
	               ENDELSE
	            ENDIF ELSE BEGIN
	               IF ((*PSTATE).EMonth EQ 4) OR ((*PSTATE).EMonth eq 6)      $
	                  OR ((*PSTATE).EMonth EQ 9) OR ((*PSTATE).EMonth EQ 11) THEN BEGIN
	                    widget_control,(*PSTATE).end_day_droplist,SET_VALUE=start_day_30, $
	              				SET_DROPLIST_SELECT=0
	               ENDIF ELSE BEGIN
	               		widget_control,(*PSTATE).end_day_droplist,SET_VALUE=start_day_31, $
	              				SET_DROPLIST_SELECT=0
	               ENDELSE
	            ENDELSE

				(*PSTATE).EDay=1

				widget_control,(*PSTATE).end_day_droplist,GET_VALUE=temp
				IF PTR_VALID((*PSTATE).ptr_EDay_list) EQ 1 THEN PTR_FREE,(*PSTATE).ptr_EDay_list
				(*PSTATE).ptr_EDay_list=PTR_NEW(temp)


	      END

	;;-----------------------------------------------------------------------------
	      Widget_Info(wWidget,FIND_BY_UNAME='end_day'):    BEGIN
	          (*PSTATE).EDay = (*((*PSTATE).ptr_EDay_list))[event.index]

			 end

	      Widget_Info(wWidget,FIND_BY_UNAME='search_butt'): BEGIN
	      ;(1)获取数据库链接
	        COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

	        DBCO=DBobj

	      STime=(*PSTATE).SMonth*31+(*PSTATE).SDay
	      ETime=(*PSTATE).EMonth*31+(*PSTATE).EDay
	      if STime GT ETime then begin
	          temp=dialog_message('输入时间不合理',/error,title='警告')
	          temp[*,*]=''
	          widget_control,(*PSTATE).mission_table,set_value=temp[*,*]
	          return
	      endif

	;		ProID=strmid(PROVINCE_CODE,0,2)

	        ;(2)读取数据
	;        SQL='SELECT business_name,month_start,day_start,kuadu,note,period FROM '+ProID+'business_time_table'
	        SQL='SELECT business_name,month_start,day_start,kuadu,note,period FROM business_time_table'
	        SQL=SQL+' where ('
	        SQL=SQL+'(month_start*31+day_start between '+strtrim(STime,2)
	        SQL=SQL+' and '+strtrim(ETime,2)+')'
	        SQL=SQL+' OR period=1)'
	        SQL=SQL+' order by month_start,day_start'

   ;***************************************************************************
	        ;获取数据长度
	        ;获取数据记录的个数
	       RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+Sql+')')
	       RecordNum = RecordNumOBJ->GETFIELD(0)
	       NUM=RecordNum

	       Obj_Destroy,RecordNumOBJ


	        IF NUM LE 0 then begin
	            temp=dialog_message('未发现符合条件的任务',/information,title='提示')
	            temp[*,*]=''
	            widget_control,(*PSTATE).mission_table,set_value=temp[*,*]
	            return
	        endif

	       ;***************************************************************************
	       arr_data=strarr(6,NUM)
	       ORS = Obj_New('IDLdbRecordset', DBCO, SQL=SQL)
	       count=0
	       IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
	         REPEAT BEGIN
	          arr_data[0,count]=ORS -> GETFIELD(0)

	                if ORS -> GETFIELD(1) eq 0 then begin
	             arr_data[1,count]=''
	          endif else begin
	             arr_data[1,count]=ORS -> GETFIELD(1)
	          endelse

	          if ORS -> GETFIELD(2) eq 0 then begin
	             arr_data[2,count]=''
	          endif else begin
	             arr_data[2,count]=ORS -> GETFIELD(2)
	          endelse

	          arr_data[3,count]=string(ORS -> GETFIELD(3),format='(f5.1)')
	          arr_data[4,count]=ORS -> GETFIELD(4)
	          arr_data[5,count]=ORS -> GETFIELD(5)
	          COUNT=COUNT+1
	         ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
	       ENDIF

	       Obj_Destroy,ORS


	        ;(3)结果显示
	        widget_control,(*PSTATE).mission_table,table_ysize=NUM

			ROW_LABELS_new=strtrim(indgen(NUM),2)
			widget_control,(*PSTATE).mission_table,ROW_LABELS=ROW_LABELS_new

	        widget_control,(*PSTATE).mission_table,set_value=arr_data
	      end
	      else:
	   endcase
	endif
;======================================================================================
	IF(WIDGET_INFO((*pState).buttopt3,/button_set) EQ 1) THEN BEGIN
	   CASE wTarget OF
	      WIDGET_INFO(wWidget,FIND_BY_UNAME='search_butt'):	BEGIN
	      	;(1)获取数据库链接
	      	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

			DBCO=DBobj

	        STime=(bin_date())[1]*31+(bin_date())[2]
	        ETime=(bin_date())[1]*31+(bin_date())[2]+(*PSTATE).Day

	      	;(2)读取数据
	      	SQL='SELECT business_name,month_start,day_start,kuadu,note,period FROM business_time_table'
;	        SQL=SQL+' where '
;	        SQL=SQL+'(((month_start*31+day_start) >='+'('+ STRTRIM(STime,2)+ '))'
;	        SQL=SQL+' AND ((month_start*31+day_start) <='+'('+ STRTRIM(ETime,2)+'))'
;	        SQL=SQL+' OR period=1)'
	      	SQL=SQL+' order by month_start,day_start'

	      	;***************************************************************************
	      	;获取数据长度
	      	;获取数据记录的个数
			RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+Sql+')')
			RecordNum = RecordNumOBJ->GETFIELD(0)
			NUM=RecordNum

			Obj_Destroy,RecordNumOBJ

			;***************************************************************************

	        IF NUM LE 0 THEN BEGIN
	           temp=DIALOG_MESSAGE('未发现符合条件的任务',/INFORMATION,TITLE='提示')
	           temp[*,*]=''
	           WIDGET_CONTROL,(*PSTATE).mission_table,SET_VALUE=temp[*,*]
	           RETURN
	        ENDIF

			arr_data=strarr(6,NUM)
			ORS = OBJ_NEW('IDLdbRecordset', DBCO, SQL=SQL)
			count=0
			IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
			   REPEAT BEGIN
			      arr_data[0,count]=ORS -> GETFIELD(0)

	              IF ORS -> GETFIELD(1) EQ 0 THEN BEGIN
				     arr_data[1,count]=''
				  ENDIF ELSE BEGIN
				     arr_data[1,count]=ORS -> GETFIELD(1)
				  ENDELSE

				  IF ORS -> GETFIELD(2) EQ 0 THEN BEGIN
				     arr_data[2,count]=''
				  ENDIF ELSE BEGIN
					 arr_data[2,count]=ORS -> GETFIELD(2)
			      ENDELSE

				  arr_data[3,count]=STRING(ORS -> GETFIELD(3),FORMAT='(f5.1)')
				  arr_data[4,count]=ORS -> GETFIELD(4)
				  arr_data[5,count]=ORS -> GETFIELD(5)

				  COUNT=COUNT+1
			   ENDREP UNTIL (ORS->MOVECURSOR(/NEXT) NE 1)
			ENDIF

			Obj_Destroy,ORS


	      	;(3)结果显示

			widget_control,(*PSTATE).mission_table,table_ysize=NUM
			ROW_LABELS_new=strtrim(indgen(NUM),2)
			widget_control,(*PSTATE).mission_table,ROW_LABELS=ROW_LABELS_new
	      	widget_control,(*PSTATE).mission_table,set_value=arr_data

	      END
	      else:
	   endcase
	endif
;===============================================================================
    CASE wTarget OF
	   WIDGET_INFO(wWidget, FIND_BY_UNAME='colse_butt'):BEGIN
	   	common_log,'关闭任务查询'
	      WIDGET_CONTROL,event.top,/DESTROY
	   END

	   WIDGET_INFO(wWidget, FIND_BY_UNAME='help_butt'):BEGIN

	      IF FILE_TEST('HELP\HELP.chm') THEN BEGIN
		     ONLINE_HELP, '任务查询', BOOK='HELP\HELP.chm', /FULL_PATH
		  ENDIF ELSE BEGIN
		     info_help=dialog_message('找不到帮助文档',title='警告')
		  ENDELSE
	   END

       ELSE:

    ENDCASE

;=========================================================================

END



PRO GL_xianshi,GROUP_LEADER=wGroup

	common_log,'启动任务查询'

   IF ( XREGISTERED('GL_xianshi') NE 0 ) THEN RETURN

   device,get_screen_size=screen_size
   xoffset=screen_size[0]/4
   yoffset=screen_size[1]/4

   COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP

   xianshi_base=WIDGET_BASE(GROUP_LEADER=BASE_TOP,COLUMN=1,TITLE='任务查询',XOFFSET=xoffset,$
                   YOFFSET=200,TLB_FRAME_ATTR=1,SPACE=2,$
                   XSIZE=525,YSIZE=340,/ALIGN_CENTER)
   ;选择时间
	   time_base=WIDGET_BASE(xianshi_base,ROW=2,FRAME=1,$
	                         SPACE=7,XSIZE=509,/ALIGN_CENTER)

	   	   space_base=WIDGET_BASE(time_base,XSIZE=10)

		   select_base=WIDGET_BASE(time_base,ROW=1,/EXCLUSIVE)

			   buttopt1=WIDGET_BUTTON(select_base,VALUE='当前时间',UNAME='time1')
			   buttopt2=WIDGET_BUTTON(select_base,VALUE='指定时间段',UNAME='time2')
			   buttopt3=WIDGET_button(select_base,VALUE='所有任务',UNAME='time3')

	   DayList=strtrim(indgen(31)+1,2)

	  	   space_base=WIDGET_BASE(time_base,XSIZE=10)

	       show_base=WIDGET_BASE(time_base)

	   	   	   select_time_base1=WIDGET_BASE(show_base,ROW=1,XSIZE=500,YSIZE=27,MAP=1)

	   		   		day_droplist=WIDGET_DROPLIST(select_time_base1,/NO_COPY,$
	                                      UNAME='Day_droplist' ,$
	                                      VALUE=DayList,TITLE='从今天起:',$
	                                      SCR_XSIZE=100)

	   				WIDGET_CONTROL,day_droplist,SET_DROPLIST_SELECT=6

	   				label=WIDGET_LABEL(select_time_base1,VALUE='日')

	   	   ;space_base=widget_base(time_base,xsize=80)

	   		   select_time_base2=WIDGET_BASE(show_base,ROW=1,XSIZE=500,YSIZE=27,MAP=0)

	   				start_time_base=WIDGET_BASE(select_time_base2,ROW=1,/ALIGN_CENTER)
	   					stime_text_label=WIDGET_LABEL(start_time_base,VALUE='开始时间:')

		   				start_month=strtrim(indgen(12)+1,2)
		   				start_month_droplist=WIDGET_DROPLIST(start_time_base,$
		                                        VALUE=start_month,$
		                                        UNAME='start_month')
		   				start_month_label=WIDGET_LABEL(start_time_base,VALUE='月')


					    ;day应根据不同的月份有所不同，如果是二月份应该只有28天或29天
					    start_day=strtrim(indgen(31)+1,2)
					    ptr_start_day=PTR_NEW(start_day)
					    start_day_droplist=WIDGET_DROPLIST(start_time_base,$
					                                      VALUE=start_day,$
					                                      UNAME='start_day')
					    start_day_label=WIDGET_LABEL(start_time_base,VALUe='日')


	   				end_time_base=WIDGET_BASE(select_time_base2,ROW=1,/ALIGN_CENTER)

	   					stime_text_label=WIDGET_LABEL(end_time_base,VALUE='结束时间:')


						end_month=strtrim(indgen(12)+1,2)
						end_month_droplist=WIDGET_DROPLIST(end_time_base,$
						                                      VALUE=end_month,$
						                                      UNAME='end_month')
						WIDGET_CONTROL,end_month_droplist,SET_DROPLIST_SELECT=11
						end_month_label=WIDGET_LABEL(end_time_base,VALUE='月')

						end_day=strtrim(indgen(31)+1,2)
						ptr_end_day=PTR_NEW(end_day)
						end_day_droplist=WIDGET_DROPLIST(end_time_base,$
						                                    VALUE=end_day,$
						                                    UNAME='end_day')

	   					WIDGET_CONTROL,end_day_droplist,SET_DROPLIST_SELECT=30
	                    end_day_label=WIDGET_LABEL(end_time_base,VALUE='日')


	       ;space_base=widget_base(time_base,xsize=80)

	   		   select_time_base3=WIDGET_BASE(show_base,ROW=1,XSIZE=500,YSIZE=27,MAP=0)

	   				day_label=WIDGET_LABEL(select_time_base3,VALUE=' 从1月1日到12月31日',SCR_XSIZE=150)

	   		   		WIDGET_CONTROL,day_droplist,SET_DROPLIST_SELECT=4

	   				;space_base=widget_base(time_base,xsize=80)


	   ;========================================================
	   ROW_LABEL=strtrim(indgen(10),2)

	   mission_table=WIDGET_TABLE(xianshi_base,FRAME=1,YSIZE=10,$
	                              X_SCROLL_SIZE=5,Y_SCROLL_SIZE=10,$
	                              ALIGNMENT=0,$
	                              ROW_LABELS=ROW_LABEL,$
	                              COLUMN_WIDTHS=[150,30,30,48,150],$
	                              /ALIGN_CENTER,$
	                              COLUMN_LABELS=['任务名称','月份', '日',$
	                                            '耗时/天','备注'],$
	                              /RESIZEABLE_COLUMNS,UNAME='mission_table')

	   ;'关闭'和'帮助'的base=============================================
	   elsebase=WIDGET_BASE(xianshi_base,ROW=1,$
	                        SPACE=100,XOFFSET=xoffset,$
							XSIZE=509,$
	                        /ALIGN_CENTER, $
	                        XPAD=38, $
	                        MAP=1,FRAME=1)

	   buttsize=75


	   		search_butt=WIDGET_BUTTON(elsebase,XSIZE=buttsize,VALUE='查询',FRAME=0,$
	       							UNAME='search_butt')

	   		help_butt=WIDGET_BUTTON(elsebase,XSIZE=buttsize,VALUE='帮助',FRAME=0,$
	                               UNAME='help_butt')

	   		colse_butt=WIDGET_BUTTON(elsebase,XSIZE=buttsize,VALUE='关闭',FRAME=0,$
	                                UNAME='colse_butt')

   ;===================================================================

   WIDGET_CONTROL,xianshi_base,/REALIZE
   WIDGET_CONTROL,search_butt,/INPUT_FOCUS

   ;=======================================================================


   info={ select_time_base1:select_time_base1,$
	      select_time_base2:select_time_base2,$
	      select_time_base3:select_time_base3,$
	      buttopt1:buttopt1,$
	      buttopt2:buttopt2,$
	      buttopt3:buttopt3,$
	      mission_table:mission_table,$
	      DayList:DayList,$
	      Day:7,$
	      ptr_SDay_list:ptr_start_day,   $
	      SMonth_list:start_month,   $
	      ptr_EDay_list:ptr_end_day,  $
	      EMonth_list:end_month,   $
	      start_day_droplist:start_day_droplist,   $
	      end_day_droplist:end_day_droplist,   $
	      SDay:1,   $
	      SMonth:1,   $
	      EDay:31,  $
	      EMonth:12 $
	    }

   pState = PTR_NEW(info,/no_copy)

   WIDGET_CONTROL,xianshi_base,SET_UVALUE=pState
   WIDGET_CONTROL,buttopt1,/SET_BUTTON

   XMANAGER,'GL_xianshi',xianshi_base,/NO_BLOCK, CLEANUP='SD_Connect_cleanup'

END