;����ʱ�䣺2006.08.21
;������Ա��������
;������ʾ����ʾ�����е������ָ��ʱ��β�ѯ����������ֵĽ���


pro GL_XIANSHI_PERIOD_TIME_EVENT,event

   wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
              widget_info(Event.id, /tree_root) : event.id)
   wWidget =  Event.top

    WIDGET_CONTROL,EVENT.TOP,GET_UVALUE = PSTATE

     y=(bin_date())[0]
           if (y mod 100 ne 0 and y mod 4 eq 0) or (y mod 400 eq 0) then begin
              flag=1	;1��ʾΪ����
           endif else begin
              flag=0
           endelse

	start_day_31=strtrim(indgen(31)+1,2)
	start_day_30=strtrim(indgen(30)+1,2)
	start_day_29=strtrim(indgen(29)+1,2)
	start_day_28=strtrim(indgen(28)+1,2)

   CASE wTarget OF
;
;;;-----------------------------------------------------------------------------
      Widget_Info(wWidget,FIND_BY_UNAME='start_month'):    BEGIN
          (*PSTATE).SMonth = (*PSTATE).SMonth_list[event.index]

			if ((*PSTATE).SMonth eq 2) then begin
              if flag eq 1 then begin
              	widget_control,(*PSTATE).start_day_droplist,SET_VALUE=start_day_29, $
              				SET_DROPLIST_SELECT=0
              endif else begin
                 widget_control,(*PSTATE).start_day_droplist,SET_VALUE=start_day_28, $
              				SET_DROPLIST_SELECT=0
              endelse
           endif else begin
               if ((*PSTATE).SMonth eq 4) or ((*PSTATE).SMonth eq 6)      $
                  or ((*PSTATE).SMonth eq 9) or ((*PSTATE).SMonth eq 11) then begin
                    widget_control,(*PSTATE).start_day_droplist,SET_VALUE=start_day_30, $
              				SET_DROPLIST_SELECT=0
               endif else begin
               		widget_control,(*PSTATE).start_day_droplist,SET_VALUE=start_day_31, $
              				SET_DROPLIST_SELECT=0
               endelse
           endelse

			(*PSTATE).SDay=1

			widget_control,(*PSTATE).start_day_droplist,get_value=temp
			if ptr_valid((*PSTATE).ptr_SDay_list) eq 1 then ptr_free,(*PSTATE).ptr_SDay_list
			(*PSTATE).ptr_SDay_list=ptr_new(temp)

          end

;;-----------------------------------------------------------------------------
      Widget_Info(wWidget,FIND_BY_UNAME='start_day'):  BEGIN
          (*PSTATE).SDay = (*((*PSTATE).ptr_SDay_list))[event.index]

           end

;-----------------------------------------------------------------------------
      Widget_Info(wWidget,FIND_BY_UNAME='end_month'):  BEGIN
          (*PSTATE).EMonth = (*PSTATE).EMonth_list[event.index]


			if ((*PSTATE).EMonth eq 2) then begin
              if flag eq 1 then begin
              	widget_control,(*PSTATE).end_day_droplist,SET_VALUE=start_day_29, $
              				SET_DROPLIST_SELECT=0
              endif else begin
                 widget_control,(*PSTATE).end_day_droplist,SET_VALUE=start_day_28, $
              				SET_DROPLIST_SELECT=0
              endelse
           endif else begin
               if ((*PSTATE).EMonth eq 4) or ((*PSTATE).EMonth eq 6)      $
                  or ((*PSTATE).EMonth eq 9) or ((*PSTATE).EMonth eq 11) then begin
                    widget_control,(*PSTATE).end_day_droplist,SET_VALUE=start_day_30, $
              				SET_DROPLIST_SELECT=0
               endif else begin
               		widget_control,(*PSTATE).end_day_droplist,SET_VALUE=start_day_31, $
              				SET_DROPLIST_SELECT=0
               endelse
           endelse

			(*PSTATE).EDay=1

			widget_control,(*PSTATE).end_day_droplist,get_value=temp
			if ptr_valid((*PSTATE).ptr_EDay_list) eq 1 then ptr_free,(*PSTATE).ptr_EDay_list
			(*PSTATE).ptr_EDay_list=ptr_new(temp)


          end
;
;;-----------------------------------------------------------------------------
      Widget_Info(wWidget,FIND_BY_UNAME='end_day'):    BEGIN
          (*PSTATE).EDay = (*((*PSTATE).ptr_EDay_list))[event.index]

		 end
;;-----------------------------------------------------------------------------
;
      Widget_Info(wWidget,FIND_BY_UNAME='search_butt'): BEGIN
        ;(1)��ȡ���ݿ�����
        COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

        DBCO=DBobj

;print,'Start_month',(*PSTATE).SMonth
;print,'start_day',(*PSTATE).SDay
;print,'end_month',(*PSTATE).EMonth
;print,'end_day',(*PSTATE).EDay
;print,'---------------------------------'

      STime=(*PSTATE).SMonth*31+(*PSTATE).SDay
      ETime=(*PSTATE).EMonth*31+(*PSTATE).EDay
      if STime GT ETime then begin
          temp=dialog_message('����ʱ�䲻����',/error,title='����')
          temp[*,*]=''
          widget_control,(*PSTATE).mission_table,set_value=temp[*,*]
          return
      endif

;		ProID=strmid(PROVINCE_CODE,0,2)

        ;(2)��ȡ����
;        SQL='SELECT business_name,month_start,day_start,kuadu,note,period FROM '+ProID+'business_time_table'
        SQL='SELECT business_name,month_start,day_start,kuadu,note,period FROM business_time_table'
        SQL=SQL+' where ('
        SQL=SQL+'(month_start*31+day_start between '+strtrim(STime,2)
        SQL=SQL+' and '+strtrim(ETime,2)+')'
        SQL=SQL+' OR period=1)'
        SQL=SQL+' order by month_start,day_start'

	CATCH, Error_status               ;��ȡ����.
     IF Error_status NE 0 THEN BEGIN
        infomation=DIALOG_MESSAGE(['�������´���',[!ERROR_STATE.MSG],'������������Ƿ���ȫ.'],TITLE='����',/ERROR)
        CATCH, /CANCEL
        RETURN                    ;������������,������ִ����������,�Ի���ִ���.
     ENDIF

        ;***************************************************************************
        ;��ȡ���ݳ���
        ;��ȡ���ݼ�¼�ĸ���
       RecordNumOBJ = OBJ_NEW('IDLDBRECORDSET',DBCO,SQL='select count(*) from ('+Sql+')')
       RecordNum = RecordNumOBJ->GETFIELD(0)
       NUM=RecordNum

       Obj_Destroy,RecordNumOBJ


        IF NUM LE 0 then begin
            temp=dialog_message('δ���ַ�������������',/information,title='��ʾ')
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


        ;(3)�����ʾ
        widget_control,(*PSTATE).mission_table,table_ysize=NUM

		ROW_LABELS_new=strtrim(indgen(NUM),2)
		widget_control,(*PSTATE).mission_table,ROW_LABELS=ROW_LABELS_new

        widget_control,(*PSTATE).mission_table,set_value=arr_data

      END

       Widget_Info(wWidget, FIND_BY_UNAME='colse_butt'):begin
         widget_control,event.top,/destroy
       end

       widget_info(wWidget, FIND_BY_UNAME='help_butt'):begin
	       if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '�����ѯ', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('�Ҳ��������ĵ�',title='����')
			endelse
		end
;         ONLINE_HELP, '�����ѯ', BOOK='HELP\HELP.chm', /FULL_PATH
;;            temp=dialog_message('ϵͳ��û�а���')
;       end

      ELSE:

   ENDCASE


end



;****************************************************************
pro GL_xianshi_period_time,GROUP_LEADER=wGroup

   IF ( XREGISTERED('GL_xianshi_period_time') NE 0 ) THEN RETURN

   device,get_screen_size=screen_size
   xoffset=screen_size[0]/5
   yoffset=screen_size[1]/5

   COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
   xianshi_base=widget_base(GROUP_LEADER=BASE_TOP,column=1,title='�����ѯ::��ָ��ʱ��β�ѯ',xoffset=xoffset+30,$
                   yoffset=220,tlb_frame_attr=1,space=2,$
                   xsize=519,ysize=299,/align_center)


   ;ѡ���ѯʱ���base====================================
   time_base=widget_base(xianshi_base,row=1,frame=1,xsize=509,$
                         space=55,/align_center)

       space_base=widget_base(time_base)

       start_time_base=widget_base(time_base,row=1,/align_center)

           stime_text_label=widget_label(start_time_base,value='��ʼʱ��:')


           start_month=strtrim(indgen(12)+1,2)
           start_month_droplist=widget_droplist(start_time_base,$
                                                value=start_month,$
                                                uname='start_month')
           start_month_label=widget_label(start_time_base,value='��')


           ;dayӦ���ݲ�ͬ���·�������ͬ������Ƕ��·�Ӧ��ֻ��28���29��
           start_day=strtrim(indgen(31)+1,2)
           ptr_start_day=ptr_new(start_day)
           start_day_droplist=widget_droplist(start_time_base,$
                                              value=start_day,$
                                              uname='start_day')
           start_day_label=widget_label(start_time_base,value='��')


       end_time_base=widget_base(time_base,row=1,/align_center)

           stime_text_label=widget_label(end_time_base,value='����ʱ��:')


           end_month=strtrim(indgen(12)+1,2)
           end_month_droplist=widget_droplist(end_time_base,$
                                              value=end_month,$
                                              uname='end_month')
           widget_control,end_month_droplist,SET_DROPLIST_SELECT=11
           end_month_label=widget_label(end_time_base,value='��')


           ;ͬ��
           end_day=strtrim(indgen(31)+1,2)
           ptr_end_day=ptr_new(end_day)
           end_day_droplist=widget_droplist(end_time_base,$
                                            value=end_day,$
                                            uname='end_day')
           widget_control,end_day_droplist,SET_DROPLIST_SELECT=30
           end_day_label=widget_label(end_time_base,value='��')


;��ѯ����б�=====================================================

   ROW_LABEL=strtrim(indgen(50),2)

   mission_table=widget_table(xianshi_base,frame=1,$
                              x_scroll_size=5,y_scroll_size=10,$
                              ALIGNMENT=0,$
                              ROW_LABELS=ROW_LABEL,$
                              COLUMN_WIDTHS=[150,30,30,48,150],$
                              /align_center,$
                              column_labels=['��������','�·�', '��',$
                                            '��ʱ/��','��ע'],$
                              /RESIZEABLE_COLUMNS,uname='mission_table')


   ;'�ر�'��'����'��base=============================================

   elsebase=widget_base(xianshi_base,row=1,space=100,$
                        xsize=509,$
                        frame=1,XPAD=36,/align_center)

       buttsize=75


       search_butt=widget_button(elsebase,xsize=buttsize,value='��ѯ',frame=0,$
                         uname='search_butt')

       help_butt=widget_button(elsebase,xsize=buttsize,value='����',frame=0,$
                               uname='help_butt')

		colse_butt=widget_button(elsebase,xsize=buttsize,value='�ر�',frame=0,$
                                uname='colse_butt')



;   ;===========================================================

   widget_control,xianshi_base,/realize
   WIDGET_CONTROL,colse_butt,/INPUT_FOCUS

   STATE = { $
     mission_table   : mission_table ,  $
        ptr_SDay_list     :   ptr_start_day,   $
        SMonth_list     :   start_month,   $
        ptr_EDay_list     :   ptr_end_day,  $
        EMonth_list     :   end_month,   $
        start_day_droplist     :   start_day_droplist,   $
        end_day_droplist     :   end_day_droplist,   $
        SDay         :    1,   $
        SMonth       :   1,   $
        EDay         :   31,  $
        EMonth       :   12   $
        }

    PSTATE = PTR_NEW(STATE, /NO_COPY)
    WIDGET_CONTROL, xianshi_base, SET_UVALUE=PSTATE

   xmanager,'GL_xianshi_period_time',xianshi_base,/no_block, cleanup='SD_Connect_cleanup'
;
;COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
;  GL_xianshi_period_time,GROUP_LEADER=BASE_TOP
;

end