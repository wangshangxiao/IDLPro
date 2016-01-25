; This file provides an example of the use of an ActiveX control
; within an IDL widget hierarchy. The process of incorporating
; ActiveX controls into IDL widget hierarchies is discussed in
; detail in the _External Development Guide_.
;
; Note: mscal.ocx must be registered on your Windows computer
; before this example will execute. See the _External Development
; Guide_ for details.

;---
PRO ActiveXCal_event, ev
    ; The ID field of the event structure will always contain the
    ; ID of the ActiveX widget, since this is the only widget that
    ; generates events. The value of the ActiveX widget is a reference
    ; to the IDLcomActiveX object that encapsulates the ActiveX control.

	uname = WIDGET_INFO(ev.ID, /UNAME)
	WIDGET_CONTROL, ev.TOP, GET_UVALUE = state

	if uname eq 'calendar'	then begin
	    WIDGET_CONTROL, ev.ID, GET_VALUE = oCal

	    ; The user value of the top-level base widget is an anonymous
	    ; structure that contains the widget IDs of the label widgets.

	    ; Use the IDLcomActiveX object's GetProperty method to retrieve
	    ; the current date information.
	    ocal->GetProperty, month=month, day=day, year=year

		WIDGET_CONTROL, state.month, GET_VALUE = omonth
	   WIDGET_CONTROL, state.day, GET_VALUE = oday
	   WIDGET_CONTROL, state.year, GET_VALUE = oyear

		if year eq 0 then year = oyear
		if month eq 0 then month = omonth
		if day eq 0 then day = oday
 ; Set the values of the label widgets to reflect the current date.
	    WIDGET_CONTROL, state.month, SET_VALUE = STRTRIM(month,2)
	    WIDGET_CONTROL, state.day, SET_VALUE = STRTRIM(day,2)
	    WIDGET_CONTROL, state.year, SET_VALUE = STRTRIM(year,2)

	    ; Free any dynamic portions of the event structure.
	    HEAP_FREE, ev
	endif else if uname eq 'OK' then begin
		WIDGET_CONTROL, state.month, GET_VALUE = month
	    WIDGET_CONTROL, state.day, GET_VALUE = day
	    WIDGET_CONTROL, state.year, GET_VALUE = year
		widget_control, state.staff.text_id, set_value=STRTRIM(year,2)+'-'+STRTRIM(month,2)+'-'+STRTRIM(day,2)
		PSTATE = state.staff.pointer

		case state.staff.detail of
		'CMD_pick_date':begin
			(*PSTATE).YEAR = year
			(*PSTATE).MONTH = month
			(*PSTATE).DAY = day
		end
		'CMD_pick_date_sta':begin
			(*PSTATE).YEAR = year
			(*PSTATE).MONTH = month
			(*PSTATE).DAY = day

			defaultnames_zsprosta,{ID:ev.id, TOP:(*PSTATE).widget_top}
		end
		'CMD_pick_date_sta_rt':begin
			(*PSTATE).YEAR = year
			(*PSTATE).MONTH = month
			(*PSTATE).DAY = day

			defaultnames_zsrtsta,{ID:ev.id, TOP:(*PSTATE).widget_top}
		end
		'CMD_pick_date_s':begin
			(*PSTATE).YEAR_START = year
		  	(*PSTATE).MONTH_START= month
		  	(*PSTATE).DAY_START = day
		end
		'CMD_pick_date_e':begin
			(*PSTATE).YEAR_END = year
		  	(*PSTATE).MONTH_END= month
		  	(*PSTATE).DAY_END = day
		end
		'CMD_pick_date_classify':begin
			(*PSTATE).YEAR = year
			(*PSTATE).MONTH = month
			(*PSTATE).DAY = day

			defaultnames_zsrtclf,{ID:ev.id, TOP:(*PSTATE).widget_top}
		end
		'CMD_pick_date_jpg':begin
			(*PSTATE).YEAR = year
			(*PSTATE).MONTH = month
			(*PSTATE).DAY = day

			defaultnames_zsjpg,{ID:ev.id, TOP:(*PSTATE).widget_top}
		end
		'CMD_pick_date_s_jpg':begin
			(*PSTATE).YEAR_START_LY_PRO		=	year-1
			(*PSTATE).MONTH_START_LY_PRO	=	month
			(*PSTATE).day_START_LY_PRO		=	day

			(*PSTATE).YEAR_START_TY_PRO		=	year
			(*PSTATE).MONTH_START_TY_PRO	=	month
			(*PSTATE).day_START_TY_PRO		=	day
		end
		'CMD_pick_date_e_jpg':begin
			(*PSTATE).YEAR_END_LY_PRO  		=	year-1
			(*PSTATE).MONTH_END_LY_PRO		=	month
			(*PSTATE).day_END_LY_PRO		=	day

			(*PSTATE).YEAR_END_TY_PRO  		=	year
			(*PSTATE).MONTH_END_TY_PRO		=	month
			(*PSTATE).day_END_TY_PRO		=	day
		end
		'CMD_pick_date_s_fz':begin
			(*PSTATE).StartYear = strtrim(year, 2)
			(*PSTATE).StartMonth = strtrim(month,2)
			(*PSTATE).StartDay = strtrim(day,2)
		end
		'CMD_pick_date_e_fz':begin
			(*PSTATE).EndYear = strtrim(year, 2)
			(*PSTATE).EndMonth = strtrim(month,2)
			(*PSTATE).EndDay = strtrim(day,2)
		end
		'YMD_BUTTON':begin
			(*PSTATE).YEAR = strtrim(year, 2)
			(*PSTATE).MONTH = strtrim(month,2)
			(*PSTATE).DAY = strtrim(day,2)
		end
		endcase

		CLOSE,/ALL
		HEAP_FREE, ev
		widget_control,ev.top,/destroy
	endif else if uname eq 'Cancel' then begin
		CLOSE,/ALL
		HEAP_FREE, ev
		widget_control,ev.top,/destroy
	endif
END
;---


;---
pro ActiveXCal, event
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	widget_control, event.id, get_uvalue=staff
	widget_control, event.id, TLB_GET_OFFSET=position
    ; Create a top-level base widget
    wBase = WIDGET_BASE(COLUMN = 1, SCR_XSIZE = 400, $
       TITLE='选择日期', TLB_FRAME_ATTR=1, xoffset=position[0],yoffset=position[1], GROUP_LEADER=event.top,/modal)

	; Instantiate the ActiveX calendar control in a widget.
    wAx=WIDGET_ACTIVEX(wBase, UNAME='calendar', $
        '{8E27C92B-1264-101C-8A2F-040224009C02}')

    ; Create base widgets to hold labels for the month, day,
    ; and year values, and set the values to initial values.
	CURRENT_TIME = STRTRIM(STRING(BIN_DATE(SYSTIME())),2)
	wSubBase = WIDGET_BASE(wBase, /ROW)
    wVoid = WIDGET_LABEL(wSubBase, VALUE = '  日期: ')
    wYear = WIDGET_LABEL(wSubBase, VALUE = CURRENT_TIME[0],/DYNAMIC_RESIZE)
;    wSubBase = WIDGET_BASE(wBase, /ROW)
    wVoid = WIDGET_LABEL(wSubBase, value = '-')
    wMonth = WIDGET_LABEL(wSubBase, value = CURRENT_TIME[1],/DYNAMIC_RESIZE)
;    wSubBase = WIDGET_BASE(wBase, /ROW)
    wVoid = WIDGET_LABEL(wSubBase, VALUE = '-')
    wDay = WIDGET_LABEL(wSubBase, VALUE = CURRENT_TIME[2],/DYNAMIC_RESIZE)

	wSubBase = WIDGET_BASE(wSubBase)
	CMD_PREFERENCE_OK = Widget_Button(wSubBase, VALUE='确定', UNAME='OK', $
	   SCR_XSIZE=55, SCR_YSIZE=20, /ALIGN_CENTER, xoffset=120 )
	CMD_PREFERENCE_CANCEL = Widget_Button(wSubBase, VALUE='取消', UNAME='Cancel', $
	   SCR_XSIZE=55, SCR_YSIZE=20, /ALIGN_CENTER, xoffset=200 )

    ; Realize the widget hierarchy.
    WIDGET_CONTROL, wBase, /REALIZE
    Widget_Control,CMD_PREFERENCE_OK,/INPUT_FOCUS

    ; Set the user value of the top-level base widget to
    ; an anonymous structure that contains the widget IDs
    ; of the label widgets.
    WIDGET_CONTROL, wBase, $
       SET_UVALUE = {month:wMonth, day:wDay, year:wYear, staff:staff}

    ; The value of an ActiveX widget is an object reference to
    ; the IDLcomActiveX object that encapsulates the ActiveX
    ; control.
    WIDGET_CONTROL, wAx, GET_VALUE = oAx

	widget_control,staff.text_id,get_value=initialtime
	if initialtime ne '' then begin
		time_text = strsplit(initialtime, '-', /EXTRACT)
		time = FIX(time_text)
		oAx->SetProperty, month=time[1], day=time[2], year=time[0]
	end
    ; Use the IDLcomActiveX object's GetProperty method to retrieve
    ; the current date information.
    oAx->GetProperty, month=month, day=day, year=year

    ; Set the values of the label widgets to reflect the selected date.
	WIDGET_CONTROL, wYear, SET_VALUE=STRTRIM(year, 2)
    WIDGET_CONTROL, wMonth, SET_VALUE=STRTRIM(month, 2)
    WIDGET_CONTROL, wDay, SET_VALUE=STRTRIM(day, 2)

;	if arg_present(c_year) eq 1 then $
;		c_year = STRTRIM(year, 2)
;	if arg_present(c_month) eq 1 then $
;		c_month = STRTRIM(month, 2)
;	if arg_present(c_day) eq 1 then $
;		c_day = STRTRIM(day, 2)

    ; Call XMANAGER to manage the widgets.
    XMANAGER, 'ActiveXCal', wBase
END
;---