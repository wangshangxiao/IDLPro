;2006.08.09
; �ɼ̻�
;����ʵʱ����в�ֵ�ּ��ļ���,���ѽ����ʾ���û�
;��ԭ�в�ֵ�ּ�ģ��Ļ����Ͻ����޸�
;-----------------------------------------------------------------
;
pro BASE_CLASSIFY_PARAMETER_SETUP_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  WIDGET_CONTROL,Event.top,get_UVALUE = pstate
  wWidget =  Event.top

  case wTarget of

    Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_PARAMETER_OK'): begin
    	 WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_1_MAX,	get_value=LEVEL_1_MAX
     	 LEVEL_1_MAX=FLOAT(LEVEL_1_MAX[0])
	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_1_MIN,	get_value=LEVEL_1_MIN
	     	LEVEL_1_MIN=FLOAT(LEVEL_1_MIN[0])
	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_2_MAX,	get_value=LEVEL_2_MAX
	     	LEVEL_2_MAX=FLOAT(LEVEL_2_MAX[0])
	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_2_MIN,	get_value=LEVEL_2_MIN
	     	LEVEL_2_MIN=FLOAT(LEVEL_2_MIN[0])
	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_3_MAX,	get_value=LEVEL_3_MAX
	     	LEVEL_3_MAX=FLOAT(LEVEL_3_MAX[0])
	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_3_MIN,	get_value=LEVEL_3_MIN
	     	LEVEL_3_MIN=FLOAT(LEVEL_3_MIN[0])
	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_4_MAX,	get_value=LEVEL_4_MAX
	     	LEVEL_4_MAX=FLOAT(LEVEL_4_MAX[0])
	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_4_MIN,	get_value=LEVEL_4_MIN
	     	LEVEL_4_MIN=FLOAT(LEVEL_4_MIN[0])
	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_5_MAX,	get_value=LEVEL_5_MAX
	     	LEVEL_5_MAX=FLOAT(LEVEL_5_MAX[0])
	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_5_MIN,	get_value=LEVEL_5_MIN
	     	LEVEL_5_MIN=FLOAT(LEVEL_5_MIN[0])
	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_6_MAX,	get_value=LEVEL_6_MAX
	     	LEVEL_6_MAX=FLOAT(LEVEL_6_MAX[0])
	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_6_MIN,	get_value=LEVEL_6_MIN
	     	LEVEL_6_MIN=FLOAT(LEVEL_6_MIN[0])
	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_7_MAX,	get_value=LEVEL_7_MAX
	     	LEVEL_7_MAX=FLOAT(LEVEL_7_MAX[0])
	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_7_MIN,	get_value=LEVEL_7_MIN
	     	LEVEL_7_MIN=FLOAT(LEVEL_7_MIN[0])
	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_8_MAX,	get_value=LEVEL_8_MAX
	     	LEVEL_8_MAX=FLOAT(LEVEL_8_MAX[0])
	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_8_MIN,	get_value=LEVEL_8_MIN
	     	LEVEL_8_MIN=FLOAT(LEVEL_8_MIN[0])
	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_9_MAX,	get_value=LEVEL_9_MAX
	     	LEVEL_9_MAX=FLOAT(LEVEL_9_MAX[0])
	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_LEVEL_9_MIN,	get_value=LEVEL_9_MIN
	     	LEVEL_9_MIN=FLOAT(LEVEL_9_MIN[0])

	     ERROR_FLAG=0

	     ;�ж��������ֵ�Ƿ����
	     IF(LEVEL_1_MAX NE LEVEL_2_MIN) THEN BEGIN
	     	ERROR_FLAG=1
	     ENDIF
	     IF(LEVEL_2_MAX NE LEVEL_3_MIN) THEN BEGIN
	     	ERROR_FLAG=1
	     ENDIF
	     IF(LEVEL_3_MAX NE LEVEL_4_MIN) THEN BEGIN
	     	ERROR_FLAG=1
	     ENDIF
	     IF(LEVEL_4_MAX NE LEVEL_5_MIN) THEN BEGIN
	     	ERROR_FLAG=1
	     ENDIF
	     IF(LEVEL_5_MAX NE LEVEL_6_MIN) THEN BEGIN
	     	ERROR_FLAG=1
	     ENDIF
	     IF(LEVEL_6_MAX NE LEVEL_7_MIN) THEN BEGIN
	       ERROR_FLAG=1
	     ENDIF
	     IF(LEVEL_7_MAX NE LEVEL_8_MIN) THEN BEGIN
	     	ERROR_FLAG=1
	     ENDIF
	     IF(LEVEL_8_MAX NE LEVEL_9_MIN) THEN BEGIN
	     	ERROR_FLAG=1
	     ENDIF
	     ;
	     IF(LEVEL_1_MIN GT LEVEL_1_MAX) THEN BEGIN
	     	ERROR_FLAG=1
	     ENDIF
	     IF(LEVEL_2_MIN GT LEVEL_2_MAX) THEN BEGIN
	     	ERROR_FLAG=1
	     ENDIF
	     IF(LEVEL_3_MIN GT LEVEL_3_MAX) THEN BEGIN
	     	ERROR_FLAG=1
	     ENDIF
	     IF(LEVEL_4_MIN GT LEVEL_4_MAX) THEN BEGIN
	     	ERROR_FLAG=1
	     ENDIF
	     IF(LEVEL_5_MIN GT LEVEL_5_MAX) THEN BEGIN
	     	ERROR_FLAG=1
	     ENDIF
	     IF(LEVEL_6_MIN GT LEVEL_6_MAX) THEN BEGIN
	     	ERROR_FLAG=1
	     ENDIF
	     IF(LEVEL_7_MIN GT LEVEL_7_MAX) THEN BEGIN
	     	ERROR_FLAG=1
	     ENDIF
	     IF(LEVEL_8_MIN GT LEVEL_8_MAX) THEN BEGIN
	     	ERROR_FLAG=1
	     ENDIF
	     IF(LEVEL_9_MIN GT LEVEL_9_MAX) THEN BEGIN
	     	ERROR_FLAG=1
	     ENDIF
		 IF ERROR_FLAG EQ 1 THEN BEGIN
		 	msg=DIALOG_MESSAGE('���������Ĳ���ֵ!',TITLE='��ʾ',/INFORMATION)
	        CLOSE,/all
	        RETURN
	     ENDIF

	     P=(*PSTATE).PSTATE_F
		 (*P).PARAMETER_SEP[0]=LEVEL_1_MIN
		 (*P).PARAMETER_SEP[1]=LEVEL_2_MIN
		 (*P).PARAMETER_SEP[2]=LEVEL_3_MIN
		 (*P).PARAMETER_SEP[3]=LEVEL_4_MIN
		 (*P).PARAMETER_SEP[4]=LEVEL_5_MIN
		 (*P).PARAMETER_SEP[5]=LEVEL_6_MIN
		 (*P).PARAMETER_SEP[6]=LEVEL_7_MIN
		 (*P).PARAMETER_SEP[7]=LEVEL_8_MIN
		 (*P).PARAMETER_SEP[8]=LEVEL_9_MIN
		 (*P).PARAMETER_SEP[9]=LEVEL_9_MAX

	     CLOSE,/all
	     WIDGET_CONTROL, event.top, /destroy
	     RETURN
    end
    Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_PARAMETER_CANCEL'): begin
    	CLOSE,/all
     	WIDGET_CONTROL, event.top, /destroy
    end
    else:
  endcase

end

pro zs_PARAMETER_SETUP_clean_up, id
	widget_control, id, get_uvalue=pstate
	ptr_free,pstate
end

pro BASE_CLASSIFY_PARAMETER_SETUP, PSTATE_F,BASE

  ;Resolve_Routine, 'CLASSIFY_PARAMETER_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

  BASE_CLASSIFY_PARAMETER_SETUP = Widget_Base( GROUP_LEADER=BASE,  $
      UNAME='BASE_CLASSIFY_PARAMETER_SETUP' ,/MODAL,XOFFSET=400 ,YOFFSET=200  $
      ,SCR_XSIZE=284 ,SCR_YSIZE=348 ,TITLE='��������' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR=1)

  BASE_BUTTON_PARAMETER = Widget_Base(BASE_CLASSIFY_PARAMETER_SETUP,  $
      UNAME='BASE_BUTTON_PARAMETER' ,FRAME=1 ,XOFFSET=7 ,YOFFSET=275  $
      ,SCR_XSIZE=264 ,SCR_YSIZE=36 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  CMD_CLASSIFY_PARAMETER_OK = Widget_Button(BASE_BUTTON_PARAMETER,  $
      UNAME='CMD_CLASSIFY_PARAMETER_OK' ,XOFFSET=22 ,YOFFSET=7  $
      ,SCR_XSIZE=80 ,SCR_YSIZE=22 ,/ALIGN_CENTER ,VALUE='ȷ��')


  CMD_CLASSIFY_PARAMETER_CANCEL =  $
      Widget_Button(BASE_BUTTON_PARAMETER,  $
      UNAME='CMD_CLASSIFY_PARAMETER_CANCEL' ,XOFFSET=160 ,YOFFSET=7  $
      ,SCR_XSIZE=80 ,SCR_YSIZE=22  ,/ALIGN_CENTER ,VALUE='�ر�')


  BASE_CLASSIFY_PARAMETER = Widget_Base(BASE_CLASSIFY_PARAMETER_SETUP,  $
      UNAME='BASE_CLASSIFY_PARAMETER' ,FRAME=1 ,XOFFSET=7 ,YOFFSET=8  $
      ,SCR_XSIZE=264 ,SCR_YSIZE=258 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  LBL_CLASSIFY_LEVEL_1_MAX = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_1_MAX' ,XOFFSET=140 ,YOFFSET=11  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='һ�����ޣ�')


  TXT_CLASSIFY_LEVEL_1_MAX = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_1_MAX' ,XOFFSET=202 ,YOFFSET=7  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[1],2)  $
      ,XSIZE=20 ,YSIZE=1)


  TXT_CLASSIFY_LEVEL_2_MAX = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_2_MAX' ,XOFFSET=202 ,YOFFSET=34  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[2],2)  $
      ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_LEVEL_2_MAX = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_2_MAX' ,XOFFSET=140 ,YOFFSET=38  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='�������ޣ�')


  TXT_CLASSIFY_LEVEL_3_MAX = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_3_MAX' ,XOFFSET=202 ,YOFFSET=61  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[3],2)  $
      ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_LEVEL_3_MAX = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_3_MAX' ,XOFFSET=140 ,YOFFSET=65  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='�������ޣ�')


  TXT_CLASSIFY_LEVEL_4_MAX = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_4_MAX' ,XOFFSET=202 ,YOFFSET=88  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[4],2)   $
      ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_LEVEL_4_MAX = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_4_MAX' ,XOFFSET=140 ,YOFFSET=92  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='�ļ����ޣ�')


  TXT_CLASSIFY_LEVEL_5_MAX = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_5_MAX' ,XOFFSET=202 ,YOFFSET=115  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[5],2)    $
      ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_LEVEL_5_MAX = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_5_MAX' ,XOFFSET=140 ,YOFFSET=119  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='�弶���ޣ�')


  TXT_CLASSIFY_LEVEL_6_MAX = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_6_MAX' ,XOFFSET=202 ,YOFFSET=142  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[6],2)  $
      ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_LEVEL_6_MAX = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_6_MAX' ,XOFFSET=140 ,YOFFSET=146  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='�������ޣ�')


  TXT_CLASSIFY_LEVEL_7_MAX = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_7_MAX' ,XOFFSET=202 ,YOFFSET=169  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[7],2)   $
      ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_LEVEL_7_MAX = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_7_MAX' ,XOFFSET=140 ,YOFFSET=173  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='�߼����ޣ�')


  TXT_CLASSIFY_LEVEL_8_MAX = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_8_MAX' ,XOFFSET=202 ,YOFFSET=196  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[8],2)    $
      ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_LEVEL_8_MAX = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_8_MAX' ,XOFFSET=140 ,YOFFSET=200  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='�˼����ޣ�')


  TXT_CLASSIFY_LEVEL_9_MAX = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_9_MAX' ,XOFFSET=202 ,YOFFSET=223  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[9],2)    $
      ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_LEVEL_9_MAX = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_9_MAX' ,XOFFSET=140 ,YOFFSET=227  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='�ż����ޣ�')


  TXT_CLASSIFY_LEVEL_9_MIN = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_9_MIN' ,XOFFSET=75 ,YOFFSET=223  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[8],2)    $
      ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_LEVEL_9_MIN = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_9_MIN' ,XOFFSET=10 ,YOFFSET=227  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='�ż����ޣ�')


  TXT_CLASSIFY_LEVEL_8_MIN = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_8_MIN' ,XOFFSET=75 ,YOFFSET=196  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[7],2)    $
      ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_LEVEL_8_MIN = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_8_MIN' ,XOFFSET=10 ,YOFFSET=200  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='�˼����ޣ�')


  TXT_CLASSIFY_LEVEL_7_MIN = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_7_MIN' ,XOFFSET=75 ,YOFFSET=169  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[6],2)    $
      ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_LEVEL_7_MIN = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_7_MIN' ,XOFFSET=10 ,YOFFSET=173  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='�߼����ޣ�')


  TXT_CLASSIFY_LEVEL_6_MIN = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_6_MIN' ,XOFFSET=75 ,YOFFSET=142  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[5],2)    $
      ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_LEVEL_6_MIN = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_6_MIN' ,XOFFSET=10 ,YOFFSET=146  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='�������ޣ�')


  TXT_CLASSIFY_LEVEL_5_MIN = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_5_MIN' ,XOFFSET=75 ,YOFFSET=115  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[4],2)    $
      ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_LEVEL_5_MIN = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_5_MIN' ,XOFFSET=10 ,YOFFSET=119  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='�弶���ޣ�')


  TXT_CLASSIFY_LEVEL_4_MIN = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_4_MIN' ,XOFFSET=75 ,YOFFSET=88  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[3],2)    $
      ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_LEVEL_4_MIN = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_4_MIN' ,XOFFSET=10 ,YOFFSET=92  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='�ļ����ޣ�')


  TXT_CLASSIFY_LEVEL_3_MIN = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_3_MIN' ,XOFFSET=75 ,YOFFSET=61  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[2],2)    $
      ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_LEVEL_3_MIN = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_3_MIN' ,XOFFSET=10 ,YOFFSET=65  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='�������ޣ�')


  TXT_CLASSIFY_LEVEL_2_MIN = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_2_MIN' ,XOFFSET=75 ,YOFFSET=34  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[1],2)    $
      ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_LEVEL_2_MIN = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_2_MIN' ,XOFFSET=10 ,YOFFSET=38  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='�������ޣ�')


  LBL_CLASSIFY_LEVEL_1_MIN = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LEVEL_1_MIN' ,XOFFSET=10 ,YOFFSET=11  $
      ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='һ�����ޣ�')


  TXT_CLASSIFY_LEVEL_1_MIN = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_LEVEL_1_MIN' ,XOFFSET=75 ,YOFFSET=7  $
      ,SCR_XSIZE=42 ,SCR_YSIZE=20 ,/EDITABLE ,VALUE=STRTRIM((*PSTATE_F).PARAMETER_SEP[0],2)    $
      ,XSIZE=20 ,YSIZE=1)

  STATE={	$
  			PSTATE_F			:	PSTATE_F			,$

  			TXT_CLASSIFY_LEVEL_1_MAX:TXT_CLASSIFY_LEVEL_1_MAX	,$
  			TXT_CLASSIFY_LEVEL_1_MIN:TXT_CLASSIFY_LEVEL_1_MIN	,$
  			TXT_CLASSIFY_LEVEL_2_MAX:TXT_CLASSIFY_LEVEL_2_MAX	,$
  			TXT_CLASSIFY_LEVEL_2_MIN:TXT_CLASSIFY_LEVEL_2_MIN	,$
  			TXT_CLASSIFY_LEVEL_3_MAX:TXT_CLASSIFY_LEVEL_3_MAX	,$
  			TXT_CLASSIFY_LEVEL_3_MIN:TXT_CLASSIFY_LEVEL_3_MIN	,$
  			TXT_CLASSIFY_LEVEL_4_MAX:TXT_CLASSIFY_LEVEL_4_MAX	,$
  			TXT_CLASSIFY_LEVEL_4_MIN:TXT_CLASSIFY_LEVEL_4_MIN	,$
  			TXT_CLASSIFY_LEVEL_5_MAX:TXT_CLASSIFY_LEVEL_5_MAX	,$
  			TXT_CLASSIFY_LEVEL_5_MIN:TXT_CLASSIFY_LEVEL_5_MIN	,$
  			TXT_CLASSIFY_LEVEL_6_MAX:TXT_CLASSIFY_LEVEL_6_MAX	,$
  			TXT_CLASSIFY_LEVEL_6_MIN:TXT_CLASSIFY_LEVEL_6_MIN	,$
  			TXT_CLASSIFY_LEVEL_7_MAX:TXT_CLASSIFY_LEVEL_7_MAX	,$
  			TXT_CLASSIFY_LEVEL_7_MIN:TXT_CLASSIFY_LEVEL_7_MIN	,$
  			TXT_CLASSIFY_LEVEL_8_MAX:TXT_CLASSIFY_LEVEL_8_MAX	,$
  			TXT_CLASSIFY_LEVEL_8_MIN:TXT_CLASSIFY_LEVEL_8_MIN	,$
  			TXT_CLASSIFY_LEVEL_9_MAX:TXT_CLASSIFY_LEVEL_9_MAX	,$
  			TXT_CLASSIFY_LEVEL_9_MIN:TXT_CLASSIFY_LEVEL_9_MIN	$
  			}

  PSTATE = PTR_NEW(STATE, /NO_COPY)
  WIDGET_CONTROL,BASE_CLASSIFY_PARAMETER_SETUP, SET_UVALUE=PSTATE

  Widget_Control, /REALIZE, BASE_CLASSIFY_PARAMETER_SETUP
  WIDGET_CONTROL,CMD_CLASSIFY_PARAMETER_CANCEL,/INPUT_FOCUS

  XManager, 'BASE_CLASSIFY_PARAMETER_SETUP', BASE_CLASSIFY_PARAMETER_SETUP, /NO_BLOCK, cleanup='zs_PARAMETER_SETUP_clean_up'
end
;
; Empty stub procedure used for autoloading.
;
pro PARAMETER_CLASSIFY, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  BASE_CLASSIFY_PARAMETER_SETUP,GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end
;-----------------------------------------------------------------


;-----------------------------------------------------------------
function CLASSIFY_MAIN,FILE_LY,FILE_TY,$
						CLOUD_LY,CLOUD_TY,$
						FILE_OUTPUT,CLOUD,$
						ARR_SEP


	 	LEVEL_1_MIN=ARR_SEP[0]
		 LEVEL_2_MIN=ARR_SEP[1]
		 LEVEL_3_MIN=ARR_SEP[2]
		 LEVEL_4_MIN=ARR_SEP[3]
		 LEVEL_5_MIN=ARR_SEP[4]
		 LEVEL_6_MIN=ARR_SEP[5]
		 LEVEL_7_MIN=ARR_SEP[6]
		 LEVEL_8_MIN=ARR_SEP[7]
		 LEVEL_9_MIN=ARR_SEP[8]

		 LEVEL_1_MAX=ARR_SEP[1]
		 LEVEL_2_MAX=ARR_SEP[2]
		 LEVEL_3_MAX=ARR_SEP[3]
		 LEVEL_4_MAX=ARR_SEP[4]
		 LEVEL_5_MAX=ARR_SEP[5]
		 LEVEL_6_MAX=ARR_SEP[6]
		 LEVEL_7_MAX=ARR_SEP[7]
		 LEVEL_8_MAX=ARR_SEP[8]
		 LEVEL_9_MAX=ARR_SEP[9]

	if file_test(FILE_LY) eq 0 then begin
		info = DIALOG_MESSAGE('�Ҳ����ļ�'+FILE_LY+',����·����!',TITLE='��ʾ')
		return, 0
	end
	if file_test(FILE_TY) eq 0 then begin
		info = DIALOG_MESSAGE('�Ҳ����ļ�'+FILE_TY+',����·����!',TITLE='��ʾ')
		return, 0
	end


	 ;�������ļ�����������ж�
	 ;Ҫ������
	 ;(1)�����ļ�Ҫ��ENVI��׼�ļ�
	 ;(2)��СҪһ��
	 ;(3)�ֱ���Ҫ��ͬ
	 ;��������⼸�������������ж�
	 IF CLOUD EQ 1 THEN BEGIN
		if file_test(CLOUD_TY) eq 0 then begin
			info = DIALOG_MESSAGE('�Ҳ����ļ�'+CLOUD_TY+',����·����!',TITLE='��ʾ')
			return, 0
		end
		if file_test(CLOUD_LY) eq 0 then begin
			info = DIALOG_MESSAGE('�Ҳ����ļ�'+CLOUD_LY+',����·����!',TITLE='��ʾ')
			return, 0
		end

		PRINT,'HERE 1'
		 FILE_INFO_TY=GET_IMAGE_INFO(FILE_TY)
		 FILE_INFO_LY=GET_IMAGE_INFO(FILE_LY)
		 CLOUD_INFO_TY=GET_IMAGE_INFO(CLOUD_TY)
		 CLOUD_INFO_LY=GET_IMAGE_INFO(CLOUD_LY)
		 IF (FILE_INFO_TY.FILE_RIGHT EQ 0) OR $
		 	(FILE_INFO_LY.FILE_RIGHT EQ 0) OR $
		 	(CLOUD_INFO_TY.FILE_RIGHT EQ 0) OR $
		 	(CLOUD_INFO_TY.FILE_RIGHT EQ 0) THEN BEGIN
		 	OK = DIALOG_MESSAGE('������ļ��д���',TITLE='��ʾ')
		 	RETURN,0
		 ENDIF

	 	 IF (FILE_INFO_TY.XSIZE NE FILE_INFO_LY.XSIZE) OR $
	 	 	(FILE_INFO_TY.YSIZE NE FILE_INFO_LY.YSIZE) OR $
		 	(CLOUD_INFO_TY.XSIZE NE CLOUD_INFO_TY.XSIZE) OR $
		 	(CLOUD_INFO_TY.YSIZE NE CLOUD_INFO_TY.YSIZE) OR $
		 	(FILE_INFO_TY.XSIZE NE CLOUD_INFO_TY.XSIZE) OR $
		 	(FILE_INFO_TY.YSIZE NE CLOUD_INFO_TY.YSIZE) THEN BEGIN
		 	OK = DIALOG_MESSAGE('�����ļ���С��һ��!',TITLE='��ʾ')
		 	RETURN,0
		 ENDIF

		 IF (FILE_INFO_TY.X_PIXELSIZE NE FILE_INFO_LY.X_PIXELSIZE) OR $
	 	 	(FILE_INFO_TY.Y_PIXELSIZE NE FILE_INFO_LY.Y_PIXELSIZE) OR $
		 	(CLOUD_INFO_TY.X_PIXELSIZE NE CLOUD_INFO_TY.X_PIXELSIZE) OR $
		 	(CLOUD_INFO_TY.Y_PIXELSIZE NE CLOUD_INFO_TY.Y_PIXELSIZE) OR $
		 	(FILE_INFO_TY.X_PIXELSIZE NE CLOUD_INFO_TY.X_PIXELSIZE) OR $
		 	(FILE_INFO_TY.Y_PIXELSIZE NE CLOUD_INFO_TY.Y_PIXELSIZE) THEN BEGIN
		 	OK = DIALOG_MESSAGE('�����ļ��ķֱ��ʲ�һ��!',TITLE='��ʾ')
		 	RETURN,0
		 ENDIF

		 IF (ABS(FILE_INFO_TY.STARTX-FILE_INFO_LY.STARTX) GT FILE_INFO_TY.X_PIXELSIZE) OR $
		 	(ABS(CLOUD_INFO_TY.STARTX-CLOUD_INFO_LY.STARTX) GT CLOUD_INFO_TY.X_PIXELSIZE) OR $
		 	(ABS(FILE_INFO_TY.STARTX-CLOUD_INFO_LY.STARTX) GT FILE_INFO_TY.X_PIXELSIZE) OR $
		 	(ABS(FILE_INFO_TY.STARTY-FILE_INFO_LY.STARTY) GT FILE_INFO_TY.Y_PIXELSIZE) OR $
		 	(ABS(CLOUD_INFO_TY.STARTY-CLOUD_INFO_LY.STARTY) GT CLOUD_INFO_TY.Y_PIXELSIZE) OR $
		 	(ABS(FILE_INFO_TY.STARTY-CLOUD_INFO_LY.STARTY) GT FILE_INFO_TY.Y_PIXELSIZE)  THEN BEGIN
		 	OK = DIALOG_MESSAGE('�����ļ��������겻һ��!',TITLE='��ʾ')
		 	RETURN,0
		 ENDIF



		 ;��ʼ�������ļ��Ķ�ȡ
		 INFILE=FILE_LY
		 Openr,Lun_FILE_LY	,FILE_LY	,/get_lun
		 Openr,Lun_FILE_TY	,FILE_TY	,/get_lun
		 Openr,Lun_FILE_CLOUD_LY	,CLOUD_LY	,/get_lun
		 Openr,Lun_FILE_CLOUD_TY	,CLOUD_TY	,/get_lun
		 Openw,Lun_FILE_OUTPUT		,FILE_OUTPUT	,/get_lun

		 ;����Ҫ������ȡ����������
		 ;�Գ���������޸�,һ�ζ�ȡһ�е�����
		 SAMPLES=FILE_INFO_TY.XSIZE
		 LINES=FILE_INFO_TY.YSIZE

		 ;�������ݵ����ͽ������ݶ�ȡ�����Ķ���
		 IF (FILE_INFO_TY.DATATYPE EQ 1) THEN BYTES_OF_DATA=1
		 case FILE_INFO_TY.DATATYPE of
	      1: begin
	             BYTES_OF_DATA=1
	         end
	      2: begin
	             BYTES_OF_DATA=2
	         end
	      4: begin
	             BYTES_OF_DATA=4
	         end
	      12:begin
	             BYTES_OF_DATA=2
	         end
	      13:begin
	             BYTES_OF_DATA=4
	         end
	      else:begin	;
	             print,'@@'+FILE_INFO_TY+':û�ж������������!'
	             return,0
	         end
	    ENDCASE

		 IF(BYTES_OF_DATA EQ 1) THEN BEGIN
		 	FILE_LY	=	BYTARR(SAMPLES)
		 	FILE_TY	=	BYTARR(SAMPLES)
		 	OUTPUT			=	BYTARR(SAMPLES)
		 ENDIF
		 IF(BYTES_OF_DATA EQ 2) THEN BEGIN
		 	FILE_LY	=	INTARR(SAMPLES)
		 	FILE_TY	=	INTARR(SAMPLES)
		 	OUTPUT			=	BYTARR(SAMPLES)
		 ENDIF
		 IF(BYTES_OF_DATA EQ 4) THEN BEGIN
		 	FILE_LY	=	FLTARR(SAMPLES)
		 	FILE_TY	=	FLTARR(SAMPLES)
		 	OUTPUT			=	BYTARR(SAMPLES)
		 ENDIF
		 CLOUD_LY		=	BYTARR(SAMPLES)
		 CLOUD_TY		=	BYTARR(SAMPLES)

		 DIFF			=	INTARR(SAMPLES)
		 IF(BYTES_OF_DATA EQ 4) THEN BEGIN
		 	DIFF			=	FLTARR(SAMPLES)
		 ENDIF


		 ;Ϊ��ʵ�ֽ���������ʾ,��������´���(����*֮��)
		 ;****************************************************************************

		 PROGRESSTIMER = OBJ_NEW("SHOWPROGRESS", TLB,/CANCELBUTTON,MESSAGE='���ڽ���ʵʱ����ֵ�ּ�..',$
			TITLE='��ֵ�ּ�')
		 PROGRESSTIMER->START
		 ;****************************************************************************

		 FOR I=0,LINES-1 DO BEGIN
		 	;****************************************************************
			CANCELLED = PROGRESSTIMER->CHECKCANCEL()
			IF CANCELLED THEN BEGIN
				OK = DIALOG_MESSAGE('�û���ֹ�˲���',TITLE='��ʾ')
				PROGRESSTIMER->DESTROY ;����������
				CLOSE,/ALL
				RETURN,0
			ENDIF
			PROGRESSTIMER->UPDATE, (FLOAT(I)/(LINES+1) * 100.0) ;��������
			;PRINT,TOTAL_COUNTY,COUNT_COUNTY
			;****************************************************************
			READU, Lun_FILE_LY	, FILE_LY
	 		READU, Lun_FILE_TY	, FILE_TY
	 		READU, Lun_FILE_CLOUD_LY	, CLOUD_LY
	 		READU, Lun_FILE_CLOUD_TY	, CLOUD_TY


		 	FOR J=0,SAMPLES-1 DO BEGIN

				;��ȡ���ݺ�ʼ�����ݽ��д���,���´�����Ƕ����ݵĴ���
				DIFF[J]=FLOAT(FILE_TY[J])-FLOAT(FILE_LY[J])

				;����ȷ��0��255Ϊ�쳣ֵ,��ѯ��Ĳ����
				IF( (FILE_TY[J] EQ 255) OR (FILE_LY[J] EQ 255)	$
					OR (CLOUD_TY[J] EQ 1) OR (CLOUD_LY[J] EQ 1)) THEN BEGIN
					;��10����ʾ����Ԫ��һ������Ԫ,��һ����Ч����
					OUTPUT[J]=BYTE(10)
				ENDIF ELSE BEGIN
					CASE 1 OF
   						(DIFF[J] GE FIX(LEVEL_1_MIN)) AND (DIFF[J] LT FIX(LEVEL_1_MAX)) : OUTPUT[J]=BYTE(1)
   						(DIFF[J] GE FIX(LEVEL_2_MIN)) AND (DIFF[J] LT FIX(LEVEL_2_MAX)) : OUTPUT[J]=BYTE(2)
   						(DIFF[J] GE FIX(LEVEL_3_MIN)) AND (DIFF[J] LT FIX(LEVEL_3_MAX)) : OUTPUT[J]=BYTE(3)
   						(DIFF[J] GE FIX(LEVEL_4_MIN)) AND (DIFF[J] LT FIX(LEVEL_4_MAX)) : OUTPUT[J]=BYTE(4)
   						(DIFF[J] GE FIX(LEVEL_5_MIN)) AND (DIFF[J] LE FIX(LEVEL_5_MAX)) : OUTPUT[J]=BYTE(5)
   						(DIFF[J] GT FIX(LEVEL_6_MIN)) AND (DIFF[J] LE FIX(LEVEL_6_MAX)) : OUTPUT[J]=BYTE(6)
   						(DIFF[J] GT FIX(LEVEL_7_MIN)) AND (DIFF[J] LE FIX(LEVEL_7_MAX)) : OUTPUT[J]=BYTE(7)
   						(DIFF[J] GT FIX(LEVEL_8_MIN)) AND (DIFF[J] LE FIX(LEVEL_8_MAX)) : OUTPUT[J]=BYTE(8)
   						(DIFF[J] GT FIX(LEVEL_9_MIN)) AND (DIFF[J] LE FIX(LEVEL_9_MAX)) : OUTPUT[J]=BYTE(9)
						ELSE: OUTPUT[J]=BYTE(10)
					ENDCASE
				ENDELSE

				;****************************************************************************************

		 	ENDFOR
		 	;�Բ�ͬ���������ͽ��д���
			WRITEU,Lun_FILE_OUTPUT	 	,OUTPUT
		 ENDFOR


		 ;�ر����д򿪵��ļ�
		 free_lun,Lun_FILE_LY
		 free_lun,Lun_FILE_TY
		 free_lun,Lun_FILE_CLOUD_LY
		 free_lun,Lun_FILE_CLOUD_TY
		 free_lun,Lun_FILE_OUTPUT

	 	;****************************************************************************
	 	;��ͷ�ļ�д��ȥ
	 	HDR_INFO=GET_IMAGE_INFO(INFILE)
	    HDR_INFO.DATATYPE=1
	    WRITE_HDR_FILE,FILE_OUTPUT,HDR_INFO
	    ;****************************************************************************

	    ;Ϊ��ʵ�ֽ���������ʾ,��������´���(����*֮��)
	 	;****************************************************************************
		PROGRESSTIMER->DESTROY ;
	 	;****************************************************************************
	 	RETURN,1

	 ENDIF ELSE BEGIN
	 	PRINT,'HERE 2'
	 	 FILE_INFO_TY=GET_IMAGE_INFO(FILE_TY)
		 FILE_INFO_LY=GET_IMAGE_INFO(FILE_LY)

		 IF (FILE_INFO_TY.FILE_RIGHT EQ 0) OR $
		 	(FILE_INFO_LY.FILE_RIGHT EQ 0) 	THEN BEGIN
		 	OK = DIALOG_MESSAGE('������ļ��д���',TITLE='��ʾ')
		 	RETURN,0
		 ENDIF

	 	 IF (FILE_INFO_TY.XSIZE NE FILE_INFO_LY.XSIZE) OR $
	 	 	(FILE_INFO_TY.YSIZE NE FILE_INFO_LY.YSIZE) THEN BEGIN
		 	OK = DIALOG_MESSAGE('�����ļ���С��һ��!',TITLE='��ʾ')
		 	RETURN,0
		 ENDIF

		 IF (FILE_INFO_TY.X_PIXELSIZE NE FILE_INFO_LY.X_PIXELSIZE) OR $
	 	 	(FILE_INFO_TY.Y_PIXELSIZE NE FILE_INFO_LY.Y_PIXELSIZE) THEN BEGIN
		 	OK = DIALOG_MESSAGE('�����ļ��ķֱ��ʲ�һ��!',TITLE='��ʾ')
		 	RETURN,0
		 ENDIF

		 IF (ABS(FILE_INFO_TY.STARTX-FILE_INFO_LY.STARTX) GT FILE_INFO_TY.X_PIXELSIZE) OR $
		 	(ABS(FILE_INFO_TY.STARTY-FILE_INFO_LY.STARTY) GT FILE_INFO_TY.Y_PIXELSIZE) $
		 	 THEN BEGIN
		 	OK = DIALOG_MESSAGE('�����ļ��������겻һ��!',TITLE='��ʾ')
		 	RETURN,0
		 ENDIF



		 ;��ʼ�������ļ��Ķ�ȡ
		 INFILE=FILE_LY
		 Openr,Lun_FILE_LY	,FILE_LY	,/get_lun
		 Openr,Lun_FILE_TY	,FILE_TY	,/get_lun
		 Openw,Lun_FILE_OUTPUT		,FILE_OUTPUT	,/get_lun

		 ;����Ҫ������ȡ����������
		 ;�Գ���������޸�,һ�ζ�ȡһ�е�����
		 SAMPLES=FILE_INFO_TY.XSIZE
		 LINES=FILE_INFO_TY.YSIZE

		 ;�������ݵ����ͽ������ݶ�ȡ�����Ķ���
		 IF (FILE_INFO_TY.DATATYPE EQ 1) THEN BYTES_OF_DATA=1
		 case FILE_INFO_TY.DATATYPE of
	      1: begin
	             BYTES_OF_DATA=1
	         end
	      2: begin
	             BYTES_OF_DATA=2
	         end
	      4: begin
	             BYTES_OF_DATA=4
	         end
	      12:begin
	             BYTES_OF_DATA=2
	         end
	      13:begin
	             BYTES_OF_DATA=4
	         end
	      else:begin	;
	             print,'@@'+FILE_INFO_TY+':û�ж������������!'
	             return,0
	         end
	    ENDCASE


		 IF(BYTES_OF_DATA EQ 1) THEN BEGIN
		 	FILE_LY	=	BYTARR(SAMPLES)
		 	FILE_TY	=	BYTARR(SAMPLES)
		 	OUTPUT			=	BYTARR(SAMPLES)
		 ENDIF
		 IF(BYTES_OF_DATA EQ 2) THEN BEGIN
		 	FILE_LY	=	INTARR(SAMPLES)
		 	FILE_TY	=	INTARR(SAMPLES)
		 	OUTPUT			=	BYTARR(SAMPLES)
		 ENDIF
		 IF(BYTES_OF_DATA EQ 4) THEN BEGIN
		 	FILE_LY	=	FLTARR(SAMPLES)
		 	FILE_TY	=	FLTARR(SAMPLES)
		 	OUTPUT			=	BYTARR(SAMPLES)
		 	PRINT,'I AM IN 1'
		 ENDIF

		 DIFF			=	INTARR(SAMPLES)
		 IF(BYTES_OF_DATA EQ 4) THEN BEGIN
		 	DIFF			=	FLTARR(SAMPLES)
		 	PRINT,'I AM IN 2'
		 ENDIF


		 ;Ϊ��ʵ�ֽ���������ʾ,��������´���(����*֮��)
		 ;****************************************************************************

		 PROGRESSTIMER = OBJ_NEW("SHOWPROGRESS", TLB,/CANCELBUTTON,MESSAGE='���ڽ���ʵʱ����ֵ�ּ�..',$
			TITLE='��ֵ�ּ�')
		 PROGRESSTIMER->START
		 ;****************************************************************************

		 FOR I=0,LINES-1 DO BEGIN
		 	;****************************************************************
			CANCELLED = PROGRESSTIMER->CHECKCANCEL()
			IF CANCELLED THEN BEGIN
				OK = DIALOG_MESSAGE('�û���ֹ�˲���',TITLE='��ʾ')
				PROGRESSTIMER->DESTROY ;����������
				CLOSE,/ALL
				RETURN,0
			ENDIF
			PROGRESSTIMER->UPDATE, (FLOAT(I)/(LINES+1) * 100.0) ;��������
			;PRINT,TOTAL_COUNTY,COUNT_COUNTY
			;****************************************************************
			READU, Lun_FILE_LY	, FILE_LY
	 		READU, Lun_FILE_TY	, FILE_TY

		 	FOR J=0,SAMPLES-1 DO BEGIN

				;��ȡ���ݺ�ʼ�����ݽ��д���,���´�����Ƕ����ݵĴ���
				DIFF[J]=FLOAT(FILE_TY[J])-FLOAT(FILE_LY[J])
				;IF((PLOWLAND EQ 0) OR (FILE_TY EQ 0) OR (FILE_LY EQ 0)) THEN BEGIN
				;ȥ���˸��ص�����
				;****************************************************************************************

				;����ȷ��0��255Ϊ�쳣ֵ,��ѯ��Ĳ����
				IF( (FILE_TY[J] EQ 255) OR (FILE_LY[J] EQ 255) OR (FILE_TY[J] EQ 0) OR (FILE_LY[J] EQ 0)) THEN BEGIN
					;��10����ʾ����Ԫ��һ������Ԫ(����Ч��Ԫ)
					OUTPUT[J]=BYTE(10)
				ENDIF ELSE BEGIN
					CASE 1 OF
   						(DIFF[J] GE FIX(LEVEL_1_MIN)) AND (DIFF[J] LT FIX(LEVEL_1_MAX)) : OUTPUT[J]=BYTE(1)
   						(DIFF[J] GE FIX(LEVEL_2_MIN)) AND (DIFF[J] LT FIX(LEVEL_2_MAX)) : OUTPUT[J]=BYTE(2)
   						(DIFF[J] GE FIX(LEVEL_3_MIN)) AND (DIFF[J] LT FIX(LEVEL_3_MAX)) : OUTPUT[J]=BYTE(3)
   						(DIFF[J] GE FIX(LEVEL_4_MIN)) AND (DIFF[J] LT FIX(LEVEL_4_MAX)) : OUTPUT[J]=BYTE(4)
   						(DIFF[J] GE FIX(LEVEL_5_MIN)) AND (DIFF[J] LE FIX(LEVEL_5_MAX)) : OUTPUT[J]=BYTE(5)
   						(DIFF[J] GT FIX(LEVEL_6_MIN)) AND (DIFF[J] LE FIX(LEVEL_6_MAX)) : OUTPUT[J]=BYTE(6)
   						(DIFF[J] GT FIX(LEVEL_7_MIN)) AND (DIFF[J] LE FIX(LEVEL_7_MAX)) : OUTPUT[J]=BYTE(7)
   						(DIFF[J] GT FIX(LEVEL_8_MIN)) AND (DIFF[J] LE FIX(LEVEL_8_MAX)) : OUTPUT[J]=BYTE(8)
   						(DIFF[J] GT FIX(LEVEL_9_MIN)) AND (DIFF[J] LE FIX(LEVEL_9_MAX)) : OUTPUT[J]=BYTE(9)
						ELSE: OUTPUT[J]=BYTE(10)
					ENDCASE
				ENDELSE

				;****************************************************************************************
		 	ENDFOR
		 	;�Բ�ͬ���������ͽ��д���
			WRITEU,Lun_FILE_OUTPUT	 	,OUTPUT
		 ENDFOR


		 ;�ر����д򿪵��ļ�
		 free_lun,Lun_FILE_LY
		 free_lun,Lun_FILE_TY
		 free_lun,Lun_FILE_OUTPUT

	 	;****************************************************************************
	 	;��ͷ�ļ�д��ȥ
	 	HDR_INFO=GET_IMAGE_INFO(INFILE)
	    HDR_INFO.DATATYPE=1
	    WRITE_HDR_FILE,FILE_OUTPUT,HDR_INFO
	    ;****************************************************************************

	    ;Ϊ��ʵ�ֽ���������ʾ,��������´���(����*֮��)
	 	;****************************************************************************
		PROGRESSTIMER->DESTROY ;
	 	;****************************************************************************
	 	RETURN,1

	 ENDELSE



end
;-----------------------------------------------------------------

;-----------------------------------------------------------------
pro BASE_TOP_CLASSIFY_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)

  COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE

  common common_setpath, ppath
  zs_in_path = (*ppath).zs_in_path
  zs_out_path = (*ppath).zs_out_path

  wWidget =  Event.top
  WIDGET_CONTROL,Event.top,get_UVALUE = pstate
  case wTarget of

    Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_CLOUD'): begin
		 WIDGET_CONTROL, EVENT.TOP,GET_UVALUE=PSTATE
	     (*PSTATE).CLOUD=EVENT.SELECT
	     WIDGET_CONTROL,(*PSTATE).CMD_CLASSIFY_CHOOSE_CLOUD_TY,SENSITIVE=EVENT.SELECT
	     WIDGET_CONTROL,(*PSTATE).CMD_CLASSIFY_CHOOSE_CLOUD_LY,SENSITIVE=EVENT.SELECT
	     WIDGET_CONTROL,(*PSTATE).TXT_CLASSIFY_CLOUD_TY,SENSITIVE=EVENT.SELECT
	     WIDGET_CONTROL,(*PSTATE).TXT_CLASSIFY_CLOUD_LY,SENSITIVE=EVENT.SELECT
	     RETURN
    end
    Widget_Info(wWidget, FIND_BY_UNAME='DST_CLASSIFY_DATA_TYPE'): begin
		;������ݵ�����
		INDEX=EVENT.INDEX
		ARR_DATA_TYPE=(*PSTATE).ARR_DATA_TYPE
		DATA_TYPE=ARR_DATA_TYPE[INDEX]
		(*PSTATE).DATA_TYPE = DATA_TYPE
		IF DATA_TYPE EQ 'NDVI' THEN begin
			(*PSTATE).PARAMETER_SEP=(*PSTATE).PARAMETER_SEP_NDVI
		endif else IF DATA_TYPE EQ 'LAI' THEN begin
			(*PSTATE).PARAMETER_SEP=(*PSTATE).PARAMETER_SEP_LAI
		endif else IF DATA_TYPE EQ 'NPP' THEN begin
			(*PSTATE).PARAMETER_SEP=(*PSTATE).PARAMETER_SEP_NPP
		endif

		PRINT,(*PSTATE).PARAMETER_SEP
		PRINT,DATA_TYPE
		defaultnames_zsrtclf, event
	    ;RETURN
    end

    Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_FILE_LY'): begin
    	FILE_LY=DIALOG_PICKFILE(filter='*.*',path=zs_in_path,title='ѡ��ȥ��ͼ��', DIALOG_PARENT=Event.id)
	    IF (FILE_LY NE '') THEN BEGIN
	          WIDGET_CONTROL, (*pstate).TXT_CLASSIFY_FILE_LY, set_VALUE=FILE_LY
	    ENDIF
	    TEMP=Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_FILE_LY')
	    WIDGET_CONTROL,TEMP, /INPUT_FOCUS
	    RETURN
    end

    Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_FILE_TY'): begin
    	FILE_TY=DIALOG_PICKFILE(filter='*.*',path=zs_in_path,title='ѡ�����ͼ��', DIALOG_PARENT=Event.id)
	     IF (FILE_TY NE '') THEN BEGIN
	          WIDGET_CONTROL, (*pstate).TXT_CLASSIFY_FILE_TY, set_VALUE=FILE_TY
	     ENDIF
	     TEMP=Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_FILE_TY')
	     WIDGET_CONTROL,TEMP, /INPUT_FOCUS
	     RETURN
    end

    Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_CHOOSE_CLOUD_TY'): begin
    	FILE_CLOUD_TY=DIALOG_PICKFILE(filter='*.*',path=zs_in_path,title='ѡ������Ƹ���ͼ��', DIALOG_PARENT=Event.id)
	     IF (FILE_CLOUD_TY NE '') THEN BEGIN
	          WIDGET_CONTROL, (*pstate).TXT_CLASSIFY_CLOUD_TY, set_VALUE=FILE_CLOUD_TY
	     ENDIF
	     TEMP=Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_CHOOSE_CLOUD_TY')
	     WIDGET_CONTROL,TEMP, /INPUT_FOCUS
	     RETURN
    end

    Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_CHOOSE_CLOUD_LY'): begin
	    FILE_CLOUD_LY=DIALOG_PICKFILE(filter='*.*',path=zs_in_path,title='ѡ��ȥ���Ƹ���ͼ��', DIALOG_PARENT=Event.id)
	     IF (FILE_CLOUD_LY NE '') THEN BEGIN
	          WIDGET_CONTROL, (*pstate).TXT_CLASSIFY_CLOUD_LY, set_VALUE=FILE_CLOUD_LY
	     ENDIF
	     ;�۽������ť
	     TEMP=Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_CHOOSE_CLOUD_LY')
	     WIDGET_CONTROL,TEMP, /INPUT_FOCUS
	     RETURN
    end

    Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_CHOOSE_OUTPUT'): begin
    	 FILE_OUTPUT=DIALOG_PICKFILE(filter='*.*',path=zs_out_path,title='�ּ�ͼ�����', DIALOG_PARENT=Event.id)
	     IF (FILE_OUTPUT NE '') THEN BEGIN
	     		outputfile=strsplit(FILE_OUTPUT,'.',/extract)
	         WIDGET_CONTROL, (*pstate).TXT_CLASSIFY_FILE_CLASSIFIED, set_VALUE=outputfile[0]
	     ENDIF
	     ;�۽������ť
	     TEMP=Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_CHOOSE_OUTPUT')
	     WIDGET_CONTROL,TEMP, /INPUT_FOCUS
	     RETURN
    end

    Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_CHANGE_PARAMETER'): begin
    	BASE_CLASSIFY_PARAMETER_SETUP,PSTATE,(*PSTATE).BASE_TOP_CLASSIFY
    	;�۽������ť
    	TEMP=Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_CHANGE_PARAMETER')
	    WIDGET_CONTROL,TEMP, /INPUT_FOCUS
    end

    Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_NOTATION'): begin
    	TEXT='������ݵ�˵��'
	    TEXT=TEXT+STRING(BYTE(13))+STRING(BYTE(10))+'���ͼ���е�������ֵ��ΧΪ1-10,'
	    TEXT=TEXT+STRING(BYTE(13))+STRING(BYTE(10))+'ÿ��ֵ����������:'
;	    TEXT=TEXT+STRING(BYTE(13))+STRING(BYTE(10))+'0 :�Ǹ���'
	    TEXT=TEXT+STRING(BYTE(13))+STRING(BYTE(10))+'1-2 :��ȥ���,����ɫ��ʾ'
;	    TEXT=TEXT+STRING(BYTE(13))+STRING(BYTE(10))+'2 :��ȥ���,����ɫ��ʾ'
	    TEXT=TEXT+STRING(BYTE(13))+STRING(BYTE(10))+'3-4 :��ȥ���Բ�,����ɫ��ʾ'
;		TEXT=TEXT+STRING(BYTE(13))+STRING(BYTE(10))+'4 :��ȥ���Բ�,����ɫ��ʾ'
		TEXT=TEXT+STRING(BYTE(13))+STRING(BYTE(10))+'5-6 :��ȥ���ƽ,����ɫ��ʾ'
;		TEXT=TEXT+STRING(BYTE(13))+STRING(BYTE(10))+'6 :��ȥ���ƽ,����ɫ��ʾ'
		TEXT=TEXT+STRING(BYTE(13))+STRING(BYTE(10))+'7-8 :��ȥ���Ժ�,����ɫ��ʾ'
;		TEXT=TEXT+STRING(BYTE(13))+STRING(BYTE(10))+'8 :��ȥ���Ժ�,����ɫ��ʾ'
		TEXT=TEXT+STRING(BYTE(13))+STRING(BYTE(10))+'9 :��ȥ���,�ú�ɫ��ʾ'
		TEXT=TEXT+STRING(BYTE(13))+STRING(BYTE(10))+'10:��Ч������,�ð�ɫ��ʾ'
		C_NOTATION,GROUP_LEADER=(*pstate).BASE_TOP_CLASSIFY,TEXT;, _EXTRA=_VWBExtra_
		;�۽������ť
    	TEMP=Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_NOTATION')
	    WIDGET_CONTROL,TEMP, /INPUT_FOCUS
	    RETURN
    end

    Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_CANCEL'): begin
    	CLOSE,/all
	    WIDGET_CONTROL, event.top, /destroy
	    RETURN
    end

    Widget_Info(wWidget, FIND_BY_UNAME='CMD_CLASSIFY_OK'): begin
    	 ;�������������м��
    	 WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_FILE_LY,get_value=FILE_LY
	     IF(FILE_LY EQ '') THEN BEGIN
	         msg=DIALOG_MESSAGE('��������ȥ����ļ�!',TITLE='��ʾ',/INFORMATION)
	         CLOSE,/all
	         RETURN
	     ENDIF
	     FILE_LY=FILE_LY[0]

	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_FILE_TY,get_value=FILE_TY
	     IF(FILE_TY EQ '') THEN BEGIN
	         msg=DIALOG_MESSAGE('�������������ļ�!',TITLE='��ʾ',/INFORMATION)
	         CLOSE,/all
	         RETURN
	     ENDIF
	     FILE_TY=FILE_TY[0]

	     ;�Ƿ�Ҫ��������ͨ���ļ�
	     IF ((*PSTATE).CLOUD EQ 1)THEN BEGIN
			 WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_CLOUD_LY,get_value=FILE_CLOUD_LY
		     IF(FILE_CLOUD_LY EQ '') THEN BEGIN
		         msg=DIALOG_MESSAGE('��������ȥ���Ʊ�ʶ�ļ�!',TITLE='��ʾ',/INFORMATION)
		         CLOSE,/all
		         RETURN
		     ENDIF
		     CLOUD_LY=FILE_CLOUD_LY[0]

			 WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_CLOUD_TY,get_value=FILE_CLOUD_TY
		     IF(FILE_CLOUD_TY EQ '') THEN BEGIN
		         msg=DIALOG_MESSAGE('��������ȥ���Ʊ�ʶ�ļ�!',TITLE='��ʾ',/INFORMATION)
		         CLOSE,/all
		         RETURN
		     ENDIF
		     CLOUD_TY=FILE_CLOUD_TY[0]
		 ENDIF

	     WIDGET_CONTROL,(*pstate).TXT_CLASSIFY_FILE_CLASSIFIED,get_value=FILE_OUTPUT
	     IF(FILE_OUTPUT EQ '') THEN BEGIN
	         msg=DIALOG_MESSAGE('��ѡ������ķּ��ļ�!',TITLE='��ʾ',/INFORMATION)
	         CLOSE,/all
	         RETURN
	     ENDIF
	     FILE_OUTPUT=FILE_OUTPUT[0]
	     RESULT=FILE_INFO(FILE_OUTPUT)
	     IF(RESULT.EXISTS NE 0) THEN BEGIN
	         msg=DIALOG_MESSAGE('����ļ��Ѿ�����,Ҫ����ô?',/Question,TITLE='����ԭ�ļ�ô')
	         IF(MSG NE 'Yes') THEN BEGIN
	         	PRINT,'BUFUGAI'
	         	CLOSE,/all
	         	RETURN
	         ENDIF
	     ENDIF

	     ;Ҳ���Ƿ�ʹ������ͨ������

	     TEMP=CLASSIFY_MAIN(FILE_LY,FILE_TY,$
						CLOUD_LY,CLOUD_TY,$
						FILE_OUTPUT,(*PSTATE).CLOUD,$
						(*PSTATE).PARAMETER_SEP)

		 ;������ݴ���ɹ���Խ��������ʾ
		 IF TEMP EQ 1 THEN BEGIN
		 	fileinfo = read_file(FILE_OUTPUT)
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
;		 	SCR_XSIZE=350.0
;		 	SCR_YSIZE=347.0
;		 	IMAGE_INFO=GET_IMAGE_INFO(FILE_OUTPUT)
;
;		 	;��������ͼ��ĳߴ�
;		 	IF(FLOAT(IMAGE_INFO.XSIZE)/FLOAT(IMAGE_INFO.YSIZE) GT SCR_XSIZE/SCR_YSIZE)	$
;		 			THEN BEGIN
;		 		Y_SIZE=SCR_YSIZE
;		 		X_SIZE=SCR_XSIZE*(FLOAT(IMAGE_INFO.XSIZE)/FLOAT(IMAGE_INFO.YSIZE))
;		 	ENDIF ELSE BEGIN
;		 		X_SIZE=SCR_XSIZE
;		 		Y_SIZE=SCR_YSIZE*(FLOAT(IMAGE_INFO.YSIZE)/FLOAT(IMAGE_INFO.XSIZE))
;		 	ENDELSE
;		 	ARR_DATA=(read_file(FILE_OUTPUT)).dataarr
;
;		 	ARR_DATA=CONGRID(ARR_DATA,X_SIZE,Y_SIZE)
;		 	WIDGET_CONTROL,(*pstate).DRAW_CLASSIFY_RESULT,get_value=DRAW_CLASSIFY_RESULT
;			WSET,DRAW_CLASSIFY_RESULT
;			ERASE
;
;			DEVICE,GET_DECOMPOSED=TEMP
;			DEVICE,DECOMPOSED=0
;
;			LOADCT,39
;			TVSCL,ARR_DATA,/ORDER
;			DEVICE,DECOMPOSED=TEMP
			msg=DIALOG_MESSAGE('��ɲ�ֵ�ּ�����!',TITLE='��ʾ',/Information )
			log, 'ʵʱ���-��ֵ�ּ�', 1
		 ENDIF
    end

    else:
  endcase

end

pro BASE_TOP_CLASSIFY, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_

  ;����ͬһ�������ظ�����
  IF ( XREGISTERED('BASE_TOP_CLASSIFY') NE 0 ) THEN RETURN

  BASE_TOP_CLASSIFY = Widget_Base( GROUP_LEADER=wGroup,  $
      UNAME='BASE_TOP_CLASSIFY' ,XOFFSET=215 ,YOFFSET=200 $
      ,TITLE='ʵʱ���--��ֵ�ּ�' ,SPACE=3 ,XPAD=3  $;,SCR_YSIZE=380+34
      ,YPAD=3 ,TLB_FRAME_ATTR=1,/row)

  BASE_CLASSIFY_PARAMETER_ALL = Widget_Base(BASE_TOP_CLASSIFY,  $
      UNAME='BASE_CLASSIFY_PARAMETER_ALL' ,FRAME=1 ,XOFFSET=6 ,YOFFSET=6  $
      ,SCR_XSIZE=246 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3,/column)

  common current_date, c_year, c_month, c_day
  subbase = widget_base(BASE_CLASSIFY_PARAMETER_ALL,/row,/frame,SPACE=5 ,XPAD=5,YPAD=3)
  LBL_CLASSIFY_DATA_TYPE = Widget_Label(subbase,/ALIGN_LEFT,VALUE='ʱ��:')
  WID_LABEL_date = Widget_text(subbase,xsize=15, /ALL_EVENTS, $
  	VALUE=strtrim(c_year,2)+'-'+strtrim(c_month,2)+'-'+strtrim(c_day,2))
  CMD_pick_date = Widget_Button(subbase,SCR_XSIZE=30,SCR_YSIZE=20,/ALIGN_CENTER, uname='CMD_pick_date', $
  	VALUE='.\Image\Calendar.bmp',/BITMAP,event_pro='ActiveXCal',uvalue={text_id:WID_LABEL_date, pointer:PTR_NEW(), detail:'CMD_pick_date_classify'})


;���BASE�������������ͺ��Ƿ�ʹ��������
  BASE_CLASSIFY_OPTION = Widget_Base(BASE_CLASSIFY_PARAMETER_ALL,  $
      UNAME='BASE_CLASSIFY_OPTION' ,FRAME=1 ,XOFFSET=6 ,YOFFSET=6  $
      ,SCR_XSIZE=234 ,SCR_YSIZE=32 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)

  LBL_CLASSIFY_DATA_TYPE = Widget_Label(BASE_CLASSIFY_OPTION,  $
      UNAME='LBL_CLASSIFY_DATA_TYPE' ,XOFFSET=8 ,YOFFSET=8  $
      ,SCR_XSIZE=35 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
      ,VALUE='����:')

  DST_CLASSIFY_DATA_TYPE = Widget_Droplist(BASE_CLASSIFY_OPTION,  $
      UNAME='DST_CLASSIFY_DATA_TYPE' ,XOFFSET=45 ,YOFFSET=5  $
      ,SCR_XSIZE=56 ,SCR_YSIZE=22 )

  BASE_CLASSIFY_CLOUD = WIDGET_BASE(BASE_CLASSIFY_OPTION,  $
      UNAME='BASE_CLASSIFY_CLOUD' ,XOFFSET=130 ,SCR_XSIZE=166  $
      ,SCR_YSIZE=25 ,TITLE='IDL' ,ROW=1 ,/NONEXCLUSIVE)

  CMD_CLASSIFY_CLOUD = WIDGET_BUTTON(BASE_CLASSIFY_CLOUD,  $
      UNAME='CMD_CLASSIFY_CLOUD' ,XOFFSET=0 ,SCR_XSIZE=110 ,SCR_YSIZE=24  $
      ,/ALIGN_LEFT ,VALUE='ʹ��������')
  WIDGET_CONTROL,CMD_CLASSIFY_CLOUD,SET_BUTTON=1


  BASE_CLASSIFY_PARAMETER = Widget_Base(BASE_CLASSIFY_PARAMETER_ALL,  $
      UNAME='BASE_CLASSIFY_PARAMETER' ,FRAME=1 $
      ,SCR_XSIZE=246,SCR_YSIZE=285 ,TITLE='IDL' ,SPACE=3 ,XPAD=3,YPAD=3)

  LBL_CLASSIFY_LY = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_LY' ,XOFFSET=8 ,YOFFSET=9  $
      ,SCR_XSIZE=114 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
      ,VALUE='ѡ��ȥ��ͼ��')


  TXT_CLASSIFY_FILE_LY = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_FILE_LY' ,XOFFSET=6 ,YOFFSET=28  $
      ,SCR_XSIZE=220 ,SCR_YSIZE=20 ,/EDITABLE ,XSIZE=20 ,YSIZE=1)


  CMD_CLASSIFY_FILE_LY =  $
      Widget_Button(BASE_CLASSIFY_PARAMETER,  $
      UNAME='CMD_CLASSIFY_FILE_LY' ,XOFFSET=190 ,YOFFSET=5  $
      ,SCR_XSIZE=36 ,SCR_YSIZE=20 ,/ALIGN_CENTER  $
      ,VALUE='open.bmp' ,/BITMAP)


  CMD_CLASSIFY_FILE_TY =  $
      Widget_Button(BASE_CLASSIFY_PARAMETER,  $
      UNAME='CMD_CLASSIFY_FILE_TY' ,XOFFSET=190 ,YOFFSET=55  $
      ,SCR_XSIZE=36 ,SCR_YSIZE=20 ,/ALIGN_CENTER  $
      ,VALUE='open.bmp' ,/BITMAP)


  TXT_CLASSIFY_FILE_TY = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_FILE_TY' ,XOFFSET=7 ,YOFFSET=78  $
      ,SCR_XSIZE=220 ,SCR_YSIZE=20 ,/EDITABLE ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_FILE_YEAR = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_TY' ,XOFFSET=8 ,YOFFSET=59  $
      ,SCR_XSIZE=114 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
      ,VALUE='ѡ�����ͼ��')


  CMD_CLASSIFY_CHOOSE_CLOUD_LY = Widget_Button(BASE_CLASSIFY_PARAMETER,  $
      UNAME='CMD_CLASSIFY_CHOOSE_CLOUD_LY' ,XOFFSET=190 ,YOFFSET=105  $
      ,SCR_XSIZE=36 ,SCR_YSIZE=20  ,/ALIGN_CENTER  $
      ,VALUE='open.bmp' ,/BITMAP)


  TXT_CLASSIFY_CLOUD_LY = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_CLOUD_LY' ,XOFFSET=7 ,YOFFSET=128  $
      ,SCR_XSIZE=220 ,SCR_YSIZE=20 ,/EDITABLE ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_CLOUD_LY = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_CLOUD_LY' ,XOFFSET=8 ,YOFFSET=109  $
      ,SCR_XSIZE=114 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
      ,VALUE='ȥ���Ƹ���ͼ��')

  CMD_CLASSIFY_CHOOSE_CLOUD_TY = Widget_Button(BASE_CLASSIFY_PARAMETER,  $
      UNAME='CMD_CLASSIFY_CHOOSE_CLOUD_TY' ,XOFFSET=190 ,YOFFSET=105+50  $
      ,SCR_XSIZE=36 ,SCR_YSIZE=20,/ALIGN_CENTER  $
      ,VALUE='open.bmp' ,/BITMAP)


  TXT_CLASSIFY_CLOUD_TY = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_CLOUD_TY' ,XOFFSET=7 ,YOFFSET=128+50  $
      ,SCR_XSIZE=220 ,SCR_YSIZE=20 ,/EDITABLE ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_CLOUD_TY = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_CLOUD_TY' ,XOFFSET=8 ,YOFFSET=109+50  $
      ,SCR_XSIZE=114 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
      ,VALUE='�����Ƹ���ͼ��')


  CMD_CLASSIFY_CHOOSE_OUTPUT = Widget_Button(BASE_CLASSIFY_PARAMETER,  $
      UNAME='CMD_CLASSIFY_CHOOSE_OUTPUT' ,XOFFSET=190 ,YOFFSET=204-48+50  $
      ,SCR_XSIZE=36 ,SCR_YSIZE=20 ,/ALIGN_CENTER  $
      ,VALUE='open.bmp' ,/BITMAP)


  TXT_CLASSIFY_FILE_CLASSIFIED = Widget_Text(BASE_CLASSIFY_PARAMETER,  $
      UNAME='TXT_CLASSIFY_FILE_CLASSIFIED' ,XOFFSET=7 ,YOFFSET=227-48+50  $
      ,SCR_XSIZE=220 ,SCR_YSIZE=20 ,/EDITABLE ,XSIZE=20 ,YSIZE=1)


  LBL_CLASSIFY_OUTPUT = Widget_Label(BASE_CLASSIFY_PARAMETER,  $
      UNAME='LBL_CLASSIFY_OUTPUT' ,XOFFSET=8 ,YOFFSET=208-48+50  $
      ,SCR_XSIZE=124 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
      ,VALUE='ѡ��ּ�ͼ�����·��')


  CMD_CLASSIFY_CHANGE_PARAMETER = Widget_Button(BASE_CLASSIFY_PARAMETER,  $
      UNAME='CMD_CLASSIFY_CHANGE_PARAMETER' ,XOFFSET=6 ,YOFFSET=254-48+50  $
      ,SCR_XSIZE=220 ,SCR_YSIZE=22 $
      ,/ALIGN_CENTER ,VALUE='���зּ������趨')




  BASE_CLASSIFY_BUTTON = Widget_Base(BASE_CLASSIFY_PARAMETER_ALL,  $
      UNAME='BASE_CLASSIFY_BUTTON' ,FRAME=1 ,XOFFSET=5 ,YOFFSET=282-48+50  $
      ,SCR_XSIZE=233 ,SCR_YSIZE=34 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3)


  CMD_CLASSIFY_OK = Widget_Button(BASE_CLASSIFY_BUTTON,  $
      UNAME='CMD_CLASSIFY_OK' ,XOFFSET=6 ,YOFFSET=5 ,SCR_XSIZE=70  $
      ,SCR_YSIZE=22  ,/ALIGN_CENTER  $
      ,VALUE='����ļ�')


  CMD_CLASSIFY_NOTATION = Widget_Button(BASE_CLASSIFY_BUTTON,  $
      UNAME='CMD_CLASSIFY_NOTATION' ,XOFFSET=81 ,YOFFSET=5 ,SCR_XSIZE=70  $
      ,SCR_YSIZE=22 ,/ALIGN_CENTER  $
      ,VALUE='���˵��')


  CMD_CLASSIFY_CANCEL = Widget_Button(BASE_CLASSIFY_BUTTON,  $
      UNAME='CMD_CLASSIFY_CANCEL' ,XOFFSET=155 ,YOFFSET=5  $
      ,SCR_XSIZE=70 ,SCR_YSIZE=22   $
      ,/ALIGN_CENTER ,VALUE='�ر�')

  BASE_CLASSIFY_DRAW = Widget_Base(BASE_TOP_CLASSIFY,  $
      UNAME='BASE_CLASSIFY_DRAW' ,FRAME=1 $
      ,SPACE=3 ,XPAD=3  $
      ,YPAD=3,/column)

  LBL_CLASSIFY_DRAW = Widget_Label(BASE_CLASSIFY_DRAW,  $
      UNAME='LBL_CLASSIFY_DRAW' ,/ALIGN_center  $
      ,VALUE='�ּ������ʾ')

  DRAW_CLASSIFY_RESULT = WIDGET_DRAW(BASE_CLASSIFY_DRAW	$
  		, UNAME='DRAW_CLASSIFY_RESULT'$
      ,SCR_XSIZE=1 ,SCR_YSIZE=1)

	base_id = Widget_Base(BASE_CLASSIFY_DRAW,SCR_XSIZE=390 ,SCR_YSIZE=380,/col,xpad=0,ypad=0,space=0,/frame)
	colorLevel = $
	[[0B,	0B,		0B],$;1��0
	 [0B,	0B,		255B],$;2��1
	 [0B,	128B,	255B],$;3��2
	 [0B,	255B,	255B],$;4��3
	 [0B,	255B,	128B],$;5��4
	 [0B,	255B,	  0B],$;6��5
	 [128B,	255B,     0B],$;7��6
	 [255B,	255B,	  0B],$;8��7
	 [255B,	128B,	  0B],$;9��8
	 [255B,	  0B,	  0B],$;10��9
	 [255B, 255B,	255B]];��Ч��Ԫ10
	 class=0
	staff_display = {$
		base_id  :base_id,$
		image    :ptr_new(/ALLOCATE_HEAP,/no_copy),$
		startx	 :0.0    , $
	    starty	 :0.0    , $
	    xsize	 :0.0    , $
	    ysize	 :0.0    , $
	    pixelsize:0.0    , $
		palette	 :colorLevel, $
		shapefile:'',$
		legend   :'',$
		class	 :class,$
		title    :''}
	pstaff_display = ptr_new(staff_display, /NO_COPY)

	widget_control, base_id, set_uvalue=pstaff_display

	display,pstaff_display

  state = { $
		widget_top : base_TOP_CLASSIFY,$

		YEAR			:	1990	,$
		MONTH			:	1		,$
		DAY				:	1		,$

        TXT_CLASSIFY_FILE_LY 		:TXT_CLASSIFY_FILE_LY			,$
        TXT_CLASSIFY_FILE_TY		:TXT_CLASSIFY_FILE_TY			,$
        TXT_CLASSIFY_FILE_CLASSIFIED:TXT_CLASSIFY_FILE_CLASSIFIED	,$
        TXT_CLASSIFY_CLOUD_TY     	:TXT_CLASSIFY_CLOUD_TY      	,$
        TXT_CLASSIFY_CLOUD_LY     	:TXT_CLASSIFY_CLOUD_LY      	,$
        BASE_TOP_CLASSIFY			:BASE_TOP_CLASSIFY				,$
        CLOUD						:1								,$
        DRAW_CLASSIFY_RESULT		:DRAW_CLASSIFY_RESULT			,$

		CMD_CLASSIFY_CHOOSE_CLOUD_TY:CMD_CLASSIFY_CHOOSE_CLOUD_TY	,$
		CMD_CLASSIFY_CHOOSE_CLOUD_LY:CMD_CLASSIFY_CHOOSE_CLOUD_LY	,$

        ARR_DATA_TYPE				:['NDVI', 'LAI','NPP' ]			,$
        DATA_TYPE					:'NDVI'							,$

		PARAMETER_SEP					:[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]	,$
		PARAMETER_SEP_NDVI				:[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]	,$
		PARAMETER_SEP_LAI				:[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]	,$
		PARAMETER_SEP_NPP				:[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]	,$
		pstaff_display : pstaff_display $
        }

    pstate = PTR_NEW(state, /no_copy)
    Widget_Control, base_TOP_CLASSIFY, set_uvalue=pstate

	WIDGET_CONTROL, CMD_pick_date, get_uvalue=staff
    staff.pointer = PSTATE
    WIDGET_CONTROL, CMD_pick_date, set_uvalue=staff

    Widget_Control, /REALIZE, BASE_TOP_CLASSIFY
    WIDGET_CONTROL,CMD_CLASSIFY_CANCEL,/INPUT_FOCUS
  	Widget_Control, DST_CLASSIFY_DATA_TYPE,SET_VALUE=(*PSTATE).ARR_DATA_TYPE

  	;**************************************************************************
	;��ͼ���ɫ�ı���
	white=!D.N_COLORS-1
	WIDGET_CONTROL,DRAW_CLASSIFY_RESULT,GET_VALUE=TMEP
	WSET,TMEP
	TEMP=INDGEN(2)
	PLOT,TEMP,BACKGROUND=WHITE
	;**************************************************************************

  ;��ѯ��NDVI�ķּ�����,�����������д�뵽����������
  	TABLE_NAME='CLASSIFY_PARAMETER_9_'+'NDVI'
	PRINT,TABLE_NAME

	;(2)����SQL���,���в�ѯ
   	COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE
   	OD=DBobj
   	ARR_SEP=FLTARR(10)

	SQL='SELECT * FROM CLASSIFY_PARAMETER_9 WHERE (DATA_TYPE = '+"'NDVI')"
	ORS = OBJ_NEW('IDLDBRECORDSET', OD, SQL=SQL)
	ARR_SEP_NDVI=FLTARR(10)
	IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
    	FOR I=0,9 DO BEGIN
    		ARR_SEP_NDVI[I]=ORS->GETFIELD(I+1)
    	ENDFOR
    ENDIF

	SQL='SELECT * FROM CLASSIFY_PARAMETER_9 WHERE (DATA_TYPE = '+"'LAI')"
	ORS = OBJ_NEW('IDLDBRECORDSET', OD, SQL=SQL)
	ARR_SEP_LAI=FLTARR(10)
	IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
    	FOR I=0,9 DO BEGIN
    		ARR_SEP_LAI[I]=ORS->GETFIELD(I+1)
    	ENDFOR
    ENDIF

	SQL='SELECT * FROM CLASSIFY_PARAMETER_9 WHERE (DATA_TYPE = '+"'NPP')"
	ORS = OBJ_NEW('IDLDBRECORDSET', OD, SQL=SQL)
	ARR_SEP_NPP=FLTARR(10)
	IF(ORS->MOVECURSOR(/FIRST) EQ 1)THEN BEGIN
    	FOR I=0,9 DO BEGIN
    		ARR_SEP_NPP[I]=ORS->GETFIELD(I+1)
    	ENDFOR
    ENDIF

	;�Ѷ�ȡ�����ķּ�����д�뵽����������
    (*PSTATE).PARAMETER_SEP_NDVI=ARR_SEP_NDVI
    (*PSTATE).PARAMETER_SEP_LAI=ARR_SEP_LAI
    (*PSTATE).PARAMETER_SEP_NPP=ARR_SEP_NPP

    (*PSTATE).PARAMETER_SEP=(*PSTATE).PARAMETER_SEP_NDVI
    PRINT, 'HERE1',(*PSTATE).PARAMETER_SEP
  ;**************************************************************************
  OBJ_DESTROY,ORS

  ;***********************************************************************
  ;Ϊ��ϵͳ��ʾ�����ӵĴ���
  TEMP=1
  IF(TEMP EQ 1)THEN BEGIN
  	;(1)���̼�������

		(*PSTATE).YEAR	=	strtrim(c_year,2)
	  	(*PSTATE).MONTH	=	strtrim(c_month,2)
	  	(*PSTATE).DAY	=	strtrim(c_day,2)

	  	WIDGET_CONTROL,TXT_CLASSIFY_FILE_LY, SET_VALUE=''
	  	WIDGET_CONTROL,TXT_CLASSIFY_FILE_TY, SET_VALUE=''
	  	WIDGET_CONTROL,TXT_CLASSIFY_CLOUD_LY, SET_VALUE=''
	  	WIDGET_CONTROL,TXT_CLASSIFY_CLOUD_LY, SET_VALUE=''

	  	WIDGET_CONTROL,TXT_CLASSIFY_FILE_CLASSIFIED, SET_VALUE=''
	  	WIDGET_CONTROL,CMD_CLASSIFY_CHOOSE_CLOUD_TY,SENSITIVE=0
	    WIDGET_CONTROL,CMD_CLASSIFY_CHOOSE_CLOUD_LY,SENSITIVE=0
	    WIDGET_CONTROL,TXT_CLASSIFY_CLOUD_TY,SENSITIVE=0
	    WIDGET_CONTROL,TXT_CLASSIFY_CLOUD_LY,SENSITIVE=0
	    (*PSTATE).CLOUD=0
	    WIDGET_CONTROL,CMD_CLASSIFY_CLOUD,SET_BUTTON=0

		defaultnames_zsrtclf,{ID:(*PSTATE).widget_top, TOP:(*PSTATE).widget_top}
  ENDIF
  ;***********************************************************************

  XManager, 'BASE_TOP_CLASSIFY', BASE_TOP_CLASSIFY, /NO_BLOCK, CLEANUP='ZS_classify_cleanup'

end

pro ZS_classify_cleanup, id
	Widget_Control, id, get_uvalue=pstate

	ptr_free, (*((*pstate).pstaff_display)).image
	ptr_free, (*pstate).pstaff_display

	ptr_free, pstate
	common_log,'�رղ�ֵ�ּ�'
end
;
; Empty stub procedure used for autoloading.
;
pro ZS_classify, GROUP_LEADER=wGroup;, _EXTRA=_VWBExtra_
	common_log,'������ֵ�ּ�'

  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  BASE_TOP_CLASSIFY, GROUP_LEADER=BASE_TOP;, _EXTRA=_VWBExtra_
end

pro defaultnames_zsrtclf, event
	Widget_Control, event.top, get_uvalue=pstate

	COMMON COMMON_SETPATH,ppath

	v_zs_in_path  = (*ppath).zs_in_path
	v_zs_out_path = (*ppath).zs_out_path

	ndvi_prefix=(*ppath).ndvi_prefix
	ndvi_suffix=(*ppath).ndvi_suffix
	lai_prefix =(*ppath).lai_prefix
	lai_suffix =(*ppath).lai_suffix
	npp_prefix =(*ppath).npp_prefix
	npp_suffix =(*ppath).npp_suffix
	cld_prefix =(*ppath).cld_prefix
	cld_suffix =(*ppath).cld_suffix
	clf_prefix =(*ppath).clf_prefix
	clf_suffix =(*ppath).clf_suffix

	tyear = strtrim((*PSTATE).YEAR,2)
	lyear = strtrim((*PSTATE).YEAR-1,2)
	;�����µ�λ������
	IF ((*PSTATE).MONTH LE 9) THEN BEGIN
		month='0'+STRTRIM((*PSTATE).MONTH,2)
	ENDIF ELSE BEGIN
		month=STRTRIM((*PSTATE).MONTH,2)
	ENDELSE
	;�������λ������
	IF ((*PSTATE).DAY LE 9) THEN BEGIN
		day='0'+STRTRIM((*PSTATE).DAY,2)
	ENDIF ELSE BEGIN
		day=STRTRIM((*PSTATE).DAY,2)
	ENDELSE

	ttime = tyear + month + day
    ltime = lyear + month + day

	case (*PSTATE).DATA_TYPE of
		'NDVI':begin
			prefix = ndvi_prefix
			suffix = ndvi_suffix
		end
		'LAI':begin
			prefix = lai_prefix
			suffix = lai_suffix
		end
		'NPP':begin
			prefix = npp_prefix
			suffix = npp_suffix
		end
		else:begin
			prefix = ndvi_prefix
			suffix = ndvi_suffix
		end
	endcase

  	WIDGET_CONTROL,(*PSTATE).TXT_CLASSIFY_FILE_LY, SET_VALUE= v_zs_in_path + prefix + ltime + suffix
  	WIDGET_CONTROL,(*PSTATE).TXT_CLASSIFY_FILE_TY, SET_VALUE= v_zs_in_path + prefix + ttime + suffix
  	WIDGET_CONTROL,(*PSTATE).TXT_CLASSIFY_CLOUD_LY, SET_VALUE= v_zs_in_path + cld_prefix + ltime + cld_suffix
  	WIDGET_CONTROL,(*PSTATE).TXT_CLASSIFY_CLOUD_TY, SET_VALUE= v_zs_in_path + cld_prefix + ttime + cld_suffix
  	WIDGET_CONTROL,(*PSTATE).TXT_CLASSIFY_FILE_CLASSIFIED, SET_VALUE= v_zs_out_path + clf_prefix + ttime + clf_suffix
end
