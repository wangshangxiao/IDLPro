;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;
; Generated on:	08/29/2006 08:16.54


;����ʱ�䣺2006.08.27
;������Ա��������
;������Ƚ���




;
pro GL_diaodu_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)


  wWidget =  Event.top

  case wTarget of

       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_close'): begin
       common_log,'�ر��������'
		  widget_control,event.top,/destroy
	   end

       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_help'):begin

			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '�������', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('�Ҳ��������ĵ�',title='����')
			endelse
		end
;		ONLINE_HELP, '�������', BOOK='HELP\HELP.chm', /FULL_PATH
;;        temp=dialog_message('ϵͳ��û�а���')
;       end

       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_GL_mis_search'):GL_xianshi            ;������ʾ
;       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_GL_pro_manager'):GL_jindu            ;���ȹ���
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_SJ_datainput'):SJ_todatabase          ;���ݵ���
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_SJ_datamaintance'):SJ_datamaintance, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra   ;����ά��
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_NQ_InterPolate'):NQ_InterPolate_intermain        ;�����ֵ
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_NQ_classify'):NQ_classify_classifyagri           ;�Աȷ���
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_FZ_calculate'):FZ_calculate                     ;ָ����ȡ
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_FZ_statistic'):FZ_statistic                     ;�ռ�ͳ��
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_DC_trendyield'):DC_TrendYield, GROUP_LEADER=wGroup     ;���Ʋ���
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_DC_floatyield'):DC_FloatYield, GROUP_LEADER=wGroup     ;��������
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_DC_yieldmerge'):DC_YieldMerge,GROUP_LEADER=groupleader ;�����ں�
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_21'):SD_ROI_Manage, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_      ;���������
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_22'):SD_Preference, GROUP_LEADER=wGroup                    ;·���趨
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_23'):SD_DB_Connect, GROUP_LEADER=wGroup                    ;���ݿ�����
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_24'):SJ_abnormalcheck, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_           ;�쳣���
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_29'):ZS_PRO_STA, GROUP_LEADER=wGroup                       ;ͳ��
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_30'):ZS_HANTS, GROUP_LEADER=wGroup                         ;�����ع�
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_31'):ZS_classify, GROUP_LEADER=wGroup                      ;��ֵ����
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_32'):ZS_STA_PRO_ABOVE_COUNTY, GROUP_LEADER=wGroup          ;���ܣ����̣�
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_33'):ZS_STA_RT_ABOVE_COUNTY, GROUP_LEADER=wGroup           ;���ܣ�ʵʱ��
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_34'):ZS_RT_STA, GROUP_LEADER=wGroup                        ;�ռ�ͳ��
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_0'):ZS_JPG, GROUP_LEADER=WGROUP
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_35'):MJ_Extrapolate_GVG, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_     ;��������
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_36'):GVG_TO_DATABASE, GROUP_LEADER=event.top, _EXTRA=_VWBExtra         ;GVG�������                                         ;�������
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_37'):MJ_Extrapolate_Proportion, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_     ;��������
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_38'):MJ_statistical, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_       ;���ͳ��
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_39'):MJ_tocalculate, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_    ;�������
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_43'):CL, GROUP_LEADER=wGroup                               ;��������


    else:
  endcase

end

pro GL_diaodu, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

	common_log,'�����������'

   IF ( XREGISTERED('GL_diaodu') NE 0 ) THEN RETURN

;  Resolve_Routine, 'GL_diaodu_jiemian_3_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  WID_BASE_1 = Widget_Base( GROUP_LEADER=BASE_TOP, UNAME='WID_BASE_1'  $
      ,XOFFSET=140 ,YOFFSET=200 ,SCR_XSIZE=768 ,SCR_YSIZE=464  $
      ,TITLE='�������' ,SPACE=3 ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR=1)


  WID_BASE_2 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_2' ,FRAME=1  $
      ,XOFFSET=7 ,YOFFSET=385 ,SCR_XSIZE=748 ,SCR_YSIZE=40  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BUTTON_close = Widget_Button(WID_BASE_2, UNAME='WID_BUTTON_close'  $
      ,XOFFSET=572 ,YOFFSET=7 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='�ر�')


  WID_BUTTON_help = Widget_Button(WID_BASE_2, UNAME='WID_BUTTON_help'  $
      ,XOFFSET=80 ,YOFFSET=7 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='����')


  WID_BASE_3 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_3' ,FRAME=1  $
      ,XOFFSET=7 ,YOFFSET=7 ,SCR_XSIZE=748 ,SCR_YSIZE=371  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BASE_0 = Widget_Base(WID_BASE_3, UNAME='WID_BASE_0' ,XOFFSET=7  $
      ,YOFFSET=7 ,SCR_XSIZE=31 ,SCR_YSIZE=356 ,TITLE='IDL' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)

;-------------------------------------------------------------------------------
     text=''+string(byte(13))+string(byte(10))+   $
          'ҵ'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '��'

  WID_label_yewu = Widget_label(WID_BASE_0, UNAME='WID_label_yewu' ,XOFFSET=1  $
      ,YOFFSET=0 ,frame=1,SCR_XSIZE=27 ,SCR_YSIZE=104, /ALIGN_CENTER  $
      ,value=text)

;-------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          'ϵ'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          'ͳ'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '��'

  WID_label_xitong = Widget_label(WID_BASE_0, UNAME='WID_label_xitong' ,XOFFSET=1  $
      ,YOFFSET=125 ,frame=1,SCR_XSIZE=27 ,SCR_YSIZE=104,/ALIGN_CENTER  $
      ,value=text)

;---------------------------------------------------------------------------------
     text=''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '��'

  WID_label_shuju = Widget_label(WID_BASE_0, UNAME='WID_label_shuju' ,XOFFSET=1  $
      ,YOFFSET=250 ,frame=1,SCR_XSIZE=27 ,SCR_YSIZE=104,/ALIGN_CENTER  $
      ,value=text)
;------------------------------------------------------------------------------------

;  WID_BASE_4 = Widget_Base(WID_BASE_3, UNAME='WID_BASE_4' ,XOFFSET=39  $
;      ,YOFFSET=7 ,SCR_XSIZE=25 ,SCR_YSIZE=356 ,TITLE='IDL' ,SPACE=3  $
;      ,XPAD=3 ,YPAD=3)
;

;  WID_DRAW_3 = Widget_DRAW(WID_BASE_4, UNAME='WID_DRAW_3' ,YOFFSET=10  $
;      ,SCR_XSIZE=25 ,SCR_YSIZE=80 )
;
;
;  WID_DRAW_4 = Widget_Draw(WID_BASE_4, UNAME='WID_DRAW_4'  $
;      ,YOFFSET=131 ,SCR_XSIZE=25 ,SCR_YSIZE=95)
;
;
;  WID_DRAW_5 = Widget_Draw(WID_BASE_4, UNAME='WID_DRAW_5'  $
;      ,YOFFSET=258 ,SCR_XSIZE=25 ,SCR_YSIZE=95)

 WID_BASE_4 = Widget_Base(WID_BASE_3, UNAME='WID_BASE_4' ,XOFFSET=37  $
      ,YOFFSET=7 ,SCR_XSIZE=29 ,SCR_YSIZE=356 ,TITLE='IDL' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)


  WID_LABEL_42 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_42'  $
      ,YOFFSET=48 ,/ALIGN_RIGHT ,VALUE='��')


;  WID_LABEL_43 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_43'  $
;      ,XOFFSET=14 ,YOFFSET=69 ,/ALIGN_LEFT ,VALUE='->')


;  WID_LABEL_44 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_44'  $
;      ,XOFFSET=10 ,YOFFSET=62 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
;      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_45 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_45'  $
      ,XOFFSET=11 ,YOFFSET=48 ,/ALIGN_LEFT ,VALUE='-->')


;  WID_LABEL_46 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_46'  $
;      ,XOFFSET=10 ,YOFFSET=33 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
;      ,/ALIGN_CENTER ,VALUE='|')


;  WID_LABEL_47 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_47'  $
;      ,XOFFSET=10 ,YOFFSET=47 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
;      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_68 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_68'  $
      ,YOFFSET=175 ,/ALIGN_RIGHT ,VALUE='��')


  WID_LABEL_69 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_69'  $
      ,XOFFSET=10 ,YOFFSET=161 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_70 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_70'  $
      ,XOFFSET=14 ,YOFFSET=175 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_71 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_71'  $
      ,XOFFSET=14 ,YOFFSET=142 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_72 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_72'  $
      ,XOFFSET=10 ,YOFFSET=147 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_73 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_73'  $
      ,XOFFSET=10 ,YOFFSET=175 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_80 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_80'  $
      ,XOFFSET=10 ,YOFFSET=189 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_81 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_81'  $
      ,XOFFSET=10 ,YOFFSET=203 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_82 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_82'  $
      ,XOFFSET=14 ,YOFFSET=209 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_74 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_74'  $
      ,YOFFSET=302 ,/ALIGN_RIGHT ,VALUE='��')


  WID_LABEL_85 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_85'  $
      ,XOFFSET=14 ,YOFFSET=334 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_86 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_86'  $
      ,XOFFSET=14 ,YOFFSET=302 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_87 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_87'  $
      ,XOFFSET=14 ,YOFFSET=269 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_88 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_88'  $
      ,XOFFSET=10 ,YOFFSET=328 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_89 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_89'  $
      ,XOFFSET=10 ,YOFFSET=314 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_90 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_90'  $
      ,XOFFSET=10 ,YOFFSET=300 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_91 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_91'  $
      ,XOFFSET=10 ,YOFFSET=274 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_92 = Widget_Label(WID_BASE_4, UNAME='WID_LABEL_92'  $
      ,XOFFSET=10 ,YOFFSET=286 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')

;===============================================================================


  WID_BASE_5 = Widget_Base(WID_BASE_3, UNAME='WID_BASE_5' ,XOFFSET=67  $
      ,YOFFSET=7 ,SCR_XSIZE=96 ,SCR_YSIZE=356 ,TITLE='IDL' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)


  WID_BUTTON_GL_mis_search = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_GL_mis_search'  $
      ,YOFFSET=42 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='image/�����ѯ.bmp',/bitmap)        ;�����ѯ


;  WID_BUTTON_GL_pro_manager = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_GL_pro_manager'  $
;      ,YOFFSET=60 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
;      ,VALUE='image/���ȹ���.bmp',/bitmap)             ;���ȹ���


  WID_BUTTON_21 = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_21'  $   ;���������
      ,YOFFSET=199 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='image/���������.bmp',/bitmap)


  WID_BUTTON_22 = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_22'  $    ;·���趨
      ,YOFFSET=167 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,value='image/·���趨.bmp',/bitmap)
;      ,VALUE='Image\LUJING01.bmp' ,/BITMAP)


  WID_BUTTON_23 = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_23'  $
      ,YOFFSET=135 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='image/���ݿ�����.bmp',/bitmap)       ;���ݿ�����


  WID_BUTTON_24 = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_24'  $
      ,YOFFSET=327 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='image/�쳣���.bmp',/bitmap)                      ;�쳣���


  WID_BUTTON_SJ_datamaintance = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_SJ_datamaintance'  $
      ,YOFFSET=295 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,value='Image\����ά��.bmp' ,/BITMAP)       ;����ά��


  WID_BUTTON_SJ_datainput = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_SJ_datainput'  $
      ,YOFFSET=263 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='Image\���ݵ���.bmp' ,/BITMAP)      ;���ݵ���

;---------------------------------------------------------------------------------
     text=''+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          'ũ'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          'ң'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          'ҵ'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '��'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          'ģ'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '��'



  WID_label_mokuai = Widget_label(WID_BASE_3, UNAME='WID_label_mokuai'  $
      ,XOFFSET=184,frame=1 ,YOFFSET=7 ,SCR_XSIZE=29 ,SCR_YSIZE=353   $
      ,/ALIGN_CENTER,value=text)
;------------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          ' �� ��'+string(byte(13))+string(byte(10))+   $
          ' �� ��'

  WID_label_zhangshi = Widget_label(WID_BASE_3, UNAME='WID_label_zhangshi'  $
      ,XOFFSET=233 ,YOFFSET=31 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)

;------------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          ' �� ��'+string(byte(13))+string(byte(10))+   $
          ' �� ��'


  WID_labe_mianji = Widget_label(WID_BASE_3, UNAME='WID_labe_mianji'  $
      ,XOFFSET=233 ,YOFFSET=126 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)

;------------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          ' �� ��'+string(byte(13))+string(byte(10))+   $
          ' Ԥ ��'


  WID_label_danchan = Widget_label(WID_BASE_3, UNAME='label_danchan'  $
      ,XOFFSET=233 ,YOFFSET=198 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)
;------------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          ' ũ ��'+string(byte(13))+string(byte(10))+   $
          ' �� ��'

  WID_label_nongqi = Widget_label(WID_BASE_3, UNAME='WID_label_nongqi'  $
      ,XOFFSET=233 ,YOFFSET=257 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)
;------------------------------------------------------------------------------------
    text=''+string(byte(13))+string(byte(10))+   $
          ' �� ��'+string(byte(13))+string(byte(10))+   $
          ' ָ ��'

  WID_label_fuzhong = Widget_label(WID_BASE_3, UNAME='WID_label_fuzhong'  $
      ,XOFFSET=233 ,YOFFSET=315 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)
;------------------------------------------------------------------------------------

;  WID_DRAW_20 = Widget_Draw(WID_BASE_3, UNAME='WID_DRAW_20'  $
;      ,XOFFSET=276 ,YOFFSET=11 ,SCR_XSIZE=25 ,SCR_YSIZE=80)


  WID_BUTTON_29 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_29'  $
      ,XOFFSET=366 ,YOFFSET=17 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\ͳ��.bmp' ,/BITMAP)       ;ͳ��

;------------------------------------------------------------------------------------
      text=' �� ��'+string(byte(13))+string(byte(10))+   $
          ' �� ��'

  WID_label_guocheng = Widget_label(WID_BASE_3, UNAME='WID_label_guocheng'  $
      ,XOFFSET=302 ,YOFFSET=17 ,SCR_XSIZE=40 ,SCR_YSIZE=25,frame=1   $
      ,value=text)


      text=  $
          ' ʵ ʱ'+string(byte(13))+string(byte(10))+   $
          ' �� ��'

  WID_label_shishi = Widget_label(WID_BASE_3, UNAME='WID_label_shishi'  $
      ,XOFFSET=302 ,YOFFSET=65 ,SCR_XSIZE=40 ,SCR_YSIZE=25,frame=1   $
      ,value=text)

;------------------------------------------------------------------------------------
  WID_BUTTON_30 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_30'  $    ;�����ع�
      ,XOFFSET=481 ,YOFFSET=17 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�����ع�.bmp' ,/BITMAP )


  WID_BUTTON_31 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_31'  $
      ,XOFFSET=366 ,YOFFSET=65 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\��ֵ�ּ�.bmp' ,/BITMAP )            ;��ֵ�ּ�


  WID_BUTTON_32 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_32'  $
      ,XOFFSET=594 ,YOFFSET=17 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\����.bmp' ,/BITMAP)      ;����


  WID_LABEL_0 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_0'  $
      ,XOFFSET=460 ,YOFFSET=23 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_1 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_1'  $
      ,XOFFSET=345 ,YOFFSET=23 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_2 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_2'  $
      ,XOFFSET=345 ,YOFFSET=71 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_3 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_3'  $
      ,XOFFSET=575 ,YOFFSET=23 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_4 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_4'  $
      ,XOFFSET=213 ,YOFFSET=45 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_6 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_6'  $
      ,XOFFSET=213 ,YOFFSET=217 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_7 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_7'  $
      ,XOFFSET=213 ,YOFFSET=277 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_8 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_8'  $
      ,XOFFSET=213 ,YOFFSET=335 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_9 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_9'  $
      ,XOFFSET=164 ,YOFFSET=152 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_10 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_10'  $
      ,XOFFSET=164 ,YOFFSET=183 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_11 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_11'  $
      ,XOFFSET=164 ,YOFFSET=215 ,/ALIGN_LEFT ,VALUE='-->')


;  WID_DRAW_21 = Widget_Draw(WID_BASE_3, UNAME='WID_DRAW_21'  $
;      ,XOFFSET=688 ,YOFFSET=15 ,SCR_XSIZE=25 ,SCR_YSIZE=78)


  WID_LABEL_12 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_12'  $
      ,XOFFSET=460 ,YOFFSET=71 ,/ALIGN_LEFT ,VALUE='-->')


  WID_BUTTON_33 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_33'  $
      ,XOFFSET=594 ,YOFFSET=65 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\����.bmp' ,/BITMAP)      ;����


  WID_BUTTON_34 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_34'  $
      ,XOFFSET=481 ,YOFFSET=65 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�ռ�ͳ��.bmp' ,/BITMAP)     ;�ռ�ͳ��


  WID_LABEL_13 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_13'  $
      ,XOFFSET=574 ,YOFFSET=71 ,/ALIGN_LEFT ,VALUE='-->')


;----------�������-����---------------------------------------------------------

;     text=''+string(byte(13))+string(byte(10))+   $
;          ''+string(byte(13))+string(byte(10))+   $
;          '��'+string(byte(13))+string(byte(10))+   $
;          '��'+string(byte(13))+string(byte(10))+   $
;          '��'+string(byte(13))+string(byte(10))+   $
;          '��'
;
;
;
;
;
;  WID_LABEL_RESULT = Widget_LABEL(WID_BASE_3, UNAME='WID_RESULT'  $   ;�������
;      ,XOFFSET=716 ,YOFFSET=12 ,SCR_XSIZE=21 ,SCR_YSIZE=84  $
;      ,/ALIGN_CENTER ,VALUE=text)

  WID_BUTTON_0 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_0'  $   ;�������
      ,XOFFSET=714 ,YOFFSET=10 ,SCR_XSIZE=25 ,SCR_YSIZE=88  $
      ,/ALIGN_CENTER ,VALUE='Image\�������.BMP',/BITMAP)



;-------------------------------------------------------------------------

  WID_BUTTON_35 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_35'  $
      ,XOFFSET=481 ,YOFFSET=160 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\��������.bmp' ,/BITMAP)       ;��������


  WID_LABEL_14 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_14'  $
      ,XOFFSET=460 ,YOFFSET=166 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_5 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_5'  $
      ,XOFFSET=213 ,YOFFSET=140 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_15 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_15'  $
      ,XOFFSET=345 ,YOFFSET=166 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_16 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_16'  $
      ,XOFFSET=345 ,YOFFSET=118 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_17 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_17'  $
      ,XOFFSET=460 ,YOFFSET=118 ,/ALIGN_LEFT ,VALUE='-->')


  WID_BUTTON_36 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_36'  $
      ,XOFFSET=366 ,YOFFSET=160 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�������.bmp' ,/BITMAP)            ;�������


  WID_BUTTON_37 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_37'  $
      ,XOFFSET=481 ,YOFFSET=112 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER,VALUE='Image\��������.bmp' ,/BITMAP)       ;��������
;==================================================================================

     text=' �� ֲ'+string(byte(13))+string(byte(10))+   $
          ' �� ��'

  WID_label_jiegou = Widget_label(WID_BASE_3, UNAME='WID_label_jiegou'  $
      ,XOFFSET=302 ,YOFFSET=158 ,SCR_XSIZE=40 ,SCR_YSIZE=25,frame=1   $
      ,value=text)

;------------------------------------------------------------------------------

     text=' �� ֲ'+string(byte(13))+string(byte(10))+   $
          ' �� ��'

  WID_label_chengshu = Widget_label(WID_BASE_3, UNAME='WID_label_chengshu'  $
      ,XOFFSET=302 ,YOFFSET=111 ,SCR_XSIZE=40 ,SCR_YSIZE=25,frame=1   $
      ,value=text)

;==================================================================================
  WID_BUTTON_38 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_38'  $
      ,XOFFSET=366 ,YOFFSET=112 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�ռ�ͳ��.bmp' ,/BITMAP)     ;�ռ�ͳ��


;  WID_DRAW_22 = Widget_Draw(WID_BASE_3, UNAME='WID_DRAW_22'  $
;      ,XOFFSET=276 ,YOFFSET=106 ,SCR_XSIZE=25 ,SCR_YSIZE=80)
;
;
;  WID_DRAW_27 = Widget_Draw(WID_BASE_3, UNAME='WID_DRAW_27'  $
;      ,XOFFSET=576 ,YOFFSET=109 ,SCR_XSIZE=25 ,SCR_YSIZE=78)


  WID_BUTTON_39 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_39'  $
      ,XOFFSET=602 ,YOFFSET=135 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�������.bmp' ,/BITMAP)           ;�������


  WID_LABEL_18 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_18'  $
      ,XOFFSET=504 ,YOFFSET=217 ,/ALIGN_LEFT ,VALUE='-->')


  WID_BUTTON_DC_floatyield = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_DC_floatyield'  $
      ,XOFFSET=411 ,YOFFSET=211 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\��������.bmp' ,/BITMAP)      ;��������


  WID_BUTTON_DC_yieldmerge = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_DC_yieldmerge'  $
      ,XOFFSET=524 ,YOFFSET=211 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�����ں�.bmp' ,/BITMAP)     ;�����ں�


  WID_LABEL_19 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_19'  $
      ,XOFFSET=390 ,YOFFSET=217 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_20 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_20'  $
      ,XOFFSET=276 ,YOFFSET=217 ,/ALIGN_LEFT ,VALUE='-->')


  WID_BUTTON_DC_trendyield = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_DC_trendyield'  $
      ,XOFFSET=296 ,YOFFSET=211 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\���Ʋ���.bmp' ,/BITMAP)       ;���Ʋ���


  WID_BUTTON_43 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_43'  $
      ,XOFFSET=638 ,YOFFSET=211 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\��������.bmp' ,/BITMAP)        ;��������


  WID_LABEL_21 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_21'  $
      ,XOFFSET=618 ,YOFFSET=217 ,/ALIGN_LEFT ,VALUE='-->')


;  WID_LABEL_22 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_22'  $
;      ,XOFFSET=661 ,YOFFSET=191 ,/ALIGN_LEFT ,VALUE='|')


  WID_LABEL_23 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_23'  $
      ,XOFFSET=659 ,YOFFSET=190 ,/ALIGN_LEFT ,VALUE='\/')


  WID_LABEL_24 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_24'  $
      ,XOFFSET=661 ,YOFFSET=177 ,/ALIGN_LEFT ,VALUE='|')


  WID_BUTTON_NQ_InterPolate = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_NQ_InterPolate'  $
      ,XOFFSET=296 ,YOFFSET=269 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�����ֵ.bmp' ,/BITMAP )             ;�����ֵ


  WID_LABEL_26 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_26'  $
      ,XOFFSET=276 ,YOFFSET=277 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_27 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_27'  $
      ,XOFFSET=390 ,YOFFSET=277 ,/ALIGN_LEFT ,VALUE='-->')


  WID_BUTTON_NQ_classify = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_NQ_classify'  $
      ,XOFFSET=411 ,YOFFSET=269 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�Աȷ���.bmp' ,/BITMAP)    ;�Աȷ���


  WID_BUTTON_FZ_calculate = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_FZ_calculate'  $
      ,XOFFSET=296 ,YOFFSET=327 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\ָ����ȡ.bmp' ,/BITMAP)     ;ָ����ȡ


  WID_LABEL_28 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_28'  $
      ,XOFFSET=276 ,YOFFSET=335 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_29 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_29'  $
      ,XOFFSET=390 ,YOFFSET=335 ,/ALIGN_LEFT ,VALUE='-->')
;
;
;  WID_BUTTON_47 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_47'  $
;      ,XOFFSET=524 ,YOFFSET=330 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
;      ,/ALIGN_CENTER ,VALUE='ͳ��')


  WID_BUTTON_FZ_statistic = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_FZ_statistic'  $
      ,XOFFSET=411 ,YOFFSET=327 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\�ռ�ͳ��.bmp' ,/BITMAP)      ;�ռ�ͳ��


;  WID_LABEL_30 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_30'  $
;      ,XOFFSET=504 ,YOFFSET=336 ,/ALIGN_LEFT ,VALUE='-->')



  WID_LABEL_25 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_25'  $
      ,XOFFSET=661 ,YOFFSET=163 ,/ALIGN_LEFT ,VALUE='|')

    WID_LABEL_31 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_31'  $
      ,XOFFSET=283 ,YOFFSET=31 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_32 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_32'  $
      ,XOFFSET=287 ,YOFFSET=26 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_35 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_35'  $
      ,XOFFSET=283 ,YOFFSET=60 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_37 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_37'  $
      ,XOFFSET=287 ,YOFFSET=67 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_95 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_95'  $
      ,XOFFSET=283 ,YOFFSET=45 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')

  WID_LABEL_40 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_40'  $
      ,XOFFSET=275 ,YOFFSET=46 ,/ALIGN_RIGHT ,VALUE='��')

;==================��������ļ�ͷ=============================================================
;
;  WID_LABEL_32 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_32'  $
;      ,XOFFSET=287 ,YOFFSET=26 ,/ALIGN_LEFT ,VALUE='->')
;
;
;  WID_LABEL_35 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_35'  $
;      ,XOFFSET=283 ,YOFFSET=60 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
;      ,/ALIGN_CENTER ,VALUE='|')
;
;
;  WID_LABEL_37 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_37'  $
;      ,XOFFSET=287 ,YOFFSET=67 ,/ALIGN_LEFT ,VALUE='->')
;
;
;  WID_LABEL_95 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_95'  $
;      ,XOFFSET=283 ,YOFFSET=45 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
;      ,/ALIGN_CENTER ,VALUE='|')
;
;  WID_LABEL_40 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_40'  $
;      ,XOFFSET=275 ,YOFFSET=146 ,/ALIGN_RIGHT ,VALUE='??')

;===========��������ļ�ͷ====================================================================


  WID_LABEL_95 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_95'  $
      ,XOFFSET=284 ,YOFFSET=140 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_31 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_31'  $
      ,XOFFSET=284 ,YOFFSET=126 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_32 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_32'  $
      ,XOFFSET=288 ,YOFFSET=121 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_35 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_35'  $
      ,XOFFSET=284 ,YOFFSET=155 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_37 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_37'  $
      ,XOFFSET=288 ,YOFFSET=162 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_40 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_40'  $
      ,XOFFSET=276 ,YOFFSET=141 ,/ALIGN_RIGHT ,VALUE='��')


;=========���ܺ�ļ�ͷ======================================================================


  WID_LABEL_101 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_101'  $
      ,XOFFSET=696 ,YOFFSET=29 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_102 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_102'  $
      ,XOFFSET=696 ,YOFFSET=42 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_104 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_104'  $
      ,XOFFSET=696 ,YOFFSET=55 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_106 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_106'  $
      ,XOFFSET=696 ,YOFFSET=68 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_108 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_108'  $
      ,XOFFSET=699 ,YOFFSET=49 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_98 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_98'  $
      ,XOFFSET=688 ,YOFFSET=24 ,/ALIGN_RIGHT ,VALUE='��')


  WID_LABEL_99 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_99'  $
      ,XOFFSET=688 ,YOFFSET=74 ,/ALIGN_RIGHT ,VALUE='��')

 ;==============�������ƺ�ļ�ͷ=====================================================


  WID_LABEL_109 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_109'  $
      ,XOFFSET=587 ,YOFFSET=142 ,/ALIGN_LEFT ,VALUE='->')


  WID_LABEL_110 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_110'  $
      ,XOFFSET=584 ,YOFFSET=163 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_111 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_111'  $
      ,XOFFSET=584 ,YOFFSET=150 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_112 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_112'  $
      ,XOFFSET=584 ,YOFFSET=137 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_113 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_113'  $
      ,XOFFSET=584 ,YOFFSET=124 ,SCR_XSIZE=4 ,SCR_YSIZE=14  $
      ,/ALIGN_CENTER ,VALUE='|')


  WID_LABEL_114 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_114'  $
      ,XOFFSET=576 ,YOFFSET=169 ,/ALIGN_RIGHT ,VALUE='��')


  WID_LABEL_115 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_115'  $
      ,XOFFSET=576 ,YOFFSET=119 ,/ALIGN_RIGHT ,VALUE='��')

  Widget_Control, WID_BASE_1, /REALIZE
  WIDGET_CONTROL,WID_BUTTON_close,/INPUT_FOCUS



 ;=============================================================================
 ;=============================================================================
 ;=============================================================================
;  WIDGET_CONTROL, WID_DRAW_0, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\left_01.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_1, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\left_02.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_2, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\left_03.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_3, GET_VALUE=DRAWID                 ;ע��Ҫ�õ����ֵ,������"����ܹ�ʵ��֮��"/REALIZE��ſ��Եõ�.
;      WSET, drawid
;      TVSCL, READ_BMP('Image\25_80.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_4, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\25_95_3.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_5, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\25_95_3.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_9, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\29_356.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_11, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\mid_01.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_12, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\mianji.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_14, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\����Ԥ��.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_16, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\ũ������.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_18, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\����ָ��.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_20, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\25_80.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_22, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\25_80.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_23, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\���̼��.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_24, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\ʵʱ���.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_26, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\��ֲ����.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_25, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\��ֲ�ṹ.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_21, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\25_78.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_27, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\25_78.bmp',/RGB),/TRUE




  XManager, 'GL_diaodu', WID_BASE_1, /NO_BLOCK
;
;	COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
;  GL_diaodu,GROUP_LEADER=BASE_TOP

end
;
; Empty stub procedure used for autoloading.
;
;pro GL_diaodu_jiemian_3, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
;  WID_BASE_1, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
;end
