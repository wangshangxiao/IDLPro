;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;
; Generated on:	08/29/2006 08:16.54


;创建时间：2006.08.27
;创建人员：杨绍锷
;任务调度界面




;
pro GL_diaodu_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)


  wWidget =  Event.top

  case wTarget of

       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_close'): begin
       common_log,'关闭任务调度'
		  widget_control,event.top,/destroy
	   end

       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_help'):begin

			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '任务调度', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('找不到帮助文档',title='警告')
			endelse
		end
;		ONLINE_HELP, '任务调度', BOOK='HELP\HELP.chm', /FULL_PATH
;;        temp=dialog_message('系统暂没有帮助')
;       end

       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_GL_mis_search'):GL_xianshi            ;任务显示
;       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_GL_pro_manager'):GL_jindu            ;进度管理
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_SJ_datainput'):SJ_todatabase          ;数据导入
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_SJ_datamaintance'):SJ_datamaintance, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra   ;数据维护
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_NQ_InterPolate'):NQ_InterPolate_intermain        ;气象插值
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_NQ_classify'):NQ_classify_classifyagri           ;对比分析
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_FZ_calculate'):FZ_calculate                     ;指数提取
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_FZ_statistic'):FZ_statistic                     ;空间统计
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_DC_trendyield'):DC_TrendYield, GROUP_LEADER=wGroup     ;趋势产量
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_DC_floatyield'):DC_FloatYield, GROUP_LEADER=wGroup     ;波动产量
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_DC_yieldmerge'):DC_YieldMerge,GROUP_LEADER=groupleader ;产量融合
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_21'):SD_ROI_Manage, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_      ;监测区定义
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_22'):SD_Preference, GROUP_LEADER=wGroup                    ;路径设定
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_23'):SD_DB_Connect, GROUP_LEADER=wGroup                    ;数据库链接
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_24'):SJ_abnormalcheck, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_           ;异常检测
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_29'):ZS_PRO_STA, GROUP_LEADER=wGroup                       ;统计
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_30'):ZS_HANTS, GROUP_LEADER=wGroup                         ;曲线重构
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_31'):ZS_classify, GROUP_LEADER=wGroup                      ;差值分类
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_32'):ZS_STA_PRO_ABOVE_COUNTY, GROUP_LEADER=wGroup          ;汇总（过程）
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_33'):ZS_STA_RT_ABOVE_COUNTY, GROUP_LEADER=wGroup           ;汇总（实时）
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_34'):ZS_RT_STA, GROUP_LEADER=wGroup                        ;空间统计
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_0'):ZS_JPG, GROUP_LEADER=WGROUP
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_35'):MJ_Extrapolate_GVG, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_     ;比例外推
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_36'):GVG_TO_DATABASE, GROUP_LEADER=event.top, _EXTRA=_VWBExtra         ;GVG数据入库                                         ;数据入库
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_37'):MJ_Extrapolate_Proportion, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_     ;成数外推
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_38'):MJ_statistical, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_       ;面积统计
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_39'):MJ_tocalculate, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_    ;面积估算
       widget_info(wWidget,FIND_BY_UNAME='WID_BUTTON_43'):CL, GROUP_LEADER=wGroup                               ;产量估算


    else:
  endcase

end

pro GL_diaodu, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

	common_log,'启动任务调度'

   IF ( XREGISTERED('GL_diaodu') NE 0 ) THEN RETURN

;  Resolve_Routine, 'GL_diaodu_jiemian_3_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

  COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP
  WID_BASE_1 = Widget_Base( GROUP_LEADER=BASE_TOP, UNAME='WID_BASE_1'  $
      ,XOFFSET=140 ,YOFFSET=200 ,SCR_XSIZE=768 ,SCR_YSIZE=464  $
      ,TITLE='任务调度' ,SPACE=3 ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR=1)


  WID_BASE_2 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_2' ,FRAME=1  $
      ,XOFFSET=7 ,YOFFSET=385 ,SCR_XSIZE=748 ,SCR_YSIZE=40  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BUTTON_close = Widget_Button(WID_BASE_2, UNAME='WID_BUTTON_close'  $
      ,XOFFSET=572 ,YOFFSET=7 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='关闭')


  WID_BUTTON_help = Widget_Button(WID_BASE_2, UNAME='WID_BUTTON_help'  $
      ,XOFFSET=80 ,YOFFSET=7 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='帮助')


  WID_BASE_3 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_3' ,FRAME=1  $
      ,XOFFSET=7 ,YOFFSET=7 ,SCR_XSIZE=748 ,SCR_YSIZE=371  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_BASE_0 = Widget_Base(WID_BASE_3, UNAME='WID_BASE_0' ,XOFFSET=7  $
      ,YOFFSET=7 ,SCR_XSIZE=31 ,SCR_YSIZE=356 ,TITLE='IDL' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)

;-------------------------------------------------------------------------------
     text=''+string(byte(13))+string(byte(10))+   $
          '业'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '务'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '管'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '理'

  WID_label_yewu = Widget_label(WID_BASE_0, UNAME='WID_label_yewu' ,XOFFSET=1  $
      ,YOFFSET=0 ,frame=1,SCR_XSIZE=27 ,SCR_YSIZE=104, /ALIGN_CENTER  $
      ,value=text)

;-------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          '系'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '统'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '设'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '定'

  WID_label_xitong = Widget_label(WID_BASE_0, UNAME='WID_label_xitong' ,XOFFSET=1  $
      ,YOFFSET=125 ,frame=1,SCR_XSIZE=27 ,SCR_YSIZE=104,/ALIGN_CENTER  $
      ,value=text)

;---------------------------------------------------------------------------------
     text=''+string(byte(13))+string(byte(10))+   $
          '数'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '据'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '管'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '理'

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
      ,YOFFSET=48 ,/ALIGN_RIGHT ,VALUE='－')


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
      ,YOFFSET=175 ,/ALIGN_RIGHT ,VALUE='－')


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
      ,YOFFSET=302 ,/ALIGN_RIGHT ,VALUE='－')


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
      ,VALUE='image/任务查询.bmp',/bitmap)        ;任务查询


;  WID_BUTTON_GL_pro_manager = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_GL_pro_manager'  $
;      ,YOFFSET=60 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
;      ,VALUE='image/进度管理.bmp',/bitmap)             ;进度管理


  WID_BUTTON_21 = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_21'  $   ;监测区定义
      ,YOFFSET=199 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='image/监测区定义.bmp',/bitmap)


  WID_BUTTON_22 = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_22'  $    ;路径设定
      ,YOFFSET=167 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,value='image/路径设定.bmp',/bitmap)
;      ,VALUE='Image\LUJING01.bmp' ,/BITMAP)


  WID_BUTTON_23 = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_23'  $
      ,YOFFSET=135 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='image/数据库链接.bmp',/bitmap)       ;数据库链接


  WID_BUTTON_24 = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_24'  $
      ,YOFFSET=327 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='image/异常检测.bmp',/bitmap)                      ;异常检测


  WID_BUTTON_SJ_datamaintance = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_SJ_datamaintance'  $
      ,YOFFSET=295 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,value='Image\数据维护.bmp' ,/BITMAP)       ;数据维护


  WID_BUTTON_SJ_datainput = Widget_Button(WID_BASE_5, UNAME='WID_BUTTON_SJ_datainput'  $
      ,YOFFSET=263 ,SCR_XSIZE=93 ,SCR_YSIZE=25 ,/ALIGN_CENTER  $
      ,VALUE='Image\数据导入.bmp' ,/BITMAP)      ;数据导入

;---------------------------------------------------------------------------------
     text=''+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '农'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '情'+string(byte(13))+string(byte(10))+   $
          ''+string(byte(13))+string(byte(10))+   $
          '遥'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '感'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '监'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '测'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '业'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '务'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '模'+string(byte(13))+string(byte(10))+    $
          ''+string(byte(13))+string(byte(10))+   $
          '块'



  WID_label_mokuai = Widget_label(WID_BASE_3, UNAME='WID_label_mokuai'  $
      ,XOFFSET=184,frame=1 ,YOFFSET=7 ,SCR_XSIZE=29 ,SCR_YSIZE=353   $
      ,/ALIGN_CENTER,value=text)
;------------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          ' 长 势'+string(byte(13))+string(byte(10))+   $
          ' 监 测'

  WID_label_zhangshi = Widget_label(WID_BASE_3, UNAME='WID_label_zhangshi'  $
      ,XOFFSET=233 ,YOFFSET=31 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)

;------------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          ' 面 积'+string(byte(13))+string(byte(10))+   $
          ' 估 算'


  WID_labe_mianji = Widget_label(WID_BASE_3, UNAME='WID_labe_mianji'  $
      ,XOFFSET=233 ,YOFFSET=126 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)

;------------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          ' 单 产'+string(byte(13))+string(byte(10))+   $
          ' 预 测'


  WID_label_danchan = Widget_label(WID_BASE_3, UNAME='label_danchan'  $
      ,XOFFSET=233 ,YOFFSET=198 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)
;------------------------------------------------------------------------------------

     text=''+string(byte(13))+string(byte(10))+   $
          ' 农 气'+string(byte(13))+string(byte(10))+   $
          ' 分 析'

  WID_label_nongqi = Widget_label(WID_BASE_3, UNAME='WID_label_nongqi'  $
      ,XOFFSET=233 ,YOFFSET=257 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)
;------------------------------------------------------------------------------------
    text=''+string(byte(13))+string(byte(10))+   $
          ' 复 种'+string(byte(13))+string(byte(10))+   $
          ' 指 数'

  WID_label_fuzhong = Widget_label(WID_BASE_3, UNAME='WID_label_fuzhong'  $
      ,XOFFSET=233 ,YOFFSET=315 ,SCR_XSIZE=40 ,SCR_YSIZE=45,frame=1   $
      ,value=text)
;------------------------------------------------------------------------------------

;  WID_DRAW_20 = Widget_Draw(WID_BASE_3, UNAME='WID_DRAW_20'  $
;      ,XOFFSET=276 ,YOFFSET=11 ,SCR_XSIZE=25 ,SCR_YSIZE=80)


  WID_BUTTON_29 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_29'  $
      ,XOFFSET=366 ,YOFFSET=17 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\统计.bmp' ,/BITMAP)       ;统计

;------------------------------------------------------------------------------------
      text=' 过 程'+string(byte(13))+string(byte(10))+   $
          ' 监 测'

  WID_label_guocheng = Widget_label(WID_BASE_3, UNAME='WID_label_guocheng'  $
      ,XOFFSET=302 ,YOFFSET=17 ,SCR_XSIZE=40 ,SCR_YSIZE=25,frame=1   $
      ,value=text)


      text=  $
          ' 实 时'+string(byte(13))+string(byte(10))+   $
          ' 监 测'

  WID_label_shishi = Widget_label(WID_BASE_3, UNAME='WID_label_shishi'  $
      ,XOFFSET=302 ,YOFFSET=65 ,SCR_XSIZE=40 ,SCR_YSIZE=25,frame=1   $
      ,value=text)

;------------------------------------------------------------------------------------
  WID_BUTTON_30 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_30'  $    ;曲线重构
      ,XOFFSET=481 ,YOFFSET=17 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\曲线重构.bmp' ,/BITMAP )


  WID_BUTTON_31 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_31'  $
      ,XOFFSET=366 ,YOFFSET=65 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\差值分级.bmp' ,/BITMAP )            ;差值分级


  WID_BUTTON_32 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_32'  $
      ,XOFFSET=594 ,YOFFSET=17 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\汇总.bmp' ,/BITMAP)      ;汇总


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
      ,/ALIGN_CENTER ,VALUE='Image\汇总.bmp' ,/BITMAP)      ;汇总


  WID_BUTTON_34 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_34'  $
      ,XOFFSET=481 ,YOFFSET=65 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\空间统计.bmp' ,/BITMAP)     ;空间统计


  WID_LABEL_13 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_13'  $
      ,XOFFSET=574 ,YOFFSET=71 ,/ALIGN_LEFT ,VALUE='-->')


;----------结果分析-按键---------------------------------------------------------

;     text=''+string(byte(13))+string(byte(10))+   $
;          ''+string(byte(13))+string(byte(10))+   $
;          '结'+string(byte(13))+string(byte(10))+   $
;          '果'+string(byte(13))+string(byte(10))+   $
;          '分'+string(byte(13))+string(byte(10))+   $
;          '析'
;
;
;
;
;
;  WID_LABEL_RESULT = Widget_LABEL(WID_BASE_3, UNAME='WID_RESULT'  $   ;结果分析
;      ,XOFFSET=716 ,YOFFSET=12 ,SCR_XSIZE=21 ,SCR_YSIZE=84  $
;      ,/ALIGN_CENTER ,VALUE=text)

  WID_BUTTON_0 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_0'  $   ;结果分析
      ,XOFFSET=714 ,YOFFSET=10 ,SCR_XSIZE=25 ,SCR_YSIZE=88  $
      ,/ALIGN_CENTER ,VALUE='Image\结果分析.BMP',/BITMAP)



;-------------------------------------------------------------------------

  WID_BUTTON_35 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_35'  $
      ,XOFFSET=481 ,YOFFSET=160 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\比例外推.bmp' ,/BITMAP)       ;比例外推


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
      ,/ALIGN_CENTER ,VALUE='Image\数据入库.bmp' ,/BITMAP)            ;数据入库


  WID_BUTTON_37 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_37'  $
      ,XOFFSET=481 ,YOFFSET=112 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER,VALUE='Image\成数外推.bmp' ,/BITMAP)       ;成数外推
;==================================================================================

     text=' 种 植'+string(byte(13))+string(byte(10))+   $
          ' 结 构'

  WID_label_jiegou = Widget_label(WID_BASE_3, UNAME='WID_label_jiegou'  $
      ,XOFFSET=302 ,YOFFSET=158 ,SCR_XSIZE=40 ,SCR_YSIZE=25,frame=1   $
      ,value=text)

;------------------------------------------------------------------------------

     text=' 种 植'+string(byte(13))+string(byte(10))+   $
          ' 成 数'

  WID_label_chengshu = Widget_label(WID_BASE_3, UNAME='WID_label_chengshu'  $
      ,XOFFSET=302 ,YOFFSET=111 ,SCR_XSIZE=40 ,SCR_YSIZE=25,frame=1   $
      ,value=text)

;==================================================================================
  WID_BUTTON_38 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_38'  $
      ,XOFFSET=366 ,YOFFSET=112 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\空间统计.bmp' ,/BITMAP)     ;空间统计


;  WID_DRAW_22 = Widget_Draw(WID_BASE_3, UNAME='WID_DRAW_22'  $
;      ,XOFFSET=276 ,YOFFSET=106 ,SCR_XSIZE=25 ,SCR_YSIZE=80)
;
;
;  WID_DRAW_27 = Widget_Draw(WID_BASE_3, UNAME='WID_DRAW_27'  $
;      ,XOFFSET=576 ,YOFFSET=109 ,SCR_XSIZE=25 ,SCR_YSIZE=78)


  WID_BUTTON_39 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_39'  $
      ,XOFFSET=602 ,YOFFSET=135 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\面积估算.bmp' ,/BITMAP)           ;面积估算


  WID_LABEL_18 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_18'  $
      ,XOFFSET=504 ,YOFFSET=217 ,/ALIGN_LEFT ,VALUE='-->')


  WID_BUTTON_DC_floatyield = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_DC_floatyield'  $
      ,XOFFSET=411 ,YOFFSET=211 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\波动产量.bmp' ,/BITMAP)      ;波动产量


  WID_BUTTON_DC_yieldmerge = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_DC_yieldmerge'  $
      ,XOFFSET=524 ,YOFFSET=211 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\产量融合.bmp' ,/BITMAP)     ;产量融合


  WID_LABEL_19 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_19'  $
      ,XOFFSET=390 ,YOFFSET=217 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_20 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_20'  $
      ,XOFFSET=276 ,YOFFSET=217 ,/ALIGN_LEFT ,VALUE='-->')


  WID_BUTTON_DC_trendyield = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_DC_trendyield'  $
      ,XOFFSET=296 ,YOFFSET=211 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\趋势产量.bmp' ,/BITMAP)       ;趋势产量


  WID_BUTTON_43 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_43'  $
      ,XOFFSET=638 ,YOFFSET=211 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\产量估算.bmp' ,/BITMAP)        ;产量估算


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
      ,/ALIGN_CENTER ,VALUE='Image\气象插值.bmp' ,/BITMAP )             ;气象插值


  WID_LABEL_26 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_26'  $
      ,XOFFSET=276 ,YOFFSET=277 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_27 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_27'  $
      ,XOFFSET=390 ,YOFFSET=277 ,/ALIGN_LEFT ,VALUE='-->')


  WID_BUTTON_NQ_classify = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_NQ_classify'  $
      ,XOFFSET=411 ,YOFFSET=269 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\对比分析.bmp' ,/BITMAP)    ;对比分析


  WID_BUTTON_FZ_calculate = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_FZ_calculate'  $
      ,XOFFSET=296 ,YOFFSET=327 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\指数提取.bmp' ,/BITMAP)     ;指数提取


  WID_LABEL_28 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_28'  $
      ,XOFFSET=276 ,YOFFSET=335 ,/ALIGN_LEFT ,VALUE='-->')


  WID_LABEL_29 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_29'  $
      ,XOFFSET=390 ,YOFFSET=335 ,/ALIGN_LEFT ,VALUE='-->')
;
;
;  WID_BUTTON_47 = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_47'  $
;      ,XOFFSET=524 ,YOFFSET=330 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
;      ,/ALIGN_CENTER ,VALUE='统计')


  WID_BUTTON_FZ_statistic = Widget_Button(WID_BASE_3, UNAME='WID_BUTTON_FZ_statistic'  $
      ,XOFFSET=411 ,YOFFSET=327 ,SCR_XSIZE=93 ,SCR_YSIZE=25  $
      ,/ALIGN_CENTER ,VALUE='Image\空间统计.bmp' ,/BITMAP)      ;空间统计


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
      ,XOFFSET=275 ,YOFFSET=46 ,/ALIGN_RIGHT ,VALUE='－')

;==================面积估算后的箭头=============================================================
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

;===========面积估算后的箭头====================================================================


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
      ,XOFFSET=276 ,YOFFSET=141 ,/ALIGN_RIGHT ,VALUE='－')


;=========汇总后的箭头======================================================================


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
      ,XOFFSET=688 ,YOFFSET=24 ,/ALIGN_RIGHT ,VALUE='－')


  WID_LABEL_99 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_99'  $
      ,XOFFSET=688 ,YOFFSET=74 ,/ALIGN_RIGHT ,VALUE='－')

 ;==============成数外推后的箭头=====================================================


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
      ,XOFFSET=576 ,YOFFSET=169 ,/ALIGN_RIGHT ,VALUE='－')


  WID_LABEL_115 = Widget_Label(WID_BASE_3, UNAME='WID_LABEL_115'  $
      ,XOFFSET=576 ,YOFFSET=119 ,/ALIGN_RIGHT ,VALUE='－')

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
;  WIDGET_CONTROL, WID_DRAW_3, GET_VALUE=DRAWID                 ;注意要得到这个值,必须在"组件架构实现之后"/REALIZE后才可以得到.
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
;      TVSCL, READ_BMP('Image\单产预测.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_16, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\农气分析.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_18, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\复种指数.bmp',/RGB),/TRUE
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
;      TVSCL, READ_BMP('Image\过程监测.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_24, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\实时监测.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_26, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\种植成数.bmp',/RGB),/TRUE
;
;  WIDGET_CONTROL, WID_DRAW_25, GET_VALUE=DRAWID
;      WSET, drawid
;      TVSCL, READ_BMP('Image\种植结构.bmp',/RGB),/TRUE
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
