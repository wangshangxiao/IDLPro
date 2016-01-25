


pro MJ_Interface_event,event

   wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
              widget_info(Event.id, /tree_root) : event.id)

   wWidget =  Event.top
   ;widget_control,wWidget,GET_UVALUE=GLleader

   CASE wTarget OF

;       widget_Info(wWidget, FIND_BY_UNAME='butt_Unsupervise_Classify'):begin   ;非监督分类
;;            MJ_Unsupervise_Classify
;           ;temp=dialog_message('开发中！',title='信息')
;           MJ_class, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
;          end

;       widget_Info(wWidget, FIND_BY_UNAME='butt_ClassifyID'):begin      ;分类标识
;;            MJ_ClassifyID
;           ;temp=dialog_message('开发中！',title='信息')
;           MJ_recode, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
;          end

       widget_Info(wWidget, FIND_BY_UNAME='butt_STA'):begin           ;统计
;            MJ_STA
          ; temp=dialog_message('开发中！',title='信息')
           ;MJ_stat, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
           MJ_statistical, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
          end


	   Widget_Info(wWidget, FIND_BY_UNAME='butt_Extrapolate'): BEGIN              ;外推
		  	     buttbase = WIDGET_INFO(Event.top,FIND_BY_UNAME = 'buttbase')
			     contextBase = WIDGET_INFO(buttbase,FIND_BY_UNAME = 'butt_Extrapolate_contextBase')
			     WIDGET_DISPLAYCONTEXTMENU,buttbase, 124,29, contextBase
		  END

	    	widget_Info(wWidget, FIND_BY_UNAME='butt_Extrapolate_GVG'):begin           ;GVG外推
			;            MJ_STA
			          ; temp=dialog_message('开发中！',title='信息')
			           MJ_Extrapolate_GVG, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
			          end

			widget_Info(wWidget, FIND_BY_UNAME='butt_Extrapolate_proportion'):begin           ;种植成数外推
			;            MJ_STA
			          ; temp=dialog_message('开发中！',title='信息')
			           MJ_Extrapolate_Proportion, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
			          end





       widget_Info(wWidget, FIND_BY_UNAME='butt_Area_Calculate'):begin      ;面积计算
;            MJ_Area_Calculate
          ; temp=dialog_message('开发中！',title='信息')
           MJ_tocalculate, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
          end

	    Widget_Info(wWidget, FIND_BY_UNAME='butt_Close'):begin      ;关闭
			widget_control,event.top,/destroy
	      end

	    widget_info(wWidget, FIND_BY_UNAME='butt_Help'):begin       ;帮助

			if file_test('HELP\HELP.chm') then begin
				ONLINE_HELP, '面积估算模块', BOOK='HELP\HELP.chm', /FULL_PATH
			endif else begin
				info_help=dialog_message('找不到帮助文档',title='警告')
			endelse
		end
;			ONLINE_HELP,'面积估算模块', BOOK='HELP\HELP.chm', /FULL_PATH
;
;		  end

      ELSE:

   ENDCASE


end




;*******************************************************************
pro MJ_Interface,GROUP_LEADER=wGroup

   IF ( XREGISTERED('base') NE 0 ) THEN RETURN

   X_TEMP=460+28
   Y_TEMP=0+114

   ;面积估算模块界面
   COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP,MENU_MANAGE
   base=widget_base(GROUP_LEADER=BASE_TOP, title='面积估算',$
                    XOFFSET=X_TEMP+X_OFFSET-140,YOFFSET=Y_TEMP+Y_OFFSET,SCR_XSIZE=141,$
                    SPACE=3 ,XPAD=3 ,YPAD=3 ,TLB_FRAME_ATTR =1)


   buttbase=widget_base(base,column=1,frame=1,xoffset=3,/align_center,UNAME='buttbase')
   button_xsize=120
   button_ysize=24

;   ;非监督分类
;   butt_Unsupervise_Classify=widget_button(buttbase,value='非监督分类'   $
;            ,uname='butt_Unsupervise_Classify',frame=0$
;            ,SCR_XSIZE=button_xsize,SCR_YSIZE=button_ysize,/align_center)
;
;   ;分类标识
;   butt_ClassifyID=widget_button(buttbase,value='分类标识'  $
;            ,uname='butt_ClassifyID',frame=0   $
;            ,SCR_XSIZE=button_xsize,SCR_YSIZE=button_ysize,/align_center)

   ;统计
   butt_STA=widget_button(buttbase,value='种植成数计算'    $
            ,uname='butt_STA',frame=0    $
            ,SCR_XSIZE=button_xsize,SCR_YSIZE=button_ysize,/align_center)



;;外推
  butt_Extrapolate=Widget_Button(buttbase, UNAME='butt_Extrapolate' ,FRAME=0,SCR_XSIZE=button_xsize ,SCR_YSIZE=button_ysize  $
      ,/ALIGN_CENTER ,VALUE='成数外推')
  butt_Extrapolate_contextBase = WIDGET_BASE(buttbase,/CONTEXT_MENU, UNAME  = 'butt_Extrapolate_contextBase')
  butt_Extrapolate_GVG=Widget_Button(butt_Extrapolate_contextBase,UNAME='butt_Extrapolate_GVG',VALUE='分类成数外推')
  butt_Extrapolate_proportion=Widget_Button(butt_Extrapolate_contextBase,UNAME='butt_Extrapolate_proportion',VALUE='种植成数外推')


   ;面积计算
   butt_Area_Calculate=widget_button(buttbase,value='面积计算'    $
            ,uname='butt_Area_Calculate',frame=0    $
            ,SCR_XSIZE=button_xsize,SCR_YSIZE=button_ysize,/align_center)



   ;"关闭"与"帮助"按键的base
   elsebase=Widget_BASE(buttbase,FRAME=1,SCR_XSIZE=button_xsize,$
            SCR_YSIZE=button_ysize+6,/ALIGN_CENTER,/ROW)
   temp=55

   ;关闭
   butt_Close=Widget_Button(elsebase,VALUE='关闭',uname='butt_Close',$
                           SCR_YSIZE=button_ysize,SCR_xSIZE=temp)

   ;帮助
   butt_Help=Widget_Button(elsebase,VALUE='帮助',uname='butt_Help',$
                          SCR_YSIZE=button_ysize,SCR_xSIZE=temp)


  Widget_Control, /REALIZE, base
  WIDGET_CONTROL,butt_Close,/INPUT_FOCUS


  XManager, 'base',Event_handle='MJ_Interface_event', base, /NO_BLOCK


end