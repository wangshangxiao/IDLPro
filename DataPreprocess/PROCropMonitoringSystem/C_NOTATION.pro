;
; IDL Event Callback Procedures
; NOTATION_eventcb
;
;
;-----------------------------------------------------------------
;蒙继华,
;20060808
;用于显示一个用于提示作用的对话框

;
PRO NOTATION_EVENTCB
END
;
; IDL WIDGET INTERFACE PROCEDURES. THIS CODE IS AUTOMATICALLY
;     GENERATED AND SHOULD NOT BE MODIFIED.

;
; GENERATED ON:	11/30/2004 22:33.20
;
PRO BASE_NOTATION_EVENT, EVENT

  WTARGET = (WIDGET_INFO(EVENT.ID,/NAME) EQ 'TREE' ?  $
      WIDGET_INFO(EVENT.ID, /TREE_ROOT) : EVENT.ID)


  WWIDGET =  EVENT.TOP

  CASE WTARGET OF

    WIDGET_INFO(WWIDGET, FIND_BY_UNAME='CMD_NOTATION_OK'): BEGIN
    	CLOSE,/ALL
     	WIDGET_CONTROL, EVENT.TOP, /DESTROY
     	RETURN
    END
    ELSE:
  ENDCASE

END

PRO BASE_NOTATION, GROUP_LEADER=WGROUP,TEXT;, _EXTRA=_VWBEXTRA_

; FLOATING OR MODAL BASES MUST HAVE A GROUP LEADER.

  IF(N_ELEMENTS(WGROUP) EQ 0)THEN $
     MESSAGE,'GROUP LEADER MUST BE SPECIFIED FOR MODAL OR FLOATING'+ $
      ' TOP LEVEL BASES'

  RESOLVE_ROUTINE, 'NOTATION_EVENTCB',/COMPILE_FULL_FILE  ; LOAD EVENT CALLBACK ROUTINES

  BASE_NOTATION = WIDGET_BASE( GROUP_LEADER=WGROUP,  $
      UNAME='BASE_NOTATION' ,XOFFSET=300 ,YOFFSET=250 ,SCR_XSIZE=279  $
      ,SCR_YSIZE=274 ,TITLE='说明' ,SPACE=3 ,XPAD=3 ,YPAD=3 ,/MODAL  $
      ,TLB_FRAME_ATTR=1)


  LBL_NOTATION = WIDGET_LABEL(BASE_NOTATION, UNAME='LBL_NOTATION'  $
      ,XOFFSET=6 ,YOFFSET=5 ,SCR_XSIZE=89 ,SCR_YSIZE=16 ,/ALIGN_LEFT  $
      ,VALUE='说明如下:')


  TXT_NOTATION = WIDGET_TEXT(BASE_NOTATION, UNAME='TXT_NOTATION'  $
      ,XOFFSET=6 ,YOFFSET=22 ,SCR_XSIZE=260 ,SCR_YSIZE=184 ,/SCROLL  $
      ,XSIZE=20 ,YSIZE=1)
  WIDGET_CONTROL,TXT_NOTATION,SET_VALUE=TEXT

  CMD_NOTATION_OK = WIDGET_BUTTON(BASE_NOTATION,  $
      UNAME='CMD_NOTATION_OK' ,XOFFSET=101 ,YOFFSET=213 ,SCR_XSIZE=74  $
      ,SCR_YSIZE=24 ,/ALIGN_CENTER,VALUE='确定')

  WIDGET_CONTROL, /REALIZE, BASE_NOTATION
  WIDGET_CONTROL,CMD_NOTATION_OK,/INPUT_FOCUS

  XMANAGER, 'BASE_NOTATION', BASE_NOTATION, /NO_BLOCK

END
;
; EMPTY STUB PROCEDURE USED FOR AUTOLOADING.
;
PRO C_NOTATION, GROUP_LEADER=WGROUP,TEXT; _EXTRA=_VWBEXTRA_
  BASE_NOTATION, GROUP_LEADER=WGROUP,TEXT; _EXTRA=_VWBEXTRA_
END
