PRO SELECTFILE_EVENT,ev
  WIDGET_CONTROL,ev.TOP,get_UValue = pState
  CASE WIDGET_INFO(ev.ID,/uname) OF
    'select': BEGIN
      ;
      file = DIALOG_PICKFILE(filter ='*.jpg')
      IF QUERY_JPEG(file) EQ 1 THEN BEGIN
        ;
        (*pState).RESULT = file
        WIDGET_CONTROL,(*pState).WTEXT, set_Value = file
        
      ENDIF
    END
    'ok' : WIDGET_CONTROL,ev.TOP,/destroy
    ELSE:
  ENDCASE
END
FUNCTION SELECTFILE
  ;
  tlb = WIDGET_BASE(title ='select files',/row)
  wText = WIDGET_TEXT(tlb,xsize =20)
  wButton = WIDGET_BUTTON(tlb,value = 'select JPG image',uname ='select')
  wButton = WIDGET_BUTTON(tlb,value = 'ok',uname ='ok')
  ;
  WIDGET_CONTROL,tlb,/realize
  pState = PTR_NEW({wText:WTEXT,result:''})
  WIDGET_CONTROL,tlb,set_UValue = pState
  XMANAGER,'selectfile',tlb;,/no_block
  r = (*pState).RESULT
  PTR_FREE,pState
  RETURN,r
  
END

PRO WTLB_EVENT,ev
  WIDGET_CONTROL,ev.TOP,get_uvalue= pstate
  
  CASE WIDGET_INFO(ev.ID,/uname) OF
    'open': BEGIN
      selectedFile = SELECTFILE()
      IF QUERY_JPEG(selectedFile) EQ 1 THEN BEGIN
        READ_JPEG,selectedFile,data,/true
        TV,data,/true
      ENDIF
      
    END
    ELSE:
  ENDCASE
END

PRO DISPLAYIMAGE
  ;        wbase -8
  ;   /       \       \
  ;  menu   toolbar  draw
  ;  open-11  open-13   -7
  ;  ENVI,/restore_base_save_files
  ;  ENVI_BATCH_INIT
  ;
  wtlb = WIDGET_BASE(title='jpeg read and display',$
    mbar = wMenuBase,$
    /column)
  wFile = WIDGET_BUTTON(wMenuBase,value='file',/menu)
  wOpen = WIDGET_BUTTON(wFile,value = 'open',uname='open',$
    EVENT_PRO = 'wMenuOpen')
  ;
  wtool = WIDGET_BASE(wtlb,/frame)
  wtoolOpen = WIDGET_BUTTON(wtool,value= 'open',uname='open')
  ;
  wDraw = WIDGET_DRAW(wtlb,xsize = 600,ysize =400)
  ;
  WIDGET_CONTROL,wtlb,/real,set_uvalue={wDraw:WDRAW}
  
  XMANAGER, 'wtlb', wtlb,/no_block
  
  
END
