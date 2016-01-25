PRO widget2 ;主程序
base = WIDGET_BASE(xoffset=300,yoffset=300,/COLUMN)

button1 = WIDGET_BUTTON(base, VALUE='One', UVALUE='ONE')
button2 = WIDGET_BUTTON(base, VALUE='Two', UVALUE='TWO')
text = WIDGET_TEXT(base, XSIZE=20)
button3 = WIDGET_BUTTON(base, value='Done', UVALUE='DONE')

WIDGET_CONTROL, base, SET_UVALUE=text
WIDGET_CONTROL, base, /REALIZE

XMANAGER, 'Widget2', base

END

PRO widget2_event, ev ;事件处理程序

     WIDGET_CONTROL, ev.top, GET_UVALUE=textwid
     WIDGET_CONTROL, ev.id, GET_UVALUE=uval
     CASE uval OF
       'ONE' : WIDGET_CONTROL, textwid, SET_VALUE='Button One Pressed'
       'TWO' : WIDGET_CONTROL, textwid, SET_VALUE='Button Two Pressed'
       'DONE': WIDGET_CONTROL, ev.top, /DESTROY
     ENDCASE
END
