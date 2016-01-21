PRO menu
print,'menu'
MyBase =WIDGET_BASE(XSIZE=300,YSIZE=200)
    MyButton = WIDGET_BUTTON(MyBase, VALUE='Welcome',    $
               EVENT_PRO='SubProcedure', XSIZE=50, YSIZE=30, $
               XOFFSET=100, YOFFSET=80 )
    WIDGET_CONTROL, MyBase, /REALIZE
    XMANAGER, 'menu', MyBase
END

PRO SubProcedure, event
    Result = DIALOG_MESSAGE('Happy You!',/INFORMATION)
END