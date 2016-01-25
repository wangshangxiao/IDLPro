Pro Display_Event, event
;
; Very simple event handler. Get array, execute the "command".
;
Widget_Control, event.top, Get_UValue=array
Widget_Control, event.id, Get_UValue=command
result = Execute(command)
END ;---------------------------------------------------------


Pro Display, array

; Fake data, if needed.

IF N_Params() EQ 0 THEN array = Dist(300,300)

tlb = Widget_Base(Column=1)

; Each button contains "command" to execute.

button1 = Widget_Button(tlb, Value='Contour', $
   UValue='CONTOUR, array')
button2 = Widget_Button(tlb, Value='Surface', $
   UValue='SURFACE, Congrid(array, 30,30)')
button3 = Widget_Button(tlb, Value='Image', $
   UValue='Erase & TVSCL, Congrid(array,300,300)')
quit = Widget_Button(tlb, Value='Quit',  $
   UValue='Widget_Control, event.top, /Destroy')
Widget_Control, tlb, /Realize

dbase = Widget_Base(Column=1, Group_Leader=tlb, $
     XOffset=100, YOffset=100)
draw = Widget_Draw(dbase, XSize=300, YSize=300)
Widget_Control, dbase, /Realize
Widget_Control, draw, Get_Value=window_index
WSet, window_index
TVSCL, Congrid(array,300,300)

; Store array in User Value of TLB.

Widget_Control, tlb, Set_UValue=array
XManager, 'Display', tlb
END ;---------------------------------------------------------

