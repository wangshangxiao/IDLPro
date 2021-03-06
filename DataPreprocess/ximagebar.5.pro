PRO XImageBar_Colors, event

   ; The event handler for the IMAGE COLORS button.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; What kind of event is this?

thisEvent = Tag_Names(event, /Structure_Name)

   ; Branch on appropriate event.

CASE thisEvent OF

'WIDGET_BUTTON': BEGIN

      ; Call the XCOLORS program.

   XColors, NColors=info.ncolors, Bottom=info.bottom, $
      Title='XImageBar Colors (' + StrTrim(info.wid,2) + ')', $
      Group_Leader=event.top, NotifyID=[event.id, event.top]
  ENDCASE

'XCOLORS_LOAD': BEGIN

      ; If 24-bit display, re-issue graphics commands.

  Device, Get_Visual_Depth=thisDepth
  IF thisDepth GT 8 THEN BEGIN
    WSet, info.wid
    ImageBar, info.image, NColors=info.ncolors, $
       Bottom=info.bottom, /EraseFirst, _Extra=info.extra
  ENDIF
  ENDCASE

ENDCASE

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------



PRO XImageBar_Quit, event

   ; Respond to QUIT button events.
   ; Destroy the widget hierarchy.

Widget_Control, event.top, /Destroy
END ;------------------------------------------------------------



PRO XImageBar_Resize, event, Group_Leader=group

   ; Respond to TLB resize events. First, get the info structure.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Resize the draw widget.

Widget_Control, info.drawID, Draw_XSize=event.x, $
   Draw_YSize=event.y

   ; Make the draw window the active window. Display the graphics.

WSet, info.wid
ImageBar, info.image, /EraseFirst, _Extra=info.extra, $
   NColors=info.ncolors, Bottom=info.bottom

   ; Replace info structure in USER VALUE of TLB.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------



PRO XImageBar, image, _Extra=extra, NColors=ncolors, $
   Bottom=bottom

   ; The widget definition module for the XImageBar program.

   ; Check for presence of keywords. Define default values.

IF N_Elements(ncolors) EQ 0 THEN ncolors = (!D.N_Colors < 256)
IF N_Elements(bottom) EQ 0 THEN bottom = 0

   ; If no image parameter is passed in, allow the user to
   ; load an image file. The image must be 2D.

IF N_Params() EQ 0 THEN BEGIN
   image = GetImage(Cancel=canceled, 'm51.dat', $
      XSize=340, YSize=440)
   IF canceled THEN RETURN
ENDIF
s = Size(image)
IF s[0] NE 2 THEN BEGIN
   Message, 'Image argument must be 2D.', /Continue
   RETURN
ENDIF

   ; Define the widgets that make up the program.

tlb = Widget_Base(Title='XImageBar Program', Column=1, $
   TLB_Size_Events=1, MBar=menubaseID)

   ; The FILE menu button.

fileID = Widget_Button(menubaseID, Value='File', Menu=1)
quitID = Widget_Button(fileID, Value='Quit', $
   Event_Pro='XImageBar_Quit')

   ; The COLORS menu button.

colors = Widget_Button(menubaseID, Value='Colors')
icolors = Widget_Button(colors, Value='Image Colors', $
   Event_Pro='XImageBar_Colors')

   ; The graphics window. Note the way it is made the
   ; current graphics window.

drawID = Widget_Draw(tlb, XSize=400, YSize=400)
Widget_Control, tlb, /Realize
Widget_Control, drawID, Get_Value=wid
WSet, wid

   ; The ImageBar program is used to display the graphics.

ImageBar, image, _Extra=extra, NColors=ncolors, Bottom=bottom

   ; If the extra variable is undefined, create fake data for it.

IF N_Elements(extra) EQ 0 THEN extra = {Junk:1}

   ; Create the info structure with all the information the
   ; program requires for its operation.

info = { image:image, $     ; The image data.
         extra:extra, $     ; Extra keywords for ImageBar.
         ncolors:ncolors, $ ; The number of colors.
         bottom:bottom, $   ; The starting color index.
         wid:wid, $         ; The window index number.
         drawID:drawID }    ; The draw widget identifier.

   ; Store the info structure in a memory location outside
   ; any program module. Use the USER VALUE of the TLB.

Widget_Control, tlb, Set_UValue=info, /No_Copy

   ; Start the Event Loop. This will be a non-blocking program.

XManager, 'ximagebar', tlb, /No_Block, Group_Leader=group, $
   Event_Handler='XImageBar_Resize'
END
