PRO XImageBar_PostScript, event

   ; Event handler to save a PostScript file.

   ; Select a file name.

thisFile = Dialog_Pickfile(/Write, File='idl.ps')
IF thisFile EQ '' THEN RETURN

   ; If the user didn't CANCEL, write the PostScript file.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Find the aspect ratio of the display window.

WSet, info.wid
keywords = PSWindow()

   ; Select and configure the PostScript device.

thisDevice = !D.Name
Set_Plot, 'PS', /Copy
Device, File=thisFile, _Extra=keywords, /Inches, $
   Bits_Per_Pixel=8

   ; Create a pseudo event of the graphic in the display window.

pseudoEvent = {WIDGET_BUTTON, ID:info.action, $
   Top:event.top, Handler:0L, Select:1}

   ; But the info structure back in storage BEFORE calling
   ; the Processing event handler.

Widget_Control, event.top, Set_UValue=info, /No_Copy

   ; Call the Processing event handler with the pseudo event.

XImageBar_Processing, pseudoEvent

   ; Close the file before restoring the display device.

Device, /Close_File
Set_Plot, thisDevice

END ;------------------------------------------------------------



PRO XImageBar_JPEG, event

   ; This event handler writes a JPEG image.

   ; Select a file name.

thisFile = Dialog_Pickfile(/Write, File='idl.jpg')
IF thisFile EQ '' THEN RETURN

Widget_Control, event.top, Get_UValue=info, /No_Copy
WSet, info.wid

   ; 8-bit or 24-bit device?

Device, Get_Visual_Depth=thisDepth
IF thisDepth EQ 8 THEN BEGIN
   snapshot = TVRD()
   TVLCT, r, g, b, /Get
   s = Size(snapshot)
   image24 = BytArr(3, s[1], s[2])
   image24[0,*,*] = r[snapshot]
   image24[1,*,*] = g[snapshot]
   image24[2,*,*] = b[snapshot]
ENDIF ELSE image24 = TVRD(True=1)

   ; Write the JPEG file.

Write_JPEG, thisFile, image24, True=1, Quality=75
Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------



PRO XImageBar_GIF, event

    ; Event handler to save file as a GIF file.

    ; Select a file name.

thisFile = Dialog_Pickfile(/Write, File='idl.gif')
IF thisFile EQ '' THEN RETURN

   ; If the user didn't CANCEL, write the GIF file.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Take snapshop of the display window. 8-bit or 24-bit?

WSet, info.wid
Device, Get_Visual_Depth=thisDepth
IF thisDepth EQ 8 THEN BEGIN

   snapshot = TVRD()

      ; Get the color table vectors for the GIF file.

   TVLCT, r, g, b, /Get

ENDIF ELSE BEGIN

   image24 = TVRD(True=1)
   snapshot = Color_Quan(image24, 1, r, g, b)

ENDELSE

   ; Write the file.

Write_GIF, thisFile, snapshot, r, g, b
Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------



PRO XImageBar_Protect_Colors, event

   ; Event handler to protect program colors.
   ; Respond if this is a BUTTON DOWN event.

IF event.type NE 0 THEN RETURN
Widget_Control, event.top, Get_UValue=info, /No_Copy
TVLCT, info.r, info.g, info.b, info.bottom
Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------



PRO XImageBar_Processing, event

   ; The event handler for the PROCESSING menu buttons.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Which button caused the event?

Widget_Control, event.id, Get_Value=buttonValue

   ; Do the appropriate processing of the image.

CASE buttonValue OF
   'Smooth': thisImage = Smooth(*info.image, 7)
   'Edge Enhance': thisImage = Sobel(*info.image)
   'Histogram Equal': thisImage = Hist_Equal(*info.image, $
       Top=info.ncolors) + info.bottom
   'Original': thisImage = *info.image
ENDCASE

   ; Display the processed image.

IF (!D.Flags AND 256) NE 0 THEN WSet, info.wid
ImageBar, thisImage, NColors=info.ncolors, $
   Bottom=info.bottom, /EraseFirst, _Extra=info.extra

   ; Update the "last action" field in the info structure.

info.action = event.id

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------



PRO XImageBar_Cleanup, tlb

   ; Cleanup routine for this program. Prevent
   ; memory leakage by deleting pointer.

Widget_Control, tlb, Get_UValue=info, /No_Copy
IF N_Elements(info) EQ 0 THEN Heap_GC ELSE $
   Ptr_Free, info.image
END ;------------------------------------------------------------



PRO XImageBar_Open, event

   ; Event handler for FILE OPEN button.

   ; Let use select file with GetImage.

image = GetData(Cancel=canceled, Parent=event.top)
IF canceled THEN RETURN
s = Size(image)
IF s[0] NE 2 THEN BEGIN
   Message, 'Image argument must be 2D.', /Continue
   RETURN
ENDIF

   ; If the file is read successfully, get info structure.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Store new image in pointer location.

*info.image = image

   ; Create a pseudo event to perform "last action".

pseudoEvent = {WIDGET_BUTTON, ID:info.action, $
   Top:event.top, Handler:0L, Select:1}
Widget_Control, info.action, Send_Event=pseudoEvent

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------



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

      ; Update the colors for this program.

   info.r = event.r(info.bottom:info.ncolors-1+info.bottom)
   info.g = event.g(info.bottom:info.ncolors-1+info.bottom)
   info.b = event.b(info.bottom:info.ncolors-1+info.bottom)

      ; If 24-bit display, re-issue graphics commands.

  Device, Get_Visual_Depth=thisDepth
  IF thisDepth GT 8 THEN BEGIN

       ; Create a pseudo event to perform "last action".

   pseudoEvent = {WIDGET_BUTTON, ID:info.action, $
      Top:event.top, Handler:0L, Select:1}
   Widget_Control, info.action, Send_Event=pseudoEvent

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

   ; Create a pseudo event to perform "last action".

pseudoEvent = {WIDGET_BUTTON, ID:info.action, $
   Top:event.top, Handler:0L, Select:1}
Widget_Control, info.action, Send_Event=pseudoEvent

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
   image = GetData(Cancel=canceled, 'm51.dat', $
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

   ; The OPEN menu button.

openID = Widget_Button(fileID, Value='Open...', $
   Event_Pro='XImageBar_Open')

   ; The SAVE AS menu buttons.

saveID = Widget_Button(fileID, Value='Save As', /Menu)
psID = Widget_Button(saveID, Value='PostScript File', $
   Event_Pro='XImageBar_PostScript')
gifID = Widget_Button(saveID, Value='GIF File', $
   Event_Pro='XImageBar_GIF')
jpegID = Widget_Button(saveID, Value='JPEG File', $
   Event_Pro='XImageBar_JPEG')

   ; The QUIT button.

quitID = Widget_Button(fileID, Value='Quit', $
   Event_Pro='XImageBar_Quit', /Separator)

   ; The COLORS menu button.

colors = Widget_Button(menubaseID, Value='Colors')
icolors = Widget_Button(colors, Value='Image Colors', $
   Event_Pro='XImageBar_Colors')

   ; Create PROCESSING menu button.

processID = Widget_Button(menubaseID, Value='Processing', $
   Event_Pro='XImageBar_Processing', /Menu)
smoothID = Widget_Button(processID, Value='Smooth')
edgeID = Widget_Button(processID, Value='Edge Enhance')
equalID = Widget_Button(processID, Value='Histogram Equal')
originalID = Widget_Button(processID, Value='Original')

   ; The graphics window. Note the way it is made the
   ; current graphics window. Turn button events on
   ; for color protection.

drawID = Widget_Draw(tlb, XSize=400, YSize=400, $
   /Button_Event, Event_Pro='XImageBar_Protect_Colors')
Widget_Control, tlb, /Realize
Widget_Control, drawID, Get_Value=wid
WSet, wid

   ; The ImageBar program is used to display the graphics.

ImageBar, image, _Extra=extra, NColors=ncolors, Bottom=bottom

   ; If the extra variable is undefined, create fake data for it.

IF N_Elements(extra) EQ 0 THEN extra = {Junk:1}

   ; Get the color table vectors and the colors for this program.

TVLCT, r, g, b, /Get
r = r[bottom:ncolors-1+bottom]
g = g[bottom:ncolors-1+bottom]
b = b[bottom:ncolors-1+bottom]

   ; Create the info structure with all the information the
   ; program requires for its operation.

info = { image:Ptr_New(image), $     ; The image data.
         extra:extra, $              ; Extra keywords for ImageBar.
         ncolors:ncolors, $          ; The number of colors.
         bottom:bottom, $            ; The starting color index.
         r:r, $                      ; The red color values.
         g:g, $                      ; The green color values.
         b:b, $                      ; The blue color values.
         action:originalID, $        ; The last program action.
         wid:wid, $                  ; The window index number.
         drawID:drawID }             ; The draw widget identifier.

   ; Store the info structure in a memory location outside
   ; any program module. Use the USER VALUE of the TLB.

Widget_Control, tlb, Set_UValue=info, /No_Copy

   ; Start the Event Loop. This will be a non-blocking program.

XManager, 'ximagebar', tlb, /No_Block, Group_Leader=group, $
   Event_Handler='XImageBar_Resize', Cleanup='XImageBar_Cleanup'
END
