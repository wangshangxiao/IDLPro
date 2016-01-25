PRO XImageBar, image

   ; The widget definition module for the XImageBar program.

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
quitID = Widget_Button(fileID, Value='Quit')

   ; The graphics window. Note the way it is made the
   ; current graphics window.

drawID = Widget_Draw(tlb, XSize=400, YSize=400)
Widget_Control, tlb, /Realize
Widget_Control, drawID, Get_Value=wid
WSet, wid

   ; The ImageBar program is used to display the graphics.

ImageBar, image

   ; Create the info structure with all the information the
   ; program requires for its operation.

info = { image:image, $  ; The image data.
         wid:wid, $      ; The window index number.
         drawID:drawID } ; The draw widget identifier.

   ; Store the info structure in a memory location outside
   ; any program module. Use the USER VALUE of the TLB.

Widget_Control, tlb, Set_UValue=info

   ; Start the Event Loop. This will be a non-blocking program.

XManager, 'ximagebar', tlb, /No_Block
END
