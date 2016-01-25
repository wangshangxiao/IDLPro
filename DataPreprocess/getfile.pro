Pro GetFile_Events, event

   ; What kind of event is this? We only want to handle button events
   ; from our APPLY or DISMISS buttons. Other events fall through.

eventName = Tag_Names(event, /Structure_Name)

IF eventName NE 'WIDGET_BUTTON' THEN RETURN

      ; Get the info structure out of the top-level base

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Which button was selected?

Widget_Control, event.id, Get_Value=buttonValue
CASE buttonValue OF

   'Dismiss' : Widget_Control, event.top, /Destroy

   'Apply' : BEGIN

         ; OK, get the information the user put into the form.
         ; Should do error checking, but...maybe later!

      Widget_Control, info.fileID, Get_Value=filename
      Widget_Control, info.xsizeID, Get_Value=xsize
      Widget_Control, info.ysizeID, Get_Value=ysize

        ; Notify the widgets about this event.

      s = SIZE(info.notifyID)
      IF s[0] EQ 1 THEN count = 0 ELSE count = s[2] - 1
      FOR j=0,count DO BEGIN

            ; Create a pseudo event.

          pseudoEvent = { GETFILE_EVENT, $
                          ID:info.notifyID[0,j], $
                          Top:info.notifyID[1,j], $
                          Handler:0L, $
                          Filename:filename[0], $
                          XSize:xsize, $
                          YSize:ysize}

          IF Widget_Info(info.notifyID(0,j), /Valid_ID) THEN $
             Widget_Control, info.notifyID(0,j), Send_Event=pseudoEvent
       ENDFOR

           ; Check the info structure back in.

       Widget_Control, event.top, Set_UValue=info, /No_Copy
       END

ENDCASE

END ;*******************************************************************



PRO GetFile, notifyID, $          ; Array of widgets to notify.
             Filename=filename, $ ; Name of file to open.
             Parent=parent, $     ; Group leader of this program.
             XSize=xsize, $       ; X Size of file to open.
             YSize=ysize          ; Y Size of file to open.

   ; This is a dialog widget to collect the filename and
   ; file sizes from the user. The widget will notify other widgets
   ; when the APPLY button is clicked. The widgets identified in the
   ; NOTIFYID parameter will be sent an event structure defined like this:
   ;
   ;  event = {GETFILE_EVENT, ID:0L, Top:0L, Handler:0L,
   ;           Filename:'', XSize:0, YSize:0}

On_Error, 2

IF N_Params() EQ 0 THEN BEGIN
   ok = Dialog_Message('Widget ID parameter required.')
   RETURN
END

   ; Check keywords.

IF N_Elements(filename) EQ 0 THEN $
   filename=Filepath(Root_Dir=Coyote(),'ctscan.dat') ELSE $
   filename=Filepath(Root_Dir=Coyote(), filename)
IF N_Elements(xsize) EQ 0 THEN xsize = 256
IF N_Elements(ysize) EQ 0 THEN ysize = 256

   ; Position the dialog in the center of the display.

Device, Get_Screen_Size=screenSize
xCenter = FIX(screenSize(0) / 2.0)
yCenter = FIX(screenSize(1) / 2.0)
xoff = xCenter - 150
yoff = yCenter - 150

   ; Create a top-level base. This is NOT a modal widget.

IF N_Elements(parent) NE 0 THEN $
   tlb = Widget_Base(Column=1, XOffset=xoff, YOffset=yoff, $
      Title='Enter File Information...', Group_Leader=parent, $
      Base_Align_Center=1) ELSE $
   tlb = Widget_Base(Column=1, XOffset=xoff, YOffset=yoff, $
      Title='Enter File Information...', Base_Align_Center=1)

   ; Make a sub-base for the filename and size widgets.

subbase = Widget_Base(tlb, Column=1, Frame=1)

   ; Create widgets for filename. Set text widget size appropriately.

filesize = StrLen(filename) * 1.25
fileID = CW_Field(subbase, Title='Filename:', Value=filename, $
   XSize=filesize)

   ; Use CW_Fields for the X and Y Size fields. Advantage: You can
   ; make the values integers rather than strings.

xsizeID = CW_Field(subbase, Title='X Size:', Value=xsize, /Integer)
ysizeID = CW_Field(subbase, Title='Y Size:', Value=ysize, /Integer)

   ; Make a button base with frame to hold DISMISS and APPLY buttons.

butbase = Widget_Base(tlb, Row=1)
dismiss = Widget_Button(butbase, Value='Dismiss')
apply = Widget_Button(butbase, Value='Apply')

   ; Realize top-level base and all of its children.

Widget_Control, tlb, /Realize

   ; Create info structure to hold information needed in event handler.

info = { fileID:fileID, $     ; Identifier of widget holding filename.
         xsizeID:xsizeID, $   ; Identifier of widget holding xsize.
         ysizeID:ysizeID, $   ; Identifier of widget holding ysize.
         notifyID:notifyID }  ; Widgets to notify at APPLY button click.

   ; Store the info structure in the top-level base

Widget_Control, tlb, Set_UValue=info, /No_Copy

   ; Register the program, set up event loop. Make this program a
   ; non-blocking widget.

XManager, 'getfile', tlb, Event_Handler='GetFile_Events', /No_Block
END ;*******************************************************************
