; 
; IDL Widget Interface Procedures. This Code is automatically 
;     generated and should not be modified.

; 
; Generated on:	02/26/2013 11:13.48
; 
pro WB_EOSap_ClassEditor_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)


  wWidget =  Event.top

  case wTarget of

    Widget_Info(wWidget, FIND_BY_UNAME='WB_EOSap_ClassEditor'): begin
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_0'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_ClassEditor_Handle_Classified_Image_File_Input, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_1'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_ClassEditor_Handle_Classified_Image_File_Output, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='bAccept'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_ClassEditor_Handle_Change_Accept, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='bAccept_1'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_ClassEditor_Handle_Add_New_Class, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_2'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_ClassEditor_Handle_Delete_New_Class, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='bAccept_0'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_ClassEditor_Handle_Save_Changes, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='Status_Led'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_Handle_test, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_About'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_ClassEditor_Handle_vito_info, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='wb_General_info'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_ClassEditor_handle_info, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='wb_Quit_Cancel'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_ClassEditor_handle_quit_or_cancel, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='bAccept_2'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_ClassEditor_Handle_Change_Reset, Event
    end
    else:
  endcase

end

pro WB_EOSap_ClassEditor, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

  Resolve_Routine, 'EOSap_ClassEditor_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines
  
  WB_EOSap_ClassEditor = Widget_Base( GROUP_LEADER=wGroup,  $
      UNAME='WB_EOSap_ClassEditor' ,XOFFSET=50 ,YOFFSET=50  $
      ,SCR_XSIZE=965 ,SCR_YSIZE=650  $
      ,NOTIFY_REALIZE='EOSap_ClassEditor_CreateObject'  $
      ,KILL_NOTIFY='EOSap_ClassEditor_DestroyObject'  $
      ,TITLE='Classification Editor (V1.0)' ,SPACE=3 ,XPAD=3 ,YPAD=3)

  
  WID_BASE_1 = Widget_Base(WB_EOSap_ClassEditor, UNAME='WID_BASE_1'  $
      ,FRAME=1 ,SCR_XSIZE=710 ,SCR_YSIZE=61 ,TITLE='IDL' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)

  
  WID_BUTTON_0 = Widget_Button(WID_BASE_1, UNAME='WID_BUTTON_0'  $
      ,XOFFSET=6 ,YOFFSET=4 ,SCR_XSIZE=126 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,TOOLTIP='  Locate input classified image.'  $
      ,VALUE='Input Classified Image')

  
  WID_BUTTON_1 = Widget_Button(WID_BASE_1, UNAME='WID_BUTTON_1'  $
      ,XOFFSET=6 ,YOFFSET=32 ,SCR_XSIZE=126 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,TOOLTIP='  Specify output classified image.'  $
      ,VALUE='Output Classified Image')

  
  tClass_Input_File = Widget_Text(WID_BASE_1,  $
      UNAME='tClass_Input_File' ,XOFFSET=140 ,YOFFSET=6  $
      ,SCR_XSIZE=560 ,SCR_YSIZE=22 ,XSIZE=20 ,YSIZE=1)

  
  tClassified_File_Output = Widget_Text(WID_BASE_1,  $
      UNAME='tClassified_File_Output' ,XOFFSET=140 ,YOFFSET=32  $
      ,SCR_XSIZE=560 ,SCR_YSIZE=22 ,XSIZE=20 ,YSIZE=1)

  
  WB_Class_From = Widget_Base(WB_EOSap_ClassEditor,  $
      UNAME='WB_Class_From' ,FRAME=1 ,XOFFSET=715 ,YOFFSET=105  $
      ,SCR_XSIZE=240 ,SCR_YSIZE=180 ,TITLE='IDL' ,SPACE=3 ,XPAD=3  $
      ,YPAD=1 ,COLUMN=1)

  
  WB_Class_To = Widget_Base(WB_EOSap_ClassEditor, UNAME='WB_Class_To'  $
      ,FRAME=1 ,XOFFSET=715 ,YOFFSET=315 ,SCR_XSIZE=240  $
      ,SCR_YSIZE=180 ,TAB_MODE=0 ,/ALIGN_LEFT ,/BASE_ALIGN_LEFT  $
      ,TITLE='IDL' ,SPACE=1 ,XPAD=3 ,YPAD=1 ,COLUMN=1)

  
  tChange_Input_Class = Widget_Label(WB_EOSap_ClassEditor,  $
      UNAME='tChange_Input_Class' ,XOFFSET=717 ,YOFFSET=90  $
      ,SCR_XSIZE=128 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='Change Input'+ $
      ' Class:')

  
  tTo_Output_Class = Widget_Label(WB_EOSap_ClassEditor,  $
      UNAME='tTo_Output_Class' ,XOFFSET=716 ,YOFFSET=300  $
      ,SCR_XSIZE=132 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='To New Output'+ $
      ' Class:')

  
  tChanged_From = Widget_Text(WB_EOSap_ClassEditor,  $
      UNAME='tChanged_From' ,XOFFSET=786 ,YOFFSET=513 ,SCR_XSIZE=170  $
      ,SCR_YSIZE=20 ,XSIZE=20 ,YSIZE=1)

  
  tFrom = Widget_Label(WB_EOSap_ClassEditor, UNAME='tFrom'  $
      ,XOFFSET=715 ,YOFFSET=515 ,SCR_XSIZE=67 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='Input Class:')

  
  tTo = Widget_Label(WB_EOSap_ClassEditor, UNAME='tTo' ,XOFFSET=714  $
      ,YOFFSET=541 ,SCR_XSIZE=70 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
      ,VALUE='Output Class:')

  
  tChanged_To = Widget_Text(WB_EOSap_ClassEditor, UNAME='tChanged_To'  $
      ,XOFFSET=786 ,YOFFSET=539 ,SCR_XSIZE=170 ,SCR_YSIZE=20  $
      ,XSIZE=20 ,YSIZE=1)

  
  bAccept = Widget_Button(WB_EOSap_ClassEditor, UNAME='bAccept'  $
      ,XOFFSET=860 ,YOFFSET=565 ,SCR_XSIZE=90 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,TOOLTIP='Set Class Changes' ,VALUE='select.bmp'  $
      ,/BITMAP)

  
  WID_BASE_6 = Widget_Base(WB_EOSap_ClassEditor, UNAME='WID_BASE_6'  $
      ,FRAME=1 ,XOFFSET=715 ,SCR_XSIZE=240 ,SCR_YSIZE=61 ,TITLE='IDL'  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3)

  
  tClass_Name = Widget_Text(WID_BASE_6, UNAME='tClass_Name'  $
      ,XOFFSET=58 ,YOFFSET=23 ,SCR_XSIZE=174 ,SCR_YSIZE=20 ,/EDITABLE  $
      ,VALUE=[ '' ] ,XSIZE=20 ,YSIZE=1)

  
  WID_LABEL_3 = Widget_Label(WID_BASE_6, UNAME='WID_LABEL_3'  $
      ,XOFFSET=70 ,YOFFSET=9 ,SCR_XSIZE=113 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='New Class Name:')

  
  bAccept_1 = Widget_Button(WID_BASE_6, UNAME='bAccept_1' ,XOFFSET=3  $
      ,YOFFSET=5 ,SCR_XSIZE=50 ,SCR_YSIZE=22 ,/ALIGN_CENTER  $
      ,TOOLTIP='Add new class' ,VALUE='plus.bmp' ,/BITMAP)

  
  WID_BUTTON_2 = Widget_Button(WID_BASE_6, UNAME='WID_BUTTON_2'  $
      ,XOFFSET=3 ,YOFFSET=34 ,SCR_XSIZE=50 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,TOOLTIP='Remove new class' ,VALUE='delete.bmp'  $
      ,/BITMAP)

  
  bAccept_0 = Widget_Button(WB_EOSap_ClassEditor, UNAME='bAccept_0'  $
      ,XOFFSET=860 ,YOFFSET=595 ,SCR_XSIZE=90 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,TOOLTIP='Save Class Changes' ,VALUE='save.bmp'  $
      ,/BITMAP)

  
  WB_ImageViewer = Widget_Base(WB_EOSap_ClassEditor,  $
      UNAME='WB_ImageViewer' ,FRAME=1 ,YOFFSET=66 ,SCR_XSIZE=710  $
      ,SCR_YSIZE=491 ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

  
  WID_BASE_5 = Widget_Base(WB_EOSap_ClassEditor, UNAME='WID_BASE_5'  $
      ,FRAME=1 ,YOFFSET=561 ,SCR_XSIZE=710 ,SCR_YSIZE=62 ,TITLE='IDL'  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3)

  
  Status_Led = Widget_Button(WID_BASE_5, UNAME='Status_Led'  $
      ,SCR_XSIZE=30 ,SCR_YSIZE=30 ,/ALIGN_CENTER ,VALUE='_LampG.bmp'  $
      ,/BITMAP)

  
  WID_BUTTON_About = Widget_Button(WID_BASE_5,  $
      UNAME='WID_BUTTON_About' ,XOFFSET=641 ,YOFFSET=10 ,SCR_XSIZE=60  $
      ,SCR_YSIZE=40 ,/ALIGN_CENTER ,TOOLTIP='Display designer info'  $
      ,VALUE='_vito.bmp' ,/BITMAP)

  
  wb_General_info = Widget_Button(WID_BASE_5, UNAME='wb_General_info'  $
      ,XOFFSET=47 ,YOFFSET=31 ,SCR_XSIZE=90 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,TOOLTIP='Display help' ,VALUE='help.bmp'  $
      ,/BITMAP)

  
  wb_Quit_Cancel = Widget_Button(WID_BASE_5, UNAME='wb_Quit_Cancel'  $
      ,XOFFSET=200 ,YOFFSET=31 ,SCR_XSIZE=90 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,TOOLTIP='Quit Application' ,VALUE='_Quit.bmp'  $
      ,/BITMAP)

  
  Status_Screen = Widget_Draw(WID_BASE_5, UNAME='Status_Screen'  $
      ,XOFFSET=40 ,YOFFSET=3 ,SCR_XSIZE=590 ,SCR_YSIZE=22 ,RETAIN=2  $
      ,GRAPHICS_LEVEL=2)

  
  bAccept_2 = Widget_Button(WB_EOSap_ClassEditor, UNAME='bAccept_2'  $
      ,XOFFSET=740 ,YOFFSET=565 ,SCR_XSIZE=90 ,SCR_YSIZE=23  $
      ,/ALIGN_CENTER ,TOOLTIP='Reset Class Changes'  $
      ,VALUE='reset.bmp' ,/BITMAP)

  Widget_Control, /REALIZE, WB_EOSap_ClassEditor

  XManager, 'WB_EOSap_ClassEditor', WB_EOSap_ClassEditor, /NO_BLOCK  ,CLEANUP='EOSap_ClassEditor_DestroyObject'  

end
; 
; Empty stub procedure used for autoloading.
; 
pro EOSap_ClassEditor, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  WB_EOSap_ClassEditor, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end
