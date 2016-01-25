; 
; IDL Widget Interface Procedures. This Code is automatically 
;     generated and should not be modified.

; 
; Generated on:	12/14/2012 14:21.09
; 
pro WB_EOSap_Smoothing_event, Event

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
      widget_info(Event.id, /tree_root) : event.id)


  wWidget =  Event.top

  case wTarget of

    Widget_Info(wWidget, FIND_BY_UNAME='WB_EOSap_Smoothing'): begin
    end
    Widget_Info(wWidget, FIND_BY_UNAME='Wb_Save_Smoothing_params'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_Handle_Save_Smoothing_params, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='Wb_Restore_smoothing_parms'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_Handle_Restore_Smoothing_params, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='Wb_Select_Image_File'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_Handle_Select_Image_File, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='Wl_Bands'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_LIST' )then $
        EOSap_Smoothing_Handle_Bands_Select, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_9'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_Handle_set_smooth_factor, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_10'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_Handle_clear_settings, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_11'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_Handle_set_ignore, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_12'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_Handle_set_zero_out, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_15'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_Handle_set_remove, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='Wl_Smooth_fac'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_LIST' )then $
        EOSap_Smoothing_Handle_Smf_Select, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='Wl_smooth_scenario'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_LIST' )then $
        EOSap_Smoothing_Handle_Scenario_Select, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_16'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_Handle_select_all, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_17'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_Handle_add_scenario, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_19'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_Handle_remove_scenario, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_13'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_Handle_set_interpolate, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_0'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_Handle_Input_directory, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_1'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_Handle_Output_directory, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='Status_Led'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_Handle_test, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_About'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_Handle_vito_info, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='wb_Start'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_handle_start, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='wb_General_info'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_handle_info, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='wb_Quit_Cancel'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        EOSap_Smoothing_handle_quit_or_cancel, Event
    end
    else:
  endcase

end

pro WB_EOSap_Smoothing, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

  Resolve_Routine, 'EOSap_Smoothing_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines
  
  WB_EOSap_Smoothing = Widget_Base( GROUP_LEADER=wGroup,  $
      UNAME='WB_EOSap_Smoothing' ,XOFFSET=5 ,YOFFSET=5 ,SCR_XSIZE=946  $
      ,SCR_YSIZE=727 ,NOTIFY_REALIZE='EOSap_Smoothing_Create_Object'  $
      ,TITLE='Interactive Spectral Smoothing  (V1.0)' ,SPACE=3 ,XPAD=3 ,YPAD=3)

  
  WID_TAB = Widget_Tab(WB_EOSap_Smoothing, UNAME='WID_TAB' ,XOFFSET=1  $
      ,SCR_XSIZE=937 ,SCR_YSIZE=637)

  
  WID_BASE = Widget_Base(WID_TAB, UNAME='WID_BASE' ,SCR_XSIZE=929  $
      ,SCR_YSIZE=611 ,TITLE='Define Smoothing Parameters' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)

  
  Wt_Smoothing_params = Widget_Text(WID_BASE,  $
      UNAME='Wt_Smoothing_params' ,XOFFSET=167 ,YOFFSET=31  $
      ,SCR_XSIZE=610 ,SCR_YSIZE=22 ,XSIZE=20 ,YSIZE=1)

  
  Wb_Save_Smoothing_params = Widget_Button(WID_BASE,  $
      UNAME='Wb_Save_Smoothing_params' ,XOFFSET=789 ,YOFFSET=31  $
      ,SCR_XSIZE=120 ,SCR_YSIZE=22 ,/ALIGN_CENTER ,VALUE='Save'+ $
      ' Smoothing params')

  
  Wb_Restore_smoothing_parms = Widget_Button(WID_BASE,  $
      UNAME='Wb_Restore_smoothing_parms' ,XOFFSET=7 ,YOFFSET=31  $
      ,SCR_XSIZE=150 ,SCR_YSIZE=22 ,/ALIGN_CENTER ,VALUE='Restore'+ $
      ' Smoothing params')

  
  Wb_Select_Image_File = Widget_Button(WID_BASE,  $
      UNAME='Wb_Select_Image_File' ,XOFFSET=7 ,YOFFSET=3  $
      ,SCR_XSIZE=150 ,SCR_YSIZE=22 ,/ALIGN_CENTER ,VALUE='Select'+ $
      ' Image File')

  
  Wt_Image_File = Widget_Text(WID_BASE, UNAME='Wt_Image_File'  $
      ,XOFFSET=166 ,YOFFSET=3 ,SCR_XSIZE=610 ,SCR_YSIZE=22 ,XSIZE=20  $
      ,YSIZE=1)

  
  WB_ImageViewer = Widget_Base(WID_BASE, UNAME='WB_ImageViewer'  $
      ,FRAME=1 ,YOFFSET=64 ,SCR_XSIZE=410 ,SCR_YSIZE=540 ,TITLE='IDL'  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3)

  
  WID_BASE_1 = Widget_Base(WID_BASE, UNAME='WID_BASE_1' ,FRAME=1  $
      ,XOFFSET=414 ,YOFFSET=297 ,SCR_XSIZE=499 ,SCR_YSIZE=312  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

  
  Wl_Bands = Widget_List(WID_BASE_1, UNAME='Wl_Bands' ,FRAME=1  $
      ,XOFFSET=4 ,YOFFSET=20 ,SCR_XSIZE=250 ,SCR_YSIZE=200 ,/MULTIPLE  $
      ,XSIZE=11 ,YSIZE=2)

  
  Wl_AvB = Widget_Label(WID_BASE_1, UNAME='Wl_AvB' ,XOFFSET=20  $
      ,SCR_XSIZE=215 ,SCR_YSIZE=18 ,/ALIGN_LEFT ,VALUE='Available'+ $
      ' bands:')

  
  WID_BUTTON_9 = Widget_Button(WID_BASE_1, UNAME='WID_BUTTON_9'  $
      ,XOFFSET=390 ,YOFFSET=32 ,SCR_XSIZE=100 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='Set smooth factor')

  
  WID_BUTTON_10 = Widget_Button(WID_BASE_1, UNAME='WID_BUTTON_10'  $
      ,XOFFSET=390 ,YOFFSET=214 ,SCR_XSIZE=100 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='Clear settings')

  
  Wt_sFac = Widget_Text(WID_BASE_1, UNAME='Wt_sFac' ,XOFFSET=408  $
      ,YOFFSET=58 ,SCR_XSIZE=63 ,SCR_YSIZE=22 ,/EDITABLE ,XSIZE=20  $
      ,YSIZE=1)

  
  WID_BUTTON_11 = Widget_Button(WID_BASE_1, UNAME='WID_BUTTON_11'  $
      ,XOFFSET=390 ,YOFFSET=94 ,SCR_XSIZE=100 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='Ignore')

  
  WID_BUTTON_12 = Widget_Button(WID_BASE_1, UNAME='WID_BUTTON_12'  $
      ,XOFFSET=390 ,YOFFSET=124 ,SCR_XSIZE=100 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='Zero out')

  
  WID_BUTTON_15 = Widget_Button(WID_BASE_1, UNAME='WID_BUTTON_15'  $
      ,XOFFSET=390 ,YOFFSET=154 ,SCR_XSIZE=100 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='Remove')

  
  Wl_Smooth_fac = Widget_List(WID_BASE_1, UNAME='Wl_Smooth_fac'  $
      ,FRAME=1 ,XOFFSET=263 ,YOFFSET=21 ,SCR_XSIZE=115 ,SCR_YSIZE=200  $
      ,/MULTIPLE ,XSIZE=11 ,YSIZE=2)

  
  Wl_smooth_scenario = Widget_List(WID_BASE_1,  $
      UNAME='Wl_smooth_scenario' ,FRAME=1 ,XOFFSET=4 ,YOFFSET=241  $
      ,SCR_XSIZE=261 ,SCR_YSIZE=67 ,/MULTIPLE ,XSIZE=11 ,YSIZE=2)

  
  WID_LABEL_1 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_1'  $
      ,XOFFSET=270 ,SCR_XSIZE=111 ,SCR_YSIZE=19 ,/ALIGN_LEFT  $
      ,VALUE='Smoothing factors')

  
  WID_BUTTON_16 = Widget_Button(WID_BASE_1, UNAME='WID_BUTTON_16'  $
      ,XOFFSET=390 ,YOFFSET=2 ,SCR_XSIZE=100 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='Select all')

  
  WID_BUTTON_17 = Widget_Button(WID_BASE_1, UNAME='WID_BUTTON_17'  $
      ,XOFFSET=278 ,YOFFSET=250 ,SCR_XSIZE=100 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='Add scenario')

  
  Wt_mask_suffix = Widget_Text(WID_BASE_1, UNAME='Wt_mask_suffix'  $
      ,XOFFSET=363 ,YOFFSET=282 ,SCR_XSIZE=123 ,SCR_YSIZE=22  $
      ,/EDITABLE ,XSIZE=20 ,YSIZE=1)

  
  Wl_AvB_2 = Widget_Label(WID_BASE_1, UNAME='Wl_AvB_2' ,XOFFSET=4  $
      ,YOFFSET=226 ,SCR_XSIZE=215 ,SCR_YSIZE=18 ,/ALIGN_LEFT  $
      ,VALUE='Smoothing scenario:')

  
  WID_BUTTON_19 = Widget_Button(WID_BASE_1, UNAME='WID_BUTTON_19'  $
      ,XOFFSET=390 ,YOFFSET=250 ,SCR_XSIZE=100 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='Remove scenario')

  
  WID_LABEL_3 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_3'  $
      ,XOFFSET=293 ,YOFFSET=287 ,SCR_XSIZE=62 ,SCR_YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='Mask suffix:')

  
  WID_BUTTON_13 = Widget_Button(WID_BASE_1, UNAME='WID_BUTTON_13'  $
      ,XOFFSET=390 ,YOFFSET=184 ,SCR_XSIZE=100 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='Interpolate')

  
  WB_ProfileViewer = Widget_Base(WID_BASE, UNAME='WB_ProfileViewer'  $
      ,FRAME=1 ,XOFFSET=414 ,YOFFSET=64 ,SCR_XSIZE=500 ,SCR_YSIZE=228  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

  
  WID_BASE_0 = Widget_Base(WID_TAB, UNAME='WID_BASE_0' ,SCR_XSIZE=929  $
      ,SCR_YSIZE=611 ,TITLE='Apply Smoothing Parameters' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)

  
  WID_BUTTON_0 = Widget_Button(WID_BASE_0, UNAME='WID_BUTTON_0'  $
      ,XOFFSET=40 ,YOFFSET=24 ,SCR_XSIZE=121 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='Input directory')

  
  Wt_Input_directory = Widget_Text(WID_BASE_0,  $
      UNAME='Wt_Input_directory' ,XOFFSET=190 ,YOFFSET=24  $
      ,SCR_XSIZE=549 ,SCR_YSIZE=22 ,XSIZE=20 ,YSIZE=1)

  
  WID_BUTTON_1 = Widget_Button(WID_BASE_0, UNAME='WID_BUTTON_1'  $
      ,XOFFSET=40 ,YOFFSET=60 ,SCR_XSIZE=121 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='Output directory')

  
  Wt_Output_directory = Widget_Text(WID_BASE_0,  $
      UNAME='Wt_Output_directory' ,XOFFSET=190 ,YOFFSET=60  $
      ,SCR_XSIZE=549 ,SCR_YSIZE=22 ,XSIZE=20 ,YSIZE=1)

  
  Wl_Image_list = Widget_List(WID_BASE_0, UNAME='Wl_Image_list'  $
      ,FRAME=1 ,XOFFSET=288 ,YOFFSET=106 ,SCR_XSIZE=339  $
      ,SCR_YSIZE=428 ,/MULTIPLE ,XSIZE=11 ,YSIZE=2)

  
  WID_BASE_4 = Widget_Base(WB_EOSap_Smoothing, UNAME='WID_BASE_4'  $
      ,FRAME=1 ,XOFFSET=27 ,YOFFSET=638 ,SCR_XSIZE=883 ,SCR_YSIZE=62  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)

  
  Status_ProgressBar = Widget_Draw(WID_BASE_4,  $
      UNAME='Status_ProgressBar' ,XOFFSET=42 ,YOFFSET=30  $
      ,SCR_XSIZE=460 ,SCR_YSIZE=25)

  
  Status_Line = Widget_Text(WID_BASE_4, UNAME='Status_Line'  $
      ,XOFFSET=2 ,YOFFSET=2 ,SCR_XSIZE=500 ,SCR_YSIZE=22 ,XSIZE=20  $
      ,YSIZE=1)

  
  Status_Led = Widget_Button(WID_BASE_4, UNAME='Status_Led'  $
      ,XOFFSET=5 ,YOFFSET=30 ,SCR_XSIZE=30 ,SCR_YSIZE=30  $
      ,/ALIGN_CENTER ,VALUE='_LampG.bmp' ,/BITMAP)

  
  WID_BUTTON_About = Widget_Button(WID_BASE_4,  $
      UNAME='WID_BUTTON_About' ,XOFFSET=821 ,YOFFSET=20 ,SCR_XSIZE=60  $
      ,SCR_YSIZE=40 ,/ALIGN_CENTER ,VALUE='_vito.bmp' ,/BITMAP)

  
  wb_Start = Widget_Button(WID_BASE_4, UNAME='wb_Start' ,XOFFSET=516  $
      ,YOFFSET=19 ,SCR_XSIZE=90 ,SCR_YSIZE=22 ,/ALIGN_CENTER  $
      ,VALUE='_Start.bmp' ,/BITMAP)

  
  wb_General_info = Widget_Button(WID_BASE_4, UNAME='wb_General_info'  $
      ,XOFFSET=617 ,YOFFSET=19 ,SCR_XSIZE=90 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='help.bmp' ,/BITMAP)

  
  wb_Quit_Cancel = Widget_Button(WID_BASE_4, UNAME='wb_Quit_Cancel'  $
      ,XOFFSET=716 ,YOFFSET=19 ,SCR_XSIZE=90 ,SCR_YSIZE=22  $
      ,/ALIGN_CENTER ,VALUE='_Quit.bmp' ,/BITMAP)

  Widget_Control, /REALIZE, WB_EOSap_Smoothing

  XManager, 'WB_EOSap_Smoothing', WB_EOSap_Smoothing, /NO_BLOCK  

end
; 
; Empty stub procedure used for autoloading.
; 
pro EOSap_Smoothing, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  WB_EOSap_Smoothing, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end
