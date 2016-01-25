HEADER
; IDL Visual Widget Builder Resource file. Version 1
; Generated on:	12/22/2012 12:28.27
VERSION 1
END

WB_EOSap_Smoothing BASE 5 5 946 727
REALIZE "EOSap_Smoothing_Create_Object"
TLB
CAPTION "Spectral smoothing  (V1.0)"
XPAD = 3
YPAD = 3
SPACE = 3
BEGIN
  WID_TAB TAB 1 0 937 637
  BEGIN
    WID_BASE BASE 0 0 929 611
    XPAD = 3
    YPAD = 3
    SPACE = 3
    CAPTION "Define Smoothing Parameters"
    BEGIN
      Wt_Smoothing_params TEXT 167 31 610 22
      WIDTH = 20
      HEIGHT = 1
      END
      Wb_Save_Smoothing_params PUSHBUTTON 789 31 120 22
      VALUE "Save Smoothing params"
      ALIGNCENTER
      ONACTIVATE "EOSap_Smoothing_Handle_Save_Smoothing_params"
      END
      Wb_Restore_smoothing_parms PUSHBUTTON 7 31 150 22
      VALUE "Restore Smoothing params"
      ALIGNCENTER
      ONACTIVATE "EOSap_Smoothing_Handle_Restore_Smoothing_params"
      END
      Wb_Select_Image_File PUSHBUTTON 7 3 150 22
      VALUE "Select Image File"
      ALIGNCENTER
      ONACTIVATE "EOSap_Smoothing_Handle_Select_Image_File"
      END
      Wt_Image_File TEXT 166 3 610 22
      WIDTH = 20
      HEIGHT = 1
      END
      WB_ImageViewer BASE 0 64 410 540
      FRAME = 1
      XPAD = 3
      YPAD = 3
      SPACE = 3
      CAPTION "IDL"
      BEGIN
      END
      WID_BASE_1 BASE 414 297 499 312
      FRAME = 1
      XPAD = 3
      YPAD = 3
      SPACE = 3
      CAPTION "IDL"
      BEGIN
        Wl_Bands LIST 4 20 250 200
        FRAME = 1
        MULTIPLE
        ONSELECT "EOSap_Smoothing_Handle_Bands_Select"
        WIDTH = 11
        HEIGHT = 2
        END
        Wl_AvB LABEL 20 0 215 18
        VALUE "Available bands:"
        ALIGNLEFT
        END
        WID_BUTTON_9 PUSHBUTTON 390 32 100 22
        VALUE "Set smooth factor"
        ALIGNCENTER
        ONACTIVATE "EOSap_Smoothing_Handle_set_smooth_factor"
        END
        WID_BUTTON_10 PUSHBUTTON 390 214 100 22
        VALUE "Clear settings"
        ALIGNCENTER
        ONACTIVATE "EOSap_Smoothing_Handle_clear_settings"
        END
        Wt_sFac TEXT 408 58 63 22
        EDITABLE
        WIDTH = 20
        HEIGHT = 1
        END
        WID_BUTTON_11 PUSHBUTTON 390 94 100 22
        VALUE "Ignore"
        ALIGNCENTER
        ONACTIVATE "EOSap_Smoothing_Handle_set_ignore"
        END
        WID_BUTTON_12 PUSHBUTTON 390 124 100 22
        VALUE "Zero out"
        ALIGNCENTER
        ONACTIVATE "EOSap_Smoothing_Handle_set_zero_out"
        END
        WID_BUTTON_15 PUSHBUTTON 390 154 100 22
        VALUE "Remove"
        ALIGNCENTER
        ONACTIVATE "EOSap_Smoothing_Handle_set_remove"
        END
        Wl_Smooth_fac LIST 263 21 115 200
        FRAME = 1
        MULTIPLE
        ONSELECT "EOSap_Smoothing_Handle_Smf_Select"
        WIDTH = 11
        HEIGHT = 2
        END
        Wl_smooth_scenario LIST 4 241 261 67
        FRAME = 1
        MULTIPLE
        ONSELECT "EOSap_Smoothing_Handle_Scenario_Select"
        WIDTH = 11
        HEIGHT = 2
        END
        WID_LABEL_1 LABEL 270 0 111 19
        VALUE "Smoothing factors"
        ALIGNLEFT
        END
        WID_BUTTON_16 PUSHBUTTON 390 2 100 22
        VALUE "Select all"
        ALIGNCENTER
        ONACTIVATE "EOSap_Smoothing_Handle_select_all"
        END
        WID_BUTTON_17 PUSHBUTTON 278 250 100 22
        VALUE "Add scenario"
        ALIGNCENTER
        ONACTIVATE "EOSap_Smoothing_Handle_add_scenario"
        END
        Wt_mask_suffix TEXT 363 282 123 22
        EDITABLE
        WIDTH = 20
        HEIGHT = 1
        END
        Wl_AvB_2 LABEL 4 226 215 18
        VALUE "Smoothing scenario:"
        ALIGNLEFT
        END
        WID_BUTTON_19 PUSHBUTTON 390 250 100 22
        VALUE "Remove scenario"
        ALIGNCENTER
        ONACTIVATE "EOSap_Smoothing_Handle_remove_scenario"
        END
        WID_LABEL_3 LABEL 293 287 62 18
        VALUE "Mask suffix:"
        ALIGNLEFT
        END
        WID_BUTTON_13 PUSHBUTTON 390 184 100 22
        VALUE "Interpolate"
        ALIGNCENTER
        ONACTIVATE "EOSap_Smoothing_Handle_set_interpolate"
        END
      END
      WB_ProfileViewer BASE 414 64 499 228
      FRAME = 1
      XPAD = 3
      YPAD = 3
      SPACE = 3
      CAPTION "IDL"
      BEGIN
      END
    END
    WID_BASE_0 BASE 0 0 929 611
    XPAD = 3
    YPAD = 3
    SPACE = 3
    CAPTION "Apply Smoothing Parameters"
    BEGIN
      WID_BUTTON_0 PUSHBUTTON 40 24 121 22
      VALUE "Input directory"
      ALIGNCENTER
      ONACTIVATE "EOSap_Smoothing_Handle_Input_directory"
      END
      Wt_Input_directory TEXT 190 24 549 22
      WIDTH = 20
      HEIGHT = 1
      END
      WID_BUTTON_1 PUSHBUTTON 40 60 121 22
      VALUE "Output directory"
      ALIGNCENTER
      ONACTIVATE "EOSap_Smoothing_Handle_Output_directory"
      END
      Wt_Output_directory TEXT 190 60 549 22
      WIDTH = 20
      HEIGHT = 1
      END
      Wl_Image_list LIST 288 106 339 428
      FRAME = 1
      MULTIPLE
      WIDTH = 11
      HEIGHT = 2
      END
    END
  END
  WID_BASE_4 BASE 27 638 883 62
  FRAME = 1
  XPAD = 3
  YPAD = 3
  SPACE = 3
  CAPTION "IDL"
  BEGIN
    Status_ProgressBar DRAW 42 30 460 25
    END
    Status_Line TEXT 2 2 500 22
    WIDTH = 20
    HEIGHT = 1
    END
    Status_Led PUSHBUTTON 5 30 30 30
    VALUE "C:\Program Files\ITT\IDL71\resource\bitmaps\_LampG.bmp"
    ALIGNCENTER
    BITMAP
    ONACTIVATE "EOSap_Smoothing_Handle_test"
    END
    WID_BUTTON_About PUSHBUTTON 821 20 60 40
    VALUE "C:\Program Files\ITT\IDL71\resource\bitmaps\_vito.bmp"
    ALIGNCENTER
    BITMAP
    ONACTIVATE "EOSap_Smoothing_Handle_vito_info"
    END
    wb_Start PUSHBUTTON 516 19 90 22
    VALUE "C:\Program Files\ITT\IDL71\resource\bitmaps\_Start.bmp"
    ALIGNCENTER
    BITMAP
    ONACTIVATE "EOSap_Smoothing_handle_start"
    END
    wb_General_info PUSHBUTTON 617 19 90 22
    VALUE "C:\Program Files\ITT\IDL71\resource\bitmaps\help.bmp"
    ALIGNCENTER
    BITMAP
    ONACTIVATE "EOSap_Smoothing_handle_info"
    END
    wb_Quit_Cancel PUSHBUTTON 716 19 90 22
    VALUE "C:\Program Files\ITT\IDL71\resource\bitmaps\_Quit.bmp"
    ALIGNCENTER
    BITMAP
    ONACTIVATE "EOSap_Smoothing_handle_quit_or_cancel"
    END
  END
END
