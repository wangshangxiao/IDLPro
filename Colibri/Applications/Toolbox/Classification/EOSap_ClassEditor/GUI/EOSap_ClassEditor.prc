HEADER
; IDL Visual Widget Builder Resource file. Version 1
; Generated on:	11/11/2013 19:48.51
VERSION 1
END

WB_EOSap_ClassEditor BASE 50 50 1200 650
REALIZE "EOSap_ClassEditor_CreateObject"
KILLNOTIFY "EOSap_ClassEditor_DestroyObject"
TLB
CAPTION "Classification Editor (V1.0)"
XPAD = 3
YPAD = 3
SPACE = 3
BEGIN
  WID_BASE_1 BASE 0 0 710 61
  FRAME = 1
  XPAD = 3
  YPAD = 3
  SPACE = 3
  CAPTION "IDL"
  BEGIN
    WID_BUTTON_0 PUSHBUTTON 6 4 174 22
    VALUE "Input Classified Image"
    ALIGNCENTER
    TOOLTIP "  Locate input classified image."
    ONACTIVATE "EOSap_ClassEditor_Handle_Classified_Image_File_Input"
    END
    WID_BUTTON_1 PUSHBUTTON 6 32 173 22
    VALUE "Output Classified Image"
    ALIGNCENTER
    TOOLTIP "  Specify output classified image."
    ONACTIVATE "EOSap_ClassEditor_Handle_Classified_Image_File_Output"
    END
    tClass_Input_File TEXT 193 5 507 22
    WIDTH = 20
    HEIGHT = 1
    END
    tClassified_File_Output TEXT 192 32 508 22
    WIDTH = 20
    HEIGHT = 1
    END
  END
  WB_Class_From BASE 715 105 240 180
  FRAME = 1
  COLUMNS = 1
  XPAD = 3
  YPAD = 1
  SPACE = 3
  CAPTION "IDL"
  BEGIN
  END
  WB_Class_To BASE 715 315 240 180
  FRAME = 1
  ALIGNLEFT
  TAB_MODE = 1
  COLUMNS = 1
  XPAD = 3
  YPAD = 1
  SPACE = 1
  CHILDALIGNLEFT
  CAPTION "IDL"
  BEGIN
  END
  tChange_Input_Class LABEL 717 90 128 18
  VALUE "Change Input Class:"
  ALIGNLEFT
  END
  tTo_Output_Class LABEL 716 300 132 18
  VALUE "To New Output Class:"
  ALIGNLEFT
  END
  tChanged_From TEXT 786 513 170 20
  WIDTH = 20
  HEIGHT = 1
  END
  tFrom LABEL 715 501 67 32
  VALUE "Input Class:"
  ALIGNLEFT
  END
  tTo LABEL 714 535 70 24
  VALUE "Output Class:"
  ALIGNLEFT
  END
  tChanged_To TEXT 786 539 170 20
  WIDTH = 20
  HEIGHT = 1
  END
  bAccept PUSHBUTTON 861 565 145 23
  VALUE "C:\Program Files\ITT\IDL64\resource\bitmaps\select.bmp"
  ALIGNCENTER
  BITMAP
  TOOLTIP "Set Class Changes"
  ONACTIVATE "EOSap_ClassEditor_Handle_Change_Accept"
  END
  WID_BASE_6 BASE 715 0 309 61
  FRAME = 1
  XPAD = 3
  YPAD = 3
  SPACE = 3
  CAPTION "IDL"
  BEGIN
    tClass_Name TEXT 96 23 200 20
    NUMITEMS = 1
    ITEM ""
    EDITABLE
    WIDTH = 20
    HEIGHT = 1
    END
    WID_LABEL_3 LABEL 95 8 113 18
    VALUE "New Class Name:"
    ALIGNLEFT
    END
    bAccept_1 PUSHBUTTON 3 5 80 22
    VALUE "C:\Program Files\ITT\IDL64\resource\bitmaps\plus.bmp"
    ALIGNCENTER
    BITMAP
    TOOLTIP "Add new class"
    ONACTIVATE "EOSap_ClassEditor_Handle_Add_New_Class"
    END
    WID_BUTTON_2 PUSHBUTTON 3 34 80 22
    VALUE "C:\Program Files\ITT\IDL64\resource\bitmaps\delete.bmp"
    ALIGNCENTER
    BITMAP
    TOOLTIP "Remove new class"
    ONACTIVATE "EOSap_ClassEditor_Handle_Delete_New_Class"
    END
  END
  bAccept_0 PUSHBUTTON 860 595 90 23
  VALUE "C:\Program Files\ITT\IDL64\resource\bitmaps\save.bmp"
  ALIGNCENTER
  BITMAP
  TOOLTIP "Save Class Changes"
  ONACTIVATE "EOSap_ClassEditor_Handle_Save_Changes"
  END
  WB_ImageViewer BASE 0 66 710 491
  FRAME = 1
  XPAD = 3
  YPAD = 3
  SPACE = 3
  CAPTION "IDL"
  BEGIN
  END
  WID_BASE_5 BASE 0 561 710 62
  FRAME = 1
  XPAD = 3
  YPAD = 3
  SPACE = 3
  CAPTION "IDL"
  BEGIN
    Status_Led PUSHBUTTON 0 0 30 30
    VALUE "C:\Program Files\ITT\IDL71\resource\bitmaps\_LampG.bmp"
    ALIGNCENTER
    BITMAP
    ONACTIVATE "EOSap_Smoothing_Handle_test"
    END
    WID_BUTTON_About PUSHBUTTON 641 10 60 40
    VALUE "C:\Program Files\ITT\IDL71\resource\bitmaps\_vito.bmp"
    ALIGNCENTER
    BITMAP
    TOOLTIP "Display designer info"
    ONACTIVATE "EOSap_ClassEditor_Handle_vito_info"
    END
    wb_General_info PUSHBUTTON 47 31 90 22
    VALUE "C:\Program Files\ITT\IDL71\resource\bitmaps\help.bmp"
    ALIGNCENTER
    BITMAP
    TOOLTIP "Display help"
    ONACTIVATE "EOSap_ClassEditor_handle_info"
    END
    wb_Quit_Cancel PUSHBUTTON 200 31 90 22
    VALUE "C:\Program Files\ITT\IDL71\resource\bitmaps\_Quit.bmp"
    ALIGNCENTER
    BITMAP
    TOOLTIP "Quit Application"
    ONACTIVATE "EOSap_ClassEditor_handle_quit_or_cancel"
    END
    Status_Screen DRAW 40 3 590 22
    COLORMODELRGBA
    OBJECTGRAPHICS
    RETAIN = 2
    END
  END
  bAccept_2 PUSHBUTTON 720 565 127 23
  VALUE "C:\Program Files\ITT\IDL64\resource\bitmaps\reset.bmp"
  ALIGNCENTER
  BITMAP
  TOOLTIP "Reset Class Changes"
  ONACTIVATE "EOSap_ClassEditor_Handle_Change_Reset"
  END
END
