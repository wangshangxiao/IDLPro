;;IMAGE_DIRECTORY:'D:\DAY\','D:\TENDAY\'
;;INDEX_DIRECTORY:
;;INDEXPARA_DIRECTORY:
;;SOILMOISTURE_DIRECTORY:


pro  systemSET_OK_BUTTON,event

   CATCH, Error_status
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      Info=DiaLOG_MESSAGE('出现错误:'+!ERROR_STATE.MSG)
      CATCH, /CANCEL
      return
   ENDIF

    widget_control, Event.top,Get_Uvalue = pstate

    Widget_Control,(*pstate).IMAGE_WorkFile_TEXT,get_value=IMAGE_DIRECTORY
    Widget_Control,(*pstate).INDEX_WorkFile_TEXT,get_value=INDEX_DIRECTORY
    Widget_Control,(*pstate).INDEXPARA_WorkFile_TEXT,get_value=INDEXPARA_DIRECTORY
    Widget_Control,(*pstate).SOILMOISTURE_WorkFile_TEXT,get_value=SOILMOISTURE_DIRECTORY

    pathfile='data\DROUGHTPath.INI'
    openw,lun,pathfile,/get_lun
    writeu,lun,'IMAGE_DIRECTORY: ' , IMAGE_DIRECTORY+string(byte(13))+string(byte(10))
    writeu,lun,'INDEX_DIRECTORY: ' , INDEX_DIRECTORY+string(byte(13))+string(byte(10))
    writeu,lun,'INDEXPARA_DIRECTORY: ' , INDEXPARA_DIRECTORY+string(byte(13))+string(byte(10))
    writeu,lun,'SOILMOISTURE_DIRECTORY: ' , SOILMOISTURE_DIRECTORY+string(byte(13))+string(byte(10))
    free_lun,lun
    CLOSE,/all
    WIDGET_CONTROL, EVENT.TOP, /destroy
end



pro systemSET_CLOSE_BUTTON, EVENT
     CLOSE,/all
     WIDGET_CONTROL, EVENT.TOP, /destroy
     RETURN
END

pro MERGEPATHSET_TOP_BASE_EVENT, EVENT
    WIDGET_CONTROL, Event.id, GET_UVALUE=uval
    widget_control, Event.top,Get_Uvalue = pstate
    Widget_Control,(*pstate).FileSelect,get_value=path
    print,'33333333333333',path
    CASE uval OF
    'IMAGE_WorkFile_BUTTON':Widget_Control,(*pstate).IMAGE_WorkFile_TEXT,set_value=path
    'INDEX_WorkFile_BUTTON': Widget_Control,(*pstate).INDEX_WorkFile_TEXT,set_value=path
    'INDEXPARA_WorkFile_BUTTON':  Widget_Control,(*pstate).INDEXPARA_WorkFile_TEXT,set_value=path
    'SOILMOISTURE_WorkFile_BUTTON':Widget_Control,(*pstate).SOILMOISTURE_WorkFile_TEXT,set_value=path
    'Field_WorkFile_BUTTON':Widget_Control,(*pstate).Field_WorkFile_TEXT,set_value=path
    'efiCop_WorkFile_BUTTON':Widget_Control,(*pstate).eficop_WorkFile_TEXT,set_value=path

     Else :Break
    endcase
end

pro SYSTYMEPATHSet

   CATCH, Error_status
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      Info=DiaLOG_MESSAGE('出现错误:'+!ERROR_STATE.MSG)
      widget_control,MERGEPATHSET_TOP_BASE,/destroy
      CATCH, /CANCEL
      return
   ENDIF

  Resolve_Routine, 'noaa_get_sys_path',/COMPILE_FULL_FILE,/either
  path = noaa_get_sys_path()
  MERGEPATHSET_TOP_BASE = Widget_Base(    $
      UNAME='POINT_TOP_BASE' ,XOFFSET=300 ,YOFFSET=200 ,SCR_XSIZE=345  $
      ,SCR_YSIZE=380,TITLE='文件路径设置' ,SPACE=3 ,XPAD=3 ,YPAD=3,EVENT_pro='MERGEPATHSET_TOP_BASE_EVENT')

  IMAGE_WorkFile_label = WIDGET_LABEL(MERGEPATHSET_TOP_BASE, VALUE='MODIS数据路径',XOFFSET=3 ,YOFFSET=3,XSIZE=80)
  IMAGE_WorkFile_TEXT = WIDGET_TEXT(MERGEPATHSET_TOP_BASE, UVALUE='IMAGE_WorkFile_TEXT',XOFFSET=93 ,YOFFSET=3,SCR_XSIZE=180)
  IMAGE_WorkFile_BUTTON =Widget_Button(MERGEPATHSET_TOP_BASE, UVALUE='IMAGE_WorkFile_BUTTON',VALUE='选择',XOFFSET=283 ,YOFFSET=3)

  INDEX_WorkFile_label = WIDGET_LABEL(MERGEPATHSET_TOP_BASE, VALUE='指数结果路径',XOFFSET=3 ,YOFFSET=30,XSIZE=80)
  INDEX_WorkFile_TEXT = WIDGET_TEXT(MERGEPATHSET_TOP_BASE, UVALUE='INDEX_WorkFile_TEXT',XOFFSET=93 ,YOFFSET=33,SCR_XSIZE=180)
  INDEX_WorkFile_BUTTON =Widget_Button(MERGEPATHSET_TOP_BASE, UVALUE='INDEX_WorkFile_BUTTON',VALUE='选择',XOFFSET=283 ,YOFFSET=33)

  INDEXPARA_WorkFile_label = WIDGET_LABEL(MERGEPATHSET_TOP_BASE, VALUE='指数参数路径',XOFFSET=3 ,YOFFSET=60,XSIZE=80)
  INDEXPARA_WorkFile_TEXT = WIDGET_TEXT(MERGEPATHSET_TOP_BASE, UVALUE='INDEXPARA_WorkFile_TEXT',XOFFSET=93 ,YOFFSET=60,SCR_XSIZE=180)
  INDEXPARA_WorkFile_BUTTON =Widget_Button(MERGEPATHSET_TOP_BASE, UVALUE='INDEXPARA_WorkFile_BUTTON',VALUE='选择',XOFFSET=283 ,YOFFSET=63)

  SOILMOISTURE_WorkFile_label = WIDGET_LABEL(MERGEPATHSET_TOP_BASE, VALUE='监测结果路径',XOFFSET=3 ,YOFFSET=90,XSIZE=80)
  SOILMOISTURE_WorkFile_TEXT = WIDGET_TEXT(MERGEPATHSET_TOP_BASE, UVALUE='SOILMOISTURE_WorkFile_TEXT',XOFFSET=93 ,YOFFSET=90,SCR_XSIZE=180)
  SOILMOISTURE_WorkFile_BUTTON =Widget_Button(MERGEPATHSET_TOP_BASE, UVALUE='SOILMOISTURE_WorkFile_BUTTON',VALUE='选择',XOFFSET=283 ,YOFFSET=93)






  STABASIC_BASE1 =Widget_Base( MERGEPATHSET_TOP_BASE,/COLUMN,TiTle=' ',/FRAME,uvalue='STABASIC_BASE1',XOFFSET=5 ,YOFFSET=120 ,SCR_XSIZE=325,SCR_YSIZE=170)
 ; FileInfo_base2=Widget_Base( STABASIC_BASE1,/Row)


  FileSelect_Base=Widget_Base(STABASIC_BASE1,SCR_YSIZE=168,SCR_XSIZE=265)
  FileSelect=CW_FILESEL (FileSelect_Base,UNAME='FileSelect',PATH=variable)
  Process_Base=Widget_Base(MERGEPATHSET_TOP_BASE,yoffset=320)
  OK_BUTTON = Widget_Button(Process_Base, UNAME='' , $
      XOFFSET=60 ,SCR_XSIZE=60 ,SCR_YSIZE=24,$
      VALUE='确定',EVENT_pro='systemSET_OK_BUTTON')


  CLOSE_BUTTON = Widget_Button(Process_Base, UNAME='', $
      XOFFSET=210 ,SCR_XSIZE=60 ,SCR_YSIZE=24,$
      VALUE='退出',EVENT_pro='systemSET_CLOSE_BUTTON')

Widget_Control,IMAGE_WorkFile_TEXT,set_value=path.IMAGE_DIRECTORY
Widget_Control,INDEX_WorkFile_TEXT,set_value=path.INDEX_DIRECTORY
Widget_Control,INDEXPARA_WorkFile_TEXT,set_value=path.INDEXPARA_DIRECTORY
Widget_Control,SOILMOISTURE_WorkFile_TEXT,set_value=path.SOILMOISTURE_DIRECTORY



  STATE = { IMAGE_DIRECTORY                 :                        ''    , $
            INDEX_DIRECTORY                 :                        ''    , $
            INDEXPARA_DIRECTORY             :                        ''    , $
            SOILMOISTURE_DIRECTORY          :                        ''    , $
            IMAGE_WorkFile_TEXT             :   IMAGE_WorkFile_TEXT        , $
            INDEX_WorkFile_TEXT             :   INDEX_WorkFile_TEXT        , $
            INDEXPARA_WorkFile_TEXT         :   INDEXPARA_WorkFile_TEXT    , $
            SOILMOISTURE_WorkFile_TEXT      :   SOILMOISTURE_WorkFile_TEXT , $

            FileSelect                      :    FileSelect                $

        }
  pstate = PTR_NEW(state, /no_copy)

  Widget_Control, MERGEPATHSET_TOP_BASE, set_uvalue=pstate
  Widget_Control, /REALIZE, MERGEPATHSET_TOP_BASE
  XManager, 'MERGEPATHSET_TOP_BASE',MERGEPATHSET_TOP_BASE, /NO_BLOCK
end
