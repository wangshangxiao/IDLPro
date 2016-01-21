PRO tabletest_event, ev

  COMPILE_OPT hidden

  ; Retrieve the anonymous structure contained in the user value of
  ; the top-level base widget. 
  WIDGET_CONTROL, ev.top, GET_UVALUE=stash

  ; Retrieve the table's selection mode and selection.
  ;disjoint = WIDGET_INFO(stash.table, /TABLE_DISJOINT_SELECTION)
  selection = WIDGET_INFO(stash.table, /TABLE_SELECT)

  ; Check to see whether a selection exists, setting the
  ; variable 'hasSelection' accordingly.
  n = N_ELEMENTS(selection)
  print,'n=',n
  IF (selection[0] ne -1) THEN hasSelection = 1 $
    ELSE hasSelection = 0

  ; If there is a selection, get the value.
  IF (hasSelection) THEN WIDGET_CONTROL, stash.table, GET_VALUE=value,/USE_TABLE_SELECT


  IF (ev.ID eq stash.Button_AddRow) THEN BEGIN
    WIDGET_CONTROL, stash.table,INSERT_ROWS = 1
  ENDIF
  
  IF (ev.ID eq stash.Button_DeleteRow) THEN BEGIN
    
    IF (hasSelection eq 0) THEN BEGIN
      information='已无数据，不能再删除！'
      info=dialog_message(/info,title='提示',information) 
    ENDIF ELSE BEGIN
      WIDGET_CONTROL, stash.table,DELETE_ROWS = 1;,/USE_TABLE_SELECT
    ENDELSE
  ENDIF

  ; If the event came from the 'Quit' button, close the application.
  IF (ev.ID eq stash.b_quit) THEN WIDGET_CONTROL, ev.TOP, /DESTROY

END


pro tabletest

  ;ab=['a','b']

  data=['a','b']
  d1=['name1','cname1']

  data=[[data],[d1]]
  d2=['name2','cname2']
  data=[[data],[d2]]
  
  ;data = [data,['name1','']]
  ;data = [data,['name2','']]
  ;data=data[1:*]

labels = ['ROIName', 'Color']


  ; Create the widget hierarchy.
  ;大base--纵向排列
  base = WIDGET_BASE(/COLUMN)
  ;子base--横向排列
  Drawbase = WIDGET_BASE(base, /ROW)
  
  ;图像窗口
  draw = WIDGET_DRAW(Drawbase, XSIZE=250, YSIZE=250)
  
  ButtonBase = WIDGET_BASE(base, /ROW)
  Button_AddRow = WIDGET_BUTTON(ButtonBase, VALUE='新增ROI')
  Button_DeleteRow = WIDGET_BUTTON(ButtonBase, VALUE='删除ROI')
  b_quit = WIDGET_BUTTON(ButtonBase, VALUE='退出')
  
  ;表格
  table = WIDGET_TABLE(base, VALUE=data,/EDITABLE, $
    ROW_LABELS='',COLUMN_LABELS=labels, COLUMN_WIDTHS=maxwidth, SCR_YSIZE=300,/ALL_EVENTS)

  ; Realize the widgets.
  WIDGET_CONTROL, base, /REALIZE

  ; Get the widget ID of the draw widget.
  WIDGET_CONTROL, draw, GET_VALUE=drawID

  ; Create an anonymous structure to hold widget IDs. This
  ; structure becomes the user value of the top-level base
  ; widget.
  stash = {draw:drawID, table:table,Button_AddRow:Button_AddRow, $
           Button_DeleteRow:Button_DeleteRow, b_quit:b_quit}
  
  ; Set the user value of the top-level base and call XMANAGER
  ; to manage everything.
  WIDGET_CONTROL, base, SET_UVALUE=stash
  XMANAGER, 'tabletest', base

end