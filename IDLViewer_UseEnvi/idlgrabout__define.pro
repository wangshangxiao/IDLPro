PRO ABOUT_EVENT, ev

  WIDGET_CONTROL, ev.TOP, get_uvalue = parent
  
  CASE TAG_NAMES(ev, /structure_name) OF
    
    'WIDGET_DRAW': BEGIN      
      ;链接至网页    
      IF ev.X GE 32 AND ev.X LE 178 AND ev.Y GE 110 AND ev.Y LE 125 THEN BEGIN
        DEVICE, cursor_standard = 32649
        IF ev.PRESS EQ 1 THEN SPAWN, 'start http://www.esrichina.com.cn', /nowait, /hide
      ENDIF ELSE IF ev.X GE 32 AND ev.X LE 233 AND ev.Y GE 65 AND ev.Y LE 81 THEN BEGIN
        DEVICE, cursor_standard = 32649
        IF ev.PRESS EQ 1 THEN  SPAWN, 'start http://blog.sina.com.cn/enviidl', /nowait, /hide
      ENDIF ELSE BEGIN
        DEVICE, cursor_standard = 32512
      ENDELSE
    END
    ELSE: RETURN
  ENDCASE
  
END


PRO IDLgrAbout::SetProperty, id = id

  IF N_ELEMENTS(id) NE 0 THEN self.ID = id
  
END



PRO IDLgrAbout::Create, parent = parent,    $
    xsize = xsize,                          $
    ysize = ysize,                          $
    title = title,                          $
    logoFile = logoFile
    
  ;构建关于界面
  About = WIDGET_BASE(group_leader = self.PARENT,  $
    tlb_frame_attr = 1,       $
    /floating,                $
    title = self.TITLE,       $
    /row, /modal)
    
  wDraw = WIDGET_DRAW(About,  $
    xsize = self.XSIZE,       $
    ysize = self.YSIZE,       $
    uname = 'About',          $
    /button_event,            $
    /MOTION_EVENTS)
  ;    event_pro = 'LinkWebsite')
    
  WIDGET_CONTROL, About, /realize
  
  WIDGET_CONTROL, wDraw, get_value = WinDraw
  WSET, WinDraw
  
  data = READ_IMAGE(self.LOGOFILE)
  TV, data, /true
  
  self.SETPROPERTY, id = About
  
  WIDGET_CONTROL, About, set_uvalue = self.PARENT
  
  XMANAGER, 'about', about, /no_block
  
END



FUNCTION IDLgrAbout::INIT, parent = parent,  $
    xsize = xsize,  $
    ysize = ysize,  $
    title = title,  $
    logoFile = logoFile
  
  ;初始化  
    
  self.PARENT = parent
  IF N_ELEMENTS(xsize) NE 0 THEN self.XSIZE = xsize
  IF N_ELEMENTS(ysize) NE 0 THEN self.YSIZE = ysize
  IF N_ELEMENTS(title) NE 0 THEN self.TITLE = title
  IF N_ELEMENTS(logoFile) NE 0 THEN self.LOGOFILE = logoFile
  
  self.CREATE
  
  RETURN, 1
  
END


PRO IDLGRABOUT__DEFINE
  
  ;关于界面 - 自定义对象
  
  structure = {IDLgrAbout,  $
    id:0,                   $
    logoFile:'',            $
    parent:0L,              $
    xsize:0,                $
    ysize:0,                $
    title:''}
END