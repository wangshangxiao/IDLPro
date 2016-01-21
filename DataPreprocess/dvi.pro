
;*****************************************************************************************************

;NDVI���㺯��

;*****************************************************************************************************


PRO dvi,infile,outfile,NIRIndex,RedIndex


  envi, /restore_base_save_files  
  envi_batch_init, log_file='batch.txt' 

  envi_open_file, infile, r_fid=fid 
  if (fid eq -1) then begin  
      envi_open_data_file, infile, r_fid=fid
         if (fid eq -1) then begin  
            envi_batch_exit  
      return  
    endif
  endif   
;  
  envi_file_query, fid, dims=dims,NL=nl,NS=ns,NB=nb,DATA_TYPE=data_type,$
                       XSTART=xstart,YSTART=ystart

  map_info = CALL_FUNCTION( 'envi_get_map_info',fid = fid)
  
;  
  ;�򿪴洢�ļ�
  openw,unit,outfile,/GET_LUN
  
  ;���÷ֿ�
  pos  = [3,2]
  
  tile_nir=CALL_FUNCTION( 'ENVI_INIT_TILE',fid,NIRIndex,INTERLEAVE=0,NUM_TILES=num_tiles,XS=dims[1],xe=dims[2],$
                                              YS=dims[3],YE=dims[4])
  tile_red=CALL_FUNCTION( 'ENVI_INIT_TILE',fid,RedIndex,INTERLEAVE=0,NUM_TILES=num_tiles,XS=dims[1],xe=dims[2],$
                                              YS=dims[3],YE=dims[4])
   progressbar=obj_new('PROGRESSBAR',$
                       text='Process',$
                       title='compute (ndvi)',$
                       color=[0,0,255],$
                       xsize=400,$
                       ysize=26 $
                        )

   progressbar->start
  
  ;�Էֿ���ݽ��д���
  for i=0L,NUM_TILES-1 do begin
  
  PROGRESSBAR->Update,fix((i+1)*100/NUM_TILES),text='completed' +strtrim(fix((i+1)*100/NUM_TILES),2)+'%'
  
  DN_NIR = CALL_FUNCTION('ENVI_GET_TILE',tile_nir, i, BAND_INDEX = NIRIndex)
  DN_RED = CALL_FUNCTION('ENVI_GET_TILE',tile_red, i, BAND_INDEX = RedIndex)
  
  
  tile_size=SIZE(DN_NIR)
  
  n=FLTARR(tile_size(1),tile_size(2))
  
  a=where((DN_NIR*1.0+DN_RED*1.0) gt 0)            ;a�����з���ģ������ֵ
  
  if(a(0) ne -1) then begin  
  n(a)= DN_NIR(a)*1.0-DN_RED(a)*1.0
  endif
  
  b=where((DN_NIR*1.0+DN_RED*1.0) eq 0)            ;;b��������ģ������ֵ
  
  if(b(0) ne -1) then begin
     n(b)=0
  endif
  

  
  
  
  WRITEU,unit,n
  
  ENDFOR
  
  ;�ͷ��豸�ļ���Ԫ��
  FREE_LUN,unit
  
  out_name=STRSPLIT(outfile,'.',/EXTRACT)
  
  ;д��ENVIͷ�ļ�
  ENVI_SETUP_HEAD,FNAME=out_name[0],BNAMES='NDVI', NS=ns,NL=nl,NB=1,$
                    DATA_TYPE=4,OFFSET=0,INTERLEAVE=0,$
                    XSTART=xstart+dims[1],YSTART=ystart+dims[3],$
                    DESCRIP='NDVI DATA',/WRITE,/OPEN,MAP_INFO=map_info
                    
  ;�ͷŷֿ���Դ
  ENVI_TILE_DONE,tile_nir
  ENVI_TILE_DONE,tile_red
                    
  
  envi_file_mng, id=fid, /remove
  
  PROGRESSBAR->Destroy
  OBJ_DESTROY,progressbar
  ;PROGRESSBAR->CLEANUP
 
  
  envi_batch_exit 

END



;*****************************************************************************************************

;�����ǽ������

;*****************************************************************************************************





;*****************************************************************************************************
PRO PROGRESSBAR_Cleanup, tlb

   Widget_Control, tlb, Get_UValue=self
   Obj_Destroy, self

END



;**********************************************************************************************8


pro Progressbar_event,ev

; This is the event handler for the program. It simply sets the CANCEL
; flag if this is a button event.

   Widget_Control, event.top, Get_UValue=self
   thisEvent = Tag_Names(event, /Structure_Name)
   IF thisEvent EQ 'WIDGET_BUTTON' THEN self -> SetProperty, Cancel=1

end








;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::Destroy
;
; PURPOSE:
;
;       Destroys both the widget hierarchy and the object.
;
; SYNTAX:
;
;      progressbar -> Destroy
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       None.
;
;*****************************************************************************************************
PRO PROGRESSBAR::Destroy

; This method takes the widget off the display and destroys the self object.

      ; Restore the old !P.Color.
  device,decomposed=1
   TVLCT, self.r, self.g, self.b, (255 < self.oldcolor)

      ; Destroy the object.

  if widget_info(self.tlb,/valid_id) then $
    Widget_Control, self.tlb, Destroy=1
   Obj_Destroy, self

END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::GETPROPERTY
;
; PURPOSE:
;
;       Allows user to get various progress bar properties.
;
; SYNTAX:
;
;       progressbar -> GetProperty, Color=currentColor
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       COLOR: The name of the color for the progress bar.
;
;       FAST_LOOP: The value of the current "fast loop" flag.
;
;       TEXT:  The textual message that goes above the progress bar.
;
;       TLB_ID:  The widget ID of the progressbar window's top-level base.
;
;*****************************************************************************************************
PRO PROGRESSBAR::GetProperty,$
                 COLOR=color,$
                 TEXT=text, $
                 TLB_ID=tlb_id


   IF Arg_Present(color) THEN color = self.color

   IF Arg_Present(text) THEN BEGIN

   IF Widget_Info(self.labelID, /Valid_ID) THEN BEGIN
         Widget_Control, self.labelID, Get_Value=text
         text = text[0]
      ENDIF ELSE text = ""
  ENDIF
  IF Arg_Present(tlb_id) THEN tlb_id = self.tlb

END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::SETPROPERTY
;
; PURPOSE:
;
;       Allows user to set various progress bar properties.
;
; SYNTAX:
;
;       progressbar -> SetProperty, Color='yellow'
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       CANCEL:        Set this keyword to set the cancelFlag field in the self object.
;
;       COLOR:         The name of the color for the progress bar. By default: "red".
;
;       Possible color names are those defined by FSC_COLOR:
;
;                Almond   Antique White      Aquamarine           Beige          Bisque           Black
;                  Blue     Blue Violet           Brown       Burlywood        Charcoal      Chartreuse
;             Chocolate           Coral        Cornsilk            Cyan  Dark Goldenrod       Dark Gray
;            Dark Green      Dark Khaki     Dark Orchid     Dark Salmon       Deep Pink     Dodger Blue
;             Firebrick    Forest Green            Gold       Goldenrod            Gray           Green
;          Green Yellow        Honeydew        Hot Pink      Indian Red           Ivory           Khaki
;              Lavender      Lawn Green     Light Coral      Light Cyan      Light Gray    Light Salmon
;          Light Yellow      Lime Green           Linen         Magenta          Maroon     Medium Gray
;         Medium Orchid        Moccasin            Navy           Olive      Olive Drab          Orange
;            Orange Red          Orchid  Pale Goldenrod      Pale Green          Papaya            Peru
;                  Pink            Plum     Powder Blue          Purple             Red            Rose
;            Rosy Brown      Royal Blue    Saddle Brown          Salmon     Sandy Brown       Sea Green
;              Seashell          Sienna        Sky Blue      Slate Gray            Snow    Spring Green
;            Steel Blue             Tan         Thistle          Tomato       Turquoise          Violet
;            Violet Red           Wheat           White          Yellow
;
;       FAST_LOOP:     Set this keyword to one to allow "fast looping". Set to 0 to turn it off.
;
;       TEXT:          The textual message that goes above the progress bar.
;
;       TITLE:         The title of the progress bar window. By default: "Progress Bar".
;
;       XOFFSET:       The X offset for the progress bar when it appears.
;
;       YOFFSET:       The Y offset of the progress bar when it appears.
;
;*****************************************************************************************************
PRO PROGRESSBAR::SetProperty, $
   CANCEL=cancel, $
   COLOR=color, $
   TEXT=text, $
   TITLE=title, $
   XOFFSET=xoffset, $
   YOFFSET=yoffset




   IF N_Elements(cancel) NE 0 THEN self.cancelFlag = Keyword_Set(cancel)

   IF N_Elements(color) NE 0 THEN BEGIN
      self.color = color

   ENDIF
   IF N_Elements(text) NE 0 THEN BEGIN
      self.text = text
      IF Widget_Info(self.labelID, /Valid_ID) THEN Widget_Control, self.labelID, Set_Value=text
   ENDIF
   IF N_Elements(title) NE 0 THEN BEGIN
      self.title = title
      IF Widget_Info(self.tlb, /Valid_ID) THEN Widget_Control, self.tlb, TLB_Set_Title=title
   ENDIF
   IF (N_Elements(xoffset) NE 0) OR (N_Elements(yoffset) NE 0) THEN BEGIN
      Widget_Control, self.tlb, XOffset=xoffset, YOffset=yoffset
   ENDIF
END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::START
;
; PURPOSE:
;
;       Puts the progress bar on the display.
;
; SYNTAX:
;
;       progressbar -> Start
;
; ARGUMENTS:
;
;       initialPercent -- The initial percentage that should be on the progress bar. By default, 0.
;
; KEYWORDS:
;
;       None.
;
;*****************************************************************************************************
PRO PROGRESSBAR::Start, initialPercent



      ; Save the old !P.Color.

   self.oldcolor = !P.Color
   TVLCT, r, g, b, /Get
   self.r = r[self.oldcolor < 255]
   self.g = g[self.oldcolor < 255]
   self.b = b[self.oldcolor < 255]


      ; Find the window index number of any open display window.

   thisWindow = !D.Window

      ; Realize the widget.

   Widget_Control, self.tlb, /Realize, Map=1

      ; Get the window index number of the draw widget.

   Widget_Control, self.drawID, Get_Value=wid
   self.wid = wid
   device,decomposed=0
   r[0]=255
   g[0]=255
   b[0]=255
   r[255]=self.color[0]
   g[255]=self.color[1]
   b[255]=self.color[2]

   self.colorindex=255
   tvlct,r,g,b
   wset,wid
   erase,color=0
;   device,decomposed=1

      ; Back to the open display window.

   IF thisWindow GE 0 THEN WSet, thisWindow

      ; Do you need a starting update?

   IF N_Elements(initialPercent) NE 0 THEN self -> Update, initialPercent

END



;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::Update
;
; PURPOSE:
;
;       Updates the progress bar
;
; SYNTAX:
;
;       progressbar -> Update, percent
;
; ARGUMENTS:
;
;       percent -- A value between 0 and 100 that represents the percentage of the progress
;                  bar that should be colored.
;
; KEYWORDS:
;
;       Text -- The message text which will be displayed above the
;               bar.
;
;       Title -- The title of the progressbar window to be updated
;
;*****************************************************************************************************
PRO PROGRESSBAR::Update, percent, Text=theText, Title=theTitle

; This method updates the display. PERCENT should be a value between 0 and 100.
; The text will be substituted for the message text.



   percent = 0 > percent < 100

      ; Update the progress box.

   thisWindow = !D.Window
   WSet, self.wid
   x1 = 0
   y1 = 0
   x2 = Fix(self.xsize  * (percent/100.0))
   y2 = self.ysize
   IF N_Elements(theText) NE 0 THEN Widget_Control, self.labelID, Set_Value=theText
   IF N_Elements(theTitle) NE 0 THEN BEGIN
       self.title = theTitle
       IF Widget_Info(self.tlb, /Valid_ID) THEN Widget_Control, self.tlb, TLB_Set_Title=theTitle
   ENDIF

      Polyfill, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Device, Color=self.colorIndex

   IF thisWindow GE 0 AND thisWindow NE self.wid THEN WSet, thisWindow

END


;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::CLEANUP
;
; PURPOSE:
;
;       Nothing to do in this cleanup routine.
;
;*****************************************************************************************************
PRO PROGRESSBAR::CLEANUP
  self->destroy
END


;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR::INIT
;
; PURPOSE:
;
;       Implements a progress bar widget functionality.
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       None.
;
;
;       GROUP_LEADER:  The group leader for the progress bar.
;
;       NOCANCEL:      Set this keyword to eliminate the CANCEL button from the progres bar.
;
;       PERCENT:       The initial percent on the progress bar. Used only if the START keyword is
;                      also set.
;
;       START:         Set this keyword if you wish to call the START method immediately upon initialization.
;
;       TEXT:          The textual message that goes above the progress bar. By default:
;                      "Operation in progress..."
;
;       TITLE:         The title of the progress bar window. By default: "Progress Bar".
;
;       XOFFSET:       The X offset for the progress bar when it appears.
;
;       XSIZE:         The X size of the progress bar itself. By default, 150 pixels.
;
;       YOFFSET:       The Y offset of the progress bar when it appears.
;
;       YSIZE:         The Y size of the progress bar itself. By default, 10 pixels.
;
;*****************************************************************************************************
FUNCTION PROGRESSBAR::INIT, $
              COLOR=color, $               ; ��������ɫ.
              GROUP_LEADER=group_leader, $ ; The identifier of the group leader widget.
              PERCENT=percent, $           ; �����ĳ�ʼֵ (Only recognized if START used.)
              START=start, $               ; Set this keyword if you wish to call the START method from INIT.
              TEXT=text, $                 ; The message text to be written over the progress bar.
              TITLE=title, $               ; The title of the top-level base widget.
              XOFFSET=xoffset, $           ; The X offset of the progress bar.
              XSIZE=xsize, $               ; The X size of the progress bar.
              YOFFSET=yoffset, $           ; The Y offset of the progress bar.
              YSIZE=ysize                  ; The Y size of the progress bar.



   IF N_Elements(color) EQ 0 THEN self.color = [255,255,0] ELSE self.color = color


   IF N_Elements(text) EQ 0 THEN text='������'
   IF N_Elements(title) EQ 0 THEN title = "�����"
   IF N_Elements(xsize) EQ 0 THEN self.xsize = 150 ELSE self.xsize = xsize
   IF N_Elements(ysize) EQ 0 THEN self.ysize = 10 ELSE self.ysize = ysize

   ; ����������.

   self.tlb = Widget_Base(Title=title, $
                          Column=1,$
                           Base_Align_Center=1, $
                             Map=0,$
                             Group_Leader=group_leader, $
                             TLB_FRAME_ATTR=11)

   self.labelID = Widget_Label(self.tlb,$
                               Value=text,$
                               /Dynamic_Resize)
   self.drawID = Widget_Draw(self.tlb,$
                              XSize=self.xsize,$
                              YSize=self.ysize)
   Widget_Control, self.tlb, Set_UValue=self




   ; Center the top-level base if offsets are not used. Othersize, use offsets.
   IF (N_Elements(xoffset) NE 0) OR (N_Elements(yoffset) NE 0) THEN BEGIN
      IF N_Elements(xoffset) EQ 0 THEN xoffset=0
      IF N_Elements(yoffset) EQ 0 THEN yoffset=0
      Widget_Control, self.tlb, XOFFSET=xoffset, YOFFSET=yoffset

   ENDIF ELSE BEGIN
      Device, Get_Screen_Size=screenSize
      IF screenSize[0] GT 2000 THEN screenSize[0] = screenSize[0]/2 ; Dual monitors.
      xCenter = screenSize(0) / 2
      yCenter = screenSize(1) / 2

      geom = Widget_Info(self.tlb, /Geometry)
      xHalfSize = geom.Scr_XSize / 2
      yHalfSize = geom.Scr_YSize / 2

      Widget_Control, self.tlb, XOffset = xCenter-xHalfSize, $
         YOffset = yCenter-yHalfSize
   ENDELSE

   ; Start it up?
   IF Keyword_Set(start) THEN self -> Start, percent

   RETURN, 1
END


;*****************************************************************************************************
;
; NAME:
;       PROGRESSBAR CLASS DEFINITION
;
; PURPOSE:
;
;       This is the PROGRESSBAR object's structure definition code.
;
;*****************************************************************************************************
PRO PROGRESSBAR__DEFINE

   struct = { PROGRESSBAR, $      ; The object class name.



              color: [0,0,0], $        ; The name of the color of the progress bar.
              colorindex:0L,$
              drawID: 0L, $       ; The identifier of the draw widget.
;
              labelID: 0L, $      ; The identifier of the label widget.
              oldcolor: 0L, $     ; The color index of !P.Color.
              r: 0B, $            ; The r value of !P.Color.
              g: 0B, $            ; The g value of !P.Color.
              b: 0B, $            ; The b value of !P.Color.
              text: "", $         ; The text message to be written over the progress bar.
              title: "", $        ; The title of the top-level base widget.
              tlb: 0L, $          ; The identifier of the top-level base.
              wid: 0L, $          ; The window index number of the draw widget.
              xsize: 0L, $        ; The XSize of the progress bar.
              ysize: 0L $         ; The YSize of the progress bar.
            }
END







