;+
; NAME:
; 
;   IOSocImageViewer::init
;
; PURPOSE:
; 
;   This method function creates and initializes an instance of the class 'IOSocImageViewer'. The obtained 
;   object is used to display images in a 'SCROLL', 'WINDOW' and 'ZOOM' window. Any event occuring in these 
;   windows can be passed back to the caller.
;
; AUTHOR:
; 
;   Luc Bertels
;   TAP - Teledetection and Earth Observation Processes 
;   VITO - Flemish Institute for Technological Research
;   Boeretang 200 
;   B-2400 Mol, Belgium 
;   http://www.vito.be
;
; CALLING SEQUENCE:
;   
;   Result = OBJ_NEW('IOSocImageViewer', wB_GUI [,OSTATUS=object reference] [,OUSER=object reference])
;                                     
; ARGUMENTS     
; 
;   wB_GUI:         Base widget of the GUI using the IOSocImageViewer class. In the definition of the GUI a 
;                   base widget named "WB_ImageViewer" should be defined with a sufficient XSIZE and YSIZE to
;                   hold the three draw widgets. 
;        
; KEYWORDS     
; 
;   OSTATUS:        An object reference to an existing status window.
;   OUSER:          Set this keyword to the object reference of the caller of this class which need to be 
;                   informed about any event occuring in the created windows. The caller should have a 
;                   procedure defined with the name: 'ImageViewer_User_Info'. For more info see the method
;                   'Inform_User'.
;
; RETURN VALUE:
; 
;   The OBJ_NEW function returns an object reference to the newly-created object.
;
; KNOWN ISSUES:
; 
;   None
;
; MODIFICATION HISTORY:
; 
;   - Written by Luc Bertels, December 2012.
;
;###########################################################################
;
; LICENCE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright © 2012 Luc Bertels, Flemish Institute for Technological Research.
;
; This software is provided "as-is", without any express or implied warranty. 
; Except in case of wilful misconduct or gross negligence the authors will 
; not be held liable for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; 4. This licence is subject to Belgian Law.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################
;
;------------------------------------------------------------------------------------------------------------
function IOSocImageViewer::init,$ 
                  wB_GUI, $
                  oStatus     = oStatus, $
                  oUser       = oUser
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Module    = 'IOSocImageViewer::init'
  
  if obj_valid(oUser) then self.oInform_user = oUser else $
    if obj_valid(self.oInform_user) then obj_destroy, self.oInform_user
    
  ;----------------------------------------------------------
  ;--- If no status object is present, create the default ---
  ;----------------------------------------------------------

   oStatus = IOScc_Init(oStatus)                      
   self.oStatus = oStatus
 
  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------
  
  WB_ImageViewer = widget_info(wB_GUI, find_by_uname='WB_ImageViewer')
  
  if ~widget_info(wB_ImageViewer, /valid_id)  then Argument = 'wB_ImageViewer'  
  
  if keyword_set(Argument) then begin
    self.oStatus->Log_Status, 'Error', 'Invalid base widget: '+Argument, Module, /Log       
    return, 0
  endif
  
  ;------------------------------------------
  ;--- Initialization of the draw widgets ---
  ;------------------------------------------
     
  widget_control, WB_ImageViewer, set_uvalue=self
  
  self.Image_Requested_Zfac  = 2.
  self.Zoom_Requested_Zfac   = 3.     
  
  Geometry = widget_info(wB_ImageViewer, /Geometry)
  
  Wd_image_yoffset  = 5
  Wd_image_xoffset  = 5
  MaxSize           = Geometry.ysize > Geometry.xsize
  MinSize           = Geometry.ysize < Geometry.xsize
  D5p8_Size         = (MaxSize - 20) / 8 *5
  D3p8_Size         = Maxsize - D5p8_Size - 20
    
  if Geometry.ysize gt Geometry.xsize then begin
  ; vertical oriented
  
    Wd_image_xsize      = MinSize - 15
    Wd_image_ysize      = D5p8_Size  
    Wd_scroll_xoffset   = 5
    Wd_scroll_yoffset   = Wd_image_ysize + 10
    Wd_scroll_xsize     = (MinSize - 15) / 2
    Wd_scroll_ysize     = D3p8_Size
    Wd_zoom_xoffset     = Wd_Scroll_xsize + 10
    Wd_zoom_yoffset     = Wd_image_ysize + 10
    Wd_zoom_xsize       = Wd_scroll_xsize
    Wd_zoom_ysize       = Wd_scroll_ysize
  endif else begin
  ; horizontal oriented

    Wd_image_xsize      = D5p8_Size
    Wd_image_ysize      = MinSize - 15    
    Wd_scroll_xoffset   = Wd_image_xsize + 10 
    Wd_scroll_yoffset   = 5  
    Wd_scroll_xsize     = D3p8_Size 
    Wd_scroll_ysize     = (MinSize - 15) / 2
    Wd_zoom_xoffset     = Wd_image_xsize + 10
    Wd_zoom_yoffset     = Wd_Scroll_ysize + 10
    Wd_zoom_xsize       = Wd_scroll_xsize
    Wd_zoom_ysize       = Wd_scroll_ysize    
  endelse
  
  Wd_Image = Widget_Draw(WB_ImageViewer, UNAME='Wd_Image' ,XOFFSET=Wd_image_xoffset  $
      ,YOFFSET=Wd_image_yoffset ,SCR_XSIZE=Wd_image_xsize ,SCR_YSIZE=Wd_image_ysize ,RETAIN=2  $
      ,GRAPHICS_LEVEL=2 ,/BUTTON_EVENTS ,/MOTION_EVENTS)

  
  Wd_Scroll = Widget_Draw(WB_ImageViewer, UNAME='Wd_Scroll'  ,XOFFSET=Wd_scroll_xoffset $
      ,YOFFSET=Wd_scroll_yoffset ,SCR_XSIZE=Wd_scroll_xsize ,SCR_YSIZE=Wd_scroll_ysize ,RETAIN=2  $
      ,GRAPHICS_LEVEL=2 ,/BUTTON_EVENTS ,/MOTION_EVENTS)

  
  Wd_Zoom = Widget_Draw(WB_ImageViewer, UNAME='Wd_Zoom' ,XOFFSET=Wd_zoom_xoffset  $
      ,YOFFSET=Wd_zoom_yoffset ,SCR_XSIZE=Wd_zoom_xsize ,SCR_YSIZE=Wd_zoom_ysize ,RETAIN=2  $
      ,GRAPHICS_LEVEL=2 ,/BUTTON_EVENTS ,/MOTION_EVENTS)   

  Widget_Control, /REALIZE, WB_ImageViewer

  XManager, 'WB_ImageViewer', WB_ImageViewer, /NO_BLOCK  
 
  ;---------------------
  ;--- Prepare views ---
  ;---------------------
  
  self.Scroll_dims  = [Wd_scroll_xsize, Wd_scroll_ysize]
  self.Image_dims   = [Wd_image_xsize, Wd_image_ysize]
  self.Zoom_dims    = [Wd_zoom_xsize, Wd_zoom_ysize]

  widget_control, Wd_Image,   get_value = oImage
  widget_control, Wd_Scroll,  get_value = oScroll
  widget_control, Wd_Zoom,    get_value = oZoom

  self.oScroll  = oScroll
  self.oImage   = oImage
  self.oZoom    = oZoom
  
  self.oPalette                     = obj_new('IDLgrPalette')
  self.oScroll_View                 = obj_new('IDLgrView')
  self.oScroll_Model                = obj_new('IDLgrModel')
  self.oScroll_Image                = obj_new('IDLgrImage', palette=self.oPalette)
  self.oScroll_Zoombox_Polygon      = obj_new('IDLgrPolygon', Color=[255,0,0])
  self.oScroll_Vector_Polygon       = obj_new('IDLgrPolygon')
  self.oScroll_Vector_Polyline      = obj_new('IDLgrPolyline')
  self.oImage_View                  = obj_new('IDLgrView')
  self.oImage_Model                 = obj_new('IDLgrModel')
  self.oImage_Image                 = obj_new('IDLgrImage', palette=self.oPalette)
  self.oImage_Zoombox_Polygon       = obj_new('IDLgrPolygon', Color=[255,0,0])
  self.oImage_Zoom_In_Out_Polyline  = obj_new('IDLgrPolyline', Color=[255,0,0])   
  self.oImage_Vector_Polygon        = obj_new('IDLgrPolygon')
  self.oImage_Vector_Polyline       = obj_new('IDLgrPolyline')
  self.oZoom_View                   = obj_new('IDLgrView')
  self.oZoom_Model                  = obj_new('IDLgrModel')
  self.oZoom_Image                  = obj_new('IDLgrImage', palette=self.oPalette)
  self.oZoom_Zoom_In_Out_Polyline   = obj_new('IDLgrPolyline', Color=[255,0,0])
  self.oZoom_Zoom_CrossHair         = obj_new('IDLgrPolyline', Color=[255,0,0])  
  self.oZoom_Vector_Polygon         = obj_new('IDLgrPolygon')
  self.oZoom_Vector_Polyline        = obj_new('IDLgrPolyline')
  
  self.oScroll_View->Add, self.oScroll_Model
  self.oScroll_Model->Add, self.oScroll_Image
  self.oScroll_Model->Add, self.oScroll_Zoombox_Polygon 
  self.oScroll_Model->Add, self.oScroll_Vector_Polygon
  self.oScroll_Model->Add, self.oScroll_Vector_Polyline
  self.oImage_View->Add, self.oImage_Model
  self.oImage_Model->Add, self.oImage_Image
  self.oImage_Model->Add, self.oImage_Zoombox_Polygon
  self.oImage_Model->Add, self.oImage_Vector_Polygon
  self.oImage_Model->Add, self.oImage_Vector_Polyline
  self.oImage_Model->Add, self.oImage_Zoom_In_Out_Polyline 
  self.oZoom_View->Add, self.oZoom_Model
  self.oZoom_Model->Add, self.oZoom_Image
  self.oZoom_Model->Add, self.oZoom_Zoom_In_Out_Polyline  
  self.oZoom_Model->Add, self.oZoom_Zoom_CrossHair    
  self.oZoom_Model->Add, self.oZoom_Vector_Polygon
  self.oZoom_Model->Add, self.oZoom_Vector_Polyline

  self.oImage->SetProperty, dimensions=self.Image_Dims
  self.oImage_View->SetProperty, viewplane_rect=[0., 0., self.Image_Dims] 
  
  self.oZoom->SetProperty, dimensions=self.Zoom_Dims
  self.oZoom_View->SetProperty, viewplane_rect=[0., 0., self.Zoom_Dims]
   
  return, 1
end