;+
; NAME:
; 
;   IOSocProfileViewer::init
;
; PURPOSE:
; 
;   This method function creates and initializes an instance of the class 'IOSocProfileViewer'. The obtained 
;   object is used to create a profile window and to interactivally visualize profiles.
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
;   Result = OBJ_NEW('IOSocProfileViewer', wB_GUI [, OSTATUS=object reference] [, OUSER=object reference] 
;                       [, XTITLE=string] [, YTITLE=string])
;                                     
; ARGUMENTS     
; 
;   wB_GUI:         Base widget of the GUI using the IOSocProfileViewer class. In the definition of the GUI 
;                   a base widget named "WB_ProfileViewer" should be defined with a sufficient XSIZE and 
;                   YSIZE to hold the draw widget.
;        
; KEYWORDS     
; 
;   OSTATUS:        An object reference to an existing status window.
;   OUSER:          Set this keyword to the object reference of the caller of this class which need to be 
;                   informed about any event occuring in the created profile window. The caller should have 
;                   a procedure defined with the name: 'ProfileViewer_User_Info'. For more info see the 
;                   method 'Inform_User'.
;   XTITLE:         Set this keyword to a string holding the title to be placed on the x-axis.
;   YTITLE:         Set this keyword to a string holding the title to be placed on the y-axis.
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
function IOSocProfileViewer::init,$ 
                  wB_GUI, $
                  oStatus     = oStatus, $
                  oUser       = oUser, $
                  xTitle      = xTitle, $
                  yTitle      = yTitle
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Module    = 'IOSocProfileViewer::init'

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
  
  WB_ProfileViewer = widget_info(wB_GUI, find_by_uname='WB_ProfileViewer')
  
  if ~widget_info(WB_ProfileViewer, /valid_id)  then Argument = 'WB_ProfileViewer'  
  
  if keyword_set(Argument) then begin
    self.oStatus->Log_Status, 'Error', 'Invalid base widget: '+Argument, Module, /Log       
    return, 0
  endif
  
  if ~keyword_set(xTitle) then xTitle='x Values'
  if ~keyword_set(yTitle) then yTitle='y Values'
  
  ;------------------------------------------
  ;--- Initialization of the draw widgets ---
  ;------------------------------------------
     
  widget_control, WB_ProfileViewer, set_uvalue=self
  
  Geometry = widget_info(WB_ProfileViewer, /Geometry)
   
  Wd_image_yoffset  = 5
  Wd_image_xoffset  = 5   
   
  Wd_Profile = Widget_Draw(WB_ProfileViewer, UNAME='Wd_Profile' ,SCR_XSIZE=Geometry.xsize $
    ,SCR_YSIZE=Geometry.ysize ,RETAIN=2 ,GRAPHICS_LEVEL=2 ,/BUTTON_EVENTS ,/MOTION_EVENTS)  
      
  Widget_Control, /REALIZE, WB_ProfileViewer

  XManager, 'WB_ProfileViewer', WB_ProfileViewer, /NO_BLOCK  
 
  ;---------------------
  ;--- Prepare views ---
  ;---------------------

  widget_control, Wd_Profile,   get_value = oProfile_window
  
  self.oProfile_window            = oProfile_window  
  self.plot_Dims                  = [Geometry.xsize, Geometry.ysize]
  self.oPlot_View                 = obj_new('IDLgrView', color=[80,80,80])
  self.oPlot_Model                = obj_new('IDLgrModel')
  self.oPlot_1st_Profile          = obj_new('IDLgrPlot', color=[0,220,0])
  self.oPlot_2nd_Profile          = obj_new('IDLgrPlot', color=[220,220,0])
  self.oPlot_x_Title              = obj_new('IDLgrText', color=[255,255,255], recompute_dimensions=2)
  self.oPlot_y_Title              = obj_new('IDLgrText', color=[255,255,255], recompute_dimensions=2)
  self.oPlot_x_Axis               = obj_new('IDLgrAxis', 0, color=[255,255,255], $
                                                title=self.oPlot_x_Title)
  self.oPlot_y_Axis               = obj_new('IDLgrAxis', 1, color=[255,255,255], $
                                                title=self.oPlot_y_Title)
  self.oPlot_Min                  = obj_new('IDLgrPlot', color=[0, 255, 255])
  self.oPlot_Max                  = obj_new('IDLgrPlot', color=[0, 255, 255])
  self.oPlot_Min_text             = obj_new('IDLgrText', color=[0, 255, 255], recompute_dimensions=2, alignment=1)
  self.oPlot_Max_text             = obj_new('IDLgrText', color=[0, 255, 255], recompute_dimensions=2)
      
  self.oPlot_x_Axis->GetProperty, TICKTEXT = oXticktext
  oXticktext->SetProperty, recompute_dimensions=2

  self.oPlot_y_Axis->GetProperty, TICKTEXT = oYticktext
  oYticktext->SetProperty, recompute_dimensions=2

  self.oPlot_View->Add, self.oPlot_Model
  self.oPlot_Model->Add, self.oPlot_1st_Profile   
  self.oPlot_Model->Add, self.oPlot_2nd_Profile   
  self.oPlot_Model->Add, self.oPlot_x_Axis
  self.oPlot_Model->Add, self.oPlot_y_Axis
  self.oPlot_Model->Add, self.oPlot_Min
  self.oPlot_Model->Add, self.oPlot_Max
  self.oPlot_Model->Add, self.oPlot_Min_text
  self.oPlot_Model->Add, self.oPlot_Max_text

  self.oProfile_window->SetProperty, dimensions=self.plot_Dims

  if keyword_set(xTitle) then self.xTitle = xTitle
  if keyword_set(yTitle) then self.yTitle = yTitle

  return, 1
end      
      