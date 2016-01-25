;+
; NAME:
; 
;   EOSap_ClassEditor::ImageViewer_User_Info
;
; PURPOSE:
; 
;   This procedure method is called by the 'IOSocImageViewer' object to inform the user of the object about an 
;   event that occured in the WIDGET_DRAW images window.
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
;   Obj->[EOSap_ClassEditor::]ImageViewer_User_Info, ImageViewerInfo
;   This procedure method is called by the 'IOSocImageViewer' object whenever an image WIDGET_DRAW event
;   occurs.
;
; ARGUMENTS 
; 
;   ImageViewerInfo:      This data structure will hold the actual pixel location.
; 
; KEYWORDS
; 
;   None
;   
; KNOWN ISSUES:
; 
;   None.
;   
; MODIFICATION HISTORY:
; 
; - Written by Luc Bertels, June 2010.
; - Updated for the Colibri release, February 2013.
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
pro EOSap_ClassEditor::ImageViewer_User_Info, ImageViewerInfo
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if ~keyword_set(ImageViewerInfo.Event) then return

  ;-- Retrieve actual image button status
  self.Image_button = self.Image_button or ImageViewerInfo.Event.Press
  self.Image_button = self.Image_button xor ImageViewerInfo.Event.Release

  ;-- Check if right mouse button pressed
  if (self.Image_button and 4) eq 4 then begin

    ;-- Check if right mouse button pressed once or in motion and process open for new vertices
    if (ImageViewerInfo.Event.Clicks eq 1 or ImageViewerInfo.Event.Type eq 2) and (self.Roi_input eq 0) then begin
     
      ;-- Define new vertex
      Actual_Pixel_Location     = ImageViewerInfo.Actual_Pixel_Location
      Actual_Pixel_Location[1]  = self.nl - Actual_Pixel_Location[1] - 1

      self.oImageViewer->PolyLine_SetProperty, AddVertices=Actual_Pixel_Location , Color=[255,0,0]
      
      if ~ptr_valid(self.pRoi_x) then self.pRoi_x = ptr_new(ImageViewerInfo.Actual_Pixel_Location[0]) else $
        *self.pRoi_x = [*self.pRoi_x, ImageViewerInfo.Actual_Pixel_Location[0]]
      if ~ptr_valid(self.pRoi_y) then self.pRoi_y = ptr_new(ImageViewerInfo.Actual_Pixel_Location[1]) else $
        *self.pRoi_y = [*self.pRoi_y, ImageViewerInfo.Actual_Pixel_Location[1]]
    endif
    
    ;-- Check if right mouse button double clicked and process open for new vertices
    if (ImageViewerInfo.Event.Clicks eq 2) and (self.Roi_input eq 0) then begin
      
      ;-- Check if enough points were specified
      if n_elements(*self.pRoi_x) eq 1 then begin
      
        if ptr_valid(self.pRoi_x) then ptr_free, self.pRoi_x
        if ptr_valid(self.pRoi_y) then ptr_free, self.pRoi_y
        return
      endif
      
      ;-- Close process for new vertex acceptance and close polygon
      self.Roi_input = 1
      self.oImageViewer->PolyLine_SetProperty, /CLOSE

      if ptr_valid(self.pRoi_x) then *self.pRoi_x = [*self.pRoi_x, (*self.pRoi_x)[0]]
      if ptr_valid(self.pRoi_y) then *self.pRoi_y = [*self.pRoi_y, (*self.pRoi_y)[0]]      
     
      ;-- Show the classes within the defined polygon
      self->Show_Actual_Classes
      return
    endif
    
    ;-- If left mouse button double clicked second time than clear polygon
    if (ImageViewerInfo.Event.Clicks eq 2) and (self.Roi_input eq 1) then begin
    
      if widget_info(self.WB_From_Group_Leader, /valid_id) then $
        widget_control, self.WB_From_Group_Leader, /destroy
        
        widget_control, widget_info(self.wWidget, find_by_uname='tChanged_From'), $
          set_value = ''
          
      ;-- Open process for new vertex acceptance and clear the ROI if created
      self.Roi_input = 0
      self.oImageViewer->PolyLine_SetProperty, /CLEAR
    
      if ptr_valid(self.pRoi_x)               then ptr_free, self.pRoi_x
      if ptr_valid(self.pRoi_y)               then ptr_free, self.pRoi_y
      if ptr_valid(self.pActual_Class_Ids)    then ptr_free,self.pActual_Class_Ids
      if ptr_valid(self.pPolygonData)         then ptr_free,self.pPolygonData
      if ptr_valid(self.pPolygonIx)           then ptr_free,self.pPolygonIx
    endif    
  endif
end