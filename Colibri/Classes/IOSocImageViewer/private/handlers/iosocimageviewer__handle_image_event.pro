;+
; NAME:
; 
;   IOSocImageViewer::handle_image_event
;
; PURPOSE:
; 
;   This private procedure method is called to handle events that occured in the IMAGE window.
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
;   Obj->[IOScocImageViewer::]handle_image_event, Event
;
; ARGUMENTS: 
; 
;   Event:          A structure holding the event information.
; 
; KEYWORDS:
; 
;   None
; 
; RETURN VALUE:
; 
;   None 
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
pro IOSocImageViewer::handle_image_event, event
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if ~ptr_valid(self.pImage) then return
  
  if event.x lt 0 then event.x = 0
  if event.x gt self.Image_dims[0] then event.x = self.Image_dims[0]-1
  if event.y lt 0 then event.y = 0
  if event.y gt self.Image_dims[1] then event.y = self.Image_dims[1]-1
  
  self.oScroll_Zoombox_Polygon->GetProperty, data=Coords

  Coords[1,*] = self.ImageInfo.nl - Coords[1,*] - 1
  
  S = round(min(Coords[0,*]) + (max(Coords[0,*]) - min(Coords[0,*]))/self.Image_dims[0] * event.x)
  L = self.ImageInfo.nl - round((min(Coords[1,*]) + (max(Coords[1,*]) - min(Coords[1,*])) / $
            self.Image_dims[1] * (self.Image_dims[1] - event.y)))

  self.Actual_Pixel_Location = [S, L]

  if obj_valid(self.oInform_user) then self->Inform_User, event
  
  case event.type of
  0: begin  ; button press
    self.Image_button_press = self.Image_button_press or Event.press
      
    if Event.y lt 18 then begin
  
      if Event.x lt 18 then begin 
        self.Image_Requested_Zfac--
        if self.Image_Requested_Zfac le 0 then self.Image_Requested_Zfac = 1
      endif 
    
      if Event.x gt 18 and Event.x lt 36 then self.Image_Requested_Zfac++
    
      self->Update_View   
    endif
  end
  1: begin  ; button release
    self.Image_button_press = self.Image_button_press xor Event.release
  end
  2: begin  ; motion
    if self.Image_button_press and 1 then begin
     
      self.Image_Pixel_Location = [S, L]

      self->Update_View
    endif
  end
  else:
  endcase
end