;+
; NAME:
; 
;   IOSocImageViewer::handle_zoom_event
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
;   Obj->[IOScocImageViewer::]handle_zoom_event, Event
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
pro IOSocImageViewer::handle_zoom_event, event
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if ~self.pImage then return
  
  self.oImage_Zoombox_Polygon->GetProperty, data=ZoomCoords

  ZoomCoords[1,*] = self.ImageInfo.nl - ZoomCoords[1,*] - 1
        
  S = min(ZoomCoords[0,*]) + (max(ZoomCoords[0,*]) - min(ZoomCoords[0,*])) /self.Zoom_Dims[0] * event.x
  L = self.ImageInfo.nl - (min(ZoomCoords[1,*]) + (max(ZoomCoords[1,*]) - min(ZoomCoords[1,*])) / $
            self.Zoom_Dims[1] * (self.Zoom_Dims[1]-event.y))

  self.Actual_Pixel_Location = [S, L]

  if obj_valid(self.oInform_user) then self->Inform_User, event

  case event.type of
  0: begin  ; button press
  
    case event.press of 
    1: begin  ; left
      if Event.y lt 18 and Event.x lt 54 then begin
        if Event.x lt 18 then begin 
          self.Zoom_Requested_Zfac--
          if self.Zoom_Requested_Zfac le 0 then self.Zoom_Requested_Zfac = 1
        endif 
    
        if Event.x gt 18 and Event.x lt 36 then self.Zoom_Requested_Zfac++
        if Event.x gt 36 and Event.x lt 54 then self.ZoomCrossHair =~ self.ZoomCrossHair  
      endif else begin      
    
        self.Image_Pixel_Location = [S, L]
      endelse
      
      self->Update_View  
      return
    end
    
    else:
    endcase
  end
  
  1: begin  ; button release
  end
  
  2: begin  ; motion
  end
  else:
  endcase
  

end