;+
; NAME:
; 
;   IOSocImageViewer::handle_scroll_event
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
;   Obj->[IOScocImageViewer::]handle_scroll_event, Event
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
pro IOSocImageViewer::handle_scroll_event, event
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if ~self.pImage then return

  self.oScroll_View->GetProperty, viewplane_rect=V    

  S = round(event.x / self.Scroll_Rfac)
  L = self.ImageInfo.nl - round((V[3]-event.y) / self.Scroll_Rfac)

  self.Actual_Pixel_Location = [S, L]

  if obj_valid(self.oInform_user) then self->Inform_User, event
  
  case event.type of
  0: begin  ; button press
    self.Scroll_button_press = self.Scroll_button_press or event.press
  end
  1: begin  ; button release
    self.Scroll_button_press = self.Scroll_button_press xor event.release
  end
  2: begin  ; motion
    if self.Scroll_Button_Press and 1 then begin

      self.Scroll_Pixel_Location = [S, L] 
      
      self->Update_View
    endif
  end
  else:
  endcase
end