;+
; NAME:
; 
;   IOSocProfileViewer::handle_profile_event
;
; PURPOSE:
; 
;   This private procedure method is called to handle events that occured in the profile window.
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
;   Obj->[IOSocProfileViewer::]Handle_Profile_Event, Event
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
pro IOSocProfileViewer::handle_profile_event, event
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if ~ptr_valid(self.pX) then return

  ;-- Calculate the X value
  self.oPlot_1st_Profile->GetProperty, XRANGE = xr, YRANGE = yr
  self.oPlot_View->GetProperty, viewplane_rect=V

  Wl_Select = xr[0] + (xr[1] - xr[0]) * V[0] + (xr[1] - xr[0]) * V[2]/self.plot_Dims[0]*event.x
  Tmp = min(abs(*self.pX - Wl_Select), i) 
  WL_Selected = (*self.pX)[i]
  
  ;-- Handle the event
  case event.type of
  0: begin  ; button press
    
    if Event.clicks eq 2 then begin ;-- Handle profile zoom
    
      self.Plot_Zoom = ~self.Plot_Zoom
      
      if self.Plot_Zoom eq 1 then self.Plot_View_X_range = self.Plot_Set_X_range else $
        self.Plot_View_X_range = [min(*self.pX), max(*self.pX)]
        
        self->Set_Plot_X_range
        self->View_Profiles
      return
    endif
  
    d = ((*self.pX)[1]-(*self.pX)[0])*3
  
    if WL_Selected ge self.Plot_Set_X_range[0]-d and $
      WL_Selected le self.Plot_Set_X_range[0]+d then begin
      self.Plot_range_selected = 1
    endif else begin
      if WL_Selected ge self.Plot_Set_X_range[1]-d and $
        WL_Selected le self.Plot_Set_X_range[1]+d then begin
        self.Plot_range_selected = 2
      endif
    endelse
  end
  
  1: begin ; release

    if self.Plot_range_selected eq 1 then self.Plot_Set_X_range[0] = WL_Selected < self.Plot_Set_X_range[1]
    if self.Plot_range_selected eq 2 then self.Plot_Set_X_range[1] = WL_Selected > self.Plot_Set_X_range[0]

    self.Plot_range_selected = 0

    self->Set_Plot_X_range
  end
  
  2: begin  ; motion

    if self.Plot_range_selected eq 0 then return

    if self.Plot_range_selected eq 1 then self.Plot_Set_X_range[0] = WL_Selected < self.Plot_Set_X_range[1]
    if self.Plot_range_selected eq 2 then self.Plot_Set_X_range[1] = WL_Selected > self.Plot_Set_X_range[0]

    self->Set_Plot_X_range
    self->Inform_User
  end
  
  else:
  endcase
end