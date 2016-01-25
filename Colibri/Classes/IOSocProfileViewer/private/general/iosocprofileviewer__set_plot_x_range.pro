;+
; NAME:
; 
;   IOSocProfileViewer::Set_Plot_X_range
;
; PURPOSE:
; 
;   This private procedure method updates the profile window with the requested x range.
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
;   Obj->[IOScocProfileViewer::]Set_Plot_X_range
;
; ARGUMENTS: 
; 
;   None
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
;------------------------------------------------------------------------
pro IOSocProfileViewer::Set_Plot_X_range
;------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if self.Plot_Set_X_range[0] eq 0 or  self.Plot_Set_X_range[1] eq 0 then return
  
  self.oPlot_1st_Profile->GetProperty, data=Spectrum, XCOORD_CONV=xs, YCOORD_CONV=ys

  if ptr_valid(self.pPlot_2nd_Profile) then begin
    yMax    = max([*self.pPlot_1st_Profile, *self.pPlot_2nd_Profile], /nan)
    yMin    = min([*self.pPlot_1st_Profile, *self.pPlot_2nd_Profile], /nan)
  endif else begin
    yMax    = max(*self.pPlot_1st_Profile, /nan)
    yMin    = min(*self.pPlot_1st_Profile, /nan) 
  endelse 

  yRange  = [yMin, yMax]

  xMin    = [self.Plot_Set_X_range[0], self.Plot_Set_X_range[0]]
  xMax    = [self.Plot_Set_X_range[1], self.Plot_Set_X_range[1]]

  self.oPlot_Min->SetProperty, datax=xMin, datay=yRange 
  self.oPlot_Min->SetProperty, XCOORD_CONV = xs, YCOORD_CONV = ys   
  self.oPlot_Max->SetProperty, datax=xMax, datay=yRange
  self.oPlot_Max->SetProperty, XCOORD_CONV = xs, YCOORD_CONV = ys  

  if self.Plot_Set_X_range[0] lt 50. then begin 
    Low   = string(self.Plot_Set_X_range[0],format='(d6.3)')
    High  = string(self.Plot_Set_X_range[1],format='(d6.3)')
  endif else begin
    Low   = string(self.Plot_Set_X_range[0],format='(d6.1)')
    High  = string(self.Plot_Set_X_range[1],format='(d6.1)')
  endelse
  
  self.oPlot_Min_text->SetProperty, strings=Low, locations=[Low, yMax], XCOORD_CONV = xs, YCOORD_CONV = ys   
  self.oPlot_Max_text->SetProperty, strings=High, locations=[High, yMax], XCOORD_CONV = xs, YCOORD_CONV = ys     

  self.oProfile_window->Draw, self.oPlot_View   
end