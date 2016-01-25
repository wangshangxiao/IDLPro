;+
; NAME:
; 
;   IOSocProfileViewer::SetProperty
;
; PURPOSE:
; 
;   This public procedure method sets the value of the property or group of properties for the plot. 
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
;   Obj->[IOSocProfileViewer::]SetProperty  [, PLOT_RANGE=vector] [, PX=pointer to a vector] 
;           [, P1ST_PROFILE=pointer to a vector] [, P2ND_PROFILE=pointer to a vector] [, XTITLE=string]
;           [, YTITLE=string]
;
; ARGUMENTS: 
; 
;   None
; 
; KEYWORDS:
; 
;   CLEAR:          Set this keyword to reset any previous settings.
;   PLOT_RANGE:     A two elements vector holding the x-range values to be plotted, i.e. zoomed in or out.
;   PX:             Pointer to a vector holding the values for the x-axis.
;   P1ST_PROFILE:   Pointer to a vector holding the values for the y-axis, i.e. the first profile.
;   P2ND_PROFILE:   Pointer to a vector holding the values for the y-axis, i.e. the second profile.
;   XTITLE:         Set this keyword to a string holding the title to be placed on the x-axis.
;   YTITLE:         Set this keyword to a string holding the title to be placed on the y-axis.
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
pro IOSocProfileViewer::SetProperty, $
                          pX=pX, $
                          p1st_Profile=p1st_Profile, $
                          p2nd_Profile=p2nd_Profile, $
                          xTitle=xTitle, $
                          yTitle=yTitle, $
                          plot_range=plot_range, $
                          clear=clear
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  Module = 'IOSocProfileViewer::SetProperty'
 
  ;------------------------
  ;--- Parameters check ---
  ;------------------------

  if keyword_set(Clear) then begin
    if ptr_valid(self.pPlot_1st_Profile) then ptr_free, self.pPlot_1st_Profile
    if ptr_valid(self.pPlot_2nd_Profile) then ptr_free, self.pPlot_2nd_Profile
  endif

  if keyword_set(pX) then begin
  
    self.pX                 = pX
    self.Plot_Set_X_range   = [min(*self.pX), max(*self.pX)]
    self.Plot_View_X_range  = [min(*self.pX), max(*self.pX)]
  endif

  if keyword_set(p1st_Profile) then begin
    if ptr_valid(self.pPlot_1st_Profile) then ptr_free, self.pPlot_1st_Profile
    self.pPlot_1st_Profile = p1st_Profile
  endif
  
  if keyword_set(p2nd_Profile) then begin
    if ptr_valid(self.pPlot_2nd_Profile) then ptr_free, self.pPlot_2nd_Profile
    self.pPlot_2nd_Profile = p2nd_Profile
  endif
  
  if keyword_set(plot_range) then begin  
    
    t = min(abs(*self.pX - plot_range[0]), L1)
    t = min(abs(*self.pX - plot_range[1]), L2)
  
    self.Plot_Set_X_range = [(*self.pX)[L1], (*self.pX)[L2]]

    self->Set_Plot_X_range
  endif

  if keyword_set(xTitle) then self.xTitle = xTitle
  if keyword_set(yTitle) then self.yTitle = yTitle  
  
  self->View_Profiles
end