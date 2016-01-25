;+
; NAME:
; 
;   IOSocProfileViewer::View_Profiles
;
; PURPOSE:
; 
;   This private procedure method will update the profile window with the requested profiles.
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
;   Obj->[IOScocProfileViewer::]View_Profiles
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
;------------------------------------------------------------------------------------------------------------
pro IOSocProfileViewer::View_Profiles
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
    
  if ptr_valid(self.pPlot_1st_Profile) then begin
  
    i1 = where(*self.pX eq self.Plot_View_X_range[0])
    i2 = where(*self.pX eq self.Plot_View_X_range[1])  
    
    iS  = i1 < i2
    iE  = i1 > i2
    
    Spectrum = (*self.pPlot_1st_Profile)[iS:iE]
    
    xr = [min((*self.pX)[iS:iE]), max((*self.pX)[iS:iE])]

    if ptr_valid(self.pPlot_2nd_Profile) then begin
      yMax    = max([*self.pPlot_1st_Profile, *self.pPlot_2nd_Profile], /nan)
      yMin    = min([*self.pPlot_1st_Profile, *self.pPlot_2nd_Profile], /nan)
    endif else begin
      yMax    = max(*self.pPlot_1st_Profile, /nan)
      yMin    = min(*self.pPlot_1st_Profile, /nan) 
    endelse 
    
    yr  = [yMin, yMax]
   
    xs = NORM_COORD(xr)
    ys = NORM_COORD(yr)  
   
    if total(finite(ys)) ne 2 then return
     
    self.oPlot_1st_Profile->SetProperty, datax=(*self.pX)[iS:iE], datay=Spectrum , $
      XCOORD_CONV = xs, YCOORD_CONV = ys

    self.oPlot_x_Title->SetProperty, strings=self.xTitle

    self.oPlot_x_Axis->SetProperty, /EXACT, RANGE=xr, XCOORD_CONV=xs, YCOORD_CONV=ys, $
      LOCATION=[xr[0], yr[0]], TICKDIR = 0, TICKLEN = (0.02*(yr[1] - yr[0]))

    self.oPlot_y_Title->SetProperty, strings=self.yTitle
    
    self.oPlot_y_Axis->SetProperty,  /EXACT, RANGE=yr, XCOORD_CONV=xs, YCOORD_CONV=ys, $
      LOCATION=[xr[0], yr[0]], TICKDIR = 0, TICKLEN = (0.02*(xr[1] - xr[0]))
      
    fx = float(self.plot_Dims)/(self.plot_Dims-[100, 70])
  
    self.oPlot_View->SetProperty, viewplane_rect=[-fx[0]/8., -fx[1]/5., fx[0], fx[1]] 
  
    if ptr_valid(self.pPlot_2nd_Profile) then begin

      self.oPlot_2nd_Profile->SetProperty, datax=(*self.pX)[iS:iE], $
            datay=(*self.pPlot_2nd_Profile)[iS:iE], XCOORD_CONV=xs, YCOORD_CONV=ys

      self.oPlot_2nd_Profile->GetProperty, XRANGE = xr2, YRANGE = yr2
  
      self.oPlot_x_Axis->SetProperty, RANGE=[xr[0] < xr2[0], xr[1] > xr2[1]], LOCATION=[xr[0] < xr2[0], yr[0] < yr2[0]]
      self.oPlot_y_Axis->SetProperty, RANGE=[yr[0] < yr2[0], yr[1] > yr2[1]], LOCATION=[xr[0] < xr2[0], yr[0] < yr2[0]]
    endif

    self->Set_Plot_X_range      
  endif
end
