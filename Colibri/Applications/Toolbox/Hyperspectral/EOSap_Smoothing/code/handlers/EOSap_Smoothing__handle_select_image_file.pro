;+
; NAME:
; 
;   EOSap_Smoothing::handle_select_image_file
;
; PURPOSE:
; 
;   This procedure methode is used to request for the image to be smoothed.
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
;   Obj->[EOSap_Smoothing::]handle_select_image_file
;
; ARGUMENTS 
; 
;   None
; 
; KEYWORDS
; 
;   None
; 
; RETURN VALUE:
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
;-------------------------------------------------------------------------
pro EOSap_Smoothing::handle_select_image_file
;-------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Image_File = envi_pickfile(Title='Select Image File.')
  
  if strlen(Image_File) lt 3 then return
  
  widget_control, widget_info(self.wWidget, find_by_uname='Wt_Image_File'), set_value=Image_File
  
  oImage_ForSmoothFac = obj_new('EOSocImages', $
                ptr_new(Image_File), $ 
                oStatus = self.oStatus, $
                /KEEP)  

  if ~obj_valid(oImage_ForSmoothFac) then return
  
  oImage_ForSmoothFac->get, nb=nb

  if nb le 3 then begin
  
    obj_destroy, oImage_ForSmoothFac
  
    self.oStatus->Log_Status, 'Error', 'Image file is not a spectral file.'
    return  
  endif
  
  if ~obj_valid(self.oImage_ForSmoothFac) then obj_destroy, self.oImage_ForSmoothFac
  
  self.oImage_ForSmoothFac = oImage_ForSmoothFac
  self.oImage_ForSmoothFac->get, pWL=pWL, Bnames=Bnames, nb=nb, pRGBdata=pRGBdata, pMapInfo=pMapInfo

  self.oProfileViewer->SetProperty, pX=pWL, /Clear
  
  self.pWl = pWL

  widget_control, widget_info(self.wWidget, find_by_uname='Wl_Bands'), $
    set_value=Bnames+'  '+strtrim(string(*pWL),2) 
    
  self.pSmoothFac = ptr_new(intarr(nb))
  self->update_smoothing_factors 
      
  self.oImageViewer->Update_view, pRGBdata, pMapInfo, /NEW
end