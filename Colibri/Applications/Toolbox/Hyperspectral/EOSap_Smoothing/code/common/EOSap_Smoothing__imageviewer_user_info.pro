;+
; NAME:
; 
;   EOSap_Smoothing::ImageViewer_User_Info
;
; PURPOSE:
; 
;   This procedure method is called by the 'IOSocImageViewer' object to inform the user of the object about an 
;   event that occured in the WIDGET_DRAW images window.
;   This procedure method is the interface with the 'IOSocImageViewer' object. It accepts the ImageViewerInfo
;   argument which holds the actual pixel location. This actual pixel location is used to retrieve the actual
;   pixel spectrum. If smoothing factors are defined these will be applied to the retrieved spectrum. Both, 
;   the non-smoothed and smoothed spectrum will be visualized by using the ProfileViewer instance.
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
;   Obj->[EOSap_Smoothing::]ImageViewer_User_Info, ImageViewerInfo
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
; - Written by Luc Bertels, December 2012.
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
pro EOSap_Smoothing::ImageViewer_User_Info, ImageViewerInfo
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  self.oImage_ForSmoothFac->get, Pixel_Location=ImageViewerInfo.Central_Pixel_Location, Spectrum=Spectrum

  if ptr_valid(self.pSpectrum) then ptr_free, self.pSpectrum
  
  self.pSpectrum = ptr_new(Spectrum)
  
  self.oProfileViewer->SetProperty, p1st_Profile=self.pSpectrum
  self->handle_smooth_spectrum, Spectrum=Spectrum

  self.oProfileViewer->SetProperty, p2nd_Profile=ptr_new(Spectrum)
end