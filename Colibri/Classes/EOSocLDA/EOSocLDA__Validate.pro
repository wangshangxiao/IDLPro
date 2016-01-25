;+
; NAME:
;
;       xxx
;
; PURPOSE:
;
;       *** Under construction ***
;
; AUTHOR:
;
;       Luc Bertels
;       TAP - Teledetection and Earth Observation Processes 
;       VITO - Flemish Institute for Technological Research
;       Boeretang 200 
;       B-2400 Mol, Belgium 
;       http://www.vito.be
;
; CALLING SEQUENCE:
;
;       xxx
;
; CALLED FROM:
; 
;       xxx
;
; RETURN VALUE:
;
;       xxx
;
; DEPENDENCIES:
;
;       xxx
;       
; KNOWN ISSUES:
;
;       xxx
;
; MODIFICATION HISTORY:
;
;       Written by Luc Bertels, xxx 2010.
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
pro EOSocLDA::Validate, $
                pValidate, $
                pValidate_Classified=pValidate_Classified
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  ;-------------------------------------------------
  ;--- Retrieve prepared LDA function parameters ---
  ;-------------------------------------------------
    
  u_feat_x_Inv_Pool_A = *self.pu_feat_x_Inv_Pool_A
  u_features_A        = (*self.pu_features)[*, 0, *]
  P_A                 = (*self.paP2)[0, *]
  u_feat_x_Inv_Pool_B = *self.pu_feat_x_Inv_Pool_B    
  u_features_B        = (*self.pu_features)[*, 1, *]
  P_B                 = (*self.paP2)[1, *]
  nClasses            = self.nClasses
  nFeatures           = self.nFeatures  
    
  ;---------------------------------------  
  ;--- Classify the validation spectra ---  
  ;---------------------------------------  

  pValidate_Classified = ptrarr(nClasses)

  ;--------------------------
  ;--- Handle all classes ---
  ;--------------------------
  
  for Ix=0, nClasses-1 do begin

    nSpectra  = n_elements((*pValidate[Ix]).RoiId)
    Class     = bytarr(nSpectra)
    
    ;--------------------------
    ;--- Handle all spectra ---
    ;--------------------------
    
    for Six=0, nSpectra-1 do begin
    
      Spectrum = (*pValidate[Ix])[Six].Spectra
      
      ;----------------------------------
      ;--- Perform LDA classification ---
      ;----------------------------------
                       
      Class[Six] = self->LDA(Spectrum)                                    
    endfor
    
    pValidate_Classified[Ix] = ptr_new(Class)
  endfor
end   