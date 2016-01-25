;+
; NAME:
; 
;   EOSocLDA::Construct_LDA_Function
; 
; PURPOSE:
; 
;   This function method will create the LDA function for the specified spectral libraries.
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
;   Result = Obj->[IOSocLDA::]Construct_LDA_Function(pClassLib)
;
; ARGUMENTS
; 
;   pLibraries:     A pointer to an array of pointers with the number of pointers equal to the number of 
;                   classes. Each pointer points to a structure array with the number of structure elements
;                   equal to the number of spectra of that class. The structure has following layout:
;                   - ImageFile:  A string holding the image file basename from which the spectrum was 
;                                 obtained.
;                   - RoiId:      A value equal to the ROIID.
;                   - Spectra:    A float array holding the spectrum.
; 
; KEYWORDS
; 
;   None
; 
; RETURN VALUE:
; 
;   A three element pointer array holding respectively: 
;     - Construct Discriminant function
;     - Calculate means for each feature in each class
;     - A priori probability values
;
; DEPENDENCIES:
; 
;   None
;       
; KNOWN ISSUES:
; 
;   None
;
; MODIFICATION HISTORY:
; 
;   - Written by Luc Bertels, June 2010.
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
function EOSocLDA::Construct_LDA_Function, pClassLib
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  ;----------------------
  ;--- Initialization ---
  ;----------------------

  nClasses    = n_elements(pClassLib)
  nFeatures   = n_elements((*pClassLib[0])[0].Spectra)
  u_features  = dblarr(nFeatures, nClasses)
  u_global    = dblarr(nFeatures)
  nSpectra    = dblarr(nClasses)
  
  ;-----------------------------------------------------
  ;--- Calculate mean for each feature in each class ---
  ;-----------------------------------------------------

  for Cx=0, nClasses-1 do begin
  
    nSpectra[Cx] = n_elements((*pClassLib[Cx]).Spectra[0,*])
  
    if nSpectra[Cx] eq 1 then begin
      u_features[*, Cx] = (*pClassLib[Cx]).Spectra
    endif else begin
      u_features[*, Cx] = total(((*pClassLib[Cx]).Spectra),2) / nSpectra[Cx]
    endelse
  endfor
    
  ;-------------------------------------------------------------
  ;--- Calculate mean for each feature in the whole data set ---
  ;-------------------------------------------------------------

  for Cx=0, nClasses-1 do begin
   
    if nSpectra[Cx] eq 1 then begin
      u_global += (*pClassLib[Cx]).Spectra
    endif else begin
      u_global += total(((*pClassLib[Cx]).Spectra),2)
    endelse
  endfor

  u_global /= total(nSpectra)

  ;-------------------------------------
  ;--- Calculate mean corrected data ---
  ;-------------------------------------

  cLib = ptrarr(nClasses)

  for Cx=0, nClasses-1 do begin

    Tmp_Class = double((*(pClassLib)[Cx]).Spectra)

    for Sx=0d, nSpectra[Cx]-1 do begin
      Tmp_Class[*,Sx] -= u_global
    endfor
  
    cLib[Cx] = ptr_new(Tmp_Class)
  endfor
  
  ;-------------------------------------------------------------
  ;--- Calculate covariance matrices for mean corrected data ---
  ;-------------------------------------------------------------

  Cov_Mx = dblarr(nFeatures,nFeatures,nClasses)

  for Cx=0, nClasses-1 do begin

    Mx = *cLib[Cx]
  
    Cov_Mx[*,*, Cx] = MATRIX_MULTIPLY( Mx, Mx, /BTRANSPOSE) / nSpectra[Cx]
  endfor
  
  ;--------------------------------------------
  ;--- Calculate pooled covariance matrices ---
  ;--------------------------------------------
  total_spectra = total(nSpectra)

  P       = dblarr(nClasses)
  Pool_Mx = dblarr(nFeatures,nFeatures,nClasses) + 1d

  for Cx=0, nClasses-1 do begin

    P[Cx]  = nSpectra[Cx]/total_spectra
  
    Pool_Mx[*, *, Cx] *= P[Cx]
  endfor

  Pooled_CovMx     = total(Cov_Mx*pool_mx,3)
  Inv_Pooled_CovMx = invert(Pooled_CovMx, Status)

  ;---------------------------------------
  ;--- Construct Discriminant function ---
  ;---------------------------------------

  ; to speed-up
  u_feat_x_Inv_Pool = ptrarr(nClasses)
  for Cx=0, nClasses-1 do u_feat_x_Inv_Pool[Cx] = ptr_new(u_features[*,Cx] # Inv_Pooled_CovMx)
  
  return, [ptr_new(u_feat_x_Inv_Pool), ptr_new(u_features), ptr_new(P)]
end
