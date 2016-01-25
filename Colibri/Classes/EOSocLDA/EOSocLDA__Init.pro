;+
; NAME:
; 
;   EOSocLDA::init
;
; PURPOSE:
; 
;   This method function initializes the LDA object using the class 'EOSocLDA'.
;   The LDA function is created using the spectra from the passed spectral libraries.
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
;   Result = OBJ_NEW('EOSocLDA', pLibraries [,OSTATUS=object reference])
;
; ARGUMENTS:
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
;   OSTATUS:        An object reference to an existing status window.
; 
; RETURN VALUE:
; 
;   The OBJ_NEW function returns an object reference to the newly-created object.
;
; DEPENDENCIES:
; 
;   IOScc_Init
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
function EOSocLDA::init,$
                  pLibraries, $
                  oStatus = oStatus
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  ;----------------------------------------------------------
  ;--- If no status object is present, create the default ---
  ;----------------------------------------------------------

  self.oStatus = IOScc_Init(oStatus, oUser=self) 

  if ~arg_present(pLibraries) then return, 1

  ;-------------------------------------
  ;--- Define all class combinations ---
  ;-------------------------------------
  
  nClasses            = n_elements(pLibraries)
  nFeatures           = n_elements((*pLibraries[0])[0].Spectra)  
  nComb               = (nClasses^2 - nClasses) / 2
  CombPtr             = 0
  u_features          = dblarr(nFeatures, 2, nComb)    
  aP2                 = dblarr(2, nComb)
  u_feat_x_Inv_Pool_A = dblarr(nFeatures, nComb)       
  u_feat_x_Inv_Pool_B = dblarr(nFeatures, nComb)       
  aClassesComb        = intarr(2, nComb)
    
  for Class_A=0, nClasses-1 do begin
    for Class_B=Class_A+1, nClasses-1 do begin
    
      aClassesComb[*, CombPtr] = [Class_A, Class_B]
          
      pClassLib = [pLibraries[Class_A], pLibraries[Class_B]] 
      pResult = self->Construct_LDA_Function(pClassLib)
      
      u_features[*, *, CombPtr]         = *pResult[1]

      u_feat_x_Inv_Pool_A[*, CombPtr]  = *(*pResult[0])[0]
      u_feat_x_Inv_Pool_B[*, CombPtr]  = *(*pResult[0])[1]
      aP2[0, CombPtr] = 0.5d * *(*pResult[0])[0] # (*pResult[1])[*,0] - alog((*pResult[2])[0])      
      aP2[1, CombPtr] = 0.5d * *(*pResult[0])[1] # (*pResult[1])[*,1] - alog((*pResult[2])[1])      
          
      CombPtr++
    endfor
  endfor
  
  ;-------------------------------------------------------------
  ;--- Saving the constructed LDA funtion to local variables ---
  ;-------------------------------------------------------------
  
  self.pu_features          = ptr_new(u_features)
  self.paP2                 = ptr_new(aP2)
  self.pu_feat_x_Inv_Pool_A = ptr_new(u_feat_x_Inv_Pool_A)
  self.pu_feat_x_Inv_Pool_B = ptr_new(u_feat_x_Inv_Pool_B)
  self.nCombinations        = nComb
  self.nClasses             = nClasses
  self.nFeatures            = nFeatures
  self.pClassComb           = ptr_new(aClassesComb)
    
  ;-------------------------------------------------------------
  ;--- Calculate class means, used for Rule image generation ---
  ;-------------------------------------------------------------
   
  ;- The rule image is a multi-band image equal to the number of classes
  aClassMeans = dblarr(2, nComb)
  
  for Cx = 0, nClasses-1 do begin
  
    iClass    = where(aClassesComb eq Cx)
    nSpectra  = n_elements((*pLibraries[Cx]).Spectra[0,*])

    for Ix=0, nSpectra-1 do begin
      
      ; Classify all training spectra (per class) and extract LDA-value for the actual class
      
      Spectrum  = transpose(((*pLibraries[Cx]).Spectra)[*,Ix])
      iA  = Spectrum # (*self.pu_feat_x_Inv_Pool_A) - (*self.paP2)[0, *]
      iB  = Spectrum # (*self.pu_feat_x_Inv_Pool_B) - (*self.paP2)[1, *]

      aClassMeans[iClass] += ([iA, iB])[iClass]
    endfor
    
    aClassMeans[iClass] /= nSpectra
  endfor
    
  self.pClassMeans = ptr_new(aClassMeans)
    
  return, 1
end