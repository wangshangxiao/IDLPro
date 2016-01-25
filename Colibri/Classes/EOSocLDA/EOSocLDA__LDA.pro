;+
; NAME:
;
;       EOSocLDA::LDA
;
; PURPOSE:
;
;       This function method is used to classify the input spectrum using the LDA function constructed in the
;       init function method.
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
;       Result = Obj->[IOSocLDA::]LDA( Spectrum [,RULE=value])
;
; ARGUMENTS:  
;
;   Spectrum:         An array of dimension (1 x n) with n equal to the number of bands. The array holds the
;                     spectrum to be classified. 
;     
; KEYWORDS:
; 
;   RULE:             Set this keyword equal to a named variable that, on return, holds an array with the 
;                     rule (cost) values of the input spectrum. The number of entries in the array equals
;                     the number of classes the LDA function is constructed with.       
; 
; RETURN VALUE:
;
;       This function method returns the class id of the input spectrum.
;
; DEPENDENCIES:
;
;       None.
;       
; KNOWN ISSUES:
;
;       None.
;
; MODIFICATION HISTORY:
;
;       Written by Luc Bertels, June 2010.
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
function EOSocLDA::LDA, Spectrum, Rule=Rule
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  ia = Spectrum # (*self.pu_feat_x_Inv_Pool_A) - (*self.paP2)[0, *] 
  ib = Spectrum # (*self.pu_feat_x_Inv_Pool_B) - (*self.paP2)[1, *]  

  iClasses  = (*self.pClassComb)[ib gt ia, indgen(self.nCombinations)]
  aClasses  = intarr(self.nClasses)

  aClasses[iClasses]++

  t = max(aClasses, Class)

  if arg_present(Rule) then begin
  
    Rules = abs(([ia, ib]-(*self.pClassMeans))/(*self.pClassMeans))
    Rule = dblarr(self.nClasses)
 
    for Ix=0, self.nClasses-1 do begin

      i         = where((*self.pClassComb) eq Ix, c)
      Rule[Ix]  = total(Rules[i]) / c    
    endfor
  end

  return, ++Class
end