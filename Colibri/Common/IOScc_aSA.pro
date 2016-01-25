;+
; NAME: 
; 
;   IOScc_aSA
;
; PURPOSE: 
; 
;   This function will calculate the maximum Spectral Angle (SA) value between a reference spectrum and its 
;   surrounding spectra as defined by the kernel size.
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
;   Value = IOScc_aSA(pBank, iActual, Radius)    
; 
; ARGUMENTS
; 
;   pBank         : An n element pointer array holding the spectral slice for which to calculate the SA 
;                   values. Here n equals to the diameter of the convolution kernel. Each pointer points to 
;                   a 2 dimensional array s x b, with s the number of samples and b the number of bands. 
;   iActual       : An index into the spectral slice indicating the central pixel for which the maximum 
;                   SA value has to be calculated.
;   Radius        : A value holding the radius of the convolution kernel. The diameter is calculated as 
;                   Radius*2 + 1.
;
; KEYWORDS
; 
;   None
;                      
; RETURN VALUE:
; 
;   The calculated maximum SA value.
;       
; KNOWN ISSUES:
;  
;   None
;
; MODIFICATION HISTORY:
; 
;   - Written by Luc Bertels, January, 2012.
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
function IOScc_aSA, pBank, iActual, Radius
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

    D           = Radius * 2 + 1                    ; diameter of the kernel
    ActSpec     = (*(pBank)[Radius])[iActual, *]    ; spectrum of the center of the kernel
    aActSpec    = ActSpec ## (intarr(D)+1)          ; replicate center spectrum to the size of the kernel
    Theta       = 0.

    ;-- Calculate the SA values of the actual spectrum with surrounding spectra and retain the highest 
    ;-- value.
    for Ix=0, D-1 do begin
      T = total(aActSpec * (*(pBank)[Ix])[iActual-Radius:iActual+Radius, *],2)
      N = sqrt(total(aActSpec^2,2) * total((*(pBank)[Ix])[iActual-Radius:iActual+Radius, *]^2,2))

      aTN = acos( T/N ) 

      i = where(finite(aTN) ne 1, C)
    
      if C gt 0 then aTN[i] = 0

      Theta = Theta > max(aTN)
    endfor
    
    return, Theta 
end
