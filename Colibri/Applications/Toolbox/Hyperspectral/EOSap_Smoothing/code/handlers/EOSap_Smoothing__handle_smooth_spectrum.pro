;+
; NAME:
; 
;   EOSap_Smoothing::handle_smooth_spectrum
;
; PURPOSE:
; 
;   This procedure methode is used to apply the specified smoothing factors to the soecified spectrum.
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
;   Obj->[EOSap_Smoothing::]handle_smooth_spectrum, SPECTRUM=variable
;
; ARGUMENTS 
; 
;   None
; 
; KEYWORDS
; 
;   SPECTRUM:           Set this keyword to a variable that upon return will hold a floating point array that contains
;                       the smoothed spectrum.
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
pro EOSap_Smoothing::handle_smooth_spectrum, Spectrum=Spectrum
;-------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  if ~ptr_valid(self.pSmoothFac) then return
  
  i = where((*self.pSmoothFac) ne 0, C)
  
  if C eq 0 then begin
    Spectrum = *self.pSpectrum * !VALUES.F_NAN
    return
  endif
  
  iN  = where(*self.pSmoothFac ne -1 and $    ; Ignore
              *self.pSmoothFac ne -2 and $    ; Zeroed
              *self.pSmoothFac ne -3 and $    ; Removed
              *self.pSmoothFac ne -4,  cN)    ; Interpolated
  iI  = where(*self.pSmoothFac eq -1, cI)
  iZ  = where(*self.pSmoothFac eq -2, cZ)
  iP  = where(*self.pSmoothFac eq -4, cP)
  iR  = where(*self.pSmoothFac eq -3, complement=iKeep, cR)
  
  Spectrum = *self.pSpectrum

  nB  = n_elements(Spectrum)
  
  SmoothFac     = intarr(nB)
  MaxSmooth     = max(fix((*self.pSmoothFac)[iN]))
  SmoothFac[iN] = fix((*self.pSmoothFac)[iN])  
  SmootSpec     = Spectrum
  SF            = SmoothFac

  iN  = where(*self.pSmoothFac ne -1 and $  ; Ignore
              *self.pSmoothFac ne -2 and $  ; Zeroed
              *self.pSmoothFac ne -3, cN)   ; Removed
  
  for Fx = 0, MaxSmooth-1 do begin
    for Bix=0, cN-1 do begin
               
      if SF[iN[Bix]] gt 0 then begin

        SF[iN[Bix]]--

        case iN[Bix] of
        iN[0]: begin
          SmootSpec[iN[Bix]] = (( 3*Spectrum[iN[Bix]] + Spectrum[iN[Bix+1]] ) / 4)
        end
            
        max(iN): begin
          SmootSpec[iN[Bix]] = (( 3*Spectrum[iN[Bix]] + Spectrum[iN[Bix-1]] ) / 4)
        end            
            
        else: begin
          SmootSpec[iN[Bix]] = (( Spectrum[iN[Bix-1]] + 2*Spectrum[iN[Bix]] + Spectrum[iN[Bix+1]]) / 4)
        end
        endcase
      endif
    endfor
           
    Spectrum = SmootSpec
  EndFor 
  
  if cZ gt 0 then begin
    Spectrum[iZ] = 0
  endif
      
  if cR gt 0 then begin
    Spectrum[iR] = 0
  endif
  
  if cP gt 1 then begin
  
    aSEi = [0, 0]                           ; To define the number of interpolation groups
    iS   = -1
    
    for i=0, cp-1 do begin
  
      if Is eq -1 then iS = (ip[i] - 1) > 0   ; start of new interval
      iE = (ip[i] + 1) < (nb-1)               ; end of the interval
  
      if i+1 eq cp then begin
        
        aSEi  = [[aSEi], [iS, iE]]
        iS    = -1
      endif else begin
    
        if iE ne ip[i+1] then begin
          aSEi  = [[aSEi], [iS, iE]]
          iS    = -1
        endif
      endelse   
    endfor
  
    ni = n_elements(aSEi[0,*])
  
    for Ix=1, ni-1 do begin
    
      iS =aSEi[0,Ix]         ; start of the interval
      iE = aSEi[1,Ix]        ; end of the interval
      
      sR  = Spectrum[iS]
      sW  = (*self.pWl)[iS]
      dR  = Spectrum[iE] - Spectrum[iS]
      dW  = (*self.pWl)[iE] - (*self.pWl)[iS]
      a   = dR / dW
     
      for iW = iS+1, iE-1 do begin
      
        aW = (*self.pWl)[iW]
        Spectrum[iW] = sR + (aW - sW)*a  
      endfor
    endfor
  endif
end  