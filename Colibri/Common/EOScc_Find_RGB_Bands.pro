;+
; NAME:
; 
;   EOScc_find_RGB_bands
;
; PURPOSE:
; 
;   This common procedure will search the Red, Green and Blue band indices for the specified image.
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
;   EOScc_find_RGB_bands [, FID=file ID] [, IMAGEFILE=string], POS=array
;                                     
; ARGUMENTS     
; 
;   None
;        
; KEYWORDS 
;
;   FID:            Use this keyword to specify a named variable that contains the file ID of the image from
;                   which to retrieve the RGB band indices.     
;   IMAGEFILE:      Set this keyword to a string holding the full path and name of the image from which the
;                   RGB band indices need to be retrieved.
;   POS:            Use this keyword to specify a named variable that upon return contains a three-element 
;                   array of the RGB band positions.
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
;   - Written by Luc Bertels, March 2012.
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
pro EOScc_find_RGB_bands, ImageFile=ImageFile, Fid=Fid, Pos=Pos
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Pos = [-1]
 
  if ~keyword_set(Fid) then begin 
    if ~keyword_set(ImageFile) then return
    if ~file_test(ImageFile) then return
    
    envi_open_file, ImageFile, R_Fid=Fid, /no_realize
  endif
  
  envi_file_query, Fid, ns=ns, nl=nl, nb=nb, dims=dims, wl=wl, bnames=bnames
  
  if wl[0] ne -1 then  begin
    
    ;-- Wavelength information was found locate the nearest band indices at
    ;-- 460 nm (B), 550 nm (R) and 640 nm (G).   
    if wl[0] lt 100 then wl = wl * 1000
    
    Tmp   = min(abs(wl - 460), iB)
    Tmp   = min(abs(wl - 550), iG)
    Tmp   = min(abs(wl - 640), iR)
    
    Pos = [iR, iG, iB]
  endif else begin
  
    ;-- Launch the ENVI Band Selectoin dialog if no wavelength information is present.
    rgb_get_bands, fid=Fid, pos=pos, dims=dims
    envi_file_mng, id=Fid, /remove
  endelse
end