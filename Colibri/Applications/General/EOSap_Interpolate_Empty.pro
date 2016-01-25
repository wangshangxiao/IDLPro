;+
; NAME: EOSap_Interpolate_Empty
;
; PURPOSE: 
;
;   This application procedure replaces the empty pixels in the input images by the interpolated value of
;   their neighbours.
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
;   EOSap_Interpolate_Empty, InFiles, OutDir [, EMPTY_VALUE=value] [, OSTATUS=object reference] 
;     [, PIMAGE=pointer][, SUFFIX=string]
;                 
; ARGUMENTS
; 
;   InFiles:        This argument will hold a string or area of strings of the full path and names of the 
;                   images for which the empty pixels, as defined by the EMPTY_VALUE keyword, have to be 
;                   interpolated.
;   OutDir:         A string holding the output directory where the output, i.e. the interpolated images, 
;                   have to be written.
;                
; KEYWORDS
; 
;   EMPTY_VALUE:    Set this keyword to the pixel value of the empty pixels in the image which need to be 
;                   replaced by the interpolated values. If this keyword is not specified the default value 
;                   0. is used, i.e. all pixels with this value will be replace by the interpolated value 
;                   of their eight surrounding pixels. At least three non-empty neighbours have to be present
;                   to allow to calculate the interpolated value. If less non-empty neighbours are present
;                   the pixel is not interpolated. The interpolation process is an iterative process. The 
;                   pixels not interpolated in the actual iteration will be handled in one of the next 
;                   iterations. The iterative interpolation process will go on untill all empty pixels are
;                   handled.
;   OSTATUS:        An object reference to an existing status window. If no valid objecet is specified the
;                   default status window will be created. 
;   PIMAGE:         If specified this keyword holds a pointer to a two or three dimensional image array for 
;                   which the empty pixels have to be interpolated. The interpolated result will be placed 
;                   back into the specified pointer. This keyword has priority over the two arguments 
;                   InFiles and OutDir.
;   SUFFIX:         Set this keyword to a string holding the suffix to be added to the output file base name.
;                   The output file base name is identical to the input file base name.If not set the default
;                   suffix is set to '_ipt'. 

;                                   
; RETURN VALUE:
;   
;   None.
;
; DEPENDENCIES:
;    
;   None.
;         
; KNOWN ISSUES:
; 
;   None.   
;
; MODIFICATION HISTORY:
; 
;   - Written by Luc Bertels, September, 2012.
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
Pro EOSap_Interpolate_Empty, $
      InFiles, $
      OutDir, $
      pImage        = pImage, $
      EMPTY_VALUE   = EMPTY_VALUE, $
      Suffix        = Suffix, $
      oStatus       = oStatus
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Module = 'EOSap_Interpolate_Empty'

  ;--------------------------------------------------------
  ;--- Creation and initialization of the status window ---
  ;--------------------------------------------------------
        
  oStatus = IOScc_Init(oStatus, $
                        Title="Image Interpolation", $
                        Log_DIRECTORY    = OutDir, $
                        Log_FILE_PREFIX  = 'Interpolate_empty')

  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------
 
  if ~ptr_valid(pImage) then begin
   
    if ~keyword_set(OutDir)  then Argument = 'OutDir'   
    if ~keyword_set(InFiles) then Argument = 'InFiles' 
  endif 
  
  if keyword_set(Argument) then begin 
    
    oStatus->Log_Status, 'Error', 'Missing argument: '+Argument, Module, /Log  
    return       
  endif

  ;--------------------------------
  ;--- Parameter initialization ---
  ;--------------------------------
  
  if ~ptr_valid(pImage) then $
    if ~file_test(OutDir, /directory) then file_mkdir, OutDir
  
  if ~keyword_set(EMPTY_VALUE) then EMPTY_VALUE = 0.
  if ~keyword_set(Suffix)      then Suffix = '_ipt'
  
  if ~ptr_valid(pImage) then nInFiles = n_elements(InFiles) else nInFiles=1
  
  d             = 3
  tD            = indgen(d)
  t1            = intarr(d)+1
  kL            = tD ## t1 - 1
  kS            = transpose(kL)

  weight        = 1./sqrt(kl^2 + ks^2)
  weight[1,1]   = 0

  ;------------------------
  ;--- Start processing ---
  ;------------------------

  for FileIx=0, nInFiles-1 do begin

    oStatus->Progress_Set, slow=[FileIx, nInFiles]
  
    if ~ptr_valid(pImage) then begin
    
      outFile = OutDir+'\' + file_basename(InFiles[FileIx], '.img') + Suffix
    
      oStatus->Log_Status, 'Busy', 'Interpolating empty pixels ('+ strtrim(string(EMPTY_VALUE),2)+ $
        ') for file: '+file_basename(InFiles[FileIx], '.img')+'.', Module, /Log
    
      envi_open_file, InFiles[FileIx], r_fid=InFid 
      envi_file_query, InFid, ns=ns, nl=nl, nb=nb, dims=dims, bnames=bnames
  
      map_info      = envi_get_map_info(fid=InFid)
    
      Fids  = lonarr(nb)
      Pos   = intarr(nb)
    endif else begin

      oStatus->Log_Status, 'Busy', 'Interpolating empty pixels ('+ strtrim(string(EMPTY_VALUE),2)+ $
        ').', Module, /Log
    
      S   = size(*pImage)
      ns  = S[1]
      nl  = S[2]      
      
      if S[0] eq 2 then nb = 1 else nb = S[3]
    endelse
    
    ;------------------------
    ;--- Handle all bands ---
    ;------------------------
    
    for Bix=0, nb-1 do begin
    
      if ~ptr_valid(pImage) then begin
        Image = envi_get_data(fid=InFid, pos=[Bix], dims=dims)
      endif else begin
        Image = (*pImage)[*,*,Bix]
      endelse
      
      Interpolated  = Image
      iValue        = where(Image eq EMPTY_VALUE, cValue)
      nPixels       = ns * nl

      ;-------------------------------------------------------
      ;--- Handle all pixels which need to be interpolated ---
      ;-------------------------------------------------------
      
      While cValue gt 0 do begin

        L = iValue / ns
        S = iValue - L * ns

        for Ix=0l, cValue - 1 do begin

          oStatus->Progress_Set, Fast=[Ix, cValue]
  
          iS  = (0 > (S[Ix]+kS)) < ns
          iL  = (0 > (L[Ix]+kL)) < nl
      
          Kern = Image[iS, iL]
      
          i = where(Kern ne EMPTY_VALUE, c)
      
          if c ge 3 then begin
            Interpolated[S[Ix],L[Ix]] = total((Kern*Weight)[i]) / total(Weight[i])
          endif
        endfor
    
        oStatus->Progress_Clear, /FAST
    
        iValue = where(Interpolated eq EMPTY_VALUE, cValue)
    
        if cValue gt 0 then Image = Interpolated
      endwhile

      if ~ptr_valid(pImage) then begin
        
        envi_enter_data, Interpolated, descrip      = 'Empty pixels interpolated by _Interpolate_empty', $
          file_type = 0, map_info = map_info, r_fid = fid_tmp
                   
        Fids[Bix] = fid_tmp
      endif else begin
      
        (*pImage)[*,*,Bix] = Interpolated
      endelse
    endfor
    
    if ~ptr_valid(pImage) then begin
                   
      envi_doit, 'cf_doit', $ 
                   fid          = Fids, $
                   pos          = Pos, $
                   dims         = [-1, 0, ns-1, 0, nl-1], $ 
                   remove       = 0, $
                   out_name     = OutFile, $ 
                   out_bname    = bnames, $
                   r_fid        = fid_Out
    
      for Ix=0, nb-1 do envi_file_mng, id=Fids[Ix], /remove               
      envi_file_mng, id=fid_Out, /remove   
      envi_file_mng, id=InFid, /remove   
    endif
  endfor
  
  oStatus->Progress_clear, /RESET
  oStatus->Log_Status, 'Ok', 'Interpolating empty pixels finished.', Module, /Log                 
end
