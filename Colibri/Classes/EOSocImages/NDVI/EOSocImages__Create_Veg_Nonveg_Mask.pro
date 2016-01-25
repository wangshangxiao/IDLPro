;+
; NAME: 
; 
;   EOSocImages::Create_Veg_Nonveg_mask
;
; PURPOSE: 
; 
;   This procedure method will create a vegetation and non-vegetation mask. 
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
;   Obj->[IOSocImages::]Create_Veg_Nonveg_mask, SelectIx [, /ERODE] [, NDVI_THRESHOLD=value], 
;     OUTPUT_DIR=string [, /OVERWRITE],  REQUEST={'NDVI' | 'RGB' | 'RGBN'}  [, RGB_BANDS=[ red band, 
;     green band, blue band] [, RGB_TRESHOLD=[red threshold, green threshold, blue threshold]
; 
; ARGUMENTS
;   
;   SelectIx:         The image index or indices in the image list, which was specified during initialization
;                     of the object, for which data has to be retrieved. If not specified the information of 
;                     the first image is returned.                 
; 
; KEYWORDS
;   
;   ERODE:            Set this keyword to apply an 3 x 3 erosion filter on the calculted vegetation, 
;                     non-vegetation masks.
;   NDVI_THRESHOLD:   Set this keyword to a threshold to be applied when calculating the vegetation, 
;                     non-vegetation mask. 
;   OUTPUT_DIR:       The output directory where the created masks need to be written. The output files are
;                     named after the image file with suffix '_veg' for the vegetation mask, '_nonveg' for 
;                     the non-vegetation mask, '_veg_lonely' for the filtered vegetation mask and 
;                     '_nonveg_lonely' for the filtered non-vegetation mask.
;   OVERWRITE:        Set this keyword to overwrite already existing output files.
;   REQUEST:          Set this keyword to respectively:
;                       'NDVI': the input images are multi- or hyperspectral images. 
;                       'RGBN': the input images are DigiCam images and contain a red, green, blue and NIR
;                               channel.
;                       'RGB' : he input images are DigiCam images and contain a red, green and blue channel.
;   RGB_BANDS:        A three element vector [red band, green band, blue band] used to hold the band indices
;                     of the RGB-images to be used.
;   RGB_TRESHOLD:     A three element vector [red threshold, green threshold, blue threshold] used to 
;                     calculate the vegetation, non-vegetation mask on DigiCam RGB-images.
;                      
; RETURN VALUE:
; 
;   None
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
;   - Written by Luc Bertels, Februari, 2012.
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
pro EOSocImages::Create_Veg_Nonveg_mask, $
                  SelectIx, $
                  Output_Dir        = Output_Dir, $
                  REQUEST           = REQUEST, $                   
                  NDVI_threshold    = NDVI_threshold, $
                  RGBN_Bands        = RGBN_Bands, $
                  RGB_Treshold      = RGB_Treshold, $
                  Erode             = Erode, $     
                  overwrite         = overwrite
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  Module = 'EOSocImages::Create_Veg_Nonveg_mask'
 
  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------
  
  if ~keyword_set(REQUEST) then Argument = 'REQUEST'
  if ~keyword_set(Output_Dir) then Argument = 'OUTPUT_DIR'
  
  if keyword_set(REQUEST) then begin
    
    case REQUEST of 
    'RGBN': begin
      if ~keyword_set(RGBN_Bands) then Argument = 'RGBN_Bands'
      if ~keyword_set(NDVI_threshold) then Argument = 'NDVI_threshold'
    end
    'NDVI': begin
      if ~keyword_set(NDVI_threshold) then Argument = 'NDVI_threshold'
    end
    'RGB': begin
      if ~keyword_set(RGBN_Bands) then Argument = 'RGBN_Bands'
      if ~keyword_set(RGB_Treshold) then Argument = 'RGB_Treshold'
    end
    else:
    end
  endif
  
  if keyword_set(Argument) then begin
  
    self.oStatus->Log_Status, 'Error', 'Missing argument: '+Argument, Module, /Log
              
    return
  endif

  ;------------------
  ;--- Processing ---
  ;------------------
  
  envi, /restore_base_save_files 
  
  if ~file_test(Output_Dir, /directory) then file_mkdir, Output_Dir
  if ~arg_present(SelectIx) then SelectIx = indgen(self.nImages)
  
  if keyword_set(ERODE) then begin
    
    Kernel_radius = 1 
  
    d         = Kernel_radius * 2 + 1
    tD        = indgen(d)
    t1        = intarr(d)+1
    kL        = tD ## t1 - Kernel_radius
    kS        = transpose(kL)
  endif
     
  ;-------------------------
  ;--- Handle all images ---
  ;-------------------------
  
  self.oStatus->Log_Status, 'Busy', 'Writing results to directory: '+Output_Dir, Module, /Log

  for Fix=0, n_elements(SelectIx)-1 do begin

    FileIx = SelectIx[Fix]

    self.oStatus->Progress_Set, slow=[Fix, n_elements(SelectIx)]

    InFile    = (*self.pImageInfo)[FileIx].ImageFile
    FileName  = file_basename(InFile, '.img')
    
    ;--------------------------------------------------  
    ;--- Claculate vegetation/none-vegetation masks --- 
    ;--------------------------------------------------  
 
    VegBaseName       = FileName+'_veg'
    NonVegBaseName    = FileName+'_nonveg'
    Veg_mask_File     = Output_Dir+VegBaseName
    NonVeg_mask_file  = Output_Dir+NonVegBaseName
    Veg_descrip       = 'Ndvi based vegetation mask generated by EOSocImages'
    NonVeg_descrip    = 'Ndvi based none-vegetation mask generated by EOSocImages'
    
    if ~keyword_set(overwrite) and $
        file_test(Veg_mask_File) eq 1 and $
        file_test(NonVeg_mask_file) eq 1 then continue      
    
    NdviFile  = Output_Dir+FileName+'_ndvi'
    
    if REQUEST eq 'NDVI' or REQUEST eq 'RGBN' and ~file_test(NdviFile) then  begin
    
      self.oStatus->Log_Status, 'Error', 'NDVI image not found: '+NdviFile, Module, /Log
         
      return    
    endif
    
    case REQUEST of
    'RGBN': begin
      
      ;--------------------------------------------------------------------
      ;--- Input are assumed to be DigiCam images (R,G,B and NIR bands) ---
      ;--------------------------------------------------------------------

      ImFid = (*self.pImageInfo)[FileIx].Fid
  
      if ImFid eq -1 then $       
        envi_open_file, InFile, /no_realize, R_fid=ImFid    
        
      envi_open_file, NdviFile, /no_realize, R_fid=ndvi_fid     
      
        ;----------------------------------------------------------------------------
        ;--- Check the NDVI threshold and if the blue band is less then the green ---
        ;--- band and the red band less then the green band.                      ---
        ;----------------------------------------------------------------------------
        
      Expr_Veg    = '(b1 lt b2) and (b3 lt b2) and (b4 ge '+ $
          strtrim(string(NDVI_threshold),2)+')'
        
      Fids        = [ImFid, ImFid, ImFid, ndvi_fid]
                    ;b1=Blue        b2=Green       b3=Red         b4=NDVI  
      Pos         = [RGBN_Bands[2], RGBN_Bands[1], RGBN_Bands[0], 0]   
    end
      
    'NDVI': begin
      
      ;------------------------------------------------------
      ;--- Input are multispectral/hyperspectral channels ---
      ;------------------------------------------------------
        
      envi_open_file, NdviFile, /no_realize, R_fid=ndvi_fid   
             
      Expr_Veg    = '(b1 ge '+ strtrim(string(NDVI_threshold),2)+')'
        
      Fids        = [ndvi_fid]
                    ;b1=NDVI
      Pos         = [0]      
    end
      
    'RGB': begin
        
      ;--------------------------------------
      ;--- Input are DigiCam RGB channels ---
      ;--------------------------------------  
          
      ImFid = (*self.pImageInfo)[FileIx].Fid
  
      if ImFid eq -1 then $ 
        envi_open_file, InFile, /no_realize, R_fid=ImFid   
      
        ;-------------------------------------------------------------------------
        ;--- G > G-Threshold and G > (R + R-Threshold) & G > (B + B-Threshold) ---
        ;-------------------------------------------------------------------------
        
      Expr_Veg    = '(b2 gt '+string(RGB_Treshold[1])+') and ' + $ 
                    '(b2 gt (b1+'+string(RGB_Treshold[0])+')) and ' + $ 
                    '(b2 gt (b3+'+string(RGB_Treshold[2])+'))'
    
      Fids        = [ImFid, ImFid, ImFid]
                    ;b1=Red         b2=Green       b3=Blue]  
      Pos         = [RGBN_Bands[0], RGBN_Bands[1], RGBN_Bands[2]]
    end
    else:
    endcase

    Expr_NonVeg = 'b1 ne 1'
    
    envi_doit, 'math_doit', $ 
                    fid       = Fids, $
                    pos       = Pos, $
                    dims      = (*self.pImageInfo)[FileIx].dims, $ 
                    exp       = Expr_Veg, $
                    out_name  = Veg_mask_File, $ 
                    r_fid     = Veg_mask_fid 
                    
    self.oStatus->Log_Status, 'Ok', $
      'Vegetation mask image created successfully: '+VegBaseName, Module, /Log  

    envi_doit, 'math_doit', $ 
                    fid       = Veg_mask_fid, $
                    pos       = 0, $
                    dims      = (*self.pImageInfo)[FileIx].dims, $ 
                    exp       = Expr_NonVeg, $
                    out_name  = NonVeg_mask_file, $ 
                    r_fid     = NoneVeg_mask_fid     
                          
    self.oStatus->Log_Status, 'Ok', $
      'Non-vegetation mask image created successfully: '+NonVegBaseName, Module, /Log  

    if keyword_set(ERODE) then begin
    
      VegFilterBaseName        = FileName+'_veg';_lonely'
      NonVegFilterBaseName     = FileName+'_nonveg';_lonely'
      Veg_mask_File_lonely     = Output_Dir+VegFilterBaseName
      NonVeg_mask_file_lonely  = Output_Dir+NonVegFilterBaseName
      Veg_descrip_lonely       = 'Filtered ndvi based vegetation mask generated by EOSocImages'
      NonVeg_descrip_lonely    = 'Filtered ndvi based none-vegetation mask generated by EOSocImages'
 
      envi_file_query, NoneVeg_mask_fid, ns=ns, nl=nl, nb=nb, dims=dims
          
      ;-----------------------------------
      ;--- Filter vegetation mask file ---
      ;-----------------------------------
      
      im        = envi_get_data($
                            fid   = Veg_mask_fid, $
                            pos   = [0], $
                            dims  = (*self.pImageInfo)[FileIx].dims)
                            
      OutIm     = im 
      iValid    = where(Im gt 0, cValid)
      L         = iValid / ns
      S         = iValid - L * ns  
  
      for Ix=0l, cValid-1 do begin
 
        if Ix mod 100 eq 0 then self.oStatus->Progress_Set, Fast=[Ix, cValid]
 
        iS      = (0 > (S[Ix]+kS)) < ns
        iL      = (0 > (L[Ix]+kL)) < nl
   
        Kernel  = Im[iS, iL]
      
        if min(kernel) eq 0 then OutIm[S[Ix], L[Ix]] = 0
      endfor
    
      self.oStatus->progress_clear, /fast

      envi_enter_data, OutIm, $
                    Descrip       = Veg_descrip_lonely, $
                    File_Type     = 0, $
                    map_info      = *(*self.pImageInfo)[FileIx].pMapInfo, $
                    r_fid         = tp_fid    
   
      envi_doit, 'cf_doit', $
                    fid           = tp_fid, $
                    pos           = 0, $
                    dims          = (*self.pImageInfo)[FileIx].dims, $
                    out_name      = Veg_mask_File_lonely, $ 
                    r_fid         = p_fid 
            
      self.oStatus->Log_Status, 'Ok', $
        'Filtered vegetation mask images created successfully: '+VegFilterBaseName, Module, /Log    
                            
      envi_file_mng, id=tp_fid, /remove  
      envi_file_mng, id=p_fid, /remove 

      
      ;----------------------------------------
      ;--- Filter none-vegetation mask file ---
      ;----------------------------------------
      
      im        = envi_get_data($
                            fid   = NoneVeg_mask_fid, $
                            pos   = [0], $
                            dims  = (*self.pImageInfo)[FileIx].dims)
                            
      OutIm     = im 
      iValid    = where(Im gt 0, cValid)
      L         = iValid / ns
      S         = iValid - L * ns  
  
      for Ix=0l, cValid-1 do begin
 
        if Ix mod 100 eq 0 then self.oStatus->Progress_Set, Fast=[Ix, cValid]
         
        iS      = (0 > (S[Ix]+kS)) < ns
        iL      = (0 > (L[Ix]+kL)) < nl
   
        Kernel  = Im[iS, iL]
      
        if min(kernel) eq 0 then OutIm[S[Ix], L[Ix]] = 0
      endfor
    
      self.oStatus->progress_clear, /fast
          
      envi_enter_data, OutIm, $
                    Descrip       = NonVeg_descrip_lonely, $
                    File_Type     = 0, $
                    map_info      = *(*self.pImageInfo)[FileIx].pMapInfo, $
                    r_fid         = tp_fid    
      
      envi_doit, 'cf_doit', $
                    fid           = tp_fid, $
                    pos           = 0, $
                    dims          = (*self.pImageInfo)[FileIx].dims, $
                    out_name      = NonVeg_mask_file_lonely, $ 
                    r_fid         = p_fid 
                    
      envi_file_mng, id=tp_fid, /remove  
      envi_file_mng, id=p_fid, /remove 

      self.oStatus->Log_Status, 'Ok', $
        'Filtered non-vegetation mask images created successfully: '+NonVegFilterBaseName, Module, /Log    
    endif
    
    if ((*self.pImageInfo)[FileIx].Fid eq -1) and keyword_set(ImFid) then $             
      envi_file_mng, id=ImFid, /remove
    
    if keyword_set(ndvi_fid)          then envi_file_mng, id=ndvi_fid, /remove
    if keyword_set(Veg_mask_fid)      then envi_file_mng, id=Veg_mask_fid, /remove
    if keyword_set(NoneVeg_mask_fid)  then envi_file_mng, id=NoneVeg_mask_fid, /remove   
  endfor

  self.oStatus->Progress_clear, /RESET  
end 