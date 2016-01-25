;+
; NAME: EOSap_DSM2DTM
;
; PURPOSE: 
;
;   This application procedure extracts the Digital Terrain Model (DTM) raster image from the specified 
;   Digital Surface Model (DSM) raster image.
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
;   EOSap_DSM2DTM, DSM_Files, OutDir [, DELTA_HEIGHT=value] [, KERNEL_RADIUS=value]
;     [OSTATUS=object reference]
;                 
; ARGUMENTS
; 
;   DSM_Files:      This argument will hold a string or area of strings of the full path and names of the 
;                   DSM images from which the DTM images have to be extracted.
;   OutDir:         A string holding the output directory where the output, i.e. the DTM images, have to be 
;                   written. The new DTM file names will have the suffix '_dtm' added to their base file name.
;                
; KEYWORDS
;  
;   DELTA_HEIGHT:   Set keyword to a value holding the height limit by which ground level (DTM) pixels are 
;                   selected. I.e. if the central pixel value of the kernel is less than the minimum value 
;                   found in the kernel added with the delta height value, than the pixel is found to be at 
;                   ground level and is a candidate for the DTM image.
;   KERNEL_RADIUS:  This keyword holds a value specifying the radius of the kernel surrounding the central
;                   pixel. This kernel will convolute over the image while calculating and evaluating if the
;                   central pixel is a candidate for the DTM image.
;   OSTATUS:        An object reference to an existing status window. If no valid objecet is specified the
;                   default status window will be created. 
;                                   
; RETURN VALUE:
;   
;   None.
;
; DEPENDENCIES:
;    
;   EOSap_Interpolate_Empty
;         
; KNOWN ISSUES:
; 
;   None.   
;
; MODIFICATION HISTORY:
; 
;   - Written by Luc Bertels, January, 2013.
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
Pro EOSap_DSM2DTM, $
      DSM_Files, $
      OutDir, $ 
      Kernel_radius   = Kernel_radius, $
      Delta_height    = Delta_height, $
      oStatus         = oStatus
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Module = 'EOSap_DSM2DTM'

  ;--------------------------------------------------------
  ;--- Creation and initialization of the status window ---
  ;--------------------------------------------------------
        
  oStatus = IOScc_Init(oStatus, $
                        Title="DTM retrieval from DSM", $
                        Log_DIRECTORY    = OutDir, $
                        Log_FILE_PREFIX  = 'DSM2DTM')

  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------
 
  if ~keyword_set(OutDir)     then Argument = 'OutDir'   
  if ~keyword_set(DSM_Files)  then Argument = 'DSM_Files' 
  
  if keyword_set(Argument) then begin 
    
    oStatus->Log_Status, 'Error', 'Missing argument: '+Argument, Module, /Log  
    return       
  endif

  ;-- Set defaults if needed
  if ~keyword_set(Kernel_radius)  then Kernel_radius = 6
  if ~keyword_set(Delta_height)   then Delta_height = 0.1

  ;--------------------------------
  ;--- Parameter initialization ---
  ;--------------------------------
  
  if ~file_test(OutDir, /directory) then file_mkdir, OutDir
  
  nDSM_Files = n_elements(DSM_Files)

  ;-- Prepare kernel image index.
  d         = Kernel_radius * 2 + 1
  tD        = indgen(d)
  t1        = intarr(d)+1
  kL        = tD ## t1 - Kernel_radius
  kS        = transpose(kL)

  ;------------------------
  ;--- Start processing ---
  ;------------------------

  for FileIx=0, nDSM_Files-1 do begin
        
    oStatus->Progress_Set, slow=[FileIx, nDSM_Files]

    FileName  = file_basename(DSM_Files[FileIx], '.img')
    DTM_File  = OutDir+'\'+FileName+'_dtm'

    oStatus->Log_Status, 'Busy', 'DTM creation from DSM file: '+FileName+'.', Module, /Log
    oStatus->Log_Status, 'Busy', '-> Using kernel radius: '+strtrim(string(Kernel_radius),2)+'.', Module, /Log
    oStatus->Log_Status, 'Busy', '-> Using delta height:'+strtrim(string(Delta_height),2)+'.', Module, /Log
                  
    envi_open_file, DSM_Files[FileIx], r_fid=InFid 
    envi_file_query, InFid, ns=ns, nl=nl, nb=nb, dims=dims, bnames=bnames

    DSM     = envi_get_data(dims=dims, fid=InFid, pos=[0])
    mapinfo = envi_get_map_info(fid=InFid)
    
    envi_file_mng, id=InFid, /remove

    DTM = fltarr(ns, nl)
    
    ;------------------------------------------------
    ;--- Convolute over all lines and all samples ---
    ;------------------------------------------------
    
    for Lx=0, nl-1 do begin

      oStatus->Progress_Set, fast=[Lx, nl]
      
      iL = Lx+kL
      if min(iL) lt 0 or max(iL) ge nl then continue
  
      for Sx=0, ns-1 do begin
      
        iS = Sx+kS
        if min(iS) lt 0 or max(iS) ge ns then continue

        ;-- Get active kernel data
        kernel = DSM[iS, iL]
        
        ;-- Check if non-valid data is present
        if min(Kernel) eq 0 then continue

        ;-- Only keep those pixels which are at ground level, i.e. within the delta height.
        if kernel[Kernel_radius,Kernel_radius]-min(kernel) lt Delta_height then $
          DTM[Sx, Lx] = DSM[Sx, Lx]
      endfor
    endfor

    pImage=ptr_new(DTM)

    ;-- Interpolate the empty pixels
    EOSap_Interpolate_Empty, pImage=pImage, oStatus=oStatus

    ;-- Save result to the output file
    envi_enter_data, *pImage, $
                    Descrip       = 'DTM created by EOSap_DSM2DTM', $
                    File_Type     = 0, $
                    map_info      = mapinfo, $
                    r_fid         = DTM_fid      

    envi_doit, 'cf_doit', $
                    fid           = DTM_fid, $
                    pos           = [0], $
                    out_dt        = 4, $
                    dims          = dims, $
                    out_name      = DTM_File, $ 
                    r_fid         = Out_fid 

    envi_file_mng, id=DTM_fid, /remove
    envi_file_mng, id=Out_fid, /remove    

    oStatus->Progress_clear, /Fast
  endfor
  
  oStatus->Progress_clear, /RESET
  oStatus->Log_Status, 'Ok', 'DSM creation finished.', Module, /Log                  
end