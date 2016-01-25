;+
; NAME:
; 
;   EOSocLibraries::Extract_Libraries_From_Roi
;
; PURPOSE:
; 
;   This method procedure extracts the necessary information from the images and accompanying ROIs and 
;   creates the spectral library text files. Those files are named according the unique ROI names with
;   the extension '.lib' added.
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
;   Obj->[EOSocLibraries::]Extract_Libraries_From_Roi(pImageList, pRoiList, LibDir)
;                           
; ARGUMENTS    
;      
;   pImageList:     A pointer to an array of strings holding the path and name of the images from which the
;                   spectral libraries have to be extracted.
;   pRoiList:       A pointer to an array of strings holding the path and name of the ROI files associated 
;                   with the images. These ROIs are used to extract the spectral profiles from the 
;                   corresponding pixels in order to generate the spectral libraries.
;   LibDir:         A string holding the directory where the library files have to be written.
;                     
; KEYWORDS
; 
;   None 
;
; KNOWN ISSUES:
;
;       None.
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
pro  EOSocLibraries::Extract_Libraries_From_Roi, $
                      pImageList, $
                      pRoiList, $
                      LibDir
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  Module = 'EOSocLibraries::Extract_Libraries_From_Roi'

  ;-------------------------
  ;--- Parameters  check ---
  ;-------------------------

  nImageFiles = n_elements(*pImageList)
  nRoiFiles   = n_elements(*pRoiList)

  ;----------------------------
  ;--- Check if files exist ---
  ;----------------------------
  
  for Ix=0, nImageFiles-1 do begin
    if file_test((*pImageList)[Ix]) ne 1 then begin
      
      self.oStatus->Log_Status, 'Warning', 'File not found: '+(*pImageList)[Ix], Module, /Log
      return
    endif
  endfor

  for Ix=0, nRoiFiles-1 do begin    
    if file_test((*pRoiList)[Ix]) ne 1 then begin
      
      self.oStatus->Log_Status, 'Warning', 'File not found: '+(*pRoiList)[Ix], Module, /Log          
      return
    endif
  endfor

  ;-------------------------------------  
  ;--- Delete all existing libraries ---
  ;-------------------------------------
 
  LibFiles = file_search(LibDir+'*.lib', count=nFiles)
  
  if nFiles gt 0 then file_delete, LibFiles

  ;-----------------------------------------  
  ;--- Retrieve uniq ROI names and color ---
  ;-----------------------------------------
  
  aFids = lonarr(nImageFiles)
  aNs   = intarr(nImageFiles)
  aNl   = intarr(nImageFiles)
  
  for Ix=0, nImageFiles-1 do begin  
  
    envi_open_file, (*pImageList)[Ix], r_fid=fid, /no_realize
    envi_file_query, fid, ns=ns, nl=nl, nb=nb
    envi_delete_rois, /all
    envi_restore_rois, (*pRoiList)[Ix]
    
    aFids[Ix] = fid
    aNs[Ix]   = ns
    aNl[Ix]   = nl
    
    roi_ids   = envi_get_roi_ids(fid=fid, roi_names=roi_names, roi_colors=roi_colors, /short_name)
    
    if roi_ids[0] eq -1 then continue

    i=where(roi_names eq 'Region #1', C, complement=ii)

    roi_ids     = roi_ids[ii]
    roi_names   = roi_names[ii]
    roi_colors  = roi_colors[*, ii]

    if Ix eq 0 then begin
      aRoi_Names      = roi_names
      aRoi_Colors     = roi_colors
      nBands          = nb
    endif else begin
    
      if nb ne nBands then return
    
      aRoi_Names  = [aRoi_Names ,roi_names]
      aRoi_Colors = [[aroi_colors], [roi_colors]]   
    endelse
  endfor
  
  if n_elements(aRoi_Names) eq 0 then return
    
  Uniq_Roi_Names  = aRoi_Names[UNIQ(aRoi_Names, SORT(aRoi_Names))]
  Uniq_Roi_Colors = aRoi_Colors[*, UNIQ(aRoi_Names, SORT(aRoi_Names))]
      
  nUniq_Roi = n_elements(Uniq_Roi_Names)

  ;------------------------------
  ;--- Open the library files ---
  ;------------------------------
  
  LibFiles = file_search(LibDir, '*.lib', count=nLibFiles)
  
  for Ix=0, nLibFiles-1 do begin
  
    file_delete, LibFiles[Ix]
  endfor
  
  for Ix=0, nUniq_Roi-1 do begin
  
    openw, lun, LibDir+Uniq_Roi_Names[Ix]+'.lib', /get_lun

    ;-- Write ROI color and number of bands.
    printf, lun, string(fix(Uniq_Roi_Colors[*, Ix]))
    printf, lun, string(fix(nb))
    
    free_lun, lun
  endfor
  
  ;------------------------------------------
  ;--- Extract ROI spectra and write info ---
  ;------------------------------------------
  
  for Ix=0, nImageFiles-1 do begin  

    envi_delete_rois, /all
    envi_restore_rois, (*pRoiList)[Ix]

    Roi_Ids = envi_get_roi_ids(fid=aFids[Ix], roi_names=roi_names, roi_colors=roi_colors, /short_name)
    BaseName  = file_basename((*pImageList)[Ix])

    i=where(roi_names eq 'Region #1', C, complement=ii)

    roi_ids     = roi_ids[ii]
    roi_names   = roi_names[ii]
    roi_colors  = roi_colors[*, ii]
           
    if obj_valid(self.oStatus) then begin
      self.oStatus->Progress_Set, slow=[Ix, nImageFiles]
      
      self.oStatus->Log_Status, 'Busy', 'Extracting libraries for file: '+BaseName, Module, /Log
    endif   
    
    for Rix=0, n_elements(Roi_Ids)-1 do begin

      if obj_valid(self.oStatus) then self.oStatus->Progress_Set, Fast=[Rix, n_elements(Roi_Ids)]
    
      i = where(Uniq_Roi_Names eq roi_names[Rix])

      if i eq -1 then continue

      data = envi_get_roi_data(roi_ids[Rix], fid=aFids[Ix], addr=addr, pos=indgen[nb]) 
    
      nSpectra = n_elements(addr)
      
      openw, lun, LibDir+Uniq_Roi_Names[i]+'.lib', /get_lun, /append
      
      for Six=0, nSpectra-1 do begin
      
        ;-- Write file name, ROI id. and spectrum. 
        writeu, lun, BaseName, string(roi_ids[Rix]),string(float(data[*, Six])) 
        printf, lun, ''
      endfor
      
      free_lun, lun
    endfor
  endfor
  
  if obj_valid(self.oStatus) then self.oStatus->Progress_clear, /RESET

  ;-----------------------------  
  ;--- Close all open images ---
  ;-----------------------------
  
  for Ix=0, nImageFiles-1 do envi_file_mng, id=aFids[Ix], /remove

  return
end
