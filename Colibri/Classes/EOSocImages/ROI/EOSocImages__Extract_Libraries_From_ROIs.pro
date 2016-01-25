;+
; NAME:
; 
;   EOSocImages::Extract_Libraries_From_Rois
;
; PURPOSE:
;
;   This procedure method extracts the spectral information from the specified images by means of the 
;   assigned ROIs.
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
;   Obj->[EOSocImages::]->Extract_Libraries_From_Roi, ROI_DIR [, AROICOLORS=variable] [, AROINAMES=variable]
;      [, BANDSIX=array] [, PLIBRARIES=variable] [, SELECTIX=array]              
;                                    
; ARGUMENTS
; 
;   ROI_DIR:      A string holding the directory where the ROI files are located.
;                   
; KEYWORDS
; 
;   AROICOLORS:   A vector or array of vectors holding the RGB color values for the different ROIs.
;   AROINAMES:    A string or array of strings holding the names of the different ROIs.
;   BANDSIX:      Set this keyword to a vector holding the band indices for which the spectral values have
;                 to be extracted. If not specified all bands are used.
;   pLibraries:   Set this keyword to a variable in which a pointer array is returned. Each pointer points to
;                 a spectral library structure array with layout:
;                   - ImageFile:  a string holding the full path and name of the image file from which the 
;                                 spectrum was extracted.
;                   - RoiName:    a string holding the ROI name of the extracted spectrum.
;                   - Spectra:    a array holding the extracted spectrum.                 
;   SelectIx:     Set this keyword to a vector holding the  image index or indices in the image list, which 
;                 was specified during initialization of the object, for which data has to be retrieved. If 
;                 not specified all images are used.
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
; EXAMPLE:
; 
;   An extensive example can be found in the examples subdirectory: IOSex_ROI_Extract_Libraries
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
pro EOSocImages::Extract_Libraries_From_Rois, $
                                    Roi_Dir, $
                                    SelectIx      = SelectIx, $
                                    BandsIx       = BandsIx, $
                                    pLibraries    = pLibraries, $
                                    aRoiNames     = aRoiNames, $
                                    aRoiColors    = aRoiColors
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  Module  = 'EOSocImages::Extract_Libraries_From_Rois'

  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------

  if ~keyword_set(Roi_Dir)        then Argument = 'Roi_Dir'  
  
  if keyword_set(Argument) then begin
    self.oStatus->Log_Status, 'Error', 'Missing argument: '+Argument, Module, /Log                
    return
  endif
  
  ;----------------------
  ;--- Initialization ---
  ;---------------------- 

  if ~arg_present(SelectIx) then SelectIx = indgen(self.nImages)
  
  nb          = (*self.pImageInfo)[SelectIx[0]].nb
  aRoiNames   = ''
  aRoiColors  = [0,0,0]
  pLibraries  = ptr_new(/allocate_heap)
  
  if keyword_set(BandsIx) then Pos = BandsIx else Pos = indgen(nb)
  if ~arg_present(SelectIx) then SelectIx = indgen(self.nImages)
  
  Lib         = {ImageFile:'', RoiName:'', Spectra:fltarr(n_elements(pos))}       
  
  ;------------------------
  ;--- Start Processing ---
  ;------------------------
  
  for Fix=0, n_elements(SelectIx)-1 do begin
   
    envi_delete_rois, /all      

    FileIx    = SelectIx[Fix]
    ImageFile = (*self.pImageInfo)[FileIx].ImageFile
    RoiFile   = Roi_Dir + file_basename(ImageFile, '.img')+'.roi'
      
    self.oStatus->Log_Status, 'Busy', 'Procesing file: '+ImageFile, Module, /Log   
     
    if file_test(RoiFile) eq 0 then continue
    
    ImFid = (*self.pImageInfo)[FileIx].Fid
  
    if ImFid eq -1 then $           
      envi_open_file, ImageFile, /no_realize, R_fid=ImFid
      
    envi_restore_rois, RoiFile
      
    RoiIds  = envi_get_roi_ids($
                              fid         = ImFid, $
                              roi_colors  = roi_colors, $
                              roi_names   = roi_names, $
                              /short_name)

    if RoiIds[0] eq -1 then continue

    ;------------------------
    ;--- Remove dummy Roi ---
    ;------------------------

    i = where(roi_names eq 'Region #1', C, Complement=iValid, nComplement = nValid)

    if nValid le 0 then continue

    if C gt 0 then begin
      
      RoiIds      = RoiIds[iValid]
      roi_colors  = roi_colors[*, iValid]
      Roi_Names   = Roi_Names[iValid]
    endif
    
    ;--------------------
    ;--- Extract Rois ---
    ;--------------------
    
    nRoiIds = n_elements(RoiIds)

    self.oStatus->Log_Status, 'Busy', 'Extracting: '+ $
      strtrim(string(nRoiIds),2)+' ROIs using ROI file: '+RoiFile, Module, /Log   

    for Rix=0, nRoiIds-1 do begin                          

      Data = envi_get_roi_data(RoiIds[Rix], $
                              fid       = ImFid, $
                              pos       = Pos)

      nSpectra  = n_elements(data[0,*])                          
      TmpLib    = replicate(Lib, nSpectra)  
                     
      TmpLib.ImageFile  = ImageFile
      TmpLib.RoiName    = Roi_Names[Rix]              
      TmpLib.Spectra    = Data
                           
      i = where(aRoiNames eq Roi_Names[Rix], C)                        
      
      if c le 0 then begin
           
        aRoiNames   = [aRoiNames, Roi_Names[Rix]]
        aRoiColors  = [[aRoiColors], [roi_colors[*,Rix]]]
        pLibraries  = [pLibraries, ptr_new(TmpLib)]
      endif else begin
        
      endelse                      
    endfor
    
    if (*self.pImageInfo)[FileIx].Fid eq -1 then $
      envi_file_mng, id=ImFid, /remove 
  endfor
    
  n = n_elements(aRoiNames)-1
  
  if n eq 0 then return
    
  aRoiNames   = aRoiNames[1:n]
  aRoiColors  = aRoiColors[*, 1:n]
  pLibraries  = pLibraries[1:n]
  
  self.oStatus->Log_Status, 'Ok', 'A total of '+strtrim(string(n),2)+' libraries were extracted successfully.', Module, /Log 
  self.oStatus->Log_Status, 'Ok'
end
 
