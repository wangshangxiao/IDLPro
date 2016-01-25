;+
; NAME:
; 
;   EOSex_ROI_Extract_Libraries
;
; PURPOSE:
; 
;   This example procedure demonstrates the use of the class method "EOSocImages::Extract_Libraries_From_Rois"
;   for extracting spectral libraries from ROIs.
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
;   EOSex_ROI_Extract_Libraries                  
;
; ARGUMENTS
;
; KEYWORDS
;                      
; DEPENDENCIES:
; 
;   This demo uses the test images located the Colibri install directory: <Dir:\>...\Colibri\Examples\
;       DigiCam_1
;       DigiCam_2
;       DigiCam_3
;       
; KNOWN ISSUES:
;
;   None.
;   
; MODIFICATION HISTORY:
; 
;   - Written by Luc Bertels, October, 2012.
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
pro EOSex_ROI_Extract_Libraries
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  ;----------------------
  ;--- Initialization ---
  ;----------------------
    
  Path = ENVI_PICKFILE(/MULTIPLE_FILES, /DIRECTORY, $
    TITLE='Locate the directory: ''..\Examples\Data''') 
  
  if ~file_test(Path, /DIRECTORY) then return
    
  ;--- Pointer to the images to be used for demo.
  pImageList   = ptr_new([Path+'\DigiCam_1', $
                          Path+'\DigiCam_2', $
                          Path+'\DigiCam_3']) 

  ;--- Directory where the ROI files are located.
  Roi_Dir = ENVI_PICKFILE(/MULTIPLE_FILES, /DIRECTORY, $
    TITLE='Select the directory where the ROI files have to be stored.')+'\'

  if ~file_test(Roi_Dir, /DIRECTORY) then return

  ;---------------------------------
  ;--- Create the status window. ---
  ;---------------------------------
      
  oStatus = IOScc_Init(oStatus, $
               Title = 'ROI_EXTRACT_LIBRARIES_DEMO', $
               Log_Directory     = Roi_Dir, $
               Log_File_Prefix   = 'ROI_EXTRACT_LIBRARIES_DEMO_log')

  ;---------------------------------------------------------
  ;--- Creation and Initialization of the oImages object ---
  ;---------------------------------------------------------

  oImages = obj_new('EOSocImages', $
                pImageList, $ 
                oStatus               = oStatus)

  ;-------------------------
  ;--- Extract Libraries ---
  ;-------------------------

  ;-- Delete the first en third ROI created earlier by ROI_CREATE_DEMO
  SelectIx=0
  oImages->Extract_Libraries_From_Rois, Roi_Dir, SelectIx=SelectIx, pLibraries=pLibraries, $
      aRoiNames=aRoiNames, aRoiColors=aRoiColors

  if float(envi_query_version()) lt 5. then begin
  
    if strlen(aRoiNames[0]) eq 0 then begin
      oStatus->Log_Status, 'Error', 'No ROI files found.'
      return
    endif
  endif else begin
    
    if *pLibraries[0] eq !NULL then begin
  
      oStatus->Log_Status, 'Error', 'No ROI files found.'
      return
    endif
  endelse
  
  ;--------------------
  ;--- Display Info ---
  ;--------------------

  print, ''
  print, '--------------------------------'
  print, ' Spectral libraries info.
  print, '--------------------------------'
  print, ''
  help, pLibraries
  print, ''
  print, '- Contents of first pointer:'
  print, ''  
  print, *pLibraries[0]
  print, ''
  print, '- Structure layout:'
  print, ''
  help, *pLibraries[0], /struct
  print, ''
  help, aRoiNames
  print, aRoiNames
  print, ''
  help, aRoiColors
  print, aRoiColors
end