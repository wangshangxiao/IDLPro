;+
; NAME:
; 
;   EOSex_SSIREE
;
; PURPOSE:
; 
;   This example procedure demonstrates the use of the application "EOSap_SSIREE" for automatic endmember
;   extraction from a set of images.
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
;   EOSex_SSIREE                  
;
; ARGUMENTS
;
; KEYWORDS
;                      
; DEPENDENCIES:
; 
;   This demo uses the test images located in sub-folder: <Dir:>\Colibri\Examples\data
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
;   - Written by Luc Bertels, February, 2013.
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
pro EOSex_SSIREE
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  ;----------------------
  ;--- Initialization ---
  ;----------------------
  
  ;--- Process the example data located in the examples data directory.                         
  Path = ENVI_PICKFILE(/MULTIPLE_FILES, /DIRECTORY, $
    TITLE='Locate the directory: ''<DIR:>\Colibri\Examples\Data''') 

  if ~file_test(Path, /DIRECTORY) then return
  
  ;--- Pointer to the images to be used for demo.
  pEndmember_ImageList   = ptr_new([$
                          Path+'\DigiCam_1', $
                          Path+'\DigiCam_2', $
                          Path+'\DigiCam_3'])
                           
  if ~file_test(Path+'\DigiCam_1') then begin
  
    ok = dialog_message('No valid images were found.', Title='EOSex_SSIREE', /error)
    return  
  endif
  
  ;--- Define output directory
  Output_Dir = ENVI_PICKFILE(/DIRECTORY, $
    TITLE='Locate the output directory.')

  if ~keyword_set(Output_Dir) then return else Output_Dir += '\' 

  Temp_Dir  = Output_Dir+'SSIREEex_Temp\'

  ;-----------------------------------------------
  ;--- Create vegetation, non-vegetation masks ---
  ;-----------------------------------------------

  ;- Creation of the status window
  oStatus = IOScc_Init(oStatus, $
                          Title = 'Example of the SSIREE algorithm)', $
                          Log_Directory     = Output_Dir, $
                          Log_File_Prefix   = 'SSIREE_log')
  

  ;- Creation and Initialization of the oImages object   
  oImages = obj_new('EOSocImages', $
                          pEndmember_ImageList, $ 
                          oStatus               = oStatus)

  ;- Calculate the ndvi images
  oImages->Calculate_NDVI, $
                          Output_Dir            = Temp_Dir, $
                          bRedNIR               = [2, 3]        ; zero based indices to the red and nir band

  if ~oStatus->Check('Ok') then return 
       
  ;- Create the vegetation/non-vegetation masks              
  oImages->Create_Veg_Nonveg_mask, $              
                          Output_Dir            = Temp_Dir, $
                          Request               = 'NDVI', $
                          NDVI_threshold        = 0.03, $       ; NDVI threshold for vegetation / non-vegetation  
                          /Erode

  if ~oStatus->Check('Ok') then return                         
               
  obj_destroy, oImages          
               
  ;-----------------------------------             
  ;--- Extract the pure endmembers ---
  ;-----------------------------------             
                          
  EOSap_SSIREE, $
            pEndmember_ImageList, $
            Output_Dir, $
            oStatus           = oStatus, $
            Purity_Method     = 'RMSE', $       ; method for purity image generation, 0 = RMSE, 1 = SA  
            nEndmembers       = 3, $            ; Search for the three purest vegetation endmembers
            Mask_Dir          = Temp_Dir, $
            Mask_Suffix       = '_veg', $
            BaseName          = 'Veg', $
            pEndMembers       = pEndMembers, $
            /CLEANUP, $                         ; delete existing endmembers
            /OVERWRITE, $                       ; overwrite existing intermediate results if requested
            /KEEP                               ; keep all intermediate results

  EOSap_SSIREE, $
            pEndmember_ImageList, $
            Output_Dir, $
            oStatus           = oStatus, $
            Purity_Method     = 'RMSE', $       ; method for purity image generation, 0 = RMSE, 1 = SA  
            nEndmembers       = 4, $            ; Search for the four purest non-vegetation endmembers
            Mask_Dir          = Temp_Dir, $
            Mask_Suffix       = '_nonveg', $
            BaseName          = 'NonVeg', $
            pEndMembers       = pEndMembers, $
            OVERWRITE         = 0, $            ; don't overwrite existing intermediate results
            /KEEP                               ; keep all intermediate results

  if oStatus->Check('Ok') then $
    oStatus->Log_Status, 'Request', 'You can view the log file at: '''+Output_Dir+ $
      ''' after closing the status window.'
end