;+
; NAME:
; 
;   EOSex_Classify
;
; PURPOSE:
; 
;   This example procedure demonstrates the use of the application "EOSap_Classify" for classification of a
;   set of images.
;   
;                       **************************
;                       *** Under construction ***  
;                       **************************
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
;   EOSex_Classify               
;
; ARGUMENTS
;
; KEYWORDS
;                      
; DEPENDENCIES:
; 
;   This demo uses the test images located in sub-folder: <Download Dir:>\Colibri\Examples\data
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
pro EOSex_Classify
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
 
; **************************
; *** Under construction ***  
; **************************
;  
  ;------------------------------------
  ;--- Define default data for test ---
  ;------------------------------------
  
  ;--- Process the example data located in the examples data directory.
  Path = ENVI_PICKFILE(/DIRECTORY, $
    TITLE='Locate the directory: ''<DIR:>\Colibri\Examples\Data''') 

  if ~file_test(Path, /DIRECTORY) then return
  
  ;--- Pointer to the example images to be used for demo.
  pImageList   = ptr_new([Path+'\DigiCam_1', $
                          Path+'\DigiCam_2', $
                          Path+'\DigiCam_3']) 

  if ~file_test(Path+'\DigiCam_1') then begin
  
    ok = dialog_message('No valid images were found.', Title='EOSex_Classify', /error)
    return  
  endif

  Class_Dir   = ENVI_PICKFILE(/DIRECTORY, $
    TITLE='Locate the directory where to store the output results.')

  if ~file_test(Class_Dir, /DIRECTORY) then return else Class_Dir += '\'

  Class_method  = 'LDA'
  OVERWRITE     = 1 
  TIFF          = 1

  EOSap_Classify, $
            pImageList, $
            Class_method, $
            Class_Dir, $
            oStatus               = oStatus, $  
            BandsIx               = BandsIx, $     
            pLibraries            = pLibraries, $
            aClass_Names          = aClass_Names, $
            aCLass_Colors         = aCLass_Colors, $
            Rule_Dir              = Class_Dir       

  oStatus->Log_Status, 'Request', 'You can view the log file at: '''+Class_Dir+ $
      ''' after closing the status window.'
    
end