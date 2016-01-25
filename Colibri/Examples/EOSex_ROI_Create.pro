;+
; NAME:
; 
;   EOSex_ROI_CREATE
;
; PURPOSE:
; 
;   This example procedure demonstrates the use of the class method "EOSocImages::Add_New_ROI" for creating 
;   ROIs.
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
;   EOSex_ROI_CREATE                  
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
pro EOSex_ROI_CREATE
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

  ;--- Directory where to store the ROI files.
  Roi_Dir = ENVI_PICKFILE(/MULTIPLE_FILES, /DIRECTORY, $
    TITLE='Select the directory where the ROI files have to be stored.')+'\'

  if ~file_test(Roi_Dir, /DIRECTORY) then return
  
  ;---------------------------------
  ;--- Create the status window. ---
  ;---------------------------------
      
  oStatus = IOScc_Init(oStatus, $
               Title = 'ROI_CREATE_DEMO', $
               Log_Directory     = Roi_Dir, $
               Log_File_Prefix   = 'ROI_CREATE_DEMO_log')

  ;---------------------------------------------------------
  ;--- Creation and Initialization of the oImages object ---
  ;---------------------------------------------------------

  oImages = obj_new('EOSocImages', $
                pImageList, $ 
                oStatus = oStatus)

  ;----------------------------------------------------
  ;--- Check if the images were successfully opened ---
  ;----------------------------------------------------

  if ~oStatus->Check('Ok') then return

  ;--------------------
  ;--- ROI Handling ---
  ;--------------------

  ;-- Remove and delete all ROIs in from the specified ROI directory.
  oImages->Delete_Roi, Roi_Dir=Roi_Dir, /All

  ;-- Use the first image 'KH_1' to demonstrate the usage of ROIs.
  ;-- Create a single pixel ROI at sample/line location 10/20, the ROI name is 'Demo1' and color is white.
  oImages->Add_New_Roi, (*pImageList)[0], 'Demo1', '2', Samples=10, Lines=10, Roi_dir=Roi_Dir
  
  ;-- Create a 3x3 pixels ROI at sample/line location 20/20, the ROI name is 'Demo2' and color is red.
  oImages->Add_New_Roi, (*pImageList)[0], 'Demo2', '3', Samples=20, Lines=10, Radius=1, Roi_dir=Roi_Dir

  ;-- Create a 7x7 pixels ROI at sample/line location 20/20, the ROI name is 'Demo3' and color is red.  
  oImages->Add_New_Roi, (*pImageList)[0], 'Demo3', '4', Samples=30, Lines=10, Radius=3, Roi_dir=Roi_Dir

  ;-- Create a 5 pixels ROI at the samples and lines as defined, the ROI name is 'Demo4' and color is yellow.
  Samples = [39, 40, 40, 40, 41]
  Lines   = [10, 9, 10, 11, 10]
  oImages->Add_New_Roi, (*pImageList)[0], 'Demo4', '5', Samples=Samples, Lines=Lines, Roi_dir=Roi_Dir

  ;-----------------------------
  ;--- Image and ROI display ---
  ;-----------------------------
  
  ;-- < ENVI 5.0
  envi_open_file, (*pImageList)[0], r_fid=ImFid
  envi_display_bands, [ImFid, ImFid, ImFid], [2, 1, 0], /new
  ;-- < ENVI 5.0
  
  ;-- > ENVI 5.0 If you have ENVI 5.0 or higher you can uncomment the next statements.
;  e      = envi(/current)
;  raster = e.OpenRaster((*pImageList)[0])
;  view   = e.GetView()
;  layer  = view.CreateLayer(raster)
  ;-- > ENVI 5.0
  
  oStatus->Log_Status, 'Request', 'Image window > Overlay > Region of Interest...'
end