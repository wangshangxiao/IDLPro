;+
; NAME: 
; 
;   EOSocLibraries::init
;
; PURPOSE:
; 
;   This function initializes the 'Libraries' object using the class 'EOSocLibraries'. The object is used to 
;   extract spectral libraries from a set of images using accompanying ROIs. The library files are created 
;   per unique ROI name. These library files are text files and contain the pixel spectra from each pixel in 
;   the ROIs. They are named according the unique ROI names and have the extension '.lib'.  
;   Their general layout is as follows:
;   First Line:       The RGB color associated with the ROI.
;   Second Line:      The number of bands the spectra contain.
;   Remaining Lines:  First column:       File name from which the spectra are extracted.
;                     Second column:      The ROI id
;                     Remaining columns:  The floating point pixel spectrum 
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
;   Result = OBJ_NEW('EOSocLibraries', LIBDIR=string [,OSTATUS=object reference], PIMAGELIST=pointer, 
;     PROILIST=pointer)
;
; ARGUMENTS 
;         
;   None.
;                     
; KEYWORDS
;
;   LIBDIR:         A string holding the directory where the libraries have to be written.
;   OSTATUS:        An object reference to an existing status window.
;   PIMAGELIST:     A pointer to an array of strings holding the path and name of the images from which the
;                   spectral libraries have to be extracted.
;   PROILIST:       A pointer to an array of strings holding the path and name of the ROI files associated 
;                   with the images. These ROIs are used to extract the spectral profiles from the 
;                   corresponding pixels in order to generate the spectral libraries.
; 
; RETURN VALUE:
; 
;   The OBJ_NEW function returns an object reference to the newly-created object.
;
; DEPENDENCIES:
; 
;   IOScc_Init
;       
; KNOWN ISSUES:
; 
;   None
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
function EOSocLibraries::init,$
                      pImageList = pImageList, $
                      pRoiList   = pRoiList, $
                      LibDir     = LibDir, $
                      oStatus    = oStatus
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Module    = 'EOSocImages::init'
  Argument  = ''

  ;----------------------------------------------------------
  ;--- If no status object is present, create the default ---
  ;----------------------------------------------------------

  self.oStatus = IOScc_Init(oStatus, oUser=self) 
 
  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------
  
  if ~keyword_set(LibDir)     then Argument = 'LibDir'  
  if ~keyword_set(pRoiList)   then Argument = 'pRoiList'  
  if ~keyword_set(pImageList) then Argument = 'pImageList'  
      
  if keyword_set(Argument) then begin
  
    self.oStatus->Log_Status, 'Error', 'Missing argument: '+Argument, Module, /Log              
    return, 0
  endif
  
  if ~ptr_valid(pImageList) then begin
    self.oStatus->Log_Status, 'Error', 'Invalid pointer: '+Argument, Module, /Log              
    return, 0
  endif  

  ;------------
  ;--- Init ---
  ;------------
  
  if file_test(LibDir, /directory) eq 0 then file_mkdir, LibDir

  ;------------------
  ;--- Processing ---
  ;------------------
  
  self->Extract_Libraries_From_Roi, $
                              pImageList, $
                              pRoiList, $
                              LibDir  
                               
  if ~self.oStatus->Check('Ok') then return, 0 else return, 1
end