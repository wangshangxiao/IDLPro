;+
; NAME:
; 
;   EOSap_ClassEditor::Handle_Classified_Image_File_Input
;
; PURPOSE:
; 
;   This procedure method is used to select the classified image which has to be edited.
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
;   Obj->[EOSap_ClassEditor::]handle_classified_image_file_input
;
; ARGUMENTS 
; 
;   None
; 
; KEYWORDS
; 
;   None
;
; DEPENDENCIES:
;   
; KNOWN ISSUES:
; 
;   None.
;   
; MODIFICATION HISTORY:
; 
; - Written by Luc Bertels, June 2010.
; - Updated for the Colibri release, February 2013.
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
pro EOSap_ClassEditor::Handle_Classified_Image_File_Input
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Module = 'EOSap_ClassEditor::Handle_Classified_Image_File_Input'

  Class_Input_File  = envi_pickfile(title='Select classified image file for editing')

  if strlen(Class_Input_File) le 0 then return

  if ~file_test(Class_Input_File) then begin
    self.oStatus->Log_Status, 'Error', 'File not found:'+Class_Input_File, Module, /LOG
    return
  endif

  if obj_valid(self.oClassImage) then obj_destroy, self.oClassImage

  self.oClassImage = obj_new('EOSocImages', $
                ptr_new(Class_Input_File), $ 
                oStatus = self.oStatus)  

  widget_control, widget_info(self.wWidget, find_by_uname='tClass_Input_File'), set_value=Class_Input_File

  self.oClassImage->get, pRGBdata=pRGBdata, pMapInfo=pMapInfo, pLookup=pLookup, nl=nl, File_Type=File_Type
  
  if File_Type ne 3 then begin
    
    self.oStatus->Log_Status, 'Error', 'Not a classified image: '+Class_Input_File
    obj_destroy, self.oClassImage
    return
  endif
  
  self.oImageViewer->Update_view, pRGBdata, pMapInfo, pLookup=pLookup, /NEW
  self->Show_Available_Classes
  
  self.nl               = nl
  self.pRGBdata         = pRGBdata
  self.Class_Input_File = Class_Input_File
  
  self.oStatus->Log_Status, 'Ok'
end