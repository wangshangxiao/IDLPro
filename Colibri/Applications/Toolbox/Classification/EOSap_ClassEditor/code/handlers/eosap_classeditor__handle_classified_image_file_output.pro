;+
; NAME:
; 
;   EOSap_ClassEditor::Handle_Classified_Image_File_Output
;
; PURPOSE:
; 
;   This procedure method is used to select the output file where the modified classified image has to be 
;   written.
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
;   Obj->[EOSap_ClassEditor::]handle_classified_image_file_output
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
pro EOSap_ClassEditor::Handle_Classified_Image_File_Output
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  default       = (strsplit(self.Class_Input_File, '.', /extract))[0]+'_Edt'
  Output_File   = envi_pickfile(title='Select directory for output', default=default, /output)

  if strlen(Output_File) le 0 then return

  widget_control, widget_info(self.wWidget, find_by_uname='tClassified_File_Output'),  $
    set_value=Output_File

  self.Class_Output_File = Output_File
  
  self.oStatus->Log_Status, 'Ok'  
end