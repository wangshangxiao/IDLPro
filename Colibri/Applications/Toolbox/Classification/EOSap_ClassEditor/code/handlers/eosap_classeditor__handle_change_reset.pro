;+
; NAME:
; 
;   EOSap_ClassEditor::Handle_Change_Reset
;
; PURPOSE:
; 
;   This procedure method is used to reset class changes within the polygon to the original classes.
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
;   Obj->[EOSap_ClassEditor::]handle_change_reset
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
pro EOSap_ClassEditor::Handle_Change_Reset
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  if ~obj_valid(self.oClassImage) then return
  
  if ~ptr_valid(self.pPolygonIx) then begin
    self.oStatus->Log_Status, 'Warning', 'First draw polygon for area to be restored'
    return
  endif

  self.oStatus->Log_Status, 'Info', ' Original classes restored'

  (*self.pClassImage)[*self.pPolygonIx]  = (*self.pRGBdata)[*self.pPolygonIx]
    
  self.oImageViewer->Update_view, self.pClassImage
end
