;+
; NAME:
; 
;   EOSap_ClassEditor::Handle_To_Class
;
; PURPOSE:
; 
;   This procedure method is called to handle events that occured when the color bar in the 'To New Output 
;   Class' listbox is clicked.
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
;   Obj->[EOSap_ClassEditor::]handle_to_class
;
; ARGUMENTS 
; 
;   event:        A structure holding the event information.
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
pro EOSap_ClassEditor::Handle_To_Class, Event
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate


  wWidget = self.wWidget

  i = where(*self.pWid_Available_Classes eq event.id, c)

  self.Selected_Available_Id    = i

  widget_control, widget_info(wWidget, find_by_uname='tChanged_To'), $
    set_value = (*self.pClassNames)[i]

  self.oStatus->Log_Status, 'Ok', '   Class to change to = '+(*self.pClassNames)[self.Selected_Available_Id]

end