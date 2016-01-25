;+
; NAME:
; 
;   IOSocStatus::Check
;
; PURPOSE:
; 
;   This method function returns to the requestor the status of the previous reported state. 
;   This allows the requestor to decide on the next processing step.
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
;   Result=Obj->[IOSocStatus::]Check [, (‘Ok’ | 'Error' | 'Warning' | 'Busy' | 'Request' | 'DontKnow')]
;
; ARGUMENTS:
;        
;   State:  A text string holding the process state which will be cheked. If it matches the 
;           state of the previous action true is returned else false is returned.
;                   
; KEYWORDS
; 
;   None
; 
; RETURN VALUE:
; 
;   The function method returns 1 if the passed state matches the state of the last reported status 
;   otherwise 0 is returned. If no or an unknown string value is specified 0 is returned.
; 
; KNOWN ISSUES:
; 
;   None.
;
; MODIFICATION HISTORY:
; 
;   - Written by Luc Bertels, May 2010.
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
function IOSocStatus::Check, State
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  if ~keyword_set(State) then State = ''
 
  if self.Status eq State then return,1 else return, 0
end