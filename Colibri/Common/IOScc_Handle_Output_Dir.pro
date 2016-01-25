;+
; NAME:
; 
;   EOSap_Smoothing::IOScc_Handle_Output_Dir
;
; PURPOSE:
; 
;   This common function is used to locate the output directory to be used by an application.
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
;   IOScc_Handle_Output_Dir [, DIRNAME=string] [, FPATH=string] [, OSTATUS=object reference] 
;
; ARGUMENTS 
; 
;   None
; 
; KEYWORDS
; 
;   DIRNAME:        Set this keyword to a string holding the title to be put on the dialog requesing for the image 
;                   directory. This string is presed by the string 'Select '. The default string is 'directory'.
;   FPATH:          When valid directory was selected this keyword will hold a string containing the full path to the 
;                   selected directory.
;   OSTATUS:        An object reference to an existing status window.
; 
; RETURN VALUE:
; 
;   If a directory was selected 1 is returned els 0 is returned
;
; DEPENDENCIES:
; 
;   None.
; 
; KNOWN ISSUES:
; 
;   None.
;
; MODIFICATION HISTORY:
; 
; - Written by Luc Bertels, June 2010.
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
;-------------------------------------------------------------------------
function IOScc_Handle_Output_Dir, $
            DirName     = DirName, $
            Fpath       = Fpath, $ 
            oStatus     = oStatus
;-------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Module = 'IOScc_Handle_Output_Dir'
  
  if ~keyword_set(DirName) then DirName = 'directory'
 
  Title = 'Select '+DirName

  if obj_valid(oStatus) then oStatus->Log_Status, 'Request', Title
 
  Fpath = dialog_pickfile(/directory, Title=Title)

  if strlen(Fpath) eq 0 then begin
    if obj_valid(oStatus) then oStatus->Progress_clear, /RESET
    return, 0
  endif else begin
    if obj_valid(oStatus) then $
      oStatus->Log_Status, 'Ok', DirName + ' selected: '+Fpath,  Module, /Log
    
    return, 1
  endelse
end