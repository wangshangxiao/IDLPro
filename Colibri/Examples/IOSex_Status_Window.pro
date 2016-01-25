;+
; NAME:
; 
;   IOSex_STATUS_WINDOW
;
; PURPOSE:
; 
;   This example procedure demonstrates the use of the class "IOSocStatus" for creating and managing the 
;   status window.
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
;   IOSex_STATUS_WINDOW [, /STATUSSCREEN]                  
;
; ARGUMENTS
;
; KEYWORDS
;              
;   STATUSSCREEN:       Set this keyword to display the info in a Status Screen window instead of on the            
;                       Status text widget.
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
;   - Written by Luc Bertels, October, 2012.
;   - Updated, LBer, February 2013, Status Screen switch added.
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
pro IOSex_STATUS_WINDOW, StatusScreen=StatusScreen
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  Module = 'IOSex_STATUS_WINDOW'

  ;-- Define output directory
  Log_Dir = ENVI_PICKFILE(/DIRECTORY, TITLE='Locate the directory where to log file needs to be written.')

  if ~keyword_set(Log_Dir) then return else Log_Dir += '\' 

  ;-- Create the status object. A log file will be written to the test directory on the 'D' drive.
  ;-- If the directory is not existing it will be created automatically.
  oStatus = OBJ_NEW('IOSocStatus', LOG_DIRECTORY=Log_Dir, LOG_FILE_PREFIX='LogDemo', $
    Title='Status Window Demo', StatusScreen=StatusScreen)                                ; LBer, February 2013

  ;-- Set the status running.
  oStatus->Log_Status, 'Busy', 'Processing started.', Module, /LOG
  
  ;-- Initiate the slow loop.
  nSlowLoops = 10
  
  for X=0, nSlowLoops-1 do begin

    oStatus->Log_Status, 'Busy', '  Entering slow loop: '+strtrim(string(X+1),2)+' of '+ $ 
      strtrim(string(nSlowLoops),2), Module, /LOG

    ;-- Report the slow progress.
    oStatus->Progress_set, SLOW=[X, nSlowLoops] 
  
    ;-- Initiate the fast loop.
    nFastLoops = 100000L
  
    for Y=0L, nFastLoops do begin
  
      ;-- Report the fast progress.
      oStatus->Progress_set, FAST=[Y, nFastLoops]
      wait, 0.0001  ; wait 0.1 ms to simulate a certain action and allow visualisation of the progress.
    endfor
    
    ;-- clear the fast progress bar.
    ostatus->progress_clear, /FAST
  endfor   

  wait, 1 ; wait 1 s before resetting the status.
  
  ;-- Set the status running.
  oStatus->Log_Status, 'Busy', 'Processing ended.', Module, /LOG
  
  ;-- reset the status.
  oStatus->Progress_clear, /RESET
  
  ;-- Post last message
  oStatus->Log_Status, 'Request', 'You can view the log file at: '''+Log_Dir+ $
      ''' after closing this status window.'
end


