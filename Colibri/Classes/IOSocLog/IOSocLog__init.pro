;+
; NAME:
; 
;   IOSocLog::init
;
; PURPOSE:
; 
;   This method function creates and initializes an instance of the class 'IOSocLog'. The obtained object is used to
;   initialize and maintain the log file.
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
;   oLog  = obj_new('IOSocLog' [,LogInfo] [,LOG_DIRECTORY=string] [,LOG_FILE_PREFIX=string])
;
; ARGUMENTS:
;     
;   LogInfo:              A string or array of strings containing the info to be written to the log file.
;       
; KEYWORDS: 
; 
;   LOG_DIRECTORY:        A text string holding the path where the log file has to be written. If the 
;                         directory does not exist it will be created.
;   LOG_FILE_PREFIX:      A text string holding the file prefix of the log file to be created. If not 
;                         specified the default ‘Log’ prefix name is given to the log filename. The log 
;                         filename is automatically generated and contains the prefix, date and time, and 
;                         has the extension '.txt'.
;
; RETURN VALUE:
; 
;   The OBJ_NEW function returns an object reference to the newly-created object. 
;   
; EXAMPLE:
; 
;   Creating a log object with the default log file name written in the specified directory. Three lines are 
;   written to the newly created log file.
;
;     oLog = obj_new('IOSocLog', ['First line', 'Second line', 'Third line'], LOG_DIRECTORY='d:\')
;   
; KNOWN ISSUES:
; 
;   None
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
function IOSocLog::init, LogInfo, $
                         LOG_DIRECTORY    = Log_Directory, $
                         LOG_FILE_PREFIX  = Log_File_Prefix                        
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if ~keyword_set(LogInfo) then LogInfo = ''

  ;-----------------------------------------
  ;--- Save info if keywords are present ---
  ;-----------------------------------------

  if keyword_set(LOG_DIRECTORY) then begin
    
    self.Log_Directory = LOG_DIRECTORY
    
    if keyword_set(LOG_FILE_PREFIX) then begin
    
      self.Log_File_Prefix = LOG_FILE_PREFIX
    endif else begin
    
      self.Log_File_Prefix = 'Log'
    endelse
  endif  
  
  ;-----------------------------------------------------------------
  ;--- If present, write the Log information to the log-file ---
  ;-----------------------------------------------------------------
  
  self->ToLog, LogInfo, $
     LOG_DIRECTORY    = Log_Directory, $
     LOG_FILE_PREFIX  = Log_File_Prefix

  return, 1
end