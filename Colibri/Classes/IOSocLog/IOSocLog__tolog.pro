;+
; NAME:
; 
;   IOSocLog::ToLog
;
; PURPOSE:
; 
;   This procedure writes log information to the log file. If requested the log file is created.
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
;   Obj->[IOSocStatus::]ToLog, [LogInfo] [,FORMAT=string] [,LOG_DIRECTORY=string] [,LOG_FILE_PREFIX=string]
;
; ARGUMENTS:
;   LogInfo:              A string or array of strings containing the info to be written to the log file.
;      
; KEYWORDS:
;
;   FORMAT:               Allows the format of the output to be specified in precise detail using a 
;                         FORTRAN-style specification.  
;   LOG_DIRECTORY:        A text string holding the path where the log file has to be written. If specified 
;                         the log file is created and opened.
;   LOG_FILE_PREFIX:      A text string holding the file prefix of the log file to be created. If not 
;                         specified a default prefix is given to the log filename. The log filename is 
;                         automatically generated and contains the prefix, date and time, and the 
;                         extension '.txt'.
;
; RETURN VALUE:
; 
;   None
;   
; DEPENDENCIES:
; 
;   Method: ToLog
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
pro IOSocLog::ToLog, LogInfo, $
                     LOG_DIRECTORY    = Log_Directory, $
                     LOG_FILE_PREFIX  = Log_File_Prefix, $
                     FORMAT           = Format
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  if ~keyword_set(LogInfo) then LogInfo = ''
  
  if self.Log_Lun ne 0 then begin
  
    ;----------------------
    ;--- LogFile exists ---
    ;----------------------
    
    if keyword_set(Format) then begin
      printf, self.Log_Lun, format=Format, LogInfo

    endif else begin
    
      for Ix=0, n_elements(LogInfo)-1 do begin
    
        ;-- Write Log info to log-file.
    
        printf, self.Log_Lun, logInfo[Ix]
      endfor
    endelse
    
    return
  endif 
    
  ;--------------------------------------------------------------
  ;--- LogFile does not exist, but log-directory is specified ---
  ;--------------------------------------------------------------

  if keyword_set(Log_Directory) then begin
    
    self.Log_Directory = Log_Directory
    
    if keyword_set(Log_File_Prefix) then begin
    
      self.Log_File_Prefix = Log_File_Prefix
    endif else begin
    
      self.Log_File_Prefix = 'Log'
    endelse
  endif
  
  ;-------------------------------------
  ;--- Create and open the log-file  ---
  ;-------------------------------------

  if keyword_set(Log_Directory) then begin

    if file_test(self.Log_Directory, /directory) eq 0 then $
                                        file_mkdir, self.Log_Directory

    LogFile = self.Log_Directory + self.Log_File_Prefix + $
                '_'+strjoin(strsplit(systime(), ' :', /extract), '_')+'.txt'
    
    if self.Log_Lun gt 0 then free_lun, self.Log_Lun
      
    openw, lun, LogFile, /get_lun
  
    self.Log_Lun = lun
  
    ;------------------------------------------------------------------
    ;--- If existing, write historical Log info to the log-file ---
    ;------------------------------------------------------------------
  
    if ptr_valid(self.pLog_History) then begin
    
      for Ix=0, n_elements(*self.pLog_History)-1 do begin
      
        printf, self.Log_Lun, (*self.pLog_History)[Ix]
      endfor
    endif
      
    self->ToLog, LogInfo
  
    return
  endif 
    
  ;---------------------------------------------------------------------
  ;--- No log-file exist, save Log info until logfile is created ---
  ;---------------------------------------------------------------------
    
  if ptr_valid(self.pLog_History) then begin
    
    *self.pLog_History = [*self.pLog_History, LogInfo]
  endif else begin
        
    self.pLog_History = ptr_new(LogInfo)
  endelse
end