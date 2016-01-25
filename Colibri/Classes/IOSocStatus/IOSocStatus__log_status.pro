;+
; NAME:
; 
;   IOSocStatus::Log_Status
;
; PURPOSE:
; 
;   To update the status information in the status window and to write the new status to the log file 
;   if present.
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
;   Obj->[IOSocStatus::]Log_Status [, State] [, Info] [, Module] [, DIRECTORY=string] [, FILE_PREFIX=string] 
;     [, /LOG]
;
; ARGUMENTS:         
;                       
;   State:               A string holding the actual state to which the status window has to be updated. The 
;                        LED colour and the prefix of the reported info is set according to the reported 
;                        state. Following states can be reported: 
;                             - 'Ok':       Lamp = green,  prefix = ''
;                             - 'Error':    Lamp = red,    prefix = '*** ERROR: '
;                             - 'Warning':  Lamp = orange, prefix = '** WARNING: '
;                             - 'Busy':     Lamp = yellow, prefix = '* INFO: '
;                             - 'Request':  Lamp = blue,   prefix = 'REQUEST: '
;                             - 'DontKnow': Lamp = dark,   prefix = '???: '
;                        If no arguments are specified the default ‘DontKnow’ is reported.  
;   Info:                A string holding additional information about the new state. This information is 
;                        reported on the status line.
;   Module:              A string holding the name of the calling procedure or function.     
;                   
; KEYWORDS:
;
;   LOG:                  Set this keyword to write the actual status information to the log file.
;   DIRECTORY:            A text string holding the path where the log file has to be written. If specified 
;                         the log file is created and opened.
;   FILE_PREFIX:          A text string holding the file prefix of the log file to be created. If not 
;                         specified a default prefix is given to the log filename. The log filename is 
;                         automatically generated and contains the prefix, date and time, and the 
;                         extension '.txt'.
;
; DEPENDENCIES:
; 
;   OSIocLog::ToLog
;       
; KNOWN ISSUES:
; 
;   None
;
; MODIFICATION HISTORY:
; 
;   - Written by Luc Bertels, May 2010.
;   - Modified LBer, January 2013: wrong keyword was passed.
;   - Added, LBer, February 2013: Status Screen added.
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
pro IOSocStatus::Log_Status, $
                    State, $
                    Info, $
                    Module, $
                    Directory   = Directory, $
                    File_Prefix = File_Prefix, $
                    Log = Log
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if ~keyword_set(State)  then State = 'DontKnow'
  if ~keyword_set(Module) then Module = ''
  if ~keyword_set(Info)   then Info = '' else Info = strjoin(string(Info))

  LogInfo     = Module+' - '+Info
  self.Status = State
  LedValue    = '_LampG.bmp'    
      
  case State of
    'Ok': begin      
      if strlen(Info) gt 0 then Info      = '*   INFO: '+LogInfo   
      TextColor         = [0, 255, 0]                                   ; LBer February, 2013
      BackgroundColor   = [120,120,120]                                 ;
    end  
    'Error': begin
      Info      = '*** ERROR: '+LogInfo 
      LedValue  = '_LampR.bmp'
      TextColor         = [255, 0, 0]                                   ; LBer February, 2013
      BackgroundColor   = [220,220,220]                                 ;
    end
    'Warning': begin
      Info      = '**  WARNING: '+LogInfo 
      LedValue  = '_LampO.bmp'
      TextColor         = [255,100, 50]                                 ; LBer February, 2013
      BackgroundColor   = [220,220,220]                                 ;
    end    
    'Busy': begin
      Info      = '*   INFO: '+LogInfo
      LedValue  = '_LampY.bmp'
      TextColor         = [255,255, 0]                                  ; LBer February, 2013
      BackgroundColor   = [120,120,120]                                 ;
    end
    'Request': begin
      Info      = 'REQUEST: '+LogInfo
      LedValue  = '_LampB.bmp'
      TextColor         = [0, 0, 255]                                   ; LBer February, 2013
      BackgroundColor   = [220,220,220]                                 ;
    end
    'DontKnow': begin
      Info      = '??? '+LogInfo
      LedValue  = '_LampD.bmp'
      TextColor         = [0,0, 100]                                    ; LBer February, 2013
      BackgroundColor   = [220,220,220]                                 ;
    end    
    else:
  endcase
   
  widget_control, self.wStatusLed, set_value = LedValue , /bitmap
  
  if widget_info(self.wStatusScreen, /valid_id) then begin                ; LBer February, 2013
                                                                          ;
    self.oText_Status_Screen->SetProperty, strings=Info, color=TextColor  ;
    self.oView_Status_Screen->SetProperty, color=BackgroundColor          ;
    self.oWindow_Status_Screen->Draw, self.oView_Status_Screen            ;
  endif
  
  if widget_info(self.wStatusLine, /valid_id) then $
    widget_control, self.wStatusLine, set_value = Info
  
  ;-- If requested pass the status info to the Log object.
  if keyword_set(Directory) or keyword_set(Log) then $    
    self->ToLog, $
            Info, $
            Log_Directory    = Directory, $     ; LBer january 2013
            Log_File_Prefix  = File_Prefix      ; LBer january 2013       
end