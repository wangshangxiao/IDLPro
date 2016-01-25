;+
; NAME:
; 
;   IOSocStatus::Progress_Clear
;
; PURPOSE:
; 
;   To reset the state of the status window. This means the progress bar is cleared, the status lamp is 
;   set to green and the info on the status line is cleared.
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
;   Obj->[IOSocStatus::]Progress_Clear [, /FAST] [, /RESET] [, /SLOW]
;
; ARGUMENTS:
;          
;   None
;                   
; KEYWORDS:
; 
;   FAST:             Set this keyword to clear the ‘fast’ progress bar. 
;   RESET:            Set this keyword to clear both the ‘fast’ and ‘slow’ progress bar, to clear the status 
;                     line and to set the LED status to green.
;   SLOW:             Set this keyword to clear the ‘slow’ progress bar.
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
;   - Added, LBer February 2013: Check presence of the progress bar widget
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
pro IOSocStatus::Progress_Clear, $
                  fast  = fast, $
                  slow  = slow, $
                  reset = reset
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  ;-- Clear status
  if keyword_set(reset) then self->log_status, 'Ok'                                                        ; LBer February 2013
  
  if ~widget_info(widget_info(self.wWidget, find_by_uname='Status_ProgressBar'), /valid_id) then return    ; LBer February 2013
  
  ;-- Clear Slow
  if keyword_set(reset) or keyword_set(slow) then begin
  
    WSet, self.wProgressBar

    x1 = 0
    x2 = self.Scr_Xsize
    y1 = 1
    y2 = 9
    Polyfill, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Device, Color=0d

    if self.ThisWindow ge 0 and self.ThisWindow ne self.wProgressBar then WSet, self.wProgressBar
  endif
  
  ;-- Clear Fast
  if keyword_set(reset) or keyword_set(fast) then begin
 
    WSet, self.wProgressBar

    x1 = 0
    x2 = self.Scr_Xsize
    y1 = 11
    y2 = 19
    Polyfill, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Device, Color=0d

    if self.ThisWindow ge 0 and self.ThisWindow ne self.wProgressBar then WSet, self.ThisWindow
  endif
end