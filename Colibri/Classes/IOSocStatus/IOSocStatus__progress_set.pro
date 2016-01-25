;+
; NAME:
; 
;   IOSocStatus::Progress_Set
;
; PURPOSE:
; 
;   To set the state of the status window. This means the progress bar is set to the values specified, 
;   the status lamp is set to green and the info on the status line is cleared.
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
;   Obj->[IOSocStatus::]Progress_Set [,FAST=[fraction, total]] [,SLOW=[fraction, total]]
;
; ARGUMENTS:
;          
;   None
;                   
; KEYWORDS:
; 
;   FAST:             Set this keyword to an array holding the fraction of the total size and the total size 
;                     of the progress loop to be shown on the fast-progress bar.
;   SLOW:             Set this keyword to an array holding the fraction of the total size and the total size 
;                     of the progress loop to be shown on the slow-progress bar.
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
pro IOSocStatus::Progress_Set, $
                  fast = fast, $
                  slow = slow
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if ~widget_info(widget_info(self.wWidget, find_by_uname='Status_ProgressBar'), /valid_id) then return    ; LBer February 2013

  ;-- Set Slow
  if keyword_set(slow) then begin
    
    ;-- To avoid unnecessary delay due to progress bar update
    if slow[1] gt 1000L then slow = slow / (slow[1] / 1000d)
      if (slow[0] mod (1)) ne 0 then return
        
    WSet, self.wProgressBar

    x1 = 0
    x2 = fix(round(self.Scr_Xsize * (Slow[0]+1) / Slow[1]))
    y1 = 1
    y2 = 9
    Polyfill, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Device, Color=16711680d

    if self.ThisWindow ge 0 and self.ThisWindow ne self.wProgressBar then WSet, self.ThisWindow
  endif
  
  ;-- Set Fast
  if keyword_set(fast) then begin

    ;-- To avoid unnecessary delay due to progress bar update
    if fast[1] gt 1000L then fast = fast / (fast[1] / 1000d)
      if (fast[0] mod (1)) ne 0 then return
      
    WSet, self.wProgressBar

    x1 = 0
    x2 = fix(round(self.Scr_Xsize * (Fast[0]+1) / Fast[1]))
    y1 = 11
    y2 = 19
    Polyfill, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Device, Color=39168d

    if self.ThisWindow ge 0 and self.ThisWindow ne self.wProgressBar then WSet, self.ThisWindow
  endif    
end