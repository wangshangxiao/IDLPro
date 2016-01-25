;+
; NAME:
; 
;   IOSocStatus__define
;
; PURPOSE:
; 
;   This procedure defines the 'IOSocStatus' class structure.
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
;   None
;
; ARGUMENTS: 
; 
;   None
; 
; KEYWORDS:
; 
;   None
; 
; RETURN VALUE:
; 
;   None 
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
pro IOSocStatus__define
;------------------------------------------------------------------------------------------------------------

  struct = { IOSocStatus,  $
              ;
              wWidget:              0l, $         ; Base widget_id of main GUI or the default status window.
              ;
              wStatusLed:           0l, $         ; Button widget_id of the status LED.
              wStatusLine:          0l, $         ; Text widget_id of the status line.
              wStatusScreen:        0l, $         ; Draw widget_id for the IDLgrText status info.
              wProgressBar:         0l, $         ; Draw widget_id of the progress bar.
              ;
              oWindow_Status_Screen: obj_new(), $ ; Object for the status screen window.
              oView_Status_Screen:  obj_new(), $  ; Object for the status screen view.
              oText_Status_Screen:  obj_new(), $  ; Object for the status screen text.
              ;
              ThisWindow:           0l, $         ; The index of the currently open window. 
              Scr_Xsize:            0l, $         ; X-size ot the progress bar.
              ;
              pobjects:             ptr_new(/allocate_heap), $ ; Used to save the user object ids.
              ;
              Status:               '', $         ; Holds the actual status.
              ;
              Inherits              IOSocLog $    ; Log file handling.
              }      
end 