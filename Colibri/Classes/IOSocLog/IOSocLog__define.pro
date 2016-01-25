;+
; NAME:
; 
;   IOSocLog__define
;
; PURPOSE:
; 
;   his procedure defines the 'IOSocLog' class structure.
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
pro IOSocLog__define
;------------------------------------------------------------------------------------------------------------

  struct = { IOSocLog,  $
        ;
        Log_Directory:      '', $                       ; String holding the path to the log directory.
        Log_File_Prefix:    '', $                       ; Log file prefix to be used in the log filename.
        Log_Lun:            0l, $                       ; The IDL file unit associated to the log file.
        pLog_History:       ptr_new(/allocate_heap)}    ; Pointer to an array of strings holding the historical 
                                                        ; status info as long as no log file is open. Once the 
                                                        ; log directory is specified, the log file is created 
                                                        ; and opened and the historical status info is flushed
                                                        ; to the file.
end    