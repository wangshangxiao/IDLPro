;+
; NAME: 
; 
;   IOScc_Init
;
; PURPOSE:
;  
;   This function will check if a status object, representing the status window, is existing. 
;   If no valid status window is present a new default status window is created.
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
;   Result = IOScc_INIT([oStatus] [,BITMAP=string] [, LOG_DIRECTORY=string] [, LOG_FILE_PREFIX=string] 
;     [, OUSER=object] [, TITLE=string])        
;     
; ARGUMENTS:
;   
;   oStatus:              The actual status object. If the argument is not present or the passed object doesn't 
;                         exist a new default status window will be created.
;                
; KEYWORDS:
; 
;   BITMAP:               A string holding the path to the user bitmap image file holding the logo to be 
;                         placed in the logo button on the status window. This keyword allows the user, when 
;                         designing his application, to replace the default logo on the status window with 
;                         the designers specific logo and to specify his own designer info bitmap. The file 
;                         holding the info bitmap should be named according to the file holding the logo 
;                         bitmap with the suffix ‘_info’ added. If not specified the default VITO logo is 
;                         used. When this VITO logo button is pressed the designer info will appear in a 
;                         separate window. This designer window can be closed by clicking in this window.
;   LOG_DIRECTORY:        A text string holding the directory location where the log-file should be written.
;   LOG_FILE_PREFIX:      A text string holding the prefix name for the log-file.
;   OUSER:                The user object which uses the status object. This is used for proper clean-up at
;                         status object termination time.
;   TITLE:                This keyword holds a text string to be put as title on the status window. If not 
;                         present the default title 'Status' is used.
;                                               
; RETURN VALUE:
;   
;   The actual or new created status object.
;
; DEPENDENCIES:
; 
;   IOSocStatus
;         
; KNOWN ISSUES:
; 
;   None
;   
; EXAMPLE:
; 
;   (1): Called from a main application which hasn't a valid status object.
;   
;     oStatus = IOScc_Init(TITLE = 'This is the apllication title', $
;                     Log_DIRECTORY    = 'D:/log/', $
;                     Log_FILE_PREFIX  = 'ThisLog')
;                         
;   (2): Called from another main object. The calling object id. is passed for proper clean-up at status
;        object termination time. If the oStatus argument contains a NULL object a new status object is 
;        created.
;     
;     self.oStatus = IOScc_Init(oStatus, $
;                          OUSER = self) 
;
; MODIFICATION HISTORY:
; 
;   - Written by Luc Bertels, February, 2012.
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
function IOScc_Init, $
                oStatus, $
                Title             = Title, $
                oUser             = oUser, $
                Log_Directory     = Log_Directory, $
                Log_File_Prefix   = Log_File_Prefix, $
                BitMap            = BitMap
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
 
  ;-- Create a status object if it is not yet existing.
  if ~keyword_set(oStatus) then $  
      oStatus = obj_new('IOSocStatus', $
                            Title = Title, $
                            Log_Directory   = Log_Directory, $
                            Log_File_Prefix = Log_File_Prefix, $ 
                            BitMap          = BitMap, $
                            oUser           = oUser, $
                            /STATUSSCREEN)
  
  return, oStatus
end