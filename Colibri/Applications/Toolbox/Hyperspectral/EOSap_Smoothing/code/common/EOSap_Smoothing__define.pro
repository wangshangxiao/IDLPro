;+
; NAME:
; 
;   EOSap_Smoothing__define
;
; PURPOSE:
; 
;   This procedure method defines the 'EOSap_Smoothing' class structure.
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
; ARGUMENTS 
; 
;   None
; 
; KEYWORDS
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
;------------------------------------------------------------------------
pro EOSap_Smoothing__define
;------------------------------------------------------------------------

    struct = { EOSap_Smoothing, $
                        wWidget:              0l, $
                        ;
                        oStatus:              obj_new(), $
                        oImageViewer:         obj_new(), $
                        oProfileViewer:       obj_new(), $
                        oImage_ForSmoothFac:  obj_new(), $
                        oImages_2bSmoothed:   obj_new(), $
                        ;
                        InDir:                ' ', $
                        OutDir:               ' ', $
                        pFileList:            ptr_new(/allocate_heap), $
                        ;
                        InFile:               ' ', $
                        InFid:                0l, $                        
                        InNs:                 0, $
                        InNl:                 0, $
                        InNb:                 0, $
                        SmFac:                0, $
                        NotSelected:          0, $
                        pSpectrum:            ptr_new(/allocate_heap), $                        
                        ;
                        pWl:                  ptr_new(/allocate_heap), $
                        pSmoothFac:           ptr_new(/allocate_heap), $
                        pScenario:            ptr_new(/allocate_heap)}
end