;+
; NAME:
; 
;   EOSap_Smoothing::init
;
; PURPOSE:
; 
;   This function method creates and initializes an instance of the class 'EOSap_Smoothing'. The obtained 
;   object is used to maintain the smoothing process.
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
;   Result = OBJ_NEW('EOSap_Smoothing', wWidget)
;
; ARGUMENTS 
; 
;  wWidget:              Base widget ID of the "Spectral Smoohing" Graphical User Interface.
; 
; KEYWORDS
; 
;   None
; 
; RETURN VALUE:
; 
;   The OBJ_NEW function returns an object reference to the newly-created object.
;
; DEPENDENCIES:
; 
;   IOSocStatus
;   IOSocImageviewer
;   
; KNOWN ISSUES:
; 
;   None.
;   
; REFERENCE
; 
;   The Interactive Spectral Smoothing (ISS) algorithm, L. Bertels, Paper in preparation.
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
;------------------------------------------------------------------------------------------------------------
function EOSap_Smoothing::init, wWidget
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

    self.wWidget  = wWidget
 
    oStatus         = obj_new('IOSocStatus',  wWidget)
    oImageViewer    = obj_new('IOSocImageViewer',   wWidget, oStatus=oStatus, oUser=self)
    oProfileViewer  = obj_new('IOSocProfileViewer', wWidget, oStatus=oStatus, oUser=self, $
                      xTitle='wavelength', yTitle='reflectance')

    self.oStatus        = oStatus
    self.oImageViewer   = oImageViewer
    self.oProfileViewer = oProfileViewer
    
    return, 1
end