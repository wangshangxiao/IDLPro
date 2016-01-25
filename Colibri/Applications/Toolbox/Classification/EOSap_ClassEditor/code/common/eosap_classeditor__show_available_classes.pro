;+
; NAME:
; 
;   EOSap_ClassEditor::show_available_classes
;
; PURPOSE:
; 
;   This procedure method is used to visualize the classes available in the classified image.
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
;   Obj->[EOSap_ClassEditor::]show_available_classes
;
; ARGUMENTS 
; 
;  None
; 
; KEYWORDS
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
; - Updated for the Colibri release, February 2013.
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
pro EOSap_ClassEditor::Show_Available_Classes
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  self.oClassImage->Get, nClasses     = nClasses, $
                         pLookup      = pLookup, $
                         pClass_name  = pClass_names, $
                         BandIx       = 0, $
                         /FULL, $
                         pBandImage   = pBandImage

  self.pLookUp              = pLookup
  self.pClassNames          = pClass_names
  self.pClassImage          = pBandImage
  
  if ptr_valid(self.pOriginalClassNames) then ptr_free, self.pOriginalClassNames
  
  self.pOriginalClassNames = ptr_new(*self.pClassNames)

  self->Update_Available_Classes
end