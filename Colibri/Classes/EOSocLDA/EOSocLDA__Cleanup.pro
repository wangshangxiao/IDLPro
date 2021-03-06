;+
; NAME:
; 
;   EOSocLDA::cleanup
;
; PURPOSE:
; 
;   Clean-up of object related data.
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
;   OBJ_DESTROY, Obj
;   
;   or
;   
;   Obj->[IOSocStatus::]Cleanup     (In a lifecycle method only.) 
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
;   - Written by Luc Bertels, November 2010.
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
pro EOSocLDA::cleanup
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if ptr_valid(self.paP2) then ptr_free, self.paP2
  if ptr_valid(self.pClassComb) then ptr_free, self.pClassComb
  if ptr_valid(self.pu_feat_x_Inv_Pool_A) then ptr_free, self.pu_feat_x_Inv_Pool_A
  if ptr_valid(self.pu_feat_x_Inv_Pool_B) then ptr_free, self.pu_feat_x_Inv_Pool_B
  if ptr_valid(self.pu_features) then ptr_free, self.pu_features
end