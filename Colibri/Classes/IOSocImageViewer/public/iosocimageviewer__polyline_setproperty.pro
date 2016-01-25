;+
; NAME:
; 
;   IOScocImageViewer::Polyline_SetProperty
;
; PURPOSE:
; 
;   This procedure method is used to add, clear or close a polyline to the view. 
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
;   Obj->[IOScocImageViewer::]Polygons_SetProperty [, ADDVERTICES=array] [, /CLEAR][, /CLOSE], [COLOR=vector] 
;
; ARGUMENTS: 
; 
;   None
; 
; KEYWORDS:
; 
;   ADDVERTICES:      Set this keyword to a 2-by-n array holding the sample and line locations of the polyline
;                     to be added to the view.
;   CLEAR:            Set this keyword to remove all existing polylines from the view.
;   CLOSE:            Set this keyword to close the existing polyline in order to obtain a polygon.
;   COLOR:            Set this keyword to a 3 element vector [red, green, blue] specifying the color used to 
;                     draw polylines.
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
pro IOSocImageViewer::Polyline_SetProperty, AddVertices=AddVertices, Clear=Clear, Close=Close, Color=Color
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  if ~ptr_valid(self.pImage) then return
  
  ;-- Remove any existing polylines from the view
  if keyword_set(CLEAR) and ptr_valid(self.pPolyLinesIx) then begin
  
    *self.pPolyLinesIx = [-1]

    if ptr_valid(self.pPolyLines)       then ptr_free, self.pPolyLines   
    if ptr_valid(self.pPolyLinesIx)     then ptr_free, self.pPolyLinesIx    
    if ptr_valid(self.pPolyLinesColors) then ptr_free, self.pPolyLinesColors 
  endif
  
  ;-- Add a new polyline to the view
  if keyword_set(ADDVERTICES) then begin

    if ~ptr_valid(self.pPolyLines) then self.pPolyLines = ptr_new(AddVertices) else $
      *self.pPolyLines = [[*self.pPolyLines], [AddVertices]]

    nVertices     = n_elements(*self.pPolyLines) / 2

    if ~ptr_valid(self.pPolyLinesIx) then self.pPolyLinesIx = ptr_new([nVertices, indgen(nVertices)]) else $  
      *self.pPolyLinesIx = [nVertices, indgen(nVertices)]
    if ~ptr_valid(self.pPolyLinesColors) then self.pPolyLinesColors = ptr_new(Color # (intarr(nVertices)+1)) else $
      *self.pPolyLinesColors = [Color # (intarr(nVertices)+1)]  
  endif 

  ;-- Close the polyline vertices to obtain a polygon
  if keyword_set(CLOSE) then begin
    if ~ptr_valid(self.pPolyLines) then return
    
    *self.pPolyLines        = [[*self.pPolyLines], [(*self.pPolyLines)[*,0]]]
    nVertices               = n_elements(*self.pPolyLines) / 2
    *self.pPolyLinesIx      = [nVertices, indgen(nVertices)]
    *self.pPolyLinesColors  = [[*self.pPolyLinesColors], [(*self.pPolyLinesColors)[*,0]]]
  endif
  
  self->Update_View 
end