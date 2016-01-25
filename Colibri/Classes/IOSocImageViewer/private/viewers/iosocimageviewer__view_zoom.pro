;+
; NAME:
; 
;   IOScocImageViewer::View_Zoom
;
; PURPOSE:
; 
;   This private procedure method is used to update the ZOOM window.
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
;   - Written by Luc Bertels, December 2012.
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
pro IOSocImageViewer::View_Zoom
;------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  self.oImage_Zoombox_Polygon->GetProperty, data=ZoomCoords

  ZoomCoords[1,*] = self.ImageInfo.nl - ZoomCoords[1,*] - 1

  Zoom        = (*self.pImage)[min(ZoomCoords[0,*]):max(ZoomCoords[0,*]), $
                               min(ZoomCoords[1,*]):max(ZoomCoords[1,*]), *]
  Zoom_ns     = float((size(Zoom))[1])
  Zoom_nl     = float((size(Zoom))[2])
        
  Zoom_xcoord_conv   = [0, 1./Zoom_ns] * self.Zoom_Dims[0]
  Zoom_ycoord_conv   = [0, 1./Zoom_nl] * self.Zoom_Dims[1]
  
  self.oZoom_Image->SetProperty, data = Zoom, interleave=[2], order=1, $
     xcoord_conv = Zoom_xcoord_conv, ycoord_conv = Zoom_ycoord_conv
     
  ;------------------------
  ;--- Show Zoom In Out ---
  ;------------------------
;
;  XCoords   = transpose([0,36,0,36,0,36,0,0,18,18,27,27,36,36]) 
;  YCoords   = transpose([18,18,9,9,0,0,18,0,18,0,18,0,18,0]) 
;
;  PolyLines = [2,0,1,2,2,3,2,4,5,2,6,7,2,8,9,2,10,11,2,12,13] 

  XCoords   = transpose([0,54,54, 0,0, 18,18, 36,36, 3,15, 21,33, 27,27, 44,46,46,44,44, 36,44, 46,54, $
                         45,45, 45,45]) 
  YCoords   = transpose([0, 0,18,18,0,  0,18,  0,18, 9, 9,  9, 9,  3,15,  8, 8,10,10, 8,  9, 9,  9, 9, $
                          0, 8, 10,18]) 

  PolyLines =           [5,0,1,2,3,4,  2,5,6, 2,7,8, 2,9,10, 2,11,12, 2,13,14, 5,15,16,17,18,19, $
                         2,20,21, 2,22,23, 2,24,25, 2,26,27]
  
  ;-----------------------
  ;--- Show cross hair ---
  ;-----------------------
  
  if self.ZoomCrossHair eq 1 then  begin

    dx = self.Zoom_Dims[0] / (Zoom_ns+1)
    dy = self.Zoom_Dims[1] / (Zoom_nl+1)
  
    XCoords_C = transpose([Zoom_ns/2*dx, (Zoom_ns/2+1)*dx, (Zoom_ns/2+1)*dx, Zoom_ns/2*dx, Zoom_ns/2*dx, $
    Zoom_ns/2*dx+dx/2, Zoom_ns/2*dx+dx/2, Zoom_ns/2*dx+dx/2, Zoom_ns/2*dx+dx/2, $
    0, Zoom_ns/2*dx, (Zoom_ns/2+1)*dx, self.Zoom_Dims[0]]) 
    YCoords_C = transpose([Zoom_nl/2*dy, Zoom_nl/2*dy, (Zoom_nl/2+1)*dy, (Zoom_nl/2+1)*dy, Zoom_nl/2*dy, $
    0, Zoom_nl/2*dy, (Zoom_nl/2+1)*dy, self.Zoom_Dims[1], $
    Zoom_nl/2*dy+dy/2, Zoom_nl/2*dy+dy/2, Zoom_nl/2*dy+dy/2, Zoom_nl/2*dy+dy/2]) 

    PolyLines_C = [5,28,29,30,31,32, 2,33,34, 2,35,36, 2,37,38, 2,39,40] 

    XCoords   = [[XCoords], [XCoords_C]]    
    YCoords   = [[YCoords], [YCoords_C]]   
    PolyLines = [PolyLines, PolyLines_C]
  endif
  
  self.oZoom_Zoom_In_Out_Polyline->SetProperty, data=[XCoords, YCoords], Color=[255,0,0], $
    polylines=Polylines, thick=1    

  ;---------------------
  ;--- Show polygons ---
  ;---------------------
  ZoomCoords[1,*] = self.ImageInfo.nl - ZoomCoords[1,*] - 1
  
  Zoom_xcoord_conv = [- min(ZoomCoords[0,*])/(max(ZoomCoords[0,*]) - min(ZoomCoords[0,*])), 1/(max(ZoomCoords[0,*]) - min(ZoomCoords[0,*]))] * self.Zoom_Dims[0]
  Zoom_ycoord_conv = [- min(ZoomCoords[1,*])/(max(ZoomCoords[1,*]) - min(ZoomCoords[1,*])), 1/(max(ZoomCoords[1,*]) - min(ZoomCoords[1,*]))] * self.Zoom_Dims[1]  
    
  if ptr_valid(self.pPolyLinesIx) then $
    self.oZoom_Vector_Polyline->SetProperty, $    
        data        = *self.pPolyLines, $
        vert_colors = *self.pPolyLinesColors, $
        polylines    = *self.pPolyLinesIx, $
        xcoord_conv = Zoom_xcoord_conv, $
        ycoord_conv = Zoom_ycoord_conv, $
        thick       = 1
                     
  self.oZoom->Draw,self.oZoom_View   
end