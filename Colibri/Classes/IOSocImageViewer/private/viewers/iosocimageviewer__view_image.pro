;+
; NAME:
; 
;   IOScocImageViewer::View_Image
;
; PURPOSE:
; 
;   This private procedure method is used to update the IMAGE window.
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
;   Obj->[IOScocImageViewer::]View_Image
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
pro IOSocImageViewer::View_Image
;------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
                           
  self.oScroll_Zoombox_Polygon->GetProperty, data=ImageCoords
  
  i = where(ImageCoords[0,*] ge self.ImageInfo.ns, C)
  if C gt 0 then ImageCoords[0,i] = self.ImageInfo.ns-1
  
  ImageCoords[1,*] = self.ImageInfo.nl - ImageCoords[1,*] - 1
 
  Image       = (*self.pImage)[min(ImageCoords[0,*]):max(ImageCoords[0,*]), $
                               min(ImageCoords[1,*]):max(ImageCoords[1,*]), *]
  Image_ns    = float((size(Image))[1])
  Image_nl    = float((size(Image))[2])
      
  xConv   = [0, 1./Image_ns] * self.Image_Dims[0]
  yConv   = [0, 1./Image_nl] * self.Image_Dims[1]
  
  self.oImage_Image->SetProperty, data = Image, interleave=[2], order=1, $
     xcoord_conv = xConv, ycoord_conv = yConv
 
  ;--------------------------
  ;--- View image zoombox ---
  ;--------------------------

  Image_ns = max(ImageCoords[0,*]) - min(ImageCoords[0,*])
  Image_nl = max(ImageCoords[1,*]) - min(ImageCoords[1,*])

  ;-- Define scroll zoombox size depending on the image draw size.
  if float(self.Image_dims[0]) / self.Zoom_dims[0] le float(self.Image_dims[1]) / self.Zoom_dims[1] then begin
  
    Zoom_ns = (Image_ns /  self.Zoom_Requested_Zfac)
    Zoom_nl = (Zoom_ns / self.Zoom_dims[0]) * self.Zoom_dims[1]
  endif else begin
  
    Zoom_nl = (Image_nl / self.Zoom_Requested_Zfac) 
    Zoom_ns = (Zoom_nl / self.Zoom_dims[1]) * self.Zoom_dims[0]
  endelse

  XCoords  = self.Image_Pixel_Location[0] + 1 + [0, Zoom_ns-1, Zoom_ns-1, 0, 0] - Zoom_ns/2
  YCoords  = self.Image_Pixel_Location[1] + [0, 0, Zoom_nl-1, Zoom_nl-1, 0] - Zoom_nl/2

  self.oScroll_Zoombox_Polygon->GetProperty, data=ImageCoords

  if min(XCoords) lt min(ImageCoords[0,*]) then XCoords+=min(ImageCoords[0,*]) - min(XCoords)
  if max(Xcoords) ge max(ImageCoords[0,*]) then XCoords-=max(Xcoords)- max(ImageCoords[0,*]-0.2)
  if min(YCoords) lt min(ImageCoords[1,*]) then YCoords+=min(ImageCoords[1,*]) - min(YCoords)
  if max(Ycoords) ge max(ImageCoords[1,*]) then YCoords-=max(Ycoords)- max(ImageCoords[1,*]-0.2)

  Image_xcoord_conv = [- min(ImageCoords[0,*])/(max(ImageCoords[0,*]) - min(ImageCoords[0,*])), 1/(max(ImageCoords[0,*]) - min(ImageCoords[0,*]))] * self.Image_Dims[0]
  Image_ycoord_conv = [- min(ImageCoords[1,*])/(max(ImageCoords[1,*]) - min(ImageCoords[1,*])), 1/(max(ImageCoords[1,*]) - min(ImageCoords[1,*]))] * self.Image_Dims[1]  
   
  self.oImage_Zoombox_Polygon->SetProperty, data=[transpose(XCoords), transpose(YCoords)], $
    Color=[255,0,0], style=1, xcoord_conv = Image_xcoord_conv, ycoord_conv = Image_ycoord_conv

  ;------------------------------
  ;--- View Image Zoom In Out ---
  ;------------------------------

  XCoords   = transpose([0,36,0,36,0,36,0,0,18,18,27,27,36,36]) 
  YCoords   = transpose([18,18,9,9,0,0,18,0,18,0,18,0,18,0]) 

  PolyLines = [2,0,1,2,2,3,2,4,5,2,6,7,2,8,9,2,10,11,2,12,13] 

  self.oImage_Zoom_In_Out_Polyline->SetProperty, data=[XCoords, YCoords], $
      polylines=Polylines, thick=1
  
  if ptr_valid(self.pPolyLinesIx) then $
    self.oImage_Vector_Polyline->SetProperty, $    
        data        = *self.pPolyLines, $
        vert_colors = *self.pPolyLinesColors, $
        polylines   = *self.pPolyLinesIx, $
        xcoord_conv = Image_xcoord_conv, $
        ycoord_conv = Image_ycoord_conv, $
        thick       = 1

  self.oImage->Draw,self.oImage_View 
end