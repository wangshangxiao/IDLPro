;+
; NAME:
; 
;   IOScocImageViewer::View_Scroll
;
; PURPOSE:
; 
;   This private procedure method is used to update the SCROLL window.
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
;------------------------------------------------------------------------------------------------------------
pro IOSocImageViewer::View_Scroll
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  ;-------------------
  ;--- View scroll ---
  ;-------------------
                          
  xSize    = round((self.ImageInfo.ns * self.Scroll_Rfac))
  ySize    = round((self.ImageInfo.nl * self.Scroll_Rfac))
  
  Scroll   = congrid(*self.pImage, xSize, ySize, self.ImageInfo.nb)

  self.oScroll->SetProperty, dimensions=[xsize, ySize]
  self.oScroll_View->SetProperty, viewplane_rect=[0., 0., xsize, ySize]

  self.oScroll_Image->SetProperty, data = Scroll, interleave=[2], order=1 
   
  ;---------------------------
  ;--- View scroll zoombox ---
  ;---------------------------

  ;-- Define scroll zoombox size depending on the image draw size.
  if float(self.ImageInfo.ns) / self.Image_dims[0] le float(self.ImageInfo.nl) / self.Image_dims[1] then begin
  
    Image_ns = float(self.ImageInfo.ns) / self.Image_Requested_Zfac
    Image_nl = (Image_ns / self.Image_dims[0]) * self.Image_dims[1]
  endif else begin
  
    Image_nl = float(self.ImageInfo.nl) / self.Image_Requested_Zfac
    Image_ns = (Image_nl / self.Image_dims[1]) * self.Image_dims[0]
  endelse

  XCoords  = ([0, Image_ns-1, Image_ns-1, 0, 0] - Image_ns/2.) + self.Scroll_Pixel_Location[0]
  YCoords  = ([0, 0, Image_nl-1, Image_nl-1, 0] - Image_nl/2.) + self.Scroll_Pixel_Location[1]

  if min(XCoords) lt 0 then XCoords-= min(XCoords)
  if max(Xcoords) gt self.ImageInfo.ns then XCoords-=(max(Xcoords)-self.ImageInfo.ns+1)
  if min(YCoords) lt 0 then YCoords-= min(YCoords)
  if max(Ycoords) ge self.ImageInfo.nl then YCoords-=(max(Ycoords)-self.ImageInfo.nl+1)

  Scroll_xcoord_conv   = [0, 1./self.ImageInfo.ns] * xsize
  Scroll_ycoord_conv   = [0, 1./self.ImageInfo.nl] * ySize

  self.oScroll_Zoombox_Polygon->SetProperty, data=[transpose(XCoords), transpose(YCoords)], $
    Color=[255,0,0], style=1, xcoord_conv = Scroll_xcoord_conv, ycoord_conv = Scroll_ycoord_conv

  if ptr_valid(self.pPolyLinesIx) then $
    self.oScroll_Vector_Polyline->SetProperty, $    
        data        = *self.pPolyLines, $
        vert_colors = *self.pPolyLinesColors, $
        polylines   = *self.pPolyLinesIx, $
        xcoord_conv = Scroll_xcoord_conv, $
        ycoord_conv = Scroll_ycoord_conv, $
        thick       = 1

  self.oScroll->Draw,self.oScroll_View 
end