;+
; NAME:
; 
;   IOScocImageViewer::Update_View
;
; PURPOSE:
; 
;   This public procedure method is used to update the IMAGE, SCROLL and ZOOM window with the image to be
;   visualized.
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
;   Obj->[IOScocImageViewer::]Update_View [/NEW] [,pImage] [,pMapInfo] [, PLOOKUP=pointer]
;
; ARGUMENTS: 
; 
;   pImage:       A pointer to the one or three (RGB) band image to be displayed.
;   pMapInfo:     A pointer to a structure holding the map information.
; 
; KEYWORDS:
; 
;   NEW:          Use this keyword to initialize the image viewer. This keyword should be set each time
;                 a new image is to be visualized within the same application.
;   PLOOKUP:      Use this keyword to specify a named variable that contains a pointer to the RGB lookup 
;                 values for each class in a classification image. This LOOKUP is a byte array 
;                 [3, num_classes]. The LOOKUP array should be specified  when the file to be viewed is an 
;                 ENVI classification file.
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
pro IOSocImageViewer::Update_View, NEW=NEW , pImage, pMapInfo, pLookup=pLookup
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  ;-- If no images was displayed yet set the NEW keyword to allow centering the zoom boxes
  if ~ptr_valid(self.pImage) then NEW=1
  
  ;-- Clear previous data on request (initialization for a new image to be displayed
  if keyword_set(NEW) then begin
  
    if ptr_valid(self.pImage)   then ptr_free, self.pImage
    if ptr_valid(self.pLookup)  then ptr_free, self.pLookup
    
    self.ImageInfo = {ImageInfo, ns:0, nl:0, nb:0, xcoord:0d, ycoord:0d, psx:0d, psy:0d}
  endif
  
  ;-- Update image data
  if ptr_valid(pImage) then begin  

    self.pImage = pImage
    ImageSize   = size(*pImage)
  
    if ImageSize[0] eq 2 then nb=1 else nb=3
  
    self.ImageInfo.ns  = ImageSize[1]
    self.ImageInfo.nl  = ImageSize[2]
    self.ImageInfo.nb  = nb 

    ;-- init map info if available
    if ptr_valid(pMapInfo) then begin

      self.ImageInfo.xcoord = (*pMapInfo).mc[2]
      self.ImageInfo.ycoord = (*pMapInfo).mc[3]
      self.ImageInfo.psx    = (*pMapInfo).ps[0]
      self.ImageInfo.psy    = (*pMapInfo).ps[1]
    endif
    
    ;-- Calculate scroll resize factor            
    self.Scroll_Rfac = min([double(self.Scroll_Dims[0])/self.ImageInfo.ns, $
                            double(self.Scroll_Dims[1])/self.ImageInfo.nl])
    
    ;-- Set zoom boxes at central position for new images
    if keyword_set(NEW) then begin   
      self.Scroll_Pixel_Location  = [self.ImageInfo.ns/2., self.ImageInfo.nl/2.]  
      self.Image_Pixel_Location   = [self.ImageInfo.ns/2., self.ImageInfo.nl/2.]   
    endif
  endif       
  
  ;-- Set lookup colors for classified images     
  if ptr_valid(pLookup) then begin
    
    self.pLookup = pLookup 
      
    Red   = (*self.pLookup)[0,*]
    Green = (*self.pLookup)[1,*]
    Blue  = (*self.pLookup)[2,*]

    self.oPalette->SetProperty, red_values=Red, green_values=Green, blue_values=Blue
  endif
  
  ;-- Update windows       
  self->View_Scroll
  self->View_Image
  self->View_Zoom
  
  ;-- Inform user if requested
  if obj_valid(self.oInform_user) then self->Inform_User   
end