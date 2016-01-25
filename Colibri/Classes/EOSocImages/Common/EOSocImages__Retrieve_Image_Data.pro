;+
; NAME:
; 
;   EOSocImages::Retrieve_Image_Data
;
; PURPOSE:
; 
;   This method function retrieves the spectrum for the requested pixel location.
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
;   Obj->[IOSocImages::]Retrieve_Image_Data, SelectIx [, BANDIX=variable] [, FOR_POLYGON=variable]
;                   [, FULL=variable] [, PBANDIMAGE=variable] [, POLYGONDATA=variable] [, POLYGONIX=variable]                     
;                   [, XPTS=variable] [, YPTS=variable]                                    
;
; ARGUMENTS     
;        
;   SelectIx:     The image index or indices in the image list, which was specified during initialization of 
;                 the object, for which data has to be retrieved. If not specified the requested information 
;                 of the first image is returned.
;        
; KEYWORDS     
;
;   BANDIX:       An index or array of indices indicating the band numbers for which the image data has to be
;                 returned.   
;   FOR_POLYGON:  Set this keyword to return the image data enclosed in the polygon for which the vertices 
;                 are specified via the keywords XPTS and YPTS. The data is returned via the keyword 
;                 POLYGONDATA whereas the data indices are returned vai the keyword POLYGONIX.
;   FULL:         Set this keyword to return the image data for the band index specified by the keyword
;                 BANDIX. If BANDIX is not specified the data for the first band is returned.
;   PBANDIMAGE:   Use this keyword to specify a named variable that contains the requested data as specified
;                 by the keyword FULL.
;   PIXEL_LOCATION: Set this keyword to a two element array containing the sample and line pixel location
;                 for which the spectrum has to be returned via the keyword PSPECTRUM.    

;   POLYGONDATA:  Use this keyword to specify a named variable that contains the polygon data as requested by
;                 the keyword FOR_POLYGON.
;   POLYGONIX:    Use this keyword to specify a named variable that contains the polygon data indices as 
;                 requested by the keyword FOR_POLYGON.
;   SPECTRUM:     Use this keyword to specify a named variable that contains the spectrum as requested by the
;                 keyword PIXEL_LOCATION. 
;   XPTS:         Use this keyword to specify an area of X values specifying the polygon vertices for which 
;                 to return the image data as requested by the keyword FOR_POLYGON.
;   YPTS:         Use this keyword to specify an area of Y values specifying the polygon vertices for which 
;                 to return the image data as requested by the keyword FOR_POLYGON. 
;   
; RETURN VALUE:
;   
;   None.
;
; KNOWN ISSUES:
; 
;   None.
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
pro EOSocImages::Retrieve_Image_Data, $
                        SelectIx, $
                        BandIx          = BandIx, $
                        FOR_POLYGON     = FOR_POLYGON, $
                        FULL            = FULL, $
                        pBandImage      = pBandImage, $
                        Pixel_Location  = Pixel_Location, $                        
                        PolygonData     = PolygonData, $
                        PolygonIx       = PolygonIx, $  
                        Spectrum        = Spectrum, $                    
                        xPts            = xPts, $
                        yPts            = yPts
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Module = 'EOSocImages::Retrieve_Image_Data'

  if ~keyword_set(SelectIx) then SelectIx = 0
  if ~keyword_set(BandIx)   then BandIx = 0
  
  Fid = (*self.pImageInfo)[SelectIx].Fid
  
  if Fid eq -1 then $
    envi_open_file, (*self.pImageInfo)[SelectIx].ImageFile, /no_realize, R_fid=fid  
    
  if keyword_set(FOR_POLYGON) then begin
  
    ;-------------------------------------------
    ;--- Retrieve image data for one polygon ---
    ;-------------------------------------------
    
    ;-- Parameters presence check
    if ~keyword_set(yPts) then Argument = 'yPts'    
    if ~keyword_set(xPts) then Argument = 'xPts'
  
    if keyword_set(Argument) then begin 
      self.oStatus->Log_Status, 'Error', 'Missing argument: '+Argument, Module, /Log          
      return
    endif

    RoiId = envi_create_roi(color=4, name='Scroll roi', $
             ns=(*self.pImageInfo)[SelectIx].ns, $
             nl=(*self.pImageInfo)[SelectIx].nl)

    envi_define_roi, RoiId, /polygon, xPts=xPts, yPts=yPts
    
    PolygonData = envi_get_roi_data(RoiId, addr=PolygonIx, fid=fid, $
                    pos=[indgen((*self.pImageInfo)[SelectIx].nb)])
  endif 
      
  if keyword_set(FULL) then begin 
  
    ;--------------------------------------------
    ;-- Retrieve image data for one band only ---
    ;--------------------------------------------
    
    BandIx = BandIx[0]  
    pBandImage = ptr_new(envi_get_data(fid=fid, dims=(*self.pImageInfo)[SelectIx].dims, pos=BandIx))
  endif 
  
   if keyword_set(PIXEL_LOCATION) then begin

    Spectrum = transpose(envi_get_slice(FID=fid, LINE=Pixel_Location[1], $
      POS=indgen((*self.pImageInfo).nb), XE=Pixel_Location[0], XS=Pixel_Location[0])) 
   endif     
     
   if (*self.pImageInfo)[SelectIx].Fid eq -1 then $    
      envi_file_mng, id=fid, /remove      
end