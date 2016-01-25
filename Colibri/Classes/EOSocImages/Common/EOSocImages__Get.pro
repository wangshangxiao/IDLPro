;+
; NAME:
; 
;   EOSocImages::get
;
; PURPOSE:
; 
;   This method procedure retrieves the requested info.
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
;   Obj->[IOSocImages::]Get, SelectIx [, BandIx=variable] [, Bnames=variable] [, dims=variable] 
;           [, dt=variable] [, File_type=variable] [, FOR_POLYGON=variable] [, FULL=variable]        
;           [, ImageFiles=variable] [, nb=variable] [, nClasses=variable] [, nl=variable] [, ns=variable]                
;           [, pBandImage=variable] [, pClass_names=variable] [, Pixel_Location=variable] 
;           [, pLookup=variable] [, pMapInfo=variable] [, PolygonData=variable] [, PolygonIx=variable]                  
;           [, pRGBdata=variable] [, pSpectrum=variable] [, pWl=variable] [, xPts=variable]               
;           [, yPts=variable]                                     
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
;   BNAMES:       Use this keyword to specify a named variable that contains the band names associated with 
;                 each band.
;   DIMS:         Use this keyword to specify a named variable that contains the spatial dimensions of the 
;                 file.
;   DT:           Use this keyword to specify a named variable that contains the IDL data type of the file.
;   FILE_TYPE:    Use this keyword to specify a named variable that contains an integer file-type value.
;   FOR_POLYGON:  Set this keyword to return the image data enclosed in the polygon for which the vertices 
;                 are specified via the keywords XPTS and YPTS. The data is returned via the keyword 
;                 POLYGONDATA whereas the data indices are returned vai the keyword POLYGONIX.
;   FULL:         Set this keyword to return the image data for the band index specified by the keyword
;                 BANDIX. If BANDIX is not specified the data for the first band is returned.
;   IMAGEFILES:   Use this keyword to specify a named variable that contains a string or string array holding
;                 the file full path and names of the images contained within the images object.
;   NB:           Use this keyword to specify a named variable that contains the number of bands in the file.
;   NCLASSES:     Use this keyword to specify a named variable that contains the number of classes for 
;                 classification images. NCLASSES is only set when the queried file is an ENVI classification
;                 file. Otherwise, the keyword returns a value of 0. 
;   NL:           Use this keyword to specify a named variable that contains the number of lines in the file.
;   NS:           Use this keyword to specify a named variable that contains the number of samples in 
;                 the file.
;   PBANDIMAGE:   Use this keyword to specify a named variable that contains the requested data as specified
;                 by the keyword FULL.
;   PCLASS_NAMES: Use this keyword to return a pointer holding a string array of class names for the
;                 classification images. The first element (Class 0) is "Unclassified." Only use CLASS_NAMES 
;                 if the result is a classification image, in which case this keyword is required. If the 
;                 result is not a classification image, this keyword is optional. 
;   PIXEL_LOCATION: Set this keyword to a two element array containing the sample and line pixel location
;                 for which the spectrum has to be returned via the keyword PSPECTRUM.    
;   PLOOKUP:      Use this keyword to specify a named variable that contains a pointer to the RGB lookup 
;                 values for each class in a classification image. This LOOKUP is a byte array 
;                 [3, num_classes]. The LOOKUP array is only set when the queried file is an ENVI 
;                 classification file. Otherwise, the LOOKUP returns a value of -1.
;   PMAPINFO:     Use this keyword to specify a named variable that contains a pointer to a map information
;                 structure. If there is no map information present a NULL pointer is returned.
;   POLYGONDATA:  Use this keyword to specify a named variable that contains the polygon data as requested by
;                 the keyword FOR_POLYGON.
;   POLYGONIX:    Use this keyword to specify a named variable that contains the polygon data indices as 
;                 requested by the keyword FOR_POLYGON.
;   PRGBDATA:     Use this keyword to specify a named variable that contains a pointer to the requested 
;                 RGB data.  
;   PWL:          Use this keyword to specify a named variable that contains a pointer to an array of 
;                 wavelength values, one for each band. If there is no wavelength associated with the file a 
;                 NULL pointer is returned.
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
;   - Written by Luc Bertels, February 2012.
;   - Added, LBer, February 2013: class information extraction
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
pro EOSocImages::get,$ 
                  SelectIx, $
                  ;
                  BandIx          = BandIx, $
                  Bnames          = Bnames, $                    
                  dims            = dims, $       
                  dt              = dt, $     
                  File_type       = File_Type, $                         
                  FOR_POLYGON     = FOR_POLYGON, $                   
                  FULL            = FULL, $        
                  ImageFiles      = ImageFiles, $  
                  nb              = nb, $                                            
                  nClasses        = nClasses, $                    
                  nl              = nl, $
                  ns              = ns, $                 
                  pBandImage      = pBandImage, $
                  pClass_names    = pClass_names, $  
                  Pixel_Location  = Pixel_Location, $
                  pLookup         = pLookup, $                  
                  pMapInfo        = pMapInfo, $
                  PolygonData     = PolygonData, $        
                  PolygonIx       = PolygonIx, $                   
                  pRGBdata        = pRGBdata, $
                  pWl             = pWl, $      
                  Spectrum        = Spectrum, $
                  xPts            = xPts, $               
                  yPts            = yPts              
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Module = 'EOSocImages::get'
 
  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------
  
  if ~keyword_set(SelectIx) then SelectIx = 0 else SelectIx=SelectIx[0]
  
  if min(SelectIx) lt 0 or max(SelectIx) ge self.nImages then begin
    
    self.oStatus->Log_Status, 'Error', $
      'Invalid file index: ' + strtrim(string(SelectIx),2), Module, /Log
 
    return
  endif

  ;------------------
  ;--- Processing ---
  ;------------------
  if arg_present(ImageFiles)      then ImageFiles   = (*self.pImageInfo)[SelectIx].ImageFile
  if arg_present(ns)              then ns           = (*self.pImageInfo)[SelectIx].ns
  if arg_present(nl)              then nl           = (*self.pImageInfo)[SelectIx].nl
  if arg_present(nb)              then nb           = (*self.pImageInfo)[SelectIx].nb
  if arg_present(dt)              then dt           = (*self.pImageInfo)[SelectIx].dt
  if arg_present(dims)            then dims         = (*self.pImageInfo)[SelectIx].dims
  if arg_present(File_Type)       then File_Type    = (*self.pImageInfo)[SelectIx].File_Type
  if arg_present(Bnames)          then Bnames       = *(*self.pImageInfo)[SelectIx].pBnames
  if arg_present(pWl)             then pWl          = (*self.pImageInfo)[SelectIx].pWl
  if arg_present(pMapInfo)        then pMapInfo     = (*self.pImageInfo)[SelectIx].pMapInfo
  
  if arg_present(nClasses)        then nClasses     = (*self.pImageInfo)[SelectIx].nClasses     ; LBer February 2013
  if arg_present(pLookup)         then pLookup      = (*self.pImageInfo)[SelectIx].pLookup      ; LBer February 2013
  if arg_present(pClass_names)    then pClass_names = (*self.pImageInfo)[SelectIx].pClass_names ; LBer February 2013
  
  if arg_present(pRGBdata)        then pRGBdata     = self->Retrieve_RGB_data(SelectIx)
  
  if keyword_set(Pixel_Location)  then self->Retrieve_Image_Data, $                             ; LBer February 2013
                                                SelectIx, $  
                                                Pixel_Location  = Pixel_Location, $
                                                Spectrum        = Spectrum
    
  if keyword_set(FOR_POLYGON)     then self->Retrieve_Image_Data, $                             ; LBer February 2013
                                                SelectIx, $
                                                /FOR_POLYGON, $
                                                xPts            = xPts, $
                                                yPts            = yPts, $
                                                PolygonData     = PolygonData, $
                                                PolygonIx       = PolygonIx

  if keyword_set(FULL)            then self->Retrieve_Image_Data, $                             ; LBer February 2013
                                                SelectIx, $                  
                                                /FULL, $
                                                pBandImage      = pBandImage, $
                                                BandIx          = BandsIx                              
end