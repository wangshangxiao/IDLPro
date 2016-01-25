;+
; NAME:
; 
;   EOSocImages::init
;
; PURPOSE:
; 
;   This method function creates and initializes an instance of the class 'EOSocImages'. The obtained object 
;   is used to extract image info and to perform basic image processing.
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
;   Result = OBJ_NEW('EOSocImages', pImageList [, /KEEP] [, OSTATUS=object reference])
;                                     
; ARGUMENTS     
; 
;   pImageList:     Pointer to an array of strings holding the full path and name of image files.
;        
; KEYWORDS     
; 
;   KEEP:           Set this keyword to keep the image files open. If not set (default) all image files are
;                   closed after the required info is extracted. 
;   OSTATUS:        An object reference to an existing status window.
;
; RETURN VALUE:
; 
;   The OBJ_NEW function returns an object reference to the newly-created object.
;
; KNOWN ISSUES:
; 
;   None
;
; MODIFICATION HISTORY:
; 
;   - Written by Luc Bertels, January 2012.
;   - Added, LBer February, 2013: class information extraction
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
function EOSocImages::init,$ 
                  pImageList, $
                  KEEP        = KEEP, $
                  oStatus     = oStatus
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Module    = 'EOSocImages::init'

  ;----------------------------------------------------------
  ;--- If no status object is present, create the default ---
  ;----------------------------------------------------------

   oStatus = IOScc_Init(oStatus)                      
   self.oStatus = oStatus
 
  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------
  
  if ~keyword_set(pImageList)  then Argument = 'pImageList'  
  
  if keyword_set(Argument) then begin
    self.oStatus->Log_Status, 'Error', 'Missing argument: '+Argument, Module, /Log       
    return, 0
  endif
  
  ;------------
  ;--- Init ---
  ;------------
  
  nImages = n_elements(*pImageList)
  
  ImageInfo = { $
        ;
        ImageFile:          '', $    
        Fid:                -1L, $
        ;
        ns:                 0, $
        nl:                 0, $
        nb:                 0, $
        dt:                 0, $
        dims:               [0, 0, 0, 0, 0], $
        file_type:          0, $
        ;
        pBnames:            ptr_new(/allocate_heap), $        
        pWl:                ptr_new(/allocate_heap), $
        pMapInfo:           ptr_new(/allocate_heap), $
        ;
        nClasses:           0, $                              ; LBer February, 2013
        pLookup:            ptr_new(/allocate_heap), $        ; LBer February, 2013
        pClass_names:       ptr_new(/allocate_heap) $         ; LBer February, 2013
        ;
        }
  
  aImageInfo  = replicate(ImageInfo, nImages)
  
  ;------------------
  ;--- Processing ---
  ;------------------
  
  for FileIx=0, nImages-1 do begin
  
    self.oStatus->Progress_Set, Fast=[FileIx, nImages]

    self.oStatus->Log_Status, 'Busy', $
      'Extracting image info from: '+(*pImageList)[FileIx], Module, /Log
    
    if ~file_test((*pImageList)[FileIx]) then begin
    
      self.oStatus->Progress_clear, /RESET    
      self.oStatus->Log_Status, 'Error', 'File not found: ' + (*pImageList)[FileIx], Module, /Log   

      return, 0
    endif
    
    envi_open_file, (*pImageList)[FileIx], /no_realize, R_fid=Fid  
    
    if Fid eq -1 then begin
    
      self.oStatus->Progress_clear, /RESET    
      self.oStatus->Log_Status, 'Error', 'No valid file found: ' + (*pImageList)[FileIx], Module, /Log 

      return, 0
    endif
  
    envi_file_query, Fid, ns=ns, nl=nl, nb=nb, dims=dims, data_type=dt, wl=wl, bnames=bnames, $
      file_type=file_type, Lookup=LookUp, Class_Names=Class_Names, Num_classes=nClasses         ; LBer February, 2013

    mapinfo = envi_get_map_info(fid=Fid)
    
    aImageInfo[FileIx].ImageFile   = (*pImageList)[FileIx]
    aImageInfo[FileIx].ns          = ns
    aImageInfo[FileIx].nl          = nl
    aImageInfo[FileIx].nb          = nb
    aImageInfo[FileIx].dt          = dt
    aImageInfo[FileIx].dims        = dims
    aImageInfo[FileIx].file_type   = file_type
    aImageInfo[FileIx].pBnames     = ptr_new(bnames)
    aImageInfo[FileIx].pWl         = ptr_new(wl)
    aImageInfo[FileIx].pMapInfo    = ptr_new(mapinfo)
    aImageInfo[FileIx].nClasses    = nClasses                           ; LBer February, 2013  
    aImageInfo[FileIx].pLookup      = ptr_new(LookUp)                   ; LBer February, 2013
    aImageInfo[FileIx].pClass_names = ptr_new(Class_Names)              ; LBer February, 2013
    
    if keyword_set(Keep) then begin
      aImageInfo[FileIx].Fid  = Fid
    endif else begin
      envi_file_mng, id=fid, /remove
    endelse
  endfor
  
  self.pImageInfo = ptr_new(aImageInfo) 
  self.nImages    = nImages
  
  ;----------------
  ;--- Finished ---
  ;----------------
  
  self.oStatus->Progress_clear, /RESET
  
  return, 1
end