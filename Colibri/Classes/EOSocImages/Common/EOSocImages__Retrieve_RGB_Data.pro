;+
; NAME:
; 
;   EOSocImages::retrieve_RGB_data
;
; PURPOSE:
; 
;   This method function retrieves the requested RGB data.
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
;   Result = Obj->[IOSocImages::]Retrieve_RGB_Data(SelectIx)                      
;                                     
; ARGUMENTS     
;        
;   SelectIx:     The image index in the image list, which was specified during initialization of the object, 
;                 for which the RGB data has to be retrieved. If not specified image index 0 is used. If an 
;                 array of indices was specified the first indes in the array will be used.
;        
; KEYWORDS     
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
function EOSocImages::retrieve_RGB_data,$ 
                  SelectIx
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
      
  Module = 'EOSocImages::retrieve_RGB_data'
  
  if ~keyword_set(SelectIx) then SelectIx = 0
  SelectIx = SelectIx[0]
              
  ImageFile = (*self.pImageInfo)[SelectIx].ImageFile

  Fid = (*self.pImageInfo)[SelectIx].Fid
  
  if Fid eq -1 then $  
    envi_open_file, ImageFile, /no_realize, R_fid=fid    
  
  envi_file_query, Fid, ns=ns, nl=nl, nb=nb, dims=dims, data_type=dt, wl=wl, bnames=bnames, $
      file_type=file_type         
              
  case envi_file_type(file_type) of
    
  'ENVI Standard': begin 
  
    EOScc_find_RGB_bands, ImageFile=ImageFile, Fid=Fid, Pos=Pos
  
    if Pos[0] eq -1 then begin
    
      self.oStatus->Log_Status, 'Error', 'No RGB indices found.', Module, /Log
      
      return, -1
    endif
  
    bB    = hist_equal(envi_get_data(fid=fid, dims=dims, pos=Pos[2]), percent=2)
    bG    = hist_equal(envi_get_data(fid=fid, dims=dims, pos=Pos[1]), percent=2)
    bR    = hist_equal(envi_get_data(fid=fid, dims=dims, pos=Pos[0]), percent=2) 
    
    Image = [[[bR]], [[bG]], [[bB]]]  
  end
    
  'ENVI Classification': begin
      
    Image = envi_get_data(fid=fid, dims=dims, pos=[0])
  end
 
  'TIFF': begin
       
    if nb ge 3 then begin
      Image         = read_tiff(ImageFile, channels=[0,1,2], interleave=2)

      R             = hist_equal(Image[*,*,0], percent=2) 
      G             = hist_equal(Image[*,*,1], percent=2) 
      B             = hist_equal(Image[*,*,2], percent=2)

      Image         = [[[R]], [[G]], [[B]]]
    endif else begin
      Image         = read_tiff(ImageFile, channels=[0])
      Image         = hist_equal(Image[*,*,0], percent=2)
    endelse     
  end
  
  else:
  endcase
  
  if (*self.pImageInfo)[SelectIx].Fid eq -1 then $
    envi_file_mng, id=fid, /remove   
    
  return, ptr_new(Image)               
end