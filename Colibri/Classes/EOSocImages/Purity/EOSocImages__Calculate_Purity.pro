;+
; NAME: 
; 
;   EOSocImages::Calculate_Purity
;
; PURPOSE: 
; 
;   This function will calculate the purity image by convoluting a 3x3 kernel over the image. Depending on 
;   the keyword set, the central pixel of the kernel will get the sum of sa/rmse between the central pixel 
;   value and its eight neighbouring pixel values. 
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
;   Obj->[EOSocImages::]Calculate_Purity, SelectIx [, BANDSIX=array] [, OUTPUT_DIR=string] [, /OVERWRITE],
;     PURITY_METHOD={'RMSE' | 'SA'} [, RADIUS=value]
;
; ARGUMENTS
; 
;   SelectIx:       The image index or indices in the image list, which was specified during initialization of 
;                   the object, for which the purity image has to be created. If not specified all images are
;                   used.  
;
; KEYWORDS
;                   
;   BANDSIX:        If present, an array of indices indicating the bands to be used in the calculation. If 
;                   not present all bands will be used.
;   OUTPUT_DIR:     The directory where the ndvi images need to be written. The files writte are named 
;                   according the input file basename with the suffix added depending on the purity method:
;                   '_ptyrmse' when the Root Mean Square Error is used, '_pty_sa' when the Spectra Angle 
;                   distance is used.
;   OVERWRITE:      Set this keyword to overwrite already existing purity images.
;   PURITY_METHOD:  Set this keyword to a string holding the method to be used for purity image calculation. 
;                   Possible values are 'RMSE' and 'SA'.
;   RADIUS:         Set this keyword to a value holding the radius of the kernel to be used for the purity
;                   calculation. If not present a radius of 1 is used.
;                      
; RETURN VALUE:
; 
;   None
;
; DEPENDENCIES:
; 
;   None
;       
; KNOWN ISSUES:
; 
;   - The outer boundary pixels are not calculated.
;
; MODIFICATION HISTORY:
; 
;   - Written by Luc Bertels, January, 2012.
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
pro EOSocImages::Calculate_Purity, $
                  SelectIx, $
                  BandsIx         = BandsIx, $
                  Output_Dir      = Output_Dir, $
                  Purity_Method   = Purity_Method, $
                  Radius          = Radius, $
                  overwrite       = overwrite
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
 
  Module = 'EOSocImages::Calculate_Purity'
 
  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------
  
  if ~keyword_set(Purity_Method) then Argument = 'Purity_Method'  
  if ~keyword_set(Output_Dir)   then Argument = 'Output_Dir'
    
  if keyword_set(Argument) then begin
  
    self.oStatus->Log_Status, 'Error', 'Missing argument: '+Argument, Module, /Log
               
    return
  endif

  if ~keyword_set(Radius) then Radius = 1

  ;--------------------------
  ;--- Prepare processing ---
  ;--------------------------
  
  ;-- Create output directory if not existing.
  if ~file_test(Output_Dir, /directory) then file_mkdir, Output_Dir
  ;-- Select all images present if no pre-selection was done.
  if ~arg_present(SelectIx) then SelectIx = indgen(self.nImages)
      
  ;-- Check is a correect purity calculation method was specified.     
  case Purity_Method of
  'RMSE': 
  'SA':   
  else: begin
    
    self.oStatus->Log_Status, 'Error', $
      'Purity calculation method not recognized: '+Purity_Method, Module, /Log
                
    return 
    end
  endcase
  
  ;-------------------------
  ;--- Handle all images ---
  ;-------------------------
  self.oStatus->Log_Status, 'Busy', 'Writing results to directory: '+Output_Dir, Module, /Log
  
  for Fix=0, n_elements(SelectIx)-1 do begin
  
    self.oStatus->Progress_Set, slow=[Fix, n_elements(SelectIx)]
    
    ;-- Handle only images which are selected.
    FileIx = SelectIx[Fix]

    ;-- Extract the required info.
    ns        = (*self.pImageInfo)[FileIx].ns
    nl        = (*self.pImageInfo)[FileIx].nl
    nb        = (*self.pImageInfo)[FileIx].nb
    InFile    = (*self.pImageInfo)[FileIx].ImageFile
    FileName  = file_basename(InFile, '.img')
    
    ;-- Update log info.
    self.oStatus->Log_Status, 'Busy', $
      'Calculating purity image using '''+Purity_Method+''' for : '+FileName, Module, /Log
       
    ;-- Prepare aoutput image info.    
    case Purity_Method of
    'RMSE': begin  
      PtyBaseName = FileName+'_ptyrmse'
      PtyFile     = Output_Dir+PtyBaseName
      descrip     = 'Purity image by EOSocImages using ''rmse.''
      end
    'SA': begin  
      PtyBaseName = FileName+'_ptysa'
      PtyFile     = Output_Dir+PtyBaseName
      descrip     = 'Purity image by EOSocImages using ''sa''
      end
    else:
    endcase    
    
    ;-- Check if purity image already exists and needs to be overwritten.
    if ~keyword_set(overwrite) and file_test(PtyFile) eq 1 then begin
      self.oStatus->Log_Status, 'Warning', '  Purity image already exist: '+PtyBaseName, Module, /Log
      continue
    endif
    
    ;-- Open the input image.
    InFid = (*self.pImageInfo)[FileIx].Fid
  
    if InFid eq -1 then $   
      envi_open_file, InFile, /no_realize, R_fid=InFid   
    
    ;-- Check if bandselection was done, if not use all bands.    
    if keyword_set(BandsIx) then Pos = BandsIx else Pos = indgen(nb)
    
    ;-- Prepare output image.
    OutIm   = make_array(ns, nl, /float, value=!VALUES.F_NAN)
    
    ;-- Prepare for kernel convolution.
    D         = Radius * 2 + 1  
    ReadPtr   = D                                   ; next line to read from image                
    pBank     = ptrarr(D)
    ThisValid = bytarr(ns)
     
    ThisValid[Radius:ns-(Radius+1)] = 1 
    
    ;-- Init pBank with the first lines of the image depending on the kernel size.  
    for Ix=0, D-1 do $
      pBank[Ix]         = ptr_new(double(envi_get_slice(fid=InFid, pos=Pos, line=Ix, xs=0, xe=(ns-1))))
    
    ;-- Convolute the kernel over all lines of the image.  
    For Lix=Radius, nl-(Radius+1) do begin
      self.oStatus->Progress_Set, Fast=[Lix, nl-(Radius+1)]

      Valid     = ThisValid 

      ;-- Retrieve valid, i.e. non-zero pixels.
      for Ix=0, D-1 do begin 
       
        iNonValid = where(total(*pBank[Ix],2) eq 0, cNonValid)
      
        if cNonValid gt 0 then Valid[iNonValid]  = 0 
      endfor
      
      iValid = where(Valid eq 1, cValid)
   
      ;-- Convolute the kernel over all valid samples of the actual lines. 
      for Six=0, cValid-1 do begin        
        case Purity_Method of
          
        'RMSE': OutIm[iValid[Six], Lix] = IOScc_aRMSE(pBank, iValid[Six], Radius) 

        'SA':   OutIm[iValid[Six], Lix] = IOScc_aSA(pBank, iValid[Six], Radius) 

        else:
        endcase
      EndFor

      if ReadPtr  eq nl then break     
      
      ;-- Prepare next spectral slice.
      pBank = shift(pBank,2)
      
      *pBank[D-1] = double(envi_get_slice(fid=InFid, pos=Pos, line=ReadPtr++, xs=0, xe=(ns-1)))  
    EndFor

    self.oStatus->progress_clear, /fast

    OutIm = OutIm/max(OutIm, /nan)
    
    envi_enter_data, OutIm, $
                    Descrip       = Descrip, $
                    File_Type     = 0, $
                    map_info      = *(*self.pImageInfo)[FileIx].pMapInfo, $
                    r_fid         = tp_fid    
   
    envi_doit, 'cf_doit', $
                    fid           = tp_fid, $
                    pos           = 0, $
                    dims          = (*self.pImageInfo)[FileIx].dims, $
                    out_name      = PtyFile, $ 
                    r_fid         = p_fid 
                    
    envi_file_mng, id=tp_fid, /remove  
    envi_file_mng, id=p_fid, /remove  
    
  if (*self.pImageInfo)[FileIx].Fid eq -1 then $    
    envi_file_mng, id=InFid, /remove  
    
    self.oStatus->Log_Status, 'Ok', 'Purity image created successfully: '+PtyBaseName, Module, /Log                           
  endfor

  self.oStatus->Progress_clear, /RESET
end
