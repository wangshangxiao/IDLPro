;+
; NAME:
; 
;   EOSocLDA::Classify
;
; PURPOSE:
; 
;   This procedure method will classify the specified images using the LDA function created during 
;   initialization.
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
;   Obj->[EOSocLDA::]Classify(pImageList [, ABAND_NAMES=string array] [, ACLASS_COLORS=array] 
;     [, ACLASS_NAMES=string array] [, BANDSIX=array] [, CLASS_DIR=string] [, /OVERWRITE] 
;     [, RULE_DIR=string] [, /TIFF]
;
; ARGUMENTS
; 
;   pImageList:     Pointer to an array of strings holding the full path and name of the image files to be 
;                   classified.
;                          
; KEYWORDS
;
;   ABAND_NAMES:    An array of strings holding the names of the different bands of the created rule image. 
;                   This keyword must be specified in the keyword RULE_DIR is present.
;   ACLASS_COLORS:  An array of vectors holding the RGB color values for the corresponding classes. This 
;                   keyword must be specified in the keyword CLASS_DIR is present.
;   ACLASS_NAMES:   An array of strings holding the class names. This keyword must be specified in the keyword
;                   CLASS_DIR is present.
;   BANDSIX:        If present, an array of indices indicating the bands to be used in the classification. 
;                   If not present all bands will be used.
;   CLASS_DIR:      If present, a string holding the output directory where the classified images need to be 
;                   stored. The directory will be created if it is not yet existing. The classified images
;                   are named after the input image suffixed with '_lda'.
;   OVERWRITE:      Set this keyword to force already existing classified results to be overwritten.
;   RULE_DIR:       If present, a string holding the output directory where the rule images need to be stored.
;                   The directory will be created if it is not yet existing. The classified images are named 
;                   after the input image suffixed with '_rule'.
;                   
;                   *** At least the CLASS_DIR or the RULE_DIR have to be specified.
;                   
;   TIFF:           Set this keyword to output the classified images in tiff format.
;
; DEPENDENCIES:
; 
;   None
;       
; KNOWN ISSUES:
; 
;   None
;
; MODIFICATION HISTORY:
; 
;   - Written by Luc Bertels, June 2010.
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
pro EOSocLDA::Classify, $          
                pImageList, $
                BandsIx         = BandsIx, $
                Class_Dir       = Class_Dir, $
                Rule_Dir        = Rule_Dir, $
                aClass_Names    = aClass_Names, $
                aCLass_Colors   = aCLass_Colors, $
                aBand_Names     = aBand_Names, $
                tiff            = tiff , $
                overwrite       = overwrite             
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Module  = 'EOSocLDA::Classify'  
  
  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------
  
  if ~keyword_set(Class_Dir) and ~keyword_set(Rule_Dir) then begin
    
    self.oStatus->Log_Status, 'Error', 'Missing argument: Class_Dir or Rule_Dir', Module, /Log
                
    return
  endif

  if keyword_set(Rule_Dir) then begin
    if ~keyword_set(aBand_Names)then Argument = 'aBand_Names'  
  endif
  
  if keyword_set(Class_Dir) then begin   
    if ~keyword_set(aCLass_Colors) then Argument = 'aCLass_Colors'  
    if ~keyword_set(aClass_Names) then Argument = 'aClass_Names'  
  endif
  
  if ~keyword_set(pImageList) then Argument = 'pImageList'  
  
  if keyword_set(Argument) then begin
    
    self.oStatus->Log_Status, 'Error', 'Missing argument: '+Argument, Module, /Log
                
    return
  endif 
  
  ;------------------
  ;--- Processing ---
  ;------------------  
  
  nImages   = n_elements(*pImageList)

  if keyword_set(Class_Dir) then  $      
    if ~file_test(Class_Dir, /directory) then file_mkdir, Class_Dir 
  if keyword_set(Rule_Dir) then  $
    if ~file_test(Rule_Dir, /directory) then file_mkdir, Rule_Dir

  ;-------------------------
  ;--- Handle all images ---
  ;-------------------------
  
  for FileIx=0, nImages-1 do begin

    self.oStatus->Progress_Set, slow=[FileIx, nImages]

    InFile = (*pImageList)[FileIx]
    
    ;-- Check if the input file exists.
    if ~file_test(InFile) then begin
      
      self.oStatus->Log_Status, 'Error', 'Image not found: ' + InFile, Module , /Log
      
      continue
    endif    
    
    ImageName   = file_basename(InFile, '.img')
    
    if keyword_set(Class_Dir) then begin $ 
      Class_File  = Class_Dir + ImageName + '_lda'

      if ~keyword_set(overwrite) and $
        file_test(Class_File) eq 1 then continue
    endif
          
    if keyword_set(Rule_Dir) then $
          Rule_File   = Rule_Dir  + ImageName + '_rule'

    self.oStatus->Log_Status, 'Busy', 'Classifying: '+ImageName, Module , /Log
             
    envi_open_file, InFile, r_fid=InFid, /no_realize
    envi_file_query, InFid, ns=ns, nl=nl, nb=nb, dims=dims
  
    if nb lt self.nFeatures then begin
 
       self.oStatus->Log_Status, 'Error', $
          'The image number of bands is inconsistant with the training set.', Module, /Log 
      
      envi_file_mng, id=InFid, /remove
      return
    endif
    
    ;--------------------------------------------
    ;--- Prepare for Class-image if requested ---
    ;--------------------------------------------
      
    if keyword_set(Class_Dir) then ClassImage = bytarr(ns, nl)
    if keyword_set(BandsIx) then Pos = BandsIx else Pos = indgen(nb)
    
    ;-------------------------------------------
    ;--- Prepare for Rule-image if requested ---
    ;-------------------------------------------
    
    if keyword_set(Rule_Dir) then openw, Rule_lun, Rule_File, /get_lun

    ;---------------------------------
    ;--- Convolute over all pixels ---
    ;---------------------------------

    for Lix=0, nl-1 do begin
    
      self.oStatus->Progress_Set, Fast=[Lix, nl]

      Data    = envi_get_slice(fid=InFid, line=Lix, pos=pos, xs=0, xe=ns-1)
      iValid  = where(total(Data,2) ne 0, cValid)
      
      if keyword_set(Rule_Dir) then RuleLine  = dblarr(ns, self.nClasses) + !VALUES.F_NAN   
         
      if cValid le 0 then begin
        if keyword_set(Rule_Dir) then writeu, Rule_lun, RuleLine
        continue
      endif
      
      for Six=0, cValid-1 do begin

        ;-- Classify on a per pixel basis.
        Class = self->LDA(Data[iValid[Six], *], Rule=Rule)
        
        if keyword_set(Class_Dir) then ClassImage[iValid[Six], Lix] = Class
        if keyword_set(Rule_Dir)  then RuleLine[iValid[Six], *] = Rule  
      endfor
   
      if keyword_set(Rule_Dir) then writeu, Rule_lun, RuleLine
    endfor
    
    Map_Info    = envi_get_map_info(fid=InFid)
    Descrip     = 'Classified image by ''EOSocLDA'''  
    Num_Classes = n_elements(aClass_Names)
   
    if keyword_set(Class_Dir) then begin
      if keyword_set(tiff) then Cl_Tmp = ClassImage
   
      envi_enter_data,  ClassImage, $
                    Class_Names   = aClass_Names, $
                    Num_Classes   = Num_Classes, $
                    Descrip       = Descrip, $
                    File_Type     = 3, $
                    Lookup        = aCLass_Colors, $
                    Map_Info      = Map_Info, $
                    r_fid         = tc_fid
  
      envi, /restore_base_save_files 
  
      envi_doit, 'cf_doit', $
                    fid           = tc_fid, $
                    pos           = [0], $
                    dims          = dims, $
                    out_name      = Class_File, $ 
                    out_dt        = 1, $ 
                    r_fid         = c_fid 

      if keyword_set(tiff) then begin

        R = bytarr(ns, nl)
        G = bytarr(ns, nl)
        B = bytarr(ns, nl)
        
        for Cix=0, Num_Classes-1 do begin
  
          i = where(Cl_Tmp eq Cix, n)
    
          if n le 0 then continue
    
          R[i] = aCLass_Colors[0, Cix]
          G[i] = aCLass_Colors[1, Cix]
          B[i] = aCLass_Colors[2, Cix]
        endfor

        envi_enter_data, R, r_fid = R_Fid, map_info=map_info
        envi_enter_data, G, r_fid = G_Fid
        envi_enter_data, B, r_fid = B_Fid

        envi_doit, 'cf_doit', $
                    fid           = [R_Fid, G_Fid, B_Fid], $
                    pos           = [0,0,0], $
                    dims          = dims, $
                    out_dt        = 1, $  
                    remove        = 0, $
                    /in_memory, $
                    r_fid         = t_fid 
        
        envi_output_to_external_format, $
                    dims          = dims, $
                    fid           = t_fid, $
                    pos           = [0,1,2], $
                    out_name      = Class_File+'_.tiff', $
                    /tiff
                    
        envi_file_mng, id = R_Fid, /remove         
        envi_file_mng, id = G_Fid, /remove         
        envi_file_mng, id = B_Fid, /remove         
        envi_file_mng, id = t_fid, /remove         
      endif  
      
      envi_file_mng, id=tc_fid, /remove  
      envi_file_mng, id=c_fid, /remove
    endif
    
    if keyword_set(Rule_Dir) then begin
    
      free_lun, Rule_lun
    
      Descrip = 'Rule image by ''EOSocLDA'''
    
      envi_setup_head, fname      = Rule_File, $
                    Descrip       = Descrip, $
                    File_Type     = 0, $
                    Map_Info      = Map_Info, $
                    ns            = ns, $
                    nl            = nl, $
                    nb            = self.nClasses, $
                    bnames        = aBand_Names, $
                    data_type     = 5, $
                    interleave    = 1, $
                    r_fid         = r_fid, $
                    /write        
      
      envi_file_mng, id=r_fid, /remove
    endif

    envi_file_mng, id=InFid, /remove
    
    self.oStatus->progress_clear, /fast    
  endfor
          
  self.oStatus->Progress_clear, /RESET  
    
  self.oStatus->Log_Status, 'Ok', 'Processing finished.', Module, /Log
       
  return
end
