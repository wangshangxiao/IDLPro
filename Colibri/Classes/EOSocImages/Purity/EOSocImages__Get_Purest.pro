;+
; NAME:
; 
;   EOSocImages::Get_Purest
;
; PURPOSE:
; 
;   This procedure method will return the purest pixel found in the purity images located in the purity
;   directory. If specified it will take into account the rule images.
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
;   Obj->[EOSocImages::]Get_Purest, SelectIx, [, MASK_DIR=string] [, MASK_SUFFIX=string] 
;     [, PENDMEMBERS=variable], PUREST=variable, PURITY_DIR=string, [, PURITY_METHOD={'RMSE' | 'SA'}], 
;     PURITY_SUFFIX=string [, RULE_DIR=string] 
;
; ARGUMENTS
; 
;   SelectIx:       The image index or indices in the image list, which was specified during initialization of 
;                   the object, from which the purest image pixel has to be searched. If not specified all 
;                   images are used.         
;                   
; KEYWORDS:
; 
;   MASK_DIR:       Set this keyword to a string holding the directory location where the mask images are 
;                   located. If specified the MASK_SUFFIX keyword must be specified as well. If a valid mask 
;                   image is found endmembers will be searched for those pixels for which the corresponding 
;                   mask pixels have a non zero value. If not specified no mask image will be taken into 
;                   account. 
;   MASK_SUFFIX:    Set this keyword to a string holding the suffix to be added to the image file name in 
;                   order to obtain the mask file name. This keyword should be specified if the MASK_DIR 
;                   keyword is specified as well.
;   PENDMEMBERS:    This keyword holds a pointer to a structure array containing the endmembers found. 
;                   This structure has following layout:
;                     - ImageFile:  A string holding the image file where the endmember was found.  
;                     - RoiName:    The name given to the ROI for the endmember.
;                     - S:          The sample locations of the endmember.
;                     - L:          The line locations of the endmember.
;                     - nPixels:    The number of pixels allocated to the endmember. 
;                     - ColorIx:    The color index for the endmember.               
;   PUREST:         A structure element holding the endmember found. This structure has following layout:
;                     - ImageFile:  A string holding the image file where the endmember was found.
;                     - S:          The sample locations of the endmember.
;                     - L:          The line locations of the endmember.                
;   PURITY_DIR:     This keyword holds a string containing the directory where the purity images are located.
;   PURITY_METHOD:  Set this keyword to a string indicating the method which was used for calculating the 
;                   purity image:
;                     - RMSE:       Root Mean Square Error
;                     - SA:         Spectral Angle distance
;                   This is used to define the correct suffix for locating the purity image.
;   RULE_DIR:       Set this keyword to the directory were the rule images are located. If not present the 
;                   rule image is not taken into account for finding the purest endmember.                       
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
;   - Written by Luc Bertels, January, 2012.
;   - Added, Febryary 2013: take into account the  mask images.
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
pro EOSocImages::Get_Purest, $
                SelectIx, $
                Purity_Dir        = Purity_Dir, $            
                Purity_Method     = Purity_Method, $
                Rule_Dir          = Rule_Dir, $
                Mask_Dir          = Mask_Dir, $                                     ; LBER, Febryary 2013
                Mask_Suffix       = Mask_Suffix, $                                  ; LBER, Febryary 2013     
                pEndMembers       = pEndMembers, $
                Purest            = Purest
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  Module = 'EOSocImages::Get_Purest'

  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------

  if ~keyword_set(Purity_Method)      then Argument = 'Purity_Method'    
  if ~keyword_set(Purity_Dir)         then Argument = 'Purity_Dir'
  
  if keyword_set(Argument) then begin    
    self.oStatus->Log_Status, 'Error', 'Missing argument: '+Argument, Module, /Log              
    return
  endif 
    
  case Purity_Method of
  'RMSE': Purity_Suffix = '_ptyrmse'
  'SA': Purity_Suffix = '_ptysa'
  else:
  endcase 
  
  ;-------------------------
  ;--- Handle all images ---
  ;-------------------------
  
  if ~arg_present(SelectIx) then SelectIx = indgen(self.nImages)

  kS  = [-1, 0, 1, -1, 0, 1 ,-1, 0, 1]
  kL  = [-1, -1, -1, 0, 0, 0, 1, 1, 1]
  
  for Fix=0, n_elements(SelectIx)-1 do begin

    FileIx = SelectIx[Fix]

    self.oStatus->Progress_Set, Fast=[Fix, n_elements(SelectIx)]
    
    InFile    = file_basename((*self.pImageInfo)[FileIx].ImageFile, '.img')   

    ;--------------------------------------
    ;--- Prepare endmember exclude list ---
    ;--------------------------------------
    
    cExclude = -1

    if ptr_valid(pEndMembers) then begin

      Ix = where((*pEndMembers).ImageFile eq InFile, cExclude)
      
      if cExclude gt 0 then begin
        
        S = (*pEndMembers)[Ix].S
        L = (*pEndMembers)[Ix].L

        for Ix=0, cExclude-1 do begin
          if Ix eq 0 then begin
            Samples   = S[Ix] + kS
            Lines     = L[Ix] + kL
          endif else begin
            Samples   = [Samples, S[Ix] + kS]
            Lines     = [Lines, L[Ix] + kL] 
          endelse
        endfor
      endif
    endif 
       
    ;------------------
    ;--- Processing ---
    ;------------------
    
    self.oStatus->Log_Status, $
      'Busy', 'Extracting purest pixel for image: '+ InFile, Module , /Log

    ;-------------------------
    ;--- Read purity image ---
    ;-------------------------
    
    PurityFile  = Purity_Dir + InFile + Purity_Suffix
    
    if ~file_test(PurityFile) then begin
      self.oStatus->Log_Status, 'Warning', 'Purity image not found: '+PurityFile, Module, /Log         
      continue
    endif
    
    envi_open_file, PurityFile, /no_realize, R_fid=PtyFid
    envi_file_query, PtyFid, ns=Pty_ns, nl=Pty_nl, nb=nb, dims=dims
    
    PtyImage = envi_get_data(fid=PtyFid, dims=dims, pos=0)
      
    envi_file_mng, id=PtyFid, /remove

    ;-----------------------
    ;--- Read mask image ---                                                        ; LBER, Febryary 2013
    ;-----------------------

    if keyword_set(Mask_Dir) then begin   
      if keyword_set(Mask_Suffix) then begin
            
        MaskFile  = Mask_Dir + InFile + Mask_Suffix         

        if file_test(MaskFile) then begin
   
          envi_open_file, MaskFile, /no_realize, R_fid=MskFid
          envi_file_query, MskFid, ns=Msk_ns, nl=Msk_nl, dims=dims
    
          MskImage = envi_get_data(fid=MskFid, dims=dims, pos=0)
      
          envi_file_mng, id=MskFid, /remove
     
          if Pty_ns eq Msk_ns and Pty_nl eq Msk_nl then begin
       
            self.oStatus->Log_Status, 'Ok', 'Taking into accout the mask image: '+ MaskFile, Module, /Log
             
            i = where(MskImage eq 0, c)
            if c gt 0 then  PtyImage[i] = !VALUES.F_NAN 
        
          endif else begin
          
            self.oStatus->Log_Status, 'Warning', 'Mask/Purity image inconsistency: '+InFile, Module, /Log              
          endelse
        endif else begin
      
          self.oStatus->Log_Status, 'Warning', 'Mask image not found: '+MaskFile, Module, /Log
        endelse
      endif else begin
      
        self.oStatus->Log_Status, 'Warning', 'No mask suffix was specified: '+Mask_Dir+InFile , Module, /Log
      endelse
    endif 
    
    ;----------------------------------------
    ;--- Exclude already found endmembers ---
    ;----------------------------------------
     
    if cExclude gt 0 then begin 
      ptyImage[Samples, Lines] = !VALUES.F_NAN
    endif
      
    self.oStatus->Log_Status, 'Ok', 'Taking into accout the purity image: '+ PurityFile, Module, /Log

    if ~keyword_set(Rule_Dir) then begin

      ;----------------------------------------------------------
      ;--- First entrance, don't take rule image into account ---
      ;----------------------------------------------------------
         
      FoundMin = min(PtyImage, Index, /nan)
    
      if Fix eq 0 then FinalMin = FoundMin
    
      if FoundMin le FinalMin then begin
    
        FinalMin        = FoundMin
        Final_FileIx    = FileIx
        Final_L         = Index / Pty_ns
        Final_S         = Index - Final_L * Pty_ns
      endif    
    endif else begin
    
      ;-------------------------------------
      ;--- Take rule images into account ---
      ;-------------------------------------   
      
      RuleFile = Rule_Dir + InFile + '_rule'    
          
      self.oStatus->Log_Status, 'Ok', 'Taking into account the rule image: '+RuleFile, Module, /Log   

      ; Make new image, taking minimum for ech pixel
      ; Take maximum image value

      envi_open_file, RuleFile, /no_realize, R_fid=RuleFid
      envi_file_query, RuleFid, ns=Rule_ns, nl=Rule_nl, nb=nb, dims=dims
      
      if Rule_ns ne Pty_ns or Rule_nl ne Pty_nl then begin
        self.oStatus->Log_Status, 'Error', $
          '  Inconsitency between RuleFile and PurityFile: '+ $
            file_basename(RuleFile)+' - '+file_basename(PurityFile), Module, /Log      
        return
      endif

      for Ix=0, nb-1 do begin
    
        RuleImage =  envi_get_data(fid=RuleFid, dims=dims, pos=Ix)  ; LDA

        if Ix eq 0 then FinalRule = RuleImage
        
        FinalRule = FinalRule < RuleImage   
      endfor

      envi_file_mng, id=RuleFid, /remove      
      
      FoundMax = max(FinalRule / PtyImage, Index, /nan)     

      if Fix eq 0 then FinalMax = FoundMax
    
      if FoundMax ge FinalMax then begin
    
        Final_Max       = FoundMax
        Final_FileIx    = FileIx
        Final_L         = Index / Rule_ns
        Final_S         = Index - Final_L * Rule_ns
      endif           
    endelse   
  endfor
  
  self.oStatus->Progress_clear, /RESET   
    
  if n_elements(Final_FileIx) eq 0 then begin 
    self.oStatus->Log_Status, 'Error', 'No valid endmember found.', Module, /Log                           
    return
  endif
 
  Purest = {ImageFile:(*self.pImageInfo)[Final_FileIx].ImageFile, $
            S:Final_S, $
            L:Final_L}
            
  self.oStatus->Log_Status, 'Ok', 'Found pure pixel in file: '+$
      file_basename((*self.pImageInfo)[Final_FileIx].ImageFile)+ $
      ' at location S: '+strtrim(string(Final_S),2)+' L: '+strtrim(string(Final_L),2), $
    Module, /Log  
  
  self.oStatus->Progress_clear, /RESET
end
