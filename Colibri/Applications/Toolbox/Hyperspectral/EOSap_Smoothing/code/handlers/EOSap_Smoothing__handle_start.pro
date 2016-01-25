;+
; NAME:
; 
;   EOSap_Smoothing::handle_start
;
; PURPOSE:
; 
;   This procedure methode is used to apply the smoothing factors to the selected images.
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
;   Obj->[EOSap_Smoothing::]handle_start
;
; ARGUMENTS 
; 
;   None
; 
; KEYWORDS
; 
;   None
; 
; RETURN VALUE:
; 
;   None 
; 
; KNOWN ISSUES:
; 
;   None.
;
; MODIFICATION HISTORY:
; 
; - Written by Luc Bertels, June 2010.
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
;-------------------------------------------------------------------------
pro EOSap_Smoothing::handle_start
;-------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
    
  if self->check_input() eq 0 then return

  ;-------------------------------
  ;--- Retrieve smooth factors ---
  ;-------------------------------
  
  FilesIx = widget_info (widget_info(self.wWidget, find_by_uname='Wl_Image_list'), /list_select)
  
  if FilesIx[0] eq -1 then begin
    self.oStatus->Log_Status, 'Warning', 'Select files to be smoothed.'
    return
  endif
  
  if ptr_valid(self.pScenario) then begin
    nScenarios  = n_elements(*self.pScenario)
    nSmtLoops   = nScenarios
    aFid        = lonarr(nScenarios)
  endif else begin
    nScenarios  = 0
    nSmtLoops   = 1
    SmoothFac   = *self.pSmoothFac
  endelse
  
  ;-----------------------------------------
  ;--- Check if bands need to be removed ---
  ;-----------------------------------------
        
  if ptr_valid(self.pScenario) then begin
      
    SmoothFac   = *((*self.pScenario)[0].SmtFac)
          
    for Ix=1, nScenarios-1 do begin
      
      iR  = where(*((*self.pScenario)[Ix].SmtFac) eq -3, cR) 
         
      if cR gt 0 then SmoothFac[iR] = -3
    endfor
  endif
      
  iR  = where(SmoothFac eq -3, complement=iKeep, cR)  
        
  ;------------------
  ;--- Processing ---
  ;------------------
  
  for FileIx=0, n_elements(FilesIx)-1 do begin
  
    ;-------------------------------
    ;--- Find correct input file ---
    ;-------------------------------
    
    if file_test(self.InDir+(*self.pFileList)[FilesIx[FileIx]]) eq 1 then $
      InFile = self.InDir+(*self.pFileList)[FilesIx[FileIx]]
    if file_test(self.InDir+(*self.pFileList)[FilesIx[FileIx]]+'.img') eq 1 then $
      InFile = self.InDir+(*self.pFileList)[FilesIx[FileIx]]+'.img'

    Outfile = self.OutDir+(strsplit(file_basename(InFile), '.', /extract))[0]+'_smt'

    envi_open_file, InFile, R_Fid=InFid
    envi_file_query, InFid, nb=nb, nl=nl, ns=ns, dims=dims, wl=wl, fwhm=fwhm
      
    mapinfo   = envi_get_map_info(fid=InFid)
    OutLine   = fltarr(Ns, Nb)
    pos       = lindgen(nb)  
          
    ;------------------------------
    ;--- Open mask files in any ---
    ;------------------------------
        
    for Ix=0, nScenarios-1 do begin    
    
      MskFile   = ''
      Tmp       = file_basename((*self.pFileList)[FilesIx[FileIx]])
      FileName  = (strsplit(Tmp, '.', /extract))[0]
      
      if file_test(self.InDir+FileName+(*self.pScenario)[Ix].Suffix) eq 1 then $
        MskFile = self.InDir+FileName+(*self.pScenario)[Ix].Suffix
      if file_test(self.InDir+FileName+(*self.pScenario)[Ix].Suffix+'.img') eq 1 then $
        MskFile = self.InDir+FileName+(*self.pScenario)[Ix].Suffix+'.img'       

      if strlen(MskFile) eq 0 then begin
      
        self.oStatus->Log_Status, 'Error', 'Mask file for scenario: '+(*self.pScenario)[Ix].Suffix+$
          ' not found.'
        return
      endif
      
      envi_open_file, MskFile, R_Fid=MskFid
      
      aFid[Ix] = MskFid
    endfor
          
    ;-----------------------
    ;--- Start smoothing ---
    ;-----------------------

    self.oStatus->Progress_Set, slow=[FileIx, n_elements(FilesIx)]
    self.oStatus->Log_Status, 'Busy', 'Smoothing image: '+  (*self.pFileList)[FilesIx[FileIx]]  
          
    openw, OutLun, Outfile, /get_lun   
           
    for HLix = 0, Nl-1 do begin
          
      self.oStatus->Progress_Set, Fast=[HLix, Nl]

      Hdata       = float(envi_get_slice(fid=InFid, pos=pos, line=HLix, xs=0, xe=Ns-1))

      for Ix=0, nSmtLoops-1 do begin
      
        ;-------------------------------------------------
        ;--- Retrieve the correct smoothing parameters ---
        ;-------------------------------------------------
      
        if ptr_valid(self.pScenario) then begin
        
          Mask      = fix(envi_get_slice(fid=aFid[Ix], pos=[0], line=HLix, xs=0, xe=Ns-1))
          iPixels   = where(Mask eq 1, c)
        
          if c eq 0 then continue    
          SmoothFac = *((*self.pScenario)[Ix].SmtFac)              
        endif else begin
        
          SmoothFac = *self.pSmoothFac
          iPixels   = indgen(ns)
        endelse
 
        OutLine     = Hdata
        MaxSmooth   = max(SmoothFac)

        iN  = where(SmoothFac ne -1 and $
                    SmoothFac ne -2 and $
                    SmoothFac ne -3 and $
                    SmoothFac ne -4 , cN)
        iI  = where(SmoothFac eq -1, cI)
        iZ  = where(SmoothFac eq -2, cZ)
        iP  = where(SmoothFac eq -4, cP)
      
        ;----------------------------------
        ;--- Apply the smoothing values ---
        ;----------------------------------
    
        for Fx = 0, MaxSmooth-1 do begin
          for Bix=0, nb-1 do begin
            if SmoothFac[Bix] gt 0 then begin

              SmoothFac[Bix]--

              case Bix of
              0: OutLine[iPixels, Bix] = (( 3*Hdata[iPixels,Bix] + Hdata[iPixels,Bix+1] ) / 4)
            
              nb-1: OutLine[iPixels, Bix] = (( 3*Hdata[iPixels,Bix] + Hdata[iPixels,Bix-1] ) / 4)          
            
              else: OutLine[iPixels, Bix] = (( Hdata[iPixels,Bix-1] + 2*Hdata[iPixels,Bix] + Hdata[iPixels,Bix+1]) / 4)
              endcase
            endif
          endfor
           
          Hdata = OutLine
        endFor
      
        ;-------------------------
        ;--- Set bands to zero ---
        ;-------------------------
      
        if cZ gt 0 then begin
          Tmp = Hdata[iPixels, *]
          Tmp[*,iZ]=0
          Hdata[iPixels, *] = Tmp        
        endif

        ;-------------------------
        ;--- Interpolate bands ---
        ;-------------------------

        if cP gt 0 then begin
        
          aSEi = [0, 0]                           ; To define the number of interpolation groups
          iS   = -1
    
          for i=0, cp-1 do begin
  
            if Is eq -1 then iS = (ip[i] - 1) > 0   ; start of new interval
            iE = (ip[i] + 1) < (nb-1)               ; end of the interval
  
            if i+1 eq cp then begin
        
              aSEi  = [[aSEi], [iS, iE]]
              iS    = -1
            endif else begin
    
              if iE ne ip[i+1] then begin
                aSEi  = [[aSEi], [iS, iE]]
                iS    = -1
              endif
            endelse   
          endfor

          ni = n_elements(aSEi[0,*])
  
          for IIx=1, ni-1 do begin
    
            iS =aSEi[0,IIx]         ; start of the interval
            iE = aSEi[1,IIx]        ; end of the interval
      
            sR  = Hdata[iPixels,iS]
            sW  = (*self.pWl)[iS]
            dR  = Hdata[iPixels,iE] - Hdata[iPixels,iS]
            dW  = (*self.pWl)[iE] - (*self.pWl)[iS]
            a   = dR / dW
     
            for iW = iS+1, iE-1 do begin
             aW = (*self.pWl)[iW]
             Hdata[iPixels,iW] = sR + (aW - sW)*a
            endfor
          endfor
        endif
      endfor 
      
      ;--------------------
      ;--- Remove bands ---
      ;--------------------
      
      if cR gt 0 then begin
        Hdata = Hdata[*, iKeep]
      endif
      
      writeu, OutLun, Hdata
    endfor

    free_lun, OutLun

    for Ix=0, nScenarios-1 do envi_file_mng, id=aFid[Ix], /remove

    descrip = 'Smoothed image bij EOSap_Smoothing'

    nb = n_elements(iKeep)

    envi_setup_head, fname=OutFile, ns=ns, nl=nl, nb=nb, data_type=4, map_info=mapinfo, $
      interleave=1, /open, /write, r_fid=Outfid, wl=wl[iKeep], fwhm=fwhm[iKeep], $
      descrip = descrip

    envi_file_mng, id=Outfid, /remove
    envi_file_mng, id=InFid, /remove
    
    free_lun, OutLun
    
    self.oStatus->Progress_Clear, /FAST    
  endfor

  self.oStatus->Progress_Clear, /SLOW
  self.oStatus->Log_Status, 'Ok', ''
end
