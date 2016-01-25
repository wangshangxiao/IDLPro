;+
; NAME: EOSap_Create_Synthetic_Image
;
; PURPOSE: 
;
;   This application procedure creates a synthetic hyperspectral image from spectra selected from an ENVI 
;   spectral library.
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
;   EOSap_Create_Synthetic_Image [, N_LEVELS=value] [, NOISE=array] [, OSTATUS=object reference] 
;                                [, OUTDIR=string] [, SPECLIB=string]
;                 
; ARGUMENTS
; 
;   None.
;                
; KEYWORDS
; 
;   N_LEVELS:       Set this keyword to a value containg the number of levels, i.e. the number of spectral
;                   mixtures to be genereated between two pure spectra from the spectral library. If the 
;                   input is an odd number it will be automatically incremented with one.
;                   The default is set to ten.
;   NOISE:          Set this keyword to a value or array of values containing the noise levels to be added to
;                   the generated synthetic spectra. White gaussian noise is added. If not specified no 
;                   noise is added. 
;   OSTATUS:        An object reference to an existing status window.    
;   OUTDIR:         Set this keyword to a string holding the path to the output directory where the synthetic
;                   images need to be written. The created synthetic image is named after the spectral 
;                   library input file name suffixed with the requested noise level, e.g. '_0dB'. If this 
;                   keyword is not set the application will present a dialog with a request to select the 
;                   output direectory.
;   SPECLIB:        Set this keyword to a string holding the full path and name of the ENVI spectral library
;                   for which a synthetic image has to be created.  If this keyword is not set the application 
;                   will present a dialog with a request to select the spectral library. The number of spectra
;                   contained in the spectral library is limited nine.
;                                   
; RETURN VALUE:
;   
;   None.
;
; DEPENDENCIES:
;    
;   None.
;         
; KNOWN ISSUES:
; 
;   None.   
;
; MODIFICATION HISTORY:
; 
;   - Written by Luc Bertels, September, 2012.
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
Pro EOSap_Create_Synthetic_Image, $
                          SpecLib     = SpecLib, $
                          OutDir      = OutDir, $
                          n_Levels    = n_Levels, $
                          Noise       = Noise, $
                          ;
                          oStatus     = oStatus
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Module = 'EOSap_Create_Synthetic_Image'
                        
  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------          
                  
  if ~keyword_set(SpecLib) then begin
    SpecLib = dialog_pickfile(Title='Select the ENVI spectral library', /must_exist)
    if strlen(SpecLib) le 3 then return
  endif

  if ~keyword_set(OutDir) then begin
    OutDir = dialog_pickfile(Title='Select output directory', /directory,/must_exist)
    if strlen(OutDir) le 3 then return
  endif
  
  ;--------------------------------------------------------
  ;--- Creation and initialization of the status window ---
  ;--------------------------------------------------------
    
  oStatus = IOScc_Init(oStatus, $
                        Title="Synthetic Image Creation", $
                        Log_DIRECTORY    = OutDir, $
                        Log_FILE_PREFIX  = 'SynthImage')   
  
  ;----------------------
  ;--- Initialization ---
  ;---------------------- 
  
  if ~keyword_set(n_Levels) then    n_Levels = 10  
  if ~keyword_set(Noise) then       Noise = [0]   
                                
  CellSize    = ++n_Levels + 5               
  KernelSize  = n_Levels
 
  envi_open_file, SpecLib, /no_realize, r_fid=Fid_SpecLib
  envi_file_query, Fid_SpecLib, ns=ns, nl=nl, wl=wl, def_bands=def_bands, $
                    file_type=file_type, spec_names=Spec_Names
                    
  nBands    = ns
  nSpectra  = nl 
  
  if nSpectra gt 9 then begin    
    oStatus->Log_Status, 'Error', 'Spectral Library file contains to many spectra: '+SpecLib, Module, /Log 
    return
  endif
  
  Spectra   = envi_get_data(fid=Fid_SpecLib, dims=[-1, 0, ns-1, 0, nl-1], pos=[0])

  
  if file_type ne 4 then begin    
    oStatus->Log_Status, 'Error', 'No Spectral Library file: '+SpecLib, Module, /Log 
    return
  endif

  OutFile = file_basename(SpecLib, '.txt')

  RandIndices   = intarr(nSpectra, nSpectra)
  OutImage      = fltarr(CellSize*nSpectra, CellSize*nSpectra, nBands)
  TempImage     = dblarr(CellSize*nSpectra, CellSize*nSpectra, nBands)
  Arr           = indgen(nSpectra)
  ImSize        = nSpectra * CellSize
  
  ;----------------------------------------------------------------------------------------
  ;--- Generate 2-dimentional matrix containing the random indices to the input spectra ---
  ;----------------------------------------------------------------------------------------

  oStatus->Log_Status, 'Ok', 'Requested levels: ' + strtrim(string(n_Levels-1),2), /Log 
  oStatus->Log_Status, 'Ok', 'Mixture model: ', Module, /Log 

  for Lix=0, nSpectra-1 do begin
  
    RandIndices[*,Lix] = Arr[sort(randomu(xx,n_elements(Arr)))]
    
    oStatus->Log_Status, 'Ok', strjoin(string(RandIndices[*,Lix])), Module, /Log 
  endfor
  
  oStatus->Progress_clear, /RESET
  
  ;-------------------------------------------------------------------------
  ;--- Create synthetic image of pure spectra based on the random matrix ---
  ;-------------------------------------------------------------------------
  
  for Lix=0, nSpectra-1 do begin
  
    oStatus->Log_Status, 'Ok', 'Spectrum '+strtrim(string[Lix],2)+': ' + $
      Spec_Names[Lix], Module, /Log 
  
    for Six=0, nSpectra-1 do begin
    
      SpecIx = RandIndices[Six, Lix]
      
      for L=0, CellSize-1 do begin
        for S=0, CellSize-1 do begin
    
          TempImage[Six*CellSize+S, Lix*CellSize+L, *] = Spectra[*, SpecIx]
    
        endfor
      endfor
    endfor
  endfor

  ;---------------------------------------------------------------
  ;--- Create linear mixed image by average convolution kernel ---
  ;---------------------------------------------------------------
  
  nNoiseFiles     = n_elements(Noise)
  pOutImageNoise  = ptrarr(nNoiseFiles)
  
  for Ix=0, nNoiseFiles-1 do pOutImageNoise[Ix] = ptr_new(fltarr(CellSize*nSpectra, CellSize*nSpectra, nBands))
  
  Kernel_radius = KernelSize / 2
  tD            = indgen(KernelSize)
  t1            = intarr(KernelSize)+1
  kL            = tD ## t1 - Kernel_radius
  kS            = transpose(kL)
  
  oStatus->Log_Status, 'Ok', 'Creating synthetic images'
  
  for Lix=0, ImSize-1 do begin
    oStatus->Progress_Set, Slow=[Lix, ImSize-1]
       
    for Six=0, ImSize-1 do begin
  
      oStatus->Progress_Set, Fast=[Six, ImSize-1]
  
      kS_S    = (Six - Kernel_radius) > 0
      kS_E    = (Six + Kernel_radius) < (ImSize - 1)
      kL_S    = (Lix - Kernel_radius) > 0
      kL_E    = (Lix + Kernel_radius) < (ImSize - 1)
      
      Kernel    = TempImage[kS_S:kS_E, kL_S:kL_E, *]
      
      OutImage[Six, Lix,*] = float((total(total(kernel, 2), 1)) / n_elements(Kernel[*,*,0]))
      
      for Ix=0, nNoiseFiles-1 do begin
      
        if Noise[Ix] eq 0 then begin
          (*pOutImageNoise[Ix])[Six, Lix,*] = OutImage[Six, Lix,*]
        endif else begin
          
          ;----------------------------------------------------
          ;--- Add the requested white gaussian noise level ---
          ;---    L = 20^10 * log (A1/A0) dB                ---
          ;---    A1/A0 = 10^(L/20)                         ---
          ;----------------------------------------------------
          
          AWGN = randomn(seed, nBands) / 10^(float(Noise[Ix])/20.)

          (*pOutImageNoise[Ix])[Six, Lix,*] = OutImage[Six, Lix,*] + OutImage[Six, Lix,*] * AWGN
        endelse
      endfor
    endfor
    
    oStatus->Progress_Clear, /Fast
  endfor
  
  ;-----------------------------
  ;--- Save synthetic images ---
  ;-----------------------------
  
  for Ix=0, nNoiseFiles-1 do begin

    out_name=OutDir+OutFile+'_'+strtrim(string(Noise[Ix]),2)+'dB'

    envi_enter_data, (*pOutImageNoise[Ix]), wl=Wavelength, r_fid=r_fid
    envi_file_query, r_fid, dims=dims, nb=nb
    t_fid = lonarr(nb) + r_fid
    pos=indgen(nBands)
    envi_doit, 'cf_doit', dims=dims, pos=pos, fid=t_fid, out_name=out_name
    envi_file_mng, id=r_fid, /remove
    
    oStatus->Log_Status, 'Ok', 'Creating synthetic image: '+out_name, Module, /Log
  endfor
    
  oStatus->Progress_clear, /RESET
end