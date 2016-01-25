;+
; NAME: 
; 
;   EOSocLibraries::Get_Libraries
;
; PURPOSE:
;
;   This method procedure returns the spectral libraries with their corresponding class names and class 
;   colors. 
;
; AUTHOR:
;
;       Luc Bertels
;       TAP - Teledetection and Earth Observation Processes 
;       VITO - Flemish Institute for Technological Research
;       Boeretang 200 
;       B-2400 Mol, Belgium 
;       http://www.vito.be
;
; CALLING SEQUENCE:
; 
;   Obj->[EOSocLibraries::]Get_Libraries, LibDir, ACLASS_COLORS=variable, ACLASS_NAMES=variable, 
;     PLIBRARIES=variable
;
; ARGUMENTS:  
; 
;   LibDir:         A string holding the directory where the library files are stored.
;   
; KEYWORDS:
;
;   ACLASS_COLORS:  Set this keyword equal to a named variable that, on return, holds an array of RGB color
;                   values for the found classes. The first class color is automatically set to [0, 0, 0]. 
;   ACLASS_NAMES:   Set this keyword equal to a named variable that, on return, holds an array of strings
;                   containing the class names from the available libraries found in the LibDir. Each class        
;                   name is extracted and named after the library name. The first entry in this string array 
;                   is automatically set to 'Unclassified'.
;   PLIBRARIES:     Set this keyword equal to a named variable that, on return, holds an array of pointers. 
;                   Each pointer points to a structure array where each element has the structure:
;                   {ImageFile:string, RoiId:value, Spectra:float array} 
;                                 
; KNOWN ISSUES:
;
;       None.
;
; MODIFICATION HISTORY:
;
;       Written by Luc Bertels, June 2010.
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
pro EOSocLibraries::Get_Libraries, $
                          LibDir, $
                          pLibraries=pLibraries, $
                          aClass_Names=aClass_Names, $
                          aCLass_Colors=aCLass_Colors
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Module = 'EOSocLibraries::Get_Libraries'

  ;---------------------------------
  ;--- Parameters presence check ---
  ;---------------------------------
  
  if ~arg_present(aCLass_Colors)  then Keyword = 'pRoiList'  
  if ~arg_present(aClass_Names)   then Keyword = 'pImageList' 
  if ~arg_present(pLibraries)     then Keyword = 'pImageList' 
  if ~keyword_set(LibDir)         then Argument = 'LibDir'   
      
  if keyword_set(Argument) then $
    self.oStatus->Log_Status, 'Error', 'Missing argument: '+Argument, Module, /Log
  
  if keyword_set(Keyword) then $
    self.oStatus->Log_Status, 'Error', 'Missing keyword: '+Keyword, Module, /Log
  
  if keyword_set(Argument) or keyword_set(Argument) then return  

  ;--------------------------------------
  ;--- Search for available libraries ---
  ;--------------------------------------
  
  if ~keyword_set(LibDir) then LibDir = ''
  
  LibFiles      = file_search(LibDir, '*.lib', count=nLibFiles)  

  if nLibFiles eq 0 then begin
    self.oStatus->Log_Status, 'Error', 'No library files found.', Module, /Log
    return
  endif
 
  FoundClasses  = strarr(nLibFiles)
  
  for Ix=0, nLibFiles-1 do FoundClasses[Ix] = file_basename(LibFiles[Ix], '.lib')
    
  ;----------------------------------------------------------
  ;--- Return all libraries if no specific were requested ---
  ;----------------------------------------------------------
    
  if ~keyword_set(aClass_Names) then aClass_Names = ['Unclassified', FoundClasses]

  FoundClasses  = 'Unclassified'
  aCLass_Colors = [0, 0, 0]
  nClasses      = n_elements(aClass_Names)
  nLibraries    = 0    
  Color         = [0, 0, 0]
  nBands        = 0
        
  ;-------------------------------------------------
  ;--- Extract library for the requested classes ---
  ;-------------------------------------------------
  
  for Ix=0, nClasses-1 do begin
     
    if obj_valid(self.oStatus) then begin
      self.oStatus->Progress_Set, slow=[Ix, nClasses]
      
      self.oStatus->Log_Status, 'Busy', 'Reading library for class: '+aClass_Names[Ix]
    endif    
  
    for Cix=0, nLibFiles-1 do begin
    
      if file_basename(LibFiles[Cix], '.lib') eq aClass_Names[Ix] then begin

        nSpectra      = file_lines(LibFiles[Cix]) - 2
    
        if nSpectra le 0 then continue
        
        FoundClasses  = [FoundClasses, aClass_Names[Ix]]   
        Tmp           = strarr(nSpectra)
  
        openr, lun, LibFiles[Cix], /get_lun
        readf, lun, Color
        readf, lun, nBands
        readf, lun, Tmp
        free_lun, lun
        
        aCLass_Colors  = [[aCLass_Colors], [Color]] 
        Lib     = {ImageFile:'', RoiId:0, Spectra:fltarr(nBands)}
        Library = replicate(Lib, nSpectra)
        
        for Lix=0, nSpectra-1 do begin
        
          if obj_valid(self.oStatus) then self.oStatus->Progress_Set, Fast=[Lix, nSpectra]
        
          T = strsplit(Tmp[Lix], ' ', /extract)
          Library[Lix].ImageFile  = T[0]
          Library[Lix].RoiId      = T[1]
          Library[Lix].Spectra    = T[2:nBands+1]
        endfor
        
        if obj_valid(self.oStatus) then self.oStatus->Progress_Clear, /FAST
        
        if nLibraries++ eq 0 then pLibraries = ptr_new(Library) else $
            pLibraries = [pLibraries, ptr_new(Library)]
      endif
    endfor
  endfor
        
  if obj_valid(self.oStatus) then self.oStatus->Progress_clear, /RESET

  if ~ptr_valid(pLibraries[0]) then begin
    self.oStatus->Log_Status, 'Error', 'No libraries were found.', Module, /Log
    return
  endif
end