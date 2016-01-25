;+
; NAME:
; 
;   IOScc_handle_input_dir
;
; PURPOSE:
; 
;   This common function is used to locate the image files to be handled by an application.
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
;   IOScc_handle_input_dir [, DIRNAME=string] [, FPATH=string] [, NFILES=value] [, OSTATUS=object reference] 
;                          [, PATH=string] [, PFILELIST=value] [, SUFFIX=string]
;
; ARGUMENTS 
; 
;   None
; 
; KEYWORDS
; 
;   DIRNAME:        Set this keyword to a string holding the title to be put on the dialog requesing for the image 
;                   directory. This string is presed by the string 'Select '. The default string is 'directory'.
;   FPATH:          When valid images were found this keyword will hold a string containing the full path to the 
;                   selected directory.
;   NFILES:         When valid images were found this keyword will hold the number of images found.
;   OSTATUS:        An object reference to an existing status window.
;   PATH:           Set this keyword to a string holding the initial path were to start browsing for the required 
;                   image directory.   
;   PFILELIST:      When valid images were found this keyword will hold a pointer to an array of strings containing
;                   the file base names of the found images.
;   SUFFIX:         Set this keyword to an optional scalar string or 1-element string array specifying a filename 
;                   suffix at the end of the basename to be added and searched for. The default is '*.hdr'.
; 
; RETURN VALUE:
; 
;   If a directory containing valid images was found 1 is returned els 0 is returned. 
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
; - Written by Luc Bertels, February 2010.
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
function IOScc_handle_input_dir, $
            DirName     = DirName, $
            Fpath       = Fpath, $
            pFileList   = pFileList, $
            nFiles      = nFiles, $
            Suffix      = Suffix, $
            Path        = Path, $
            oStatus     = oStatus
;-------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  Module = 'IOScc_handle_input_dir'

    if ~keyword_set(DirName) then DirName = 'directory'
 
    Title = 'Select '+DirName
    State = 'Ok'
    
    if obj_valid(oStatus) then oStatus->Log_Status, 'Request', Title
 
    if keyword_set(Suffix) then begin
      Search_for = '*.'+Suffix
    endif else begin
      Search_for = '*.hdr'
    endelse
 
    if keyword_set(Path) then begin
      FPath = dialog_pickfile(/directory, Title=Title, /must_exist, Path=Path)
    endif else begin
      FPath = dialog_pickfile(/directory, Title=Title, /must_exist)
    endelse

    if strlen(FPath) ne 0 then begin

      F = file_search(FPath+Search_for, count=nF)

      if nF gt 0 then begin
        Files = make_array( nF, /string)

        for Fi = 0, nF-1 do begin
          if Search_for[0] eq '*.hdr' then begin

            File = file_basename(F[Fi], '.hdr')
            
            case 1 of 
            file_test(FPath+File): Files[Fi] = File
            file_test(FPath+File+'.img'): Files[Fi] = File+'.img'
            else: begin
              if obj_valid(oStatus) then $
                oStatus->Log_Status, 'Error', 'File inconsistency found: '+File, Module, /Log
              
              return, 0
            endelse
            endcase
          endif else begin
            Files[Fi] = file_basename(F[Fi])
          endelse
        endfor
      endif else begin
        State = 'Nok'
      endelse
     
      pFileList = ptr_new(Files)
      nFiles    = n_elements(*pFileList)
    endif else State = 'Nok'
    
    if State eq 'Ok' then begin
      if obj_valid(oStatus) then oStatus->Log_Status, 'Ok', DirName + ' selected: '+Fpath, Module, /Log
          
      return, 1
    endif else begin
      oStatus->Progress_clear, /RESET 
      return, 0
    endelse
end