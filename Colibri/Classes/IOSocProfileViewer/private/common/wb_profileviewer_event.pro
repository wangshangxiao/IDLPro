;+
; NAME:
; 
;   WB_PROFILEVIEWER_EVENT
;
; PURPOSE:
; 
;   This private procedure method is the primary profile viewer event handler. It redirects the event to the 
;   corresponding event handler.
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
;   The procedure is called automatically when an event occures.
;
; ARGUMENTS: 
; 
;   event
; 
; KEYWORDS:
; 
;   None
; 
; RETURN VALUE:
; 
;   None 
;
; KNOWN ISSUES:
; 
;   None
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
pro WB_ProfileViewer_Event, event
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  WB_ProfileViewer = widget_info(event.top, find_by_uname='WB_ProfileViewer')
  
  widget_control,WB_ProfileViewer, get_uvalue=oProfileViewer

  if ~obj_valid(oProfileViewer) then return

  case event.id of
    Widget_Info(WB_ProfileViewer, FIND_BY_UNAME='Wd_Profile'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_DRAW' )then $
        oProfileViewer->handle_profile_event, Event
    end

    else:
  endcase
end