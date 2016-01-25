;+
; NAME:
; 
;   EOSap_ClassEditor::show_actual_classes
;
; PURPOSE:
; 
;   This procedure method is used to visualize the classes selected by the drawn polygon.
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
;   Obj->[EOSap_ClassEditor::]show_actual_classes
;
; ARGUMENTS 
; 
;  None
; 
; KEYWORDS
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
; - Updated for the Colibri release, February 2013.
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
pro EOSap_ClassEditor::Show_Actual_Classes
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate

  self.oClassImage->Get, /FOR_POLYGON, xpts=*self.pRoi_x, ypts=*self.pRoi_y, $
    PolygonData=PolygonData, PolygonIx=PolygonIx

  ActualClasses   = PolygonData[UNIQ(PolygonData, SORT(PolygonData))]
  nActualClasses  = n_elements(ActualClasses)

  WB_Class_From = widget_info(self.wWidget, find_by_uname='WB_Class_From')

  if widget_info(self.WB_From_Group_Leader, /valid_id) then $
    widget_control, self.WB_From_Group_Leader, /destroy

  if nActualClasses gt 8 then begin

    ySize = nActualClasses * 20 + 10

    WB_Class = widget_base(WB_Class_From, /column, xsize=235, ysize=ySize, /scroll, $
      x_scroll_size=220, y_scroll_size=162, space=0)
  endif else begin
    WB_Class = widget_base(WB_Class_From, /column, xsize=235, ysize=250, space=0)
  endelse
  
  aWid_Actual_Classes = lonarr(nActualClasses)
  
  for Ix=0, nActualClasses-1 do begin

    WB_From_Class = widget_base(WB_Class, /row, xsize=250, ysize=20, group_Leader=WB_Class)
      W_draw  = widget_draw(WB_From_Class, /color_model, graphics_level=1, $
        xsize=20, ysize=15, group_Leader=WB_Class, /button_events, $
        event_pro='EOSap_ClassEditor_FromClass_EH', uvalue=self)
      W_label = widget_label(WB_From_Class, value=(*self.pClassNames)[ActualClasses[Ix]], $
        group_Leader=WB_Class)

      widget_control, W_draw, GET_VALUE=oWindow
      wset, owindow
      Color = (*self.pLookUp)[*,ActualClasses[Ix]]
      erase, ishft(ulong(Color[2]),16)+ishft(ulong(Color[1]),8)+ulong(Color[0])

    aWid_Actual_Classes[Ix] = W_draw
  endfor

  self.WB_From_Group_Leader = WB_Class

  if ptr_valid(self.pWid_Actual_Classes)  then ptr_free,self.pWid_Actual_Classes
  if ptr_valid(self.pActual_Class_Ids)    then ptr_free,self.pActual_Class_Ids
  if ptr_valid(self.pPolygonData)         then ptr_free,self.pPolygonData
  if ptr_valid(self.pPolygonIx)           then ptr_free,self.pPolygonIx

  self.pWid_Actual_Classes  = ptr_new(aWid_Actual_Classes)
  self.pActual_Class_Ids    = ptr_new(ActualClasses)
  self.pPolygonData         = ptr_new(PolygonData)
  self.pPolygonIx           = ptr_new(PolygonIx)
end