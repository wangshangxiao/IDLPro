;+
; NAME:
; 
;   IOSocProfileViewer__define
;
; PURPOSE:
; 
;   This procedure defines the 'IOSocProfileViewer' class structure.
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
;   None
;
; ARGUMENTS: 
; 
;   None
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
pro IOSocProfileViewer__define
;------------------------------------------------------------------------------------------------------------

  struct = { IOSocProfileViewer,  $
        ;
        wB_ProfileViewer:               0l, $
        oStatus:                        obj_new(), $
        ;
        oInform_user:                   obj_new(), $
        ;
        xTitle:                         '', $
        yTitle:                         '', $
        ;
        plot_Dims:                      [0,0], $
        ;
        oProfile_window:                obj_new(), $
        oPlot_View:                     obj_new(), $
        oPlot_Model:                    obj_new(), $
        oPlot_1st_Profile:              obj_new(), $
        oPlot_2nd_Profile:              obj_new(), $ 
        oPlot_x_Title:                  obj_new(), $
        oPlot_y_Title:                  obj_new(), $
        oPlot_x_Axis:                   obj_new(), $
        oPlot_y_Axis:                   obj_new(), $
        oPlot_Min:                      obj_new(), $
        oPlot_Max:                      obj_new(), $
        oPlot_Min_text:                 obj_new(), $
        oPlot_Max_text:                 obj_new(), $   
        ;
        pX:                             ptr_new(/allocate_heap), $
        pPlot_1st_Profile:              ptr_new(/allocate_heap), $
        pPlot_2nd_Profile:              ptr_new(/allocate_heap), $
        ;
        Plot_range_selected:            0, $
        Plot_Zoom:                      0, $
        Plot_Set_X_range:               [0., 0.], $
        Plot_View_X_range:              [0., 0.], $        
        ;
        dummy:                          0 $
        }
end        