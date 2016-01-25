;+
; NAME:
; 
;   EOSap_ClassEditor__define
;
; PURPOSE:
; 
;   This procedure defines the 'EOSap_ClassEditor' class structure.
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
;------------------------------------------------------------------------
pro EOSap_ClassEditor__define
;------------------------------------------------------------------------

    struct = { EOSap_ClassEditor, $
                wWidget:                  0l, $
                ;
                oStatus:                  obj_new(), $
                oImageViewer:             obj_new(), $
                oClassImage:              obj_new(), $
                ;
                pLookUp:                  ptr_new(/allocate_heap), $
                pClassNames:              ptr_new(/allocate_heap), $
                pClassImage:              ptr_new(/allocate_heap), $
                pRGBdata:                 ptr_new(/allocate_heap), $  
                pOriginalClassNames:      ptr_new(/allocate_heap), $  
                ;
                pPolygonData:             ptr_new(/allocate_heap), $
                pPolygonIx:               ptr_new(/allocate_heap), $
                ;
                Roi_input:                0, $
                nl:                       0, $
                pRoi_x:                   ptr_new(/allocate_heap), $            ;
                pRoi_y:                   ptr_new(/allocate_heap), $            ;
                ;
                Class_Input_File:         ' ', $
                Class_Output_File:        ' ', $
                Image_button:             0, $
                ;
                WB_From_Group_Leader:     0l, $
                pWid_Actual_Classes:      ptr_new(/allocate_heap), $
                pActual_Class_Ids:        ptr_new(/allocate_heap), $
                WB_To_Group_Leader:       0l, $
                pWid_Available_Classes:   ptr_new(/allocate_heap), $
                ;
                Selected_Actual_Id:       0, $
                Selected_Available_Id:    0, $
                ;
                Dummy:                    0 $
                }
end