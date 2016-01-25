;+
; NAME:
; 
;   IOSocImageViewer__define
;
; PURPOSE:
; 
;   This procedure defines the 'IOSocImageViewer' class structure.
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
pro IOSocImageViewer__define
;------------------------------------------------------------------------------------------------------------

  struct = { IOSocImageViewer,  $
        ;
        wB_ImageViewer:                 0l, $
        oStatus:                        obj_new(), $
        ;
        oInform_user:                   obj_new(), $
        ;
        pImage:                         ptr_new(/allocate_heap), $
        ImageInfo:                      {ImageInfo, ns:0, nl:0, nb:0, xcoord:0d, ycoord:0d, psx:0d, psy:0d}, $
        pLookup:                        ptr_new(/allocate_heap), $
        ;
        Scroll_Pixel_Location:          [0, 0], $
        Image_Pixel_Location:           [0, 0], $
        Actual_Pixel_Location:          [0, 0], $
        ;
        pPolyGons:                      ptr_new(/allocate_heap), $
        pPolyGonsIx:                    ptr_new(/allocate_heap), $
        pPolyGonsColors:                ptr_new(/allocate_heap), $
        ;
        pPolyLines:                     ptr_new(/allocate_heap), $
        pPolyLinesIx:                   ptr_new(/allocate_heap), $
        pPolyLinesColors:               ptr_new(/allocate_heap), $
        ;
        pROIs:                          ptr_new(/allocate_heap), $
        ;
        Scroll_dims:                    [0, 0], $
        Image_dims:                     [0, 0], $
        Zoom_dims:                      [0, 0], $
        ;
        Image_Image_Coords:             [0d, 0d, 0d, 0d], $
        Image_Zoom_Coords:              [0d, 0d, 0d, 0d], $
        ;
        Image_Requested_Zfac:           0., $
        Zoom_Requested_Zfac:            0., $
        ;
        Scroll_Rfac:                    0d, $
        Image_Rfac:                     0d, $
        Zoom_Rfac:                      0d, $
        ;
        Scroll_button_press:            0, $
        Image_button_press:             0, $
        Zoom_button_press:              0, $
        ;
        ZoomCrossHair:                  0, $
        ;
        oScroll:                        obj_new(), $
        oImage:                         obj_new(), $
        oZoom:                          obj_new(), $
        ; 
        oPalette:                       obj_new(), $
        oScroll_View:                   obj_new(), $
        oScroll_Model:                  obj_new(), $
        oScroll_Image:                  obj_new(), $
        oScroll_Zoombox_Polygon:        obj_new(), $
        oScroll_Vector_Polygon:         obj_new(), $
        oScroll_Vector_Polyline:        obj_new(), $
        oImage_View:                    obj_new(), $
        oImage_Model:                   obj_new(), $
        oImage_Image:                   obj_new(), $
        oImage_Zoombox_Polygon:         obj_new(), $
        oImage_Vector_Polygon:          obj_new(), $        
        oImage_Vector_Polyline:         obj_new(), $        
        oImage_Zoom_In_Out_Polyline:    obj_new(), $   
        oZoom_View:                     obj_new(), $
        oZoom_Model:                    obj_new(), $
        oZoom_Image:                    obj_new(), $
        oZoom_Zoom_In_Out_Polyline:     obj_new(), $
        oZoom_Zoom_CrossHair:           obj_new(), $ 
        oZoom_Vector_Polygon:           obj_new(), $
        oZoom_Vector_Polyline:          obj_new(), $
        ;
        dummy:                          0 $
        }
end