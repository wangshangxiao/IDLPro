;+
; NAME:
;    
;   IOSocStatus::Init
;
; PURPOSE:
; 
;   This method function creates and initializes an instance of the class 'IOSocStatus'. The obtained object 
;   is used to update the status window with the state and progress of the running process.
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
;   Result = OBJ_NEW('IOSocStatus' [, wWidget] [, BITMAP=string] [, LOG_DIRECTORY=string] 
;     [, LOG_FILE_PREFIX=string] [, LOGINFO=string] [, /STATUSSCREEN] [, TITLE=string] 
;     [, USER=object reference)
;
; ARGUMENTS:         
; 
;   wWidget:              Widget_id of the Graphical User Interface which has a status object assigned. 
;                         The GUI should have at least three widgets:
;                           - A button widget defined as bitmap with username "Status_Led"
;                           - A text widget with username "Status_Line"
;                           - A draw widget with username "Status_ProgressBar"
;                         If this argument is not present a default status object is created.  
;                                   
; KEYWORDS:
;
;   BITMAP:               A string holding the path to the user bitmap image file holding the logo to be 
;                         placed in the logo button on the status window. This keyword allows the user, when 
;                         designing his application, to replace the default logo on the status window with the 
;                         designers specific logo and to specify his own designer info bitmap. The file 
;                         holding the info bitmap should be named according to the file holding the logo bitmap
;                         with the suffix ‘_info’ added. 
;   LOG_DIRECTORY:        A text string holding the path where the log file has to be written. If specified
;                         the log file is created and opened.
;   LOG_FILE_PREFIX:      A text string holding the file prefix of the log file to be created. If not 
;                         specified a default prefix is given to the log filename. The log filename 
;                         is automatically generated and contains the prefix, date and time, and the 
;                         extension '.txt'.
;   LOGINFO:              A string holding the info which has to be explicitly logged into the log file. As 
;                         long as no log file exists this and future log-info will be kept in memory. As soon
;                         as the Log directory is specified all the memory saved Log information is written 
;                         to the log file.
;   STATUSSCREEN:         Set this keyword to display the info in a Status Screen Draw widget instead of on 
;                         the Status Text widget.                 
;   TITLE:                A text string holding the title to be given to the newly created default status 
;                         window.
;   USER:                 The object reference which created the default status window.
;
; RETURN VALUE:
; 
;   The OBJ_NEW function returns an object reference to the newly-created object. 
;
; DEPENDENCIES:
; 
;   OSIocLog::ToLog
;   
; KNOWN ISSUES:
; 
;   None.
;
; EXAMPLE:
; 
;   (1): Creating the default status window. 
;        
;     oStatus = obj_new('IOSocStatus')
;     
;       The title of the default status object is passed via the TITLE keyword.
;       
;     oStatus = obj_new('IOSocStatus' TITLE = 'This is the title')
; 
;   (2): A GUI is present which also has the status widgets available (Status_Led, Status_Line and 
;        Status_ProgressBar. Here ‘wWidget’ is the top level widget ID. 
; 
;     oStatus = obj_new('IOSocStatus', wWidget)
;     
;   (3): An extensive example can be found in the examples subdirectory: IOSex_Status_Window
;                     
; MODIFICATION HISTORY:
; - Written by Luc Bertels, May 2010.
; - Added, LBer, February 2013: Check presence of the progress bar widget.
; - Added, LBer, February 2013: Status Screen added.
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
function IOSocStatus::Init, wWidget, $
                          Title           = Title, $
                          LogInfo         = LogInfo, $
                          Log_Directory   = Log_Directory, $
                          Log_File_Prefix = Log_File_Prefix, $
                          BitMap          = BitMap, $
                          oUser           = oUser, $
                          StatusScreen    = StatusScreen                            ; LBer February, 2013
;------------------------------------------------------------------------------------------------------------
compile_opt strictarr, logical_predicate
  
  if ~keyword_set(BitMap) then BitMap = '_VITO.bmp'  
    
  ;-- If no GUI is present, create the default status window.
  if ~arg_present(wWidget) then begin 
  
    ;-- Apply the default title if no title is specified.
    if ~keyword_set(Title) then Title = 'Status'

    ;-- Create the default status window.
    wWidget = Widget_Base(UNAME='wWidget' ,FRAME=1  $
      ,XOFFSET=0 ,YOFFSET=0 ,SCR_XSIZE=700 ,SCR_YSIZE=90  $
      ,TITLE=Title ,SPACE=3 ,XPAD=3 ,YPAD=3, /TLB_KILL_REQUEST_EVENTS )

    if keyword_set(StatusScreen) then begin                                     ; LBer February, 2013

      Wd_Status_Screen = Widget_Draw(wWidget, RETAIN=2 ,GRAPHICS_LEVEL=2, $     ; LBer February, 2013
        UNAME='Status_Screen' ,XOFFSET=50 ,YOFFSET=5  $                         ;  
        ,SCR_XSIZE=570 ,SCR_YSIZE=22)                                           ;
    endif else begin
    
      Status_Line = Widget_Text(wWidget, UNAME='Status_Line'  $
        ,XOFFSET=50 ,YOFFSET=5 ,SCR_XSIZE=570 ,SCR_YSIZE=22 ,XSIZE=20  $
        ,YSIZE=1)       
    endelse
    
    Status_ProgressBar = Widget_Draw(wWidget,  $
      UNAME='Status_ProgressBar' ,XOFFSET=50 ,YOFFSET=35  $
      ,SCR_XSIZE=570 ,SCR_YSIZE=22)

;    if float(envi_query_version()) lt 5. then begin

      Status_Led = Widget_Button(wWidget, UNAME='Status_Led'  $
        ,XOFFSET=10 ,YOFFSET=20 ,SCR_XSIZE=30 ,SCR_YSIZE=30 $
        ,/ALIGN_CENTER ,VALUE='_LampG.bmp' ,/BITMAP, Tooltip='Close Status Window.')
        
      WID_BUTTON_13 = Widget_Button(wWidget, UNAME='WID_BUTTON_About'  $
        ,XOFFSET=630 ,YOFFSET=12 ,SCR_XSIZE=60 ,SCR_YSIZE=40 $
        ,/ALIGN_CENTER ,VALUE=BitMap ,/BITMAP, uvalue=BitMap, Tooltip='Display Designer Info.')
;    endif else begin
;  
;      Status_Led = Widget_Button(wWidget, UNAME='Status_Led'  $
;        ,XOFFSET=10 ,YOFFSET=20 ,SCR_XSIZE=30 ,SCR_YSIZE=30 ,mask=0  $
;        ,/ALIGN_CENTER ,VALUE='_LampG.bmp' ,/BITMAP, Tooltip='Close Status Window.')
;      
;      WID_BUTTON_13 = Widget_Button(wWidget, UNAME='WID_BUTTON_About'  $
;        ,XOFFSET=630 ,YOFFSET=12 ,SCR_XSIZE=60 ,SCR_YSIZE=40 ,mask=0  $
;        ,/ALIGN_CENTER ,VALUE=BitMap ,/BITMAP, uvalue=BitMap, Tooltip='Display Designer Info.')
;    endelse

    CenterTLB, wWidget
      
    Widget_Control, /REALIZE, wWidget

    XManager, '_Handle_About', wWidget, /NO_BLOCK  
  
    widget_control, wWidget, set_uvalue=self  
  endif else begin
    if Widget_Info(Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_About'), /valid_id) eq 1 then $ 
      widget_control, Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_About'), set_uvalue=BitMap
  endelse
  
  ;-- Check if the passed User object is valid.
  if obj_valid(oUser) then begin 
  
    ;-- Save the object.
    if ptr_valid(self.pobjects) eq 0 then self.pobjects = ptr_new(oUser) else $
      *self.pobjects = [*self.pobjects, oUser]
  endif
  
  self.wWidget       = wWidget
  self.wStatusLed    = widget_info(self.wWidget, find_by_uname='Status_Led')   
  self.wStatusLine   = widget_info(self.wWidget, find_by_uname='Status_Line') 
  self.wStatusScreen = widget_info(self.wWidget, find_by_uname='Status_Screen') 

  ;-- Prepare the StatusScreen if requested. 
  if keyword_set(StatusScreen) or widget_info(self.wStatusScreen, /valid_id) then begin ; LBer February, 2013
                                                                                    ;
    widget_control, self.wStatusScreen, get_value = oWindow_Status_Screen           ; 
                                                                                    ;
    oView_Status_Screen   = obj_new('IDLgrView', color=[120,120,120], viewplane_rect=[-0.02,-1,4,4]) ;
    oModel_Status_Screen  = obj_new('IDLgrModel')                                   ;  
    oText_Status_Screen   = obj_new('IDLgrText');,  recompute_dimensions=2)         ;

    oView_Status_Screen->Add, oModel_Status_Screen                                  ;
    oModel_Status_Screen->Add, oText_Status_Screen                                  ;
    oWindow_Status_Screen->Draw, oView_Status_Screen                                ;
                                                                                    ;
    self.oWindow_Status_Screen  = oWindow_Status_Screen                             ;
    self.oView_Status_Screen    = oView_Status_Screen                               ;
    self.oText_Status_Screen    = oText_Status_Screen                               ;
                                                                                    ;
    oWindow_Status_Screen->Draw, oView_Status_Screen                                ;
  endif                                                                             ;

  ;-- Check if progress bar widget is present.                                                      ; LBer February, 2013
  ;-- Save important data to be used when updating the progress bar.
  if widget_info(widget_info(wWidget, find_by_uname='Status_ProgressBar'), /valid_id) then begin    ; LBer February, 2013
  
    self.thisWindow = !D.Window
    self.Scr_Xsize  = (widget_info(widget_info(wWidget, find_by_uname='Status_ProgressBar'), /geometry)).scr_xsize
  
    Widget_Control, widget_info(wWidget, find_by_uname='Status_ProgressBar'), Get_Value=wWdraw
  
    self.wProgressBar = wWdraw
  endif
  
  ;-- Prepare data to be written to the log file if present.
  If  ~keyword_set(LogInfo) then LogInfo = ''
  If  keyword_set(Title)    then LogInfo = ['Log-file for: '+Title, LogInfo]
  
  self.Status = 'Ok'
  
  ;-- Create the log file (if specified) and write the Log information.
  self->ToLog, $
            LogInfo, $
            Log_Directory    = Log_Directory, $
            Log_File_Prefix  = Log_File_Prefix
  
  return, 1
end