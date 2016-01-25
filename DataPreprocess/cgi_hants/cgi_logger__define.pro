; $Id: cgi_logger__define.pro,v 1.0 2005/07/20 16:05:29 wita Exp $
;
; Copyright (c) 2005, Allard de Wit
;+
; NAME:
;	  CGI_LOGGER__DEFINE
;
; PURPOSE:
;	  Provides simple logging under IDL
;
;
; CALLING SEQUENCE:
;	  Logger_object = OBJ_NEW("cgi_logger")
;
; INPUTS:
;	  None
;
; KEYWORD PARAMETERS:
;	  LOG_FILE:	String keyword providing name of the log file for output.
;	 	          Using 'IDL_logger.txt' if not specified.
;
;   LOG_INIT_MESSAGE: First string written to the log file in order to 
;                     identify the log file. If not specified a default
;                     string will be used.
;
; OUTPUTS:
;   None
;
;
; EXAMPLE:
;		 logger = OBJ_NEW("cgi_logger", log_file="my_log.txt",$
;                      log_init_message="My first log file")
;
;   logger->add_log_message, "IDL is Cool"  
;   logger->add_log_message, ["Python is even cooler", "Or you better use both!"] 
;   logger->write_log_messages
;   OBJ_DESTROY, logger
;
; MODIFICATION HISTORY:
; 	Written by:	Allard de Wit, July 2005.
;-

PRO cgi_logger::write_log_messages
  
  PRINT, "Writing log to: " + *self.log_filename
  OPENW, unit, *self.log_filename, /GET_LUN
  log = *self.log_messages
  FOR i=0L, N_ELEMENTS(log)-1 DO PRINTF, unit, log[i]
  PRINTF, unit, "Log closed at: " + SYSTIME()
  CLOSE, unit & FREE_LUN, unit

END

;------------------------------------------------------------

PRO cgi_logger::add_log_message, str
  
  str=STRING(str)
  *self.log_messages=[*self.log_messages, str]

END

;------------------------------------------------------------

FUNCTION cgi_logger::cleanup

  PTR_FREE, self.log_filename, self.log_messages

END

;------------------------------------------------------------

FUNCTION cgi_logger::init, log_file=log_file, log_init_message=log_init_message
  
  IF N_ELEMENTS(log_init_message) EQ 0 THEN $
    log_init_message="Simple IDL logger created by Allard de Wit - 2005"
  IF N_ELEMENTS(log_file) EQ 0 THEN $
    log_file="IDL_logger.txt"
  self.log_filename=PTR_NEW(log_file)
  start_message=[STRING(log_init_message), "Log created at: " + SYSTIME()]
  self.log_messages=PTR_NEW(start_message)
  print, "Logger initialised!"
  RETURN, 1

END

;------------------------------------------------------------

PRO cgi_logger__define

	void={cgi_logger, $
				log_messages:PTR_NEW(), $     ; ptr to log messages
        log_filename:PTR_NEW()}       ; ptr to log filename

END
