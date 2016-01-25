; $Id: CGI_HANTS.pro,v 1.3 2005/07/18 16:35:52 scottm Exp $
;
;+
; NAME:
;   CGI_HANTS
;
; PURPOSE:
;   ENVI wrapper for fourier analyses through NDVI time series
;
; CATEGORY:
;   1D signal processing
;
; CALLING SEQUENCE:
;   CGI_HANTS, FET=value, FREQS=[f1,f2,f3,..], RANGE=[min,max], $
;          TAT=value, iMAX=value, /PLOT, DATA_IF=data input file,
;          MASK_IF=mask input file, HANTS_OF=output file for FFT components, $
;          STATUS_OF=output file with status information, $
;          SMOOTH_OF=output file with smoothed results, $
;          INTERP_OF=output file with interpolated results
;
; INPUTS:
;      Input: n-layer file with NDVI time-series
;      Mask:  1-layer file with a mask, processing will only be carried
;             out where mask has value '1'
;
; KEYWORD VARIABLES
;   Processing Keywords (OBLIGATORY):
;     FET:    Fit Error Tolerance, is maximum tolerable downward deviation
;             between fourier fit and NDVI data values (in DN values)
;     FREQS:  1D array with frequences that should be selected from the
;             fourier spectrum. i.e. freqs=[0,1,2,3] to use the fourier
;             compoments 0 (mean), 1 (1 sine wave), 2 (2 sine waves) and 3.
;     RANGE:  Array of size 2 to specify the minimum and maximum valid data
;             values. i.e. range=[1,254]
;     TAT:    Throw Away Treshold, is maximum number of NDVI observations that
;             can be discarded in the fitting process.
;     iMAX:   Maximum nr. of iterations to be performed during processing
;
;   I/O Keywords (OPTIONAL):
;     DATA_IF:  (full path to) Filename used as input for processing (NDVI),
;               will ask for filename if not specified
;     MASK_IF:  (full path to) Filename used as mask,
;               will ask for filename if not specified
;     HANTS_OF: (full path to) Filename used to write FFT components,
;               set this to an empty string ("") to avoid output being written
;               to disk.
;     STATUS_OF: (full path to) Filename used to write processing status info
;                set this to an empty string ("") to avoid output being written
;                to disk.
;     SMOOTH_OF: (full path to) Filename used to write smoothed results
;                set this to an empty string ("") to avoid output being written
;                to disk.
;     INTERP_OF: (full path to) Filename used to write interpolated results
;                set this to an empty string ("") to avoid output being written
;                to disk.
;
;   Other Keywords (OPTIONAL):"
;     LOG_FILE: (full path to) Filename used to write the log to."
;
;     PLOT:     Visualise the optimisation process. Only applied for debugging
;               because it is very slow!
;
; SIDE EFFECTS:
;   Will need ENVI to run
;
; MODIFICATION HISTORY:
;   Written by:  Allard de Wit, Auguest 2003
;   Modified by: Allard de Wit, April 2004 (General cleanup and improvements)
;   Modified by: Allard de Wit, July 2005 (specifying input on commandline added,
;                                          logging added)
;
; LICENSE:
;   This software is made available under the GPL. See http://www.gnu.org/licenses/gpl.html
;-

PRO cgi_hants,FET=FET,freqs=freqs,RANGE=RANGE,TAT=TAT,iMAX=iMAX,log_file=log_file, data_if=data_if, mask_if=mask_if, $
    hants_of=hants_of, status_of=status_of, smooth_of=smooth_of, $
    interp_of=interp_of

  COMPILE_OPT idl2
  
  ;Check for correct command-line inputs
  IF (N_ELEMENTS(freqs) EQ 0) THEN BEGIN
    PRINT, "USAGE: CGI_HANTS, FET=<value>, FREQS=[f1,f2,f3,..], RANGE=[min,max], $"
    PRINT, "       TAT=<value>, iMAX=<value>, /PLOT, DATA_IF=<data input file>,"
    PRINT, "       MASK_IF=<mask input file>, HANTS_OF=<output file for FFT components>, $"
    PRINT, "       STATUS_OF=<output file with status information>, $"
    PRINT, "       SMOOTH_OF=<output file with smoothed results>, $"
    PRINT, "       INTERP_OF=<output file with interpolated results>"
    PRINT, "INPUTS:"
    PRINT, "      No commandline input, will ask for two input files:"
    PRINT, "      Input: n-layer file with NDVI time-series"
    PRINT, "      Mask:  1-layer file with a mask, processing will only be carried"
    PRINT, "             out where mask has value '1'"
    PRINT, ""
    PRINT, "KEYWORD VARIABLES"
    PRINT, "  Processing keywords (OBLIGATORY):"
    PRINT, "      FET:   Fit Error Tolerance, is maximum tolerable downward deviation"
    PRINT, "             between fourier fit and NDVI data values (in DN values)"
    PRINT, "    FREQS:   1D array with frequences that should be selected from the "
    PRINT, "             fourier spectrum. i.e. freqs=[0,1,2,3] to use the fourier"
    PRINT, "             compoments 0 (mean), 1 (1 sine wave), 2 (2 sine waves) and 3."
    PRINT, "    RANGE:   Array of size 2 to specify the minimum and maximum valid data"
    PRINT, "             values. i.e. range=[1,254]"
    PRINT, "      TAT:   Throw Away Treshold, is maximum number of NDVI observations that"
    PRINT, "             can be discarded in the fitting process."
    PRINT, "     iMAX:   Maximum nr. of iterations to be performed during processing"
    PRINT, ""
    PRINT, "  I/O Keywords (OPTIONAL):"
    PRINT, "     DATA_IF:  (full path to) Filename used as input for processing (NDVI),"
    PRINT, "               will ask for filename if not specified"
    PRINT, "     MASK_IF:  (full path to) Filename used as mask,"
    PRINT, "               will ask for filename if not specified"
    PRINT, "     HANTS_OF: (full path to) Filename used to write status information,"
    PRINT, "               set this to an empty string ("") to avoid output being written"
    PRINT, "               to disk. Will ask for filename if not specified"
    PRINT, "     STATUS_OF: (full path to) Filename used to write processing status info"
    PRINT, "                set this to an empty string ("") to avoid output being written"
    PRINT, "                to disk. Will ask for filename if not specified"
    PRINT, "     SMOOTH_OF: (full path to) Filename used to write smoothed results"
    PRINT, "                set this to an empty string ("") to avoid output being written"
    PRINT, "                to disk. Will ask for filename if not specified"
    PRINT, "     INTERP_OF: (full path to) Filename used to write interpolated results"
    PRINT, "                set this to an empty string ("") to avoid output being written"
    PRINT, "                to disk. Will ask for filename if not specified"
    PRINT, ""
    PRINT, "  Other Keywords (OPTIONAL):"
    PRINT, "     LOG_FILE: (full path to) Filename used to write the log to."
    PRINT, "     PLOT:   Visualise the optimisation process. Only applied for debugging"
    PRINT, "             because it is very slow!"
    RETURN
  ENDIF
  
  ;Define file units
  unit1=0 & unit2=0 & unit3=0 & unit4=0
  
  ;Start logging
  IF N_ELEMENTS(log_file) EQ 0 THEN log_file="HANTS_log.txt"
  hants_log=obj_new("cgi_logger", log_file=log_file, log_init_message="Log file created by HANTS")
  
  ;Define error handler for CGI_HANTS routine
  CATCH, error_status
  IF error_status NE 0 THEN BEGIN
    ;Close any open files
    IF unit1 GT 0 THEN BEGIN CLOSE, unit1 & FREE_LUN, unit1 & ENDIF
    IF unit2 GT 0 THEN BEGIN CLOSE, unit1 & FREE_LUN, unit1 & ENDIF
    IF unit3 GT 0 THEN BEGIN CLOSE, unit1 & FREE_LUN, unit1 & ENDIF
    IF unit4 GT 0 THEN BEGIN CLOSE, unit1 & FREE_LUN, unit1 & ENDIF
    ;Close and destroy log
    hants_log->write_log_messages
    OBJ_DESTROY, hants_log
    CATCH, /CANCEL
    RETURN
  ENDIF
  
  ;Select input file (NDVI or something else)
  IF N_ELEMENTS(data_if) EQ 0 THEN BEGIN
    envi_select, title='Input Filename', fid=infid, /FILE_ONLY, /NO_DIMS, /NO_SPEC
    IF (infid eq -1) THEN MESSAGE
  ENDIF ELSE BEGIN
    envi_open_file, data_if, r_fid=infid, /NO_INTERACTIVE_QUERY, /NO_REALIZE
    IF (infid eq -1) THEN BEGIN
      hants_log->add_log_message, "ERROR: Specified input file '" + data_if + "' not found!"
      MESSAGE, "ERROR: Specified input file '" + data_if + "' not found!"
    ENDIF
  ENDELSE
  
  ;Query input file for information (lines, columns, bands, etc)
  envi_file_query, infid, data_type=data_type, xstart=xstart, $
    ystart=ystart, interleave=interleave, nb=nb, nl=nl, ns=ns,$
    fname=fname
    
  map_info=envi_get_map_info(fid=infid)
  proj_info=envi_get_projection(fid=infid)
  
  
  hants_log->add_log_message, "Using input file: " + fname
  
  ;Select MASK file
  IF N_ELEMENTS(mask_if) EQ 0 THEN BEGIN
    envi_select, title='Mask Filename', fid=maskfid, /FILE_ONLY, /NO_DIMS, /NO_SPEC
    IF (maskfid eq -1) THEN MESSAGE
  ENDIF ELSE BEGIN
    envi_open_file, mask_if, r_fid=maskfid, /NO_INTERACTIVE_QUERY, /NO_REALIZE
    IF (maskfid eq -1) THEN BEGIN
      hants_log->add_log_message, "ERROR: Specified mask file '" + mask_if + "' not found!"
      MESSAGE, "ERROR: Specified mask file '", mask_if, "' not found!"
    ENDIF
  ENDELSE
  
  ;Query mask file for information (lines, columns, bands, etc)
  envi_file_query, maskfid,  nb=m_nb, nl=m_nl, ns=m_ns, fname=m_fname
  hants_log->add_log_message, "Using mask file: " + m_fname
  
  ;Check if mask and input file have equal number of lines and columns
  IF (ns NE m_ns) OR (nl NE m_nl) THEN BEGIN
    hants_log->add_log_message, "Mask file and input file do not have an " + $
      "equal number of lines and/or columns!"
    MESSAGE, "Mask file and input file do not have an equal number of lines and/or columns!"
  ENDIF
  
  ; Get output files and check for an output filename
  IF N_ELEMENTS(hants_of) EQ 0 THEN BEGIN
    hants_of=envi_pickfile(filter='*.img', title='Output file for fourier components!')
    IF (hants_of eq "") THEN MESSAGE
    OPENW, unit1, hants_of, /GET_LUN
  ENDIF ELSE BEGIN
    IF hants_of EQ "" THEN $
      unit1=0 $
    ELSE $
      OPENW, unit1, hants_of, /GET_LUN
  ENDELSE
  hants_log->add_log_message, "Write fourier components to: " + hants_of
  
  IF N_ELEMENTS(status_of) EQ 0 THEN BEGIN
    status_of=envi_pickfile(filter='*.img', title='Output file for status indicator!')
    IF (status_of eq "") THEN MESSAGE
    OPENW, unit2, status_of, /GET_LUN
  ENDIF ELSE BEGIN
    IF status_of EQ "" THEN $
      unit2=0 $
    ELSE $
      OPENW, unit2, status_of, /GET_LUN
  ENDELSE
  hants_log->add_log_message, "Write status info to: " + status_of
  
  IF N_ELEMENTS(smooth_of) EQ 0 THEN BEGIN
    smooth_of=envi_pickfile(filter='*.img', title='Output file for smoothed results!')
    IF (smooth_of eq "") THEN MESSAGE
    OPENW, unit3, smooth_of, /GET_LUN
  ENDIF ELSE BEGIN
    IF smooth_of EQ "" THEN $
      unit3=0 $
    ELSE $
      OPENW, unit3, smooth_of, /GET_LUN
  ENDELSE
  hants_log->add_log_message, "Write smoothed results to: " + smooth_of
  
  IF N_ELEMENTS(interp_of) EQ 0 THEN BEGIN
    interp_of=envi_pickfile(filter='*.img', title='Output file for interpolated results!')
    IF (interp_of eq "") THEN MESSAGE
    OPENW, unit4, interp_of, /GET_LUN
  ENDIF ELSE BEGIN
    IF interp_of EQ "" THEN $
      unit4=0 $
    ELSE $
      OPENW, unit4, interp_of, /GET_LUN
  ENDELSE
  hants_log->add_log_message, "Write interpolated results to: " + interp_of
  
  ;Open input files and initialise tiling
  tile_id1 = envi_init_tile(infid, INDGEN(nb), num_tiles=num_tiles, interleave=1)
  tile_id2 = envi_init_tile(maskfid, [0], num_tiles=mask_tiles, interleave=1)
  hants_log->add_log_message, "Initialised tiling!"
  
  ;Initialise reporting
  rstr=['Applying HANTS']
  envi_report_init, rstr, title="Processing", base=base, /interrupt
  envi_report_inc, base, num_tiles
  
  ;Main processing loop
  nr_freqs=N_ELEMENTS(freqs)
  FOR i=0L, num_tiles-1 DO BEGIN
    hants_log->add_log_message, "Processing tile: " + STRING(i)
    envi_report_stat, base, i, num_tiles
    data = envi_get_tile(tile_id1, i)
    mask = envi_get_tile(tile_id2, i)
    datasize=SIZE(data, /DIMENSIONS)
    result=DCOMPLEXARR(datasize[0], nr_freqs)
    status=INTARR(datasize[0],3)
    smNDVI=FLTARR(datasize[0], datasize[1])
    iNDVI=FLTARR(datasize[0], datasize[1])
    tmp1=INTARR(3) & tmp2=FLTARR(datasize[1]) & tmp3=tmp2
    FOR j=0, datasize[0]-1 DO BEGIN
      IF mask[j] EQ 1 THEN BEGIN
        result[j,*]=cgi_fftndvi(data[j,*], FET=FET,freqs=freqs, RANGE=RANGE, $
          status=tmp1, smNDVI=tmp2, iNDVI=tmp3, iMAX=iMAX, TAT=TAT,_EXTRA=extra)

        ;
        IF tmp1[0] EQ -1 THEN $
          hants_log->add_log_message, "Processing error occured at pixel X,Y: " + $
          STRCOMPRESS(STRING(i+1)+","+STRING(j+1), /REMOVE_ALL)
      ENDIF ELSE BEGIN
        tmp1=0 & tmp2=FLTARR(datasize[1]) & tmp3=tmp2
      ENDELSE
      status[j,*]=tmp1
      smNDVI[j,*]=tmp2
      iNDVI[j,*]=tmp3
    ENDFOR
    IF unit1 GT 0 THEN WRITEU, unit1, result
    IF unit2 GT 0 THEN WRITEU, unit2, status
    IF unit3 GT 0 THEN WRITEU, unit3, smNDVI
    IF unit4 GT 0 THEN WRITEU, unit4, iNDVI
  ENDFOR
  envi_report_init, base=base, /finish
  envi_tile_done, tile_id1
  envi_tile_done, tile_id2
  hants_log->add_log_message, "Closing ENVI tiling"
  envi_file_mng, id=infid, /remove
  envi_file_mng, id=maskfid, /remove
  
  IF unit1 GT 0 THEN BEGIN
    CLOSE, unit1 & FREE_LUN, unit1
    envi_setup_head, fname=hants_of, ns=ns, nl=nl, nb=nr_freqs, $
      data_type=9, offset=0, interleave=1, $
      xstart=xstart, ystart=ystart, $
      descrip='HANTS FFT Frequencies', /write, /open;, $
      
    map_info=map_info
    
    
    hants_log->add_log_message, "Closing and writing header for: " + hants_of
  ENDIF
  IF unit2 GT 0 THEN BEGIN
    CLOSE, unit2 & FREE_LUN, unit2
    envi_setup_head, fname=status_of, ns=ns, nl=nl, nb=3, $
      data_type=2, offset=0, interleave=1, $
      xstart=xstart, ystart=ystart, $
      descrip='HANTS status', /write, /open, $
      bnames=['Nr. of retained data points', $
      'Nr. of iterations 1st loop','Nr. of iterations 2nd loop'],map_info=map_info
    hants_log->add_log_message, "Closing and writing header for: " + status_of
  ENDIF
  IF unit3 GT 0 THEN BEGIN
    CLOSE, unit3 & FREE_LUN, unit3
    envi_setup_head, fname=smooth_of, ns=ns, nl=nl, nb=datasize[1], $
      data_type=4, offset=0, interleave=1, $
      xstart=xstart, ystart=ystart, $
      descrip='HANTS Smoothed Results', /write, /open;, $
    map_info=map_info
    hants_log->add_log_message, "Closing and writing header for: " + smooth_of
  ENDIF
  IF unit4 GT 0 THEN BEGIN
    CLOSE, unit4 & FREE_LUN, unit4
    envi_setup_head, fname=interp_of, ns=ns, nl=nl, nb=datasize[1], $
      data_type=4, offset=0, interleave=1, $
      xstart=xstart, ystart=ystart, $
      descrip='HANTS Interpolated Results', /write, /open;, $
    map_info=map_info
    hants_log->add_log_message, "Closing and writing header for: " + interp_of
  ENDIF
  
  ;Write and close log
  hants_log->write_log_messages
  OBJ_DESTROY, hants_log
  
END
