;

PRO RESIZE, ROI$, INPUT$, PIXEL_SIZE
	DATA = READ_TIFF(INPUT$, GEOTIFF = GEOTIFF)
;	DATA = DATA * (DATA LT 10000)
	ROI = READ_TIFF(ROI$, GEOTIFF = ROI_GEOTIFF)
	A = SIZE(ROI, /DIMENSIONS)
	X0 = GEOTIFF.MODELTIEPOINTTAG[3]
	Y0 = GEOTIFF.MODELTIEPOINTTAG[4]
	ROIX0 = ROI_GEOTIFF.MODELTIEPOINTTAG[3]
	ROIY0 = ROI_GEOTIFF.MODELTIEPOINTTAG[4]
	STARTX = FIX((ROIX0 - X0) / PIXEL_SIZE + 0.5)
	STARTY = FIX((Y0 - ROIY0) / PIXEL_SIZE + 0.5)
	ENDX = STARTX + A[0] - 1
	ENDY = STARTY + A[1] - 1
	DATA = DATA[STARTX : ENDX, STARTY : ENDY]
	OUTPUT$ = 'F:\SR_ndvi_data\china\sub\' + FILE_BASENAME(INPUT$)
	WRITE_TIFF, OUTPUT$, DATA, GEOTIFF=ROI_GEOTIFF
END


PRO WRITE_MOSAIC_PRM, FILES, PRM$
	OPENW, LUN, PRM$, /GET_LUN
		FOR I = 0, N_ELEMENTS(FILES) - 1 DO BEGIN
			PRINTF, LUN, FILES[I]
		ENDFOR
	IF LUN NE -1L THEN FREE_LUN,LUN
END

FUNCTION MRT_MOSAICK, FILES, DIR
	IF(N_ELEMENTS(FILES) GT 1) THEN BEGIN
		FIELD = STRSPLIT(FILE_BASENAME(FILES[0]), '.', /EXTRACT)
		PRM$ = DIR.OUTPUT + FIELD[1] + '_M.PRM'
		OUTPUT$ = DIR.OUTPUT + FIELD[1] + '.HDF'
		WRITE_MOSAIC_PRM,FILES,PRM$
		IF(FILE_TEST(PRM$)) THEN BEGIN
			PRINT, 'MRT_MOSAICKING: ', FILES
			PRINT, 'RPM$: ', PRM$
			SPAWN, 'CMD/E:ON/C "SET MRTDATADIR=' + DIR.MRT_DATA + '&&' + DIR.MRT_DIR + 'MRTMOSAIC -i ' + PRM$ + ' -o ' + OUTPUT$ + ' -s "1 0 0""', RESULT,ERRRESULT
			IF N_ELEMENTS(RESULT) LE 1 then ANS = '' ELSE BEGIN
				LEN = N_ELEMENTS(RESULT)

				IF LEN LT 4 THEN RETURN, ''

				ANS = RESULT[LEN-4]
        	ENDELSE
        	IF ANS EQ 'Finished mosaicking!'  THEN BEGIN
        		PRINT, 'STATUS: DONE!'
        		RETURN, OUTPUT$
        	ENDIF ELSE BEGIN
        		PRINT, 'STATUS: ERROR!'
        		PRINT, RESULT
        		RETURN, ''
        	ENDELSE
		ENDIF ELSE BEGIN
			PRINT, 'MOSAICK_PRM FILE NOT FOUND!'
			RETURN, ''
		ENDELSE
	ENDIF ELSE BEGIN
		RETURN, FILES
	ENDELSE
END


PRO WRITE_RESAMPLE_PRM, PRM$, INPUT$, OUTPUT$, GEO_INFO, PIXEL_SIZE
	OPENW, LUN, PRM$, /GET_LUN
		PRINTF, LUN, 'INPUT_FILENAME = ' + INPUT$
		PRINTF, LUN, 'SPECTRAL_SUBSET = ( 1 )'
		PRINTF, LUN, 'SPATIAL_SUBSET_TYPE = INPUT_LAT_LONG'
		PRINTF, LUN, 'SPATIAL_SUBSET_UL_CORNER = ( ' + GEO_INFO.ULLAT + ' ' + GEO_INFO.ULLONG + ' )'
		PRINTF, LUN, 'SPATIAL_SUBSET_LR_CORNER = ( ' + GEO_INFO.LRLAT + ' ' + GEO_INFO.LRLONG + ' )'
		PRINTF, LUN, 'OUTPUT_FILENAME = ' + OUTPUT$
		PRINTF, LUN, 'RESAMPLING_TYPE = BILINEAR'
		PRINTF, LUN, 'OUTPUT_PROJECTION_TYPE = AEA'

;		PRINTF, LUN, 'OUTPUT_PROJECTION_TYPE = GEO'

		PRINTF, LUN, 'OUTPUT_PROJECTION_PARAMETERS = ( '
		PRINTF, LUN, ' 0.0 0.0 25.0'
		PRINTF, LUN, ' 47.0 110.0 0.0'
		PRINTF, LUN, ' 4000000.0 0.0 0.0'

;		PRINTF, LUN, ' 0.0 0.0 0.0'
;		PRINTF, LUN, ' 0.0 0.0 0.0'
;		PRINTF, LUN, ' 0.0 0.0 0.0'

		PRINTF, LUN, ' 0.0 0.0 0.0'
		PRINTF, LUN, ' 0.0 0.0 0.0 )'
		PRINTF, LUN, 'DATUM = WGS84'
		PRINTF, LUN, 'OUTPUT_PIXEL_SIZE = ' + STRTRIM(STRING(PIXEL_SIZE),2)
	IF LUN NE -1L THEN FREE_LUN,LUN
END




PRO	MRT_RESAMPLE, INPUT$, OUTPUT$, DIR, GEO_INFO, PIXEL_SIZE
	PRM$ = STRMID(OUTPUT$, 0, STRLEN(OUTPUT$) - 4) + '_R.PRM'
	WRITE_RESAMPLE_PRM, PRM$, INPUT$, OUTPUT$, GEO_INFO, PIXEL_SIZE
	IF(FILE_TEST(PRM$)) THEN BEGIN
		PRINT, 'MRT_RESAMPLING: ', INPUT$
		PRINT, 'RPM$: ', PRM$
		SPAWN, 'CMD/E:ON/C "SET MRTDATADIR=' + DIR.MRT_DATA + '&&' + DIR.MRT_DIR + 'RESAMPLE -p ' + PRM$ + '"', RESULT,ERRRESULT
		IF N_ELEMENTS(RESULT) LE 1 then ANS = '' ELSE BEGIN
			LEN = N_ELEMENTS(RESULT)
			ANS = RESULT[LEN-4]
       	ENDELSE
       	IF ANS EQ 'Finished processing!'  THEN BEGIN
        	PRINT, 'STATUS: DONE!'
        ENDIF ELSE BEGIN
        	PRINT, 'STATUS: ERROR!'
        	PRINT, RESULT
        ENDELSE
	ENDIF ELSE BEGIN
		PRINT, 'RESAMPLE_PRM FILE NOT FOUND!'
	ENDELSE
END







PRO MRT_BATCH, YEAR, MOD_MARK

	DIR = {MRT_DIR: 'C:\MRT\bin\',$
		   MRT_DATA: 'C:\MRT\data\', $
		   HDF_DIR: 'F:\SR_ndvi_data\2013ndvi\China\',$
		   OUTPUT: 'F:\SR_ndvi_data\2013ndvi\China\out\',$
	     OUTPUT8bit: 'F:\SR_ndvi_data\2013ndvi\China\',$
       OUTPUT_HDR: 'F:\SR_ndvi_data\2013ndvi\China\hdr\'$
		  }

;test
;	GEO_INFO = { ULLAT: '26', $
;				 ULLONG: '90', $
;				 LRLAT: '25', $
;				 LRLONG: '91' $
;			   }

;			   ENVI,/RESTORE_BASE_SAVE_FILES
;    ENVI_BATCH_INIT,/NO_STATUS_WINDOW
;China
		GEO_INFO = { ULLAT: '55', $
				 ULLONG: '65', $
				 LRLAT: '15', $
				 LRLONG: '135' $
			   }
	;
;	GEO_INFO = { ULLAT: '50', $
;				 ULLONG: '60', $
;				 LRLAT: '-30', $
;				 LRLONG: '180' $
;			   }



;	GEO_INFO = { ULLAT: '65', $
;				 ULLONG: '-25', $
;				 LRLAT: '30', $
;				 LRLONG: '100' $
;			   }



;	GEO_INFO = { ULLAT: '-0', $
;				 ULLONG: '-72', $
;				 LRLAT: '-40', $
;				 LRLONG: '-34' $
;			   }

;	GEO_INFO = { ULLAT: '-19', $
;				 ULLONG: '117', $
;				 LRLAT: '-40', $
;				 LRLONG: '170' $
;			   }

;	GEO_INFO = { ULLAT: '-19', $
;				 ULLONG: '21', $
;				 LRLAT: '-30', $
;				 LRLONG: '35' $
;			   }

	PIXEL_SIZE = 1000

;	PIXEL_SIZE = 0.0083333333

	START = {YEAR: YEAR, MONTH: 01, DAY: 01}
	FINISH = {YEAR: YEAR, MONTH:12, DAY:31}
	J0 = JULDAY(12, 31, START.YEAR - 1)
	JSTART = JULDAY(START.MONTH, START.DAY, START.YEAR) - J0
	JFINISH = JULDAY(FINISH.MONTH, FINISH.DAY, FINISH.YEAR) - J0
	FOR I = JSTART, JFINISH DO BEGIN
		JDAY = I + J0
		CALDAT, JDAY, MONTH, DAY, YEAR
		MARK = STRTRIM(STRING(YEAR), 2) + STRTRIM(STRING(I, FORMAT = '(I3.3)'), 2)
		FILES = FILE_SEARCH(DIR.HDF_DIR + 'M?D13A2.A' + MARK + '*.HDF', COUNT = COUNT)
;		FILES = FILE_SEARCH(DIR.HDF_DIR + 'M?D15A2.A' + MARK + '*.HDF', COUNT = COUNT)
		TEMPFILE = ''
		IF(COUNT GT 1) THEN BEGIN
			TEMPFILE = MRT_MOSAICK(FILES, DIR)
		ENDIF ELSE BEGIN
			IF(COUNT EQ 1) THEN BEGIN
				TEMPFILE = FILES
			ENDIF
		ENDELSE



		IF TEMPFILE NE '' THEN BEGIN
			OUTPUT$ = DIR.OUTPUT + 'A' + MARK + '.TIF'
			MRT_RESAMPLE, TEMPFILE, OUTPUT$, DIR, GEO_INFO, PIXEL_SIZE
			data = read_tiff(DIR.OUTPUT + 'A' + MARK + '.1_km_16_days_NDVI.TIF', geotiff = geotiff)
;            data = read_tiff(DIR.OUTPUT + 'A' + MARK + '.Fpar_1km.tif', geotiff = geotiff)
;           data = read_tiff(DIR.OUTPUT + 'A' + MARK + '.Lai_1km.tif', geotiff = geotiff)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			indices1 = where(data eq -3000)
			indices2 = where(data ne -3000)
			if(indices1[0] ne -1) then data[indices1] = 0

			if(indices2[0] ne -1) then begin
				data[indices2] = (data[indices2] / 10000. + 1) * 127
				data = fix(data + 0.5, type = 1)
			endif


			 out_name = DIR.OUTPUT8bit +'8bit-' +MARK+ '_NDVI.TIF'
			write_tiff, out_name, data, geotiff = geotiff
;


			out_name = DIR.OUTPUT8bit + FILE_BASENAME(out_name)
		 envi, /restore_base_save_files
		 envi_open_file, out_name, r_fid=fid


     if (fid eq -1) then begin
;            envi_batch_exit
     endif

     ENVI_FILE_QUERY,fid,nb=nb,dims=dims
		out_name = DIR.OUTPUT_HDR +'8bit-' +MARK
    ;
    ; Create the new output file. Do not
    ; remove the input file after the
    ; new file has been created.
    ;
     envi_doit, 'cf_doit', $
                   fid=fid, pos=indgen(1), dims=dims, $
                   remove=0, out_name=out_name, $
                   r_fid=r_fid ; , OUT_DT=1
			;filetemp = file_search(DIR.OUTPUT+'*')
			;FILE_DELETE, filetemp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;		write_tiff, DIR.OUTPUT + 'A' + MARK + '_8Lai_1km.tif', data, geotiff = geotiff
	;		RESIZE, ROI$, DIR.OUTPUT + 'A' + MARK + '_8Lai_1km.tif', PIXEL_SIZE
;			write_tiff, DIR.OUTPUT + 'A' + MARK + '_Fpar_1km.tif', data, geotiff = geotiff
;			RESIZE, ROI$, DIR.OUTPUT + 'A' + MARK + '_Fpar_1km.tif', PIXEL_SIZE
		ENDIF ELSE BEGIN

		ENDELSE
	ENDFOR
END


pro batch
  envi,/RESTORE_BASE_SAVE_FILES
  ENVI_BATCH_INIT,/NO_STATUS_WINDOW
	for i = 2013L, 2013 do begin
		MRT_BATCH, i
	endfor
	ENVI_BATCH_EXIT
end

;pro saveas
;	inputs = file_search('D:\MODIS\20130603\China\out\China\*.tif',count = count)
;	for i = 0, count -1 do begin
;		save_stadard, inputs[i]
;	end
;end