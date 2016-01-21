PRO RESIZE, ROI$, INPUT$, PIXEL_SIZE
	DATA = READ_TIFF(INPUT$, GEOTIFF = GEOTIFF)
	DATA = DATA * (DATA LT 10000)
	ROI = READ_TIFF(ROI$, GEOTIFF = ROI_GEOTIFF)
	A = SIZE(ROI, /DIMENSIONS)
	X0 = GEOTIFF.MODELTIEPOINTTAG[3]
	Y0 = GEOTIFF.MODELTIEPOINTTAG[4]
	ROIX0 = ROI_GEOTIFF.MODELTIEPOINTTAG[3]
	ROIY0 = ROI_GEOTIFF.MODELTIEPOINTTAG[4]
	STARTX = FIX((ROIX0 - X0) / PIXEL_SIZE)
	STARTY = FIX((Y0 - ROIY0) / PIXEL_SIZE)
	ENDX = STARTX + A[0] - 1
	ENDY = STARTY + A[1] - 1
	DATA = DATA[STARTX : ENDX, STARTY : ENDY] * ROI
	OUTPUT$ = 'D:\exchange\ethopia_sr\' + FILE_BASENAME(INPUT$)
	WRITE_TIFF, OUTPUT$, DATA, GEOTIFF=ROI_GEOTIFF, /SHORT
END

;**************��.HDF�ļ���ͷ�ļ���;���������ļ���UL��LR��γ��***************************
FUNCTION ReadHead_file,inputfile

  IF (N_PARAMS() NE 1) THEN MESSAGE, 'Incorrect number of arguments'
  ON_ERROR, 2           ;return to caller


    IF ~FILE_TEST(inputfile) THEN BEGIN
       Info = DIALOG_MESSAGE('�Ҳ���ָ����ͷ�ļ�',TITLE='����')
       RETURN,''
    ENDIF


    OPENR,lun,inputfile,/GET_LUN
    result = FSTAT(lun)
    headdata = BYTARR(result.SIZE)
    READU,lun,headdata
    FREE_LUN,lun


    index0 = STRPOS(headdata, 'UL_CORNER_LATLON = ')
    index1 = STRPOS(headdata, 'UR_CORNER_LATLON = ')
    index2 = STRPOS(headdata, 'LR_CORNER_LATLON = ')
    index3 = STRPOS(headdata, '# UL_CORNER_XY =')


  UL = STRMID(headdata,index0+21,index1-index0-21-2-2)   ;�س���ռ2���ֽ�
  LR = STRMID(headdata,index2+21,index3-index2-21-2-2-2)

  UL_ARR = STRSPLIT(UL,/EXTRACT)
  UL_x=UL_ARR[0]
  UL_y=UL_ARR[1]
  LR_ARR = STRSPLIT(LR,/EXTRACT)
  LR_x=LR_ARR[0]
  LR_y=LR_ARR[1]


  RETURN,fileinfo={UL_x     :  UL_x     ,$   ;���������ļ���UL��LR��γ��
                     UL_y     :  UL_y       ,$
                     LR_x     :  LR_x       ,$
                     LR_y     :  LR_y }

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

       ErrorTxt = DIR.OUTPUT+'Erro.txt'


		OUTPUT$ = DIR.OUTPUT + FIELD[1] + '.HDF'
		WRITE_MOSAIC_PRM,FILES,PRM$
		IF(FILE_TEST(PRM$)) THEN BEGIN
			PRINT, 'MRT_MOSAICKING: ', FILES
			PRINT, 'RPM$: ', PRM$
			;print,'CMD/E:ON/C "SET MRTDATADIR=' + DIR.MRT_DATA + '&&' + DIR.MRT_DIR + 'MRTMOSAIC -i ' + PRM$ + ' -o ' + OUTPUT$ + ' -s "1 0 0" ', RESULT,ERRRESULT
			
			SPAWN, 'CMD/E:ON/C "SET MRTDATADIR=' + DIR.MRT_DATA + '&&' + DIR.MRT_DIR + 'MRTMOSAIC -i ' + PRM$ + ' -o ' + OUTPUT$ + ' -s "1 0 0" ', RESULT,ERRRESULT
			IF N_ELEMENTS(RESULT) LE 1 then ANS = '' ELSE BEGIN
				LEN = N_ELEMENTS(RESULT)
				if len le 4 then begin

                  openw,lun,ErrorTxt,/get_lun,/append

                  printf,lun,FIELD[1] + '.HDF'

                  free_lun,lun

                  return,''

				endif
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
		PRINTF, LUN, 'SPECTRAL_SUBSET = ( 1 0 )'
		PRINTF, LUN, 'SPATIAL_SUBSET_TYPE = INPUT_LAT_LONG'
		PRINTF, LUN, 'SPATIAL_SUBSET_UL_CORNER = ( ' + GEO_INFO.ULLAT + ' ' + GEO_INFO.ULLONG + ' )'
		PRINTF, LUN, 'SPATIAL_SUBSET_LR_CORNER = ( ' + GEO_INFO.LRLAT + ' ' + GEO_INFO.LRLONG + ' )'
		PRINTF, LUN, 'OUTPUT_FILENAME = ' + OUTPUT$
		PRINTF, LUN, 'RESAMPLING_TYPE = BILINEAR'

;		PRINTF, LUN, 'OUTPUT_PROJECTION_TYPE = AEA'
		PRINTF, LUN, 'OUTPUT_PROJECTION_TYPE = GEO'

		PRINTF, LUN, 'OUTPUT_PROJECTION_PARAMETERS = ( '
;
;		PRINTF, LUN, ' 0.0 0.0 25.0'
;		PRINTF, LUN, ' 47.0 110.0 0.0'
;		PRINTF, LUN, ' 4000000.0 0.0 0.0'

		PRINTF, LUN, ' 0.0 0.0 0.0'
		PRINTF, LUN, ' 0.0 0.0 0.0'
		PRINTF, LUN, ' 0.0 0.0 0.0'

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
	DIR = {MRT_DIR: 'D:\MRT\bin\',$
		   MRT_DATA: 'D:\MRT\data', $
		   HDF_DIR: 'D:\Modis\HDF\MODI13A2A2013353\NorthAmerica\', $
		   OUTPUT: 'D:\Modis\HDF\MODI13A2A2013353\NorthAmerica\out\',$
		   OUTPUT8bit: 'D:\Modis\HDF\MODI13A2A2013353\NorthAmerica\8bit\',$
		   OUTPUT_HDR: 'D:\Modis\HDF\MODI13A2A2013353\NorthAmerica\hdr\'$
		  }
	;China

;	GEO_INFO = { ULLAT: '55', $
;				 ULLONG: '70', $
;			 LRLAT: '10', $
;				 LRLONG: '135' $
;				}
;China
;	GEO_INFO = { ULLAT: '55', $
;				 ULLONG: '65', $
;				 LRLAT: '15', $
;				 LRLONG: '135' $
;			   }
;   North American
	GEO_INFO = { ULLAT: '60.00', $
				 ULLONG: '-160.00', $
				 LRLAT: '10.00', $
				 LRLONG: '-52.20' $
			   }
;   United States
;  GEO_INFO = { ULLAT: '49.1', $
;         ULLONG: '-125', $
;         LRLAT: '25.43', $
;         LRLONG: '-88.67' $
;         }

  ;europe
;  GEO_INFO = { ULLAT: '66.6', $
;         ULLONG: '-9.83', $
;         LRLAT: '36.25', $
;         LRLONG: '88.4' $
;                }
	;europe_india
;	GEO_INFO = { ULLAT: '60', $
;				 ULLONG: '-20', $
;				 LRLAT: '0', $
;				 LRLONG: '115.48' $
;               }


; south_american
; GEO_INFO = { ULLAT: '-0', $
;        ULLONG: '-78.32', $
;        LRLAT: '-40', $
;        LRLONG: '-30' $
;        }


;  Argentina
; GEO_INFO = { ULLAT: '-29.62', $
;        ULLONG: '-65.52', $
;        LRLAT: '-38.70', $
;        LRLONG: '-57.87' $
;        }
;
;	Brazil
;	GEO_INFO = { ULLAT: '-8.92', $
;				 ULLONG: '-58.76', $
;				 LRLAT: '-32.25', $
;				 LRLONG: '-37.61' $
;			   }
;
;
; Venezuela
; GEO_INFO = { ULLAT: '-17.62', $
;        ULLONG: '-62.429', $
;        LRLAT: '-37.192', $
;        LRLONG: '-48.806' $
;        }


;	;Australia
;	GEO_INFO = { ULLAT: '-27.48', $
;				 ULLONG: '113.15', $
;				 LRLAT: '-44', $
;				 LRLONG: '154' $
;			   }


;	;South Afica
;	GEO_INFO = { ULLAT: '-21.5', $
;				 ULLONG: '16', $
;				 LRLAT: '-35.2', $
;				 LRLONG: '33.2' $
;			   }

;	;Nigeria
;	GEO_INFO = { ULLAT: '14', $
;				 ULLONG: '2.6', $
;				 LRLAT: '4.17', $
;				 LRLONG: '14.8' $
;			   }

;	;Ethopia
;	GEO_INFO = { ULLAT: '15.08', $
;				 ULLONG: '32.75', $
;				 LRLAT: '3.13', $
;				 LRLONG: '48.25' $
;			   }
;	;egypt
;	GEO_INFO = { ULLAT: '32', $
;				 ULLONG: '24.5', $
;				 LRLAT: '21.5', $
;				 LRLONG: '36' $
;			   }
   ;iran
;	GEO_INFO = { ULLAT: '40', $
;				 ULLONG: '44', $
;				 LRLAT: '25', $
;				 LRLONG: '63.5' $
;			   }

	PIXEL_SIZE = 0.0083333333

;    PIXEL_SIZE = 0.25



    ROI$ = 'C:\MRT\CHINA_1KM_ROI.TIF'
	START = {YEAR: YEAR, MONTH: 1, DAY:1}
	FINISH = {YEAR: YEAR, MONTH:12, DAY:31}
	J0 = JULDAY(12, 31, START.YEAR - 1)
	JSTART = JULDAY(START.MONTH, START.DAY, START.YEAR) - J0
	JFINISH = JULDAY(FINISH.MONTH, FINISH.DAY, FINISH.YEAR) - J0
	FOR I = JSTART, JFINISH DO BEGIN
		JDAY = I + J0
		CALDAT, JDAY, MONTH, DAY, YEAR
		MARK = STRTRIM(STRING(YEAR), 2) + STRTRIM(STRING(I, FORMAT = '(I3.3)'), 2)
		MARK_date = STRTRIM(STRING(I, FORMAT = '(I3.3)'), 2)
		FILES = FILE_SEARCH(DIR.HDF_DIR + 'MOD' + MOD_MARK + 'A2.A' + MARK + '*.HDF', COUNT = COUNT)
		TEMPFILE = ''
		IF(COUNT GT 1) THEN BEGIN
			TEMPFILE = MRT_MOSAICK(FILES, DIR)
		ENDIF ELSE BEGIN
			IF(COUNT EQ 1) THEN BEGIN
				TEMPFILE = FILES
			ENDIF
		ENDELSE
		IF TEMPFILE NE '' THEN BEGIN

			OUTPUT$ = DIR.OUTPUT + 'A' + MARK + '.tif'

			MRT_RESAMPLE, TEMPFILE, OUTPUT$, DIR, GEO_INFO, PIXEL_SIZE

            file = file_search(DIR.OUTPUT + 'A' + MARK +'*NDVI.tif',count=count_file)

           if count_file ne 0 then begin

           for k=0,count_file-1 do begin

            image = read_tiff(file[0],geotiff = geotiff)

            index=where(image ne -3000,count)

			image[index] = fix((image[index] + 10000.) * 0.0127)

			index=where(image eq -3000,count)
			if(count gt 0) then image[index]=0.
			OUTPUT$ = DIR.OUTPUT +MARK + '.tif'

            OUTPUT$_8bit = DIR.OUTPUT8bit +'8bit-' +MARK + '.tif'

            write_tiff,OUTPUT$_8bit,image,geotiff= geotiff;,/short

         ;=====the format of ENVI stardard format=============================
          ; First restore all the base save files.

            compile_opt IDL2

           ENVI,/RESTORE_BASE_SAVE_FILES
           ENVI_BATCH_INIT,/NO_STATUS_WINDOW




 ;	        envi_batch_init ;, log_file=filepath_out+'Log.txt'
 	        envi_open_file, OUTPUT$_8bit, r_fid=fid
	        if (fid eq -1) then begin
	           envi_batch_exit
	        endif

            envi_file_query, fid, ns=ns, nl=nl, nb=nb
	        t_fid = lonarr(nb) + fid
	        dims = [-1, 0, ns-1, 0, nl-1]
	        pos  = lindgen(nb)

	        name_len=strlen(OUTPUT$_8bit)
            date=STRMID(OUTPUT$_8bit,name_len-7,3)

           case date of
			'001': out_date='0101'
			'017': out_date='0102'
			'033': out_date='0103'
			'049': out_date='0201'
			'065': out_date='0202'
			'081': out_date='0203'
			'097': out_date='0301'
			'113': out_date='0302'
			'129': out_date='0303'
			'145': out_date='0401'
			'153': out_date='0401'
			'161': out_date='0402'
			'177': out_date='0403'
			'193': out_date='0501'
			'209': out_date='0502'
			'225': out_date='0503'
			'241': out_date='0601'
			'257': out_date='0602'
			'273': out_date='0603'
			'289': out_date='0701'
			'305': out_date='0702'
			'321': out_date='0703'
			'337': out_date='0801'
			'353': out_date='0802'
			else:
		  endcase



	        out_name = DIR.OUTPUT_HDR +STRTRIM(STRING(YEAR), 2) + out_date
	  ;
	  ; Create the new output file. Do not
	  ; remove the input file after the
	  ; new file has been created.
	  ;
	        envi_doit, 'cf_doit', $
	                 fid=fid, pos=pos, dims=dims, $
	                 remove=0, out_name=out_name, $
	                 r_fid=r_fid ; , OUT_DT=1
    		filetemp = file_search(DIR.OUTPUT+'*')
				FILE_DELETE, filetemp
	        endfor
         endif

		ENDIF ELSE BEGIN
			PRINT, 'û��Ҫ����RESAMPLE���ļ���������MOSAICK���?����(' + MARK + ')��'
		ENDELSE
	ENDFOR
END


pro batch
	MOD_MARK = '13'
	time = systime(1)
	for i = 2013L, 2013 do begin
		MRT_BATCH, i, MOD_MARK
	endfor
	time1 = systime(1)
	print, time1-time
end