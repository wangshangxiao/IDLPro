PRO reproject_mod04
COMPILE_OPT IDL2

ENVI, /RESTORE_BASE_SAVE_FILES
ENVI_BATCH_INIT, LOG_FILE = 'reproj_mod04.log'
input_directory = 'D:\share\MOD04_h26_v04\'
output_directory = 'D:\share\MOD04_h26_v04\'

IF ~STRCMP(STRMID(input_directory, 0, 1, /REVERSE_OFFSET), '\') THEN BEGIN
input_directory = input_directory + '\'
ENDIF

IF ~STRCMP(STRMID(output_directory, 0, 1, /REVERSE_OFFSET), '\') THEN BEGIN
output_directory = output_directory + '\'
ENDIF

input_filenames = FILE_SEARCH(input_directory +  '*.hdf')

IF SIZE(input_filenames, /N_ELEMENT) GE 1 THEN BEGIN
;Output method schema is:
;0 = Standard, 1 = Projected, 2 = Standard and Projected
out_method = 1
name = 'China_Lambert_Conformal_Conic'
datum = 'WGS-84'
params = [6378137.0, 6356752.3, 0.000000,$
105.000000, 0.0, 0.0, 25.000000, 47.000000]
type = 4
out_projection = ENVI_PROJ_CREATE(TYPE = type, NAME = name, DATUM = datum,$
PARAMS = params) 

interpolation_method = 6
swath_name = 'mod04'
sd_names = ['Image_Optical_Depth_Land_And_Ocean']

filecount = SIZE(input_filenames, /DIMENSIONS)
FOR i = 0, filecount[0] - 1 DO BEGIN
tmp_filename = input_filenames[i]
;fname = tmp_filename 
;out_rootname = STRMID(fname, 0, STRLEN(fname) - 4)
out_rootname = STRMID(tmp_filename,strlen(input_directory),40)

CONVERT_MODIS_DATA, IN_FILE = tmp_filename, OUT_PATH = output_directory, $
OUT_ROOT = out_rootname, /HIGHER_PRODUCT, /SWATH, $
SWT_NAME = swath_name, OUT_METHOD = out_method, $
INTERP_METHOD = interpolation_method, SD_NAMES = sd_names, $
NUM_X_PTS = 50, NUM_Y_PTS = 50, /NO_MSG, OUT_PROJ = out_projection, $
BACKGROUND = 0.0, FILL_REPLACE_VALUE = 0.0
fids = envi_get_file_ids()
for k = 0, n_elements(fids) - 1 do begin
envi_file_mng, id=fids[k], /remove
endfor

ENDFOR
ENDIF

ENVI_BATCH_EXIT
PRINT, 'Job Done!'
END