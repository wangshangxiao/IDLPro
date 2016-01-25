;**************************************************************************
;���¸�,�ɼ̻�
;��������ϵͳ�Ĺ�������������
;2006.07.26
;**************************************************************************

;========����Ϊԭ���룬����Ϊ�������޸ĵĴ���=20070531===============================
PRO C_COMMON_BLOCK

   ;����3����������,������ϵͳ������.
   ;ע��:(1)common�����ж���ı���Ҫ������ϵͳ�еõ����ã������һ��������,��ε�һ�����࿴����.
   ;     (2)������ϵͳ��,Ҫ��һ��������,��ֱ����"Build Order"���϶�Ҫ�����λ�û����ڹ��̵Ĺ������б��а���ĸ˳��Ӧ���ڵ�һλ.
   ;     (3)����������ʱ,���õ��ö��幫�������Ĺ�����,ֱ���� "common,������������".
   ;     (4)���ù�������ʱ,���ؼ��ϱ���,ֱ���ù���ģ��������!��������˱�����,����밴����ʱ��˳��,�����������ù�������.

   COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE  ;Ҳ���Բ���ʼ��

   yesORno=0                                    ;��ʶ�������Ƿ�ɹ��ı���
   DBobj=OBJ_NEW('IDLdbDatabase')             	;��ʶ���ݿ����

   FILE_PATH=''									;���õ��ļ�����·��
   Year=strmid(systime(),3,4,/REVERSE_OFFSET)   ;����Ҫ��������,��ʼֵΪ��ǰ���

	ParaFile = 'text\Province_Code.txt'

	IF NOT FILE_TEST(ParaFile) THEN BEGIN
		INFO = DIALOG_MESSAGE('ϵͳ��װĿ¼���Ҳ������������ļ�"Province_Code.txt"!',TITLE='����')
		RETURN
	ENDIF

    OPENR,lun,ParaFile,/GET_LUN
    FileInfo = FSTAT(lun)
    paradata = BYTARR(FileInfo.SIZE)
    READU,lun,paradata
    FREE_LUN,lun

    enter=string(byte(13))+string(byte(10))   					  ;�س���ASCII��ֵ

    index = STRPOS(paradata, 'Province_Code = ')

	PROVINCE_CODE = STRMID(paradata,index+16,6)
	print,'Province_Code = ',PROVINCE_CODE

	SDFile = 'text\SD_PREFERENCE.txt'

	IF NOT FILE_TEST(ParaFile) THEN BEGIN
		INFO = DIALOG_MESSAGE('ϵͳ��װĿ¼���Ҳ������������ļ�"SD_PREFERENCE.txt"!',TITLE='����')
		RETURN
	ENDIF

;===========================================================
	filelines = FILE_LINES(SDFile)
	temp = strarr(filelines)
	OPENR, LUN, SDFile, /GET_LUN
	READF, LUN, temp
	FREE_LUN, LUN

	FILE_PATH=temp[0]
	DSN=temp[1]
	USER_NAME=temp[2]
	PWD=temp[3]

END
;;=======����Ϊ�������޸ĵĴ���=20070810=========================


PRO PRO_COMMON_BASE
	COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP,MENU_MANAGE
	MENU_OPERATION=1;�����˵���ID
	MENU_MANAGE=1
	BASE_TOP=1
	X_OFFSET=0
	Y_OFFSET=0
END
;
PRO PRO_COMMON_DC
	COMMON DC_BLOCK,NewCropID		;���幫��ģ��,ֻ���������ģ����
	NewCropID = '31'				;Ĭ��Ϊ31-������(�����Ա������ִ�����,���ϵ������궬С���,��������)
END

pro common_current_date
	common current_date, year, month, day
	result = bin_date()
	year = result[0]
	month = result[1]
	day = result[2]
end
;
PRO PRO_COMMON_SETPATH
	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF
	COMMON COMMON_SETPATH,ppath;

	path = {$
			zs_in_path :'' ,$
			zs_out_path:'' ,$
			mj_in_path :'' ,$
			dc_out_path:'' ,$
			cl_out_path:'' ,$
			nq_in_path :'' ,$
			nq_out_path:'' ,$
			fz_in_path :'' ,$
			fz_out_path:'' ,$
			ndvi_prefix:'',$
			ndvi_suffix:'',$
			lai_prefix :'',$
			lai_suffix :'',$
			npp_prefix :'',$
			npp_suffix :'',$
			cld_prefix :'',$
			cld_suffix :'',$
			clf_prefix :'',$
			clf_suffix :'',$
			nq_dem_file:''$
			}

	ppath = ptr_new(path, /NO_COPY)

	pathfile = '.\text\common_path.txt'
	if file_test(pathfile) eq 0 then begin
		OPENW, lun, pathfile, /GET_LUN
		context = '����·���趨'
		context = [[context],'..\ZS\NDVI\']
		context = [[context],'..\ZS\RESULT\']
		context = [[context],'..\MJ\']
		context = [[context],'..\DC\']
		context = [[context],'..\CL\']
		context = [[context],'..\NQ\DEM\']
		context = [[context],'..\NQ\RESULT\']
		context = [[context],'..\FZ\NDVI\']
		context = [[context],'..\FZ\RESULT\']
		context = [[context],'syn']
		context = [[context],'ndvi']
		context = [[context],'syn']
		context = [[context],'lai']
		context = [[context],'syn']
		context = [[context],'npp']
		context = [[context],'syn']
		context = [[context],'cld']
		context = [[context],'cls']
		context = [[context],'ndvi']
		context = [[context],'']
		context = [[context],'END']
		PRINTF, lun, context, FORMAT='(%"%s")'
		CLOSE, lun
		FREE_LUN, lun
	endif

	nlines = file_lines(pathfile)
	context = strarr(nlines)
	OPENR, lun, pathfile, /GET_LUN
	READF, lun, context
	line = 1
	(*ppath).zs_in_path  = context[line++]
	(*ppath).zs_out_path = context[line++]
	(*ppath).mj_in_path  = context[line++]
	(*ppath).dc_out_path = context[line++]
	(*ppath).cl_out_path = context[line++]
	(*ppath).nq_dem_file =context[line]
	(*ppath).nq_in_path  = context[line++]
	(*ppath).nq_out_path = context[line++]
	(*ppath).fz_in_path  = context[line++]
	(*ppath).fz_out_path = context[line++]

	(*ppath).ndvi_prefix=context[line++]
	(*ppath).ndvi_suffix=context[line++]
	(*ppath).lai_prefix =context[line++]
	(*ppath).lai_suffix =context[line++]
	(*ppath).npp_prefix =context[line++]
	(*ppath).npp_suffix =context[line++]
	(*ppath).cld_prefix =context[line++]
	(*ppath).cld_suffix =context[line++]
	(*ppath).clf_prefix =context[line++]
	(*ppath).clf_suffix =context[line++]


	CLOSE, lun
	FREE_LUN, lun
END