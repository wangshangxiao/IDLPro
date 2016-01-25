;**************************************************************************
;徐新刚,蒙继华
;定义整个系统的公共变量区变量
;2006.07.26
;**************************************************************************

;========以上为原代码，下面为杨绍锷修改的代码=20070531===============================
PRO C_COMMON_BLOCK

   ;设置3个公共变量,在整个系统中运用.
   ;注意:(1)common过程中定义的变量要在整个系统中得到运用，必须第一个被编译,如何第一个被编看下面.
   ;     (2)在整个系统中,要第一个被编译,则直接在"Build Order"中拖动要编译的位置或者在工程的过程名列表中按字母顺序应排在第一位.
   ;     (3)其他在引用时,不用调用定义公共变量的过程名,直接以 "common,公共变量块名".
   ;     (4)引用公共变量时,不必加上变量,直接用公共模块名即可!如果加上了变量名,则必须按定义时的顺序,否则会错误引用公共变量.

   COMMON COMMON_BLOCK,yesORno,DBobj,FILE_PATH,Year,DSN,USER_NAME,PWD,PROVINCE_CODE  ;也可以不初始化

   yesORno=0                                    ;标识库链接是否成功的变量
   DBobj=OBJ_NEW('IDLdbDatabase')             	;标识数据库对象

   FILE_PATH=''									;设置的文件访问路径
   Year=strmid(systime(),3,4,/REVERSE_OFFSET)   ;设置要估算的年份,初始值为当前年份

	ParaFile = 'text\Province_Code.txt'

	IF NOT FILE_TEST(ParaFile) THEN BEGIN
		INFO = DIALOG_MESSAGE('系统安装目录下找不到参数设置文件"Province_Code.txt"!',TITLE='警告')
		RETURN
	ENDIF

    OPENR,lun,ParaFile,/GET_LUN
    FileInfo = FSTAT(lun)
    paradata = BYTARR(FileInfo.SIZE)
    READU,lun,paradata
    FREE_LUN,lun

    enter=string(byte(13))+string(byte(10))   					  ;回车符ASCII码值

    index = STRPOS(paradata, 'Province_Code = ')

	PROVINCE_CODE = STRMID(paradata,index+16,6)
	print,'Province_Code = ',PROVINCE_CODE

	SDFile = 'text\SD_PREFERENCE.txt'

	IF NOT FILE_TEST(ParaFile) THEN BEGIN
		INFO = DIALOG_MESSAGE('系统安装目录下找不到参数设置文件"SD_PREFERENCE.txt"!',TITLE='警告')
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
;;=======以上为杨绍锷修改的代码=20070810=========================


PRO PRO_COMMON_BASE
	COMMON COMMON_BASE,MENU_OPERATION,X_OFFSET,Y_OFFSET,BASE_TOP,MENU_MANAGE
	MENU_OPERATION=1;操作菜单的ID
	MENU_MANAGE=1
	BASE_TOP=1
	X_OFFSET=0
	Y_OFFSET=0
END
;
PRO PRO_COMMON_DC
	COMMON DC_BLOCK,NewCropID		;定义公共模块,只供单产相关模块用
	NewCropID = '31'				;默认为31-春玉米(长城以北地区种春玉米,以南地区收完冬小麦后,种夏玉米)
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
		context = '公共路径设定'
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