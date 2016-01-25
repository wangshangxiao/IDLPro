PRO VI_CAL_RVI,data,red_band,nir_band,geoinfo,outputbase
;��ֲֵ��ָ��

	on_error,2

	common_log,'����RVI'

	temp1=float(data[*,*,nir_band])
	temp2=float(data[*,*,red_band])

	RVI=temp1/temp2

	index=where(temp2 eq 0.0)
	if index ne [-1] then RVI[index]=0.0

	outputfile=outputbase+'_rvi.tif'
	write_tiff,outputfile,RVI,geotiff=geoinfo,/float,planarconfig=2
END

PRO VI_CAL_NDVI,data,red_band,nir_band,geoinfo,outputbase
;��һ��ֲ��ָ��

	on_error,2

	common_log,'����NDVI'

	temp1=float(data[*,*,nir_band])-float(data[*,*,red_band])
	temp2=float(data[*,*,nir_band])+float(data[*,*,red_band])

	NDVI=temp1/temp2

	index=where(temp2 eq 0.0)
	if index ne [-1] then NDVI[index]=-1.0
	index=where(ndvi ge 1.0)
	if index ne [-1] then NDVI[index]=1.0

	outputfile=outputbase+'_ndvi.tif'
	write_tiff,outputfile,NDVI,geotiff=geoinfo,/float,planarconfig=2

END

PRO VI_CAL_EVI,data,blue_band,red_band,nir_band,geoinfo,outputbase
;��ǿ��ֲ��ָ��

	on_error,2

	common_log,'����EVI'

	G=2.5
	C1=6.0
	C2=7.5
	L=1.0

	temp1=float(data[*,*,nir_band])-float(data[*,*,red_band])
	temp2=float(data[*,*,nir_band])+C1*float(data[*,*,red_band])-C2*float(data[*,*,blue_band])+L

	EVI=G*(temp1/temp2)*(1+L)

	index=where(temp2 eq 0.0)
	if index ne [-1] then EVI[index]=0.0

	outputfile=outputbase+'_evi.tif'
	write_tiff,outputfile,EVI,geotiff=geoinfo,/float,planarconfig=2

END
;

PRO VI_CAL_SAVI,data,red_band,nir_band,geoinfo,outputbase
;��������ֲ��ָ��

	on_error,2

	common_log,'����SAVI'

	L=0.5

	temp1=(1+L)*(float(data[*,*,nir_band])-float(data[*,*,red_band]))
	temp2=float(data[*,*,nir_band])+float(data[*,*,red_band])+L

	SAVI=temp1/temp2

	index=where(temp2 eq 0.0)
	if index ne [-1] then SAVI[index]=0.0

	outputfile=outputbase+'_savi.tif'
	write_tiff,outputfile,SAVI,geotiff=geoinfo,/float,planarconfig=2

END
;
PRO VI_CAL_VARI,data,blue_band,green_band,red_band,geoinfo,outputbase
;�ɼ�������迹ֲ��ָ��

	on_error,2

	common_log,'VARI'

	temp1=float(data[*,*,green_band])-float(data[*,*,red_band])
	temp2=float(data[*,*,green_band])+float(data[*,*,red_band])-float(data[*,*,blue_band])

	VARI=temp1/temp2

	index=where(temp2 eq 0.0)
	if index ne [-1] then VARI[index]=0.0

	outputfile=outputbase+'_vari.tif'
	write_tiff,outputfile,VARI,geotiff=geoinfo,/float,planarconfig=2

END

PRO VI_CALCULATION,Event

	on_error,2

	widget_control,event.top,get_uvalue=PSTATE
	widget_control,(*PSTATE).input,get_value=inputfile
	widget_control,(*PSTATE).output,get_value=outputbase

	if file_test(inputfile) eq 0 then begin
		Result = DIALOG_MESSAGE('�������ݲ�����', /CENTER)
		return
	end

	if file_test(outputbase) eq 0 then begin
		Result = DIALOG_MESSAGE('����������ļ���δָ��!', /CENTER)
		return
	end

	if ((*PSTATE).RVI_Status eq 0) and $
		((*PSTATE).NDVI_Status eq 0) and $
		((*PSTATE).EVI_Status eq 0) and $
		((*PSTATE).SAVI_Status eq 0) and $
		((*PSTATE).VARI_Status eq 0) then begin

		Result = DIALOG_MESSAGE('��ѡ����Ҫ�����ֲ��ָ������', /CENTER)
		return
	endif

	PROGRESSTIMER = OBJ_NEW("SHOWPROGRESS", TLB,/CANCELBUTTON)
	PROGRESSTIMER->START

	CANCELLED = PROGRESSTIMER->CHECKCANCEL()
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('�û���ֹ�˲���')
		PROGRESSTIMER->DESTROY ;����������
		RETURN
	ENDIF

	inputtempfile=strsplit(inputfile,'.',/extract)
	inputfile=inputtempfile[0]+'.tif'
	outputtempfile=strsplit(file_basename(inputfile),'.',/extract)
	outputbase=common_filepath(outputbase)+outputtempfile

	blue_band=widget_info((*PSTATE).WID_DROPLIST_blue,/DROPLIST_SELECT)
	green_band=widget_info((*PSTATE).WID_DROPLIST_green,/DROPLIST_SELECT)
	red_band=widget_info((*PSTATE).WID_DROPLIST_red,/DROPLIST_SELECT)
	nir_band=widget_info((*PSTATE).WID_DROPLIST_nir,/DROPLIST_SELECT)

	data=read_tiff(inputfile,geotiff=geoinfo,interleave=2)

	common_log,'��ȡӰ������'

	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('�û���ֹ�˲���')
		PROGRESSTIMER->DESTROY ;����������
		RETURN
	ENDIF

	PROGRESSTIMER->UPDATE, 5

	if (*PSTATE).RVI_Status eq 1 then $
		VI_CAL_RVI,data,red_band,nir_band,geoinfo,outputbase

	PROGRESSTIMER->UPDATE, 20
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('�û���ֹ�˲���')
		PROGRESSTIMER->DESTROY ;����������
		RETURN
	ENDIF


	if (*PSTATE).NDVI_Status eq 1 then $
		VI_CAL_NDVI,data,red_band,nir_band,geoinfo,outputbase

	PROGRESSTIMER->UPDATE, 40
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('�û���ֹ�˲���')
		PROGRESSTIMER->DESTROY ;����������
		RETURN
	ENDIF

	if (*PSTATE).EVI_Status eq 1 then $
		VI_CAL_EVI,data,blue_band,red_band,nir_band,geoinfo,outputbase

	PROGRESSTIMER->UPDATE, 60
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('�û���ֹ�˲���')
		PROGRESSTIMER->DESTROY ;����������
		RETURN
	ENDIF

	if (*PSTATE).SAVI_Status eq 1 then $
		VI_CAL_SAVI,data,red_band,nir_band,geoinfo,outputbase

	PROGRESSTIMER->UPDATE, 80
	IF CANCELLED THEN BEGIN
		OK = DIALOG_MESSAGE('�û���ֹ�˲���')
		PROGRESSTIMER->DESTROY ;����������
		RETURN
	ENDIF

	if (*PSTATE).VARI_Status eq 1 then $
		VI_CAL_VARI,data,blue_band,green_band,red_band,geoinfo,outputbase

	PROGRESSTIMER->UPDATE, 100
	PROGRESSTIMER->DESTROY

	result=dialog_message('������ɣ�',/information)

	common_log,'�������'

END