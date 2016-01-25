;Preprocessing Hyperion 1GST
pro preprocessing_hyp,input_filepath,output_filepath,in_GST,in_BFNP

	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		if size(progresstimer, /TYPE) ne 0 then $
			progresstimer -> Destroy
			common_log,'�������'
		PRINT, 'Error index: ', Error_status
		PRINT, 'Error message: ', !ERROR_STATE.MSG
		Result = DIALOG_MESSAGE(!ERROR_STATE.MSG, /CENTER)
		help, /last_message, output=errtext
		Result = DIALOG_MESSAGE(errtext, /CENTER)
		CATCH, /CANCEL
		return
	ENDIF

	PROGRESSTIMER = OBJ_NEW("SHOWPROGRESS", TLB,/CANCELBUTTON)
	PROGRESSTIMER->START

;	����ԭʼ��������Ŀ¼������ȡӰ�����Ƽ�����
	file_path=input_filepath
	fname=file_search(file_path+'*.tif')
	file_num=n_elements(fname)
;	print,file_num
	if (file_num eq 0 or file_num lt 242) then begin
		Result = DIALOG_MESSAGE('ָ�����ļ�����û��hyperion���ݻ����ݲ�ȫ', /CENTER)
		PROGRESSTIMER->DESTROY ;����������
		return
	endif

;	��Ӱ�����Ԥ����
	for fnum=0,file_num-1,1 do begin


		CANCELLED = PROGRESSTIMER->CHECKCANCEL()
		IF CANCELLED THEN BEGIN
			OK = DIALOG_MESSAGE('�û���ֹ�˲���')
			PROGRESSTIMER->DESTROY ;����������
			common_log,'�û�ȡ���˲���'
			RETURN
		ENDIF

		PROGRESSTIMER->UPDATE, ((fnum*100.0)/(file_num-1)) ;��������

		if query_tiff(fname[fnum]) eq 0 then begin
			Result = DIALOG_MESSAGE('Hyperion�����ļ���ʽ����'+fname[fnum], /CENTER)
			PROGRESSTIMER->DESTROY ;����������
			common_log,'Hyperion�����ļ���ʽ�д���'
			return
		end
;		������ת

;		����Ӱ�񣬲������ά��
		inputdata=read_tiff(fname[fnum],geotiff=geoinfo,interleave=2)
		img_size=size(inputdata)

		if fnum eq 0 then begin
			for col=0,img_size[1],1 do begin
				temptotal=total(inputdata[col,*])
				if temptotal ne 0 then begin
					x1=col
					y1=where(inputdata[col,*])
					break
				endif
			endfor
	;		print,x1,y1[0]

			for row=0,img_size[2],1 do begin
				temptotal=total(inputdata[*,row])
				if temptotal ne 0 then begin
					x2=where(inputdata[*,row])
					y2=row
					break
				endif
			endfor
	;		print,x2[0],y2

	;		calculate the angle
			angle=-atan(float(x2[0]-x1)/float(y2-y1[0]-1))*!RADEG
			print,angle
		endif

		data=fltarr(img_size[1],100)
		data=[[data],[inputdata],[data]]
		img_size=size(data,/dimensions)

;		��תӰ�񣬲������ά��
;		rot�����еڶ�������Ϊ��ת�Ƕ�
		data=rot(data,angle,1,img_size[0]/2,img_size[1]/2,/interp)
		img_size=size(data,/dimensions)

;		ȥ����
;		����������֮����Ԫ�ĻҶ�б��S
	  	S=fltarr(img_size[0],img_size[1])
		blnum=fltarr(img_size[0])
		for num=1,img_size[0]-2, 1 do begin
			S[num,*]=float(data[num-1,*]+data[num+1,*]-2*data[num,*])/ $
					abs(data[num+1,*]-data[num-1,*])
;			���ݻҶ�б����ֵ(GST)�ж��Ƿ�Ϊ����Ԫ��������������Ԫ�ĸ���
			bpnum=where(S[num,*] ge in_GST,num1,ncomplement=num2)
;			���㻵��Ԫ���ı���
			blnum[num]=float(num1)/float(num1+num2)
		endfor

;		print,'bpnum: ',bpnum
;		�Ƚϻ���Ԫ��������BFNP(Badline Flag Number Percentage)������BFNP���ж�Ϊ����
		blindex=where(blnum ge in_BFNP,num3)
;		print,'blindex: ',blindex,num3

;		���жϳ����Ļ�������������Ԫ�ľ�ֵ�����滻��ʵ���޲�
		for num=0,num3-1,1 do begin
			data[blindex[num],*]=float(data[blindex[num]-1,*]+ $
								data[blindex[num]+1,*])/2
		endfor

;		���Է�������ֵת��(�ɼ��⡢���������40���̲��������80)
;		��λת����FLAASH������ĵ�λ��W/cm2 * nm* sr
		if fnum lt 70 then data=data*0.1/40.0 else data=data*0.1/80.0

;		ȥ��ֱ��������(ȫ�ֹ�һ������)
;		��������Ӱ��ľ�ֵ�ͱ�׼��
		img_index=where(data gt 0.0)
		if img_index[0] ne -1 then begin
			img_mean=mean(data[img_index],/NAN)
			img_std=stddev(data[img_index],/NAN)
		endif else begin
			img_mean=0
			img_std=1
		endelse

		for num=0,img_size[0]-1,1 do begin
;			�����еľ�ֵ���׼��
			col_index=where(data[num,*] gt 0.0)
			if col_index[0] ne -1 then begin
				col_mean=mean(data[num,col_index],/NAN)
				col_std=stddev(data[num,col_index],/NAN)
			endif else begin
				col_mean=0
				col_std=1
			endelse
;			����ϵ��a��b
			a=img_std/col_std
			b=img_mean-a*col_mean
;			print,a,' $ ',data[num,600],' $ ',b
;			�����µ�����ֵ
			data[num,*]=a*data[num,*]+b

		endfor

		data=rot(data,0-angle,1,img_size[0]/2,img_size[1]/2,/interp)
		data=data[*,100:img_size[1]-101]

;		��ò��к��Ӱ���С
		img_size=size(data,/dimensions)

;		�������Ԥ�����Ӱ��
		output=output_filepath+'band'+strtrim(string(fnum+1),2)+'.tif'
		write_tiff,output,data,geotiff=geoinfo,/float,planarconfig=2

	endfor

	PROGRESSTIMER->DESTROY

	result=dialog_message('������ɣ�',/information)
	common_log,'�������'
end