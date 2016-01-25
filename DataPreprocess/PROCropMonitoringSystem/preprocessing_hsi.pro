pro preprocessing_hsi,inputfile,outputfile

	CATCH, Error_status
	;This statement begins the error handler:
	IF Error_status NE 0 THEN BEGIN
		if size(progresstimer, /TYPE) ne 0 then $
			progresstimer -> Destroy
			common_log,'计算出错'
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

	data=read_tiff(inputfile,geotiff=geoinfo,interleave=2)
	data_size=size(data)

	common_log,'读取HJ星HSI数据'

	PROGRESSTIMER->UPDATE, 5 ;继续进行
	inputdata=data[*,*,0]
	for col=0,data_size[1],1 do begin
		temptotal=total(inputdata[col,*])
		if temptotal ne 0 then begin
			x1=col
			y1=where(inputdata[col,*])
			break
		endif
	endfor

	for row=0,data_size[2],1 do begin
		temptotal=total(inputdata[*,row])
		if temptotal ne 0 then begin
			x2=where(inputdata[*,row])
			y2=row
			break
		endif
	endfor

;		calculate the angle
	angle=-atan(float(x2[0]-x1)/float(y2-y1[0]-1))*!RADEG

	for time=0,1,1 do begin
		for fnum=0,data_size[3]-1,1 do begin

			CANCELLED = PROGRESSTIMER->CHECKCANCEL()
			IF CANCELLED THEN BEGIN
				OK = DIALOG_MESSAGE('用户终止了操作')
				PROGRESSTIMER->DESTROY ;结束进度条
				RETURN
			ENDIF

	;		数据旋转
			tempdata=fltarr(data_size[1],100)
			data2=[[tempdata],[data[*,*,fnum]],[tempdata]]

			img_size=size(data2,/dimensions)
			data2=rot(data2,angle,1,img_size[0]/2,img_size[1]/2,/interp)

			img_size=size(data2,/dimensions)

			if time eq 0 then begin
				stack=fltarr(img_size[0],img_size[1]-200)
				fnum=0
				break
			endif

	;		去条带噪声
			img_index=where(data2 gt 0.0)
			if img_index[0] ne -1 then begin
				img_mean=mean(data2[img_index],/NAN)
				img_std=stddev(data2[img_index],/NAN)
			endif else begin
				img_mean=0
				img_std=1
			endelse

			for num=0,img_size[0]-1,1 do begin

				col_index=where(data2[num,*] gt 0.0)
				if col_index[0] ne -1 then begin
					if n_elements(data2[num,col_index]) lt 2 then begin
						col_mean=(data2[num,col_index])[0]
						col_std=1
					endif else begin
						col_mean=mean(data2[num,col_index],/NAN)
						col_std=stddev(data2[num,col_index],/NAN)
					endelse
				endif else begin
					col_mean=0
					col_std=1
				endelse

				a=img_std/col_std
				b=img_mean-a*col_mean
				data2[num,*]=a*data2[num,*]+b
			endfor
			data2=data2/(1000.0)

			data2=rot(data2,0-angle,1,img_size[0]/2,img_size[1]/2,/interp)
			data2=data2[*,100:img_size[1]-101]
			stack=[[[stack]],[[data2]]]

			PROGRESSTIMER->UPDATE, (5+(fnum*100.0)/(data_size[3]-1)) ;继续进行

		endfor
	endfor

	write_tiff,outputfile,stack[*,*,1:*],geotiff=geoinfo,/float,planarconfig=2

	PROGRESSTIMER->DESTROY

	result=dialog_message('计算完成！',/information)
	common_log,'处理完毕'
end