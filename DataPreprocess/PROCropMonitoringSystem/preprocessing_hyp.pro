;Preprocessing Hyperion 1GST
pro preprocessing_hyp,input_filepath,output_filepath,in_GST,in_BFNP

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

;	设置原始数据所在目录，并获取影像名称及数量
	file_path=input_filepath
	fname=file_search(file_path+'*.tif')
	file_num=n_elements(fname)
;	print,file_num
	if (file_num eq 0 or file_num lt 242) then begin
		Result = DIALOG_MESSAGE('指定的文件夹下没有hyperion数据或数据不全', /CENTER)
		PROGRESSTIMER->DESTROY ;结束进度条
		return
	endif

;	对影像进行预处理
	for fnum=0,file_num-1,1 do begin


		CANCELLED = PROGRESSTIMER->CHECKCANCEL()
		IF CANCELLED THEN BEGIN
			OK = DIALOG_MESSAGE('用户终止了操作')
			PROGRESSTIMER->DESTROY ;结束进度条
			common_log,'用户取消了操作'
			RETURN
		ENDIF

		PROGRESSTIMER->UPDATE, ((fnum*100.0)/(file_num-1)) ;继续进行

		if query_tiff(fname[fnum]) eq 0 then begin
			Result = DIALOG_MESSAGE('Hyperion数据文件格式错误！'+fname[fnum], /CENTER)
			PROGRESSTIMER->DESTROY ;结束进度条
			common_log,'Hyperion数据文件格式有错误'
			return
		end
;		数据旋转

;		读入影像，并获得其维数
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

;		旋转影像，并获得其维数
;		rot函数中第二个参数为旋转角度
		data=rot(data,angle,1,img_size[0]/2,img_size[1]/2,/interp)
		img_size=size(data,/dimensions)

;		去坏线
;		计算列与列之间像元的灰度斜率S
	  	S=fltarr(img_size[0],img_size[1])
		blnum=fltarr(img_size[0])
		for num=1,img_size[0]-2, 1 do begin
			S[num,*]=float(data[num-1,*]+data[num+1,*]-2*data[num,*])/ $
					abs(data[num+1,*]-data[num-1,*])
;			根据灰度斜率阈值(GST)判断是否为坏像元，并返回两类像元的个数
			bpnum=where(S[num,*] ge in_GST,num1,ncomplement=num2)
;			计算坏像元数的比例
			blnum[num]=float(num1)/float(num1+num2)
		endfor

;		print,'bpnum: ',bpnum
;		比较坏像元数比例与BFNP(Badline Flag Number Percentage)，大于BFNP的判断为坏线
		blindex=where(blnum ge in_BFNP,num3)
;		print,'blindex: ',blindex,num3

;		将判断出来的坏线用其两边像元的均值进行替换，实现修补
		for num=0,num3-1,1 do begin
			data[blindex[num],*]=float(data[blindex[num]-1,*]+ $
								data[blindex[num]+1,*])/2
		endfor

;		绝对辐射亮度值转换(可见光、近红外除以40，短波红外除以80)
;		单位转换成FLAASH输入项的单位μW/cm2 * nm* sr
		if fnum lt 70 then data=data*0.1/40.0 else data=data*0.1/80.0

;		去垂直条带噪声(全局归一化方法)
;		计算整景影像的均值和标准差
		img_index=where(data gt 0.0)
		if img_index[0] ne -1 then begin
			img_mean=mean(data[img_index],/NAN)
			img_std=stddev(data[img_index],/NAN)
		endif else begin
			img_mean=0
			img_std=1
		endelse

		for num=0,img_size[0]-1,1 do begin
;			计算列的均值与标准差
			col_index=where(data[num,*] gt 0.0)
			if col_index[0] ne -1 then begin
				col_mean=mean(data[num,col_index],/NAN)
				col_std=stddev(data[num,col_index],/NAN)
			endif else begin
				col_mean=0
				col_std=1
			endelse
;			计算系数a和b
			a=img_std/col_std
			b=img_mean-a*col_mean
;			print,a,' $ ',data[num,600],' $ ',b
;			计算新的数据值
			data[num,*]=a*data[num,*]+b

		endfor

		data=rot(data,0-angle,1,img_size[0]/2,img_size[1]/2,/interp)
		data=data[*,100:img_size[1]-101]

;		获得裁切后的影像大小
		img_size=size(data,/dimensions)

;		输出经过预处理的影像
		output=output_filepath+'band'+strtrim(string(fnum+1),2)+'.tif'
		write_tiff,output,data,geotiff=geoinfo,/float,planarconfig=2

	endfor

	PROGRESSTIMER->DESTROY

	result=dialog_message('计算完成！',/information)
	common_log,'处理完毕'
end