;计算大气水汽含量
pro water_vapor,input$,output$,smark,part
	DN_band2=read_tiff(input$+'_EV_250_RefSB_b1' + part + '.tif',geotiff=geotiff)
	DN_band17=read_tiff(input$+'_EV_1KM_RefSB_b11' + part + '.tif',geotiff=geotiff)
	DN_band18=read_tiff(input$+'_EV_1KM_RefSB_b12' + part + '.tif',geotiff=geotiff)
	DN_band19=read_tiff(input$+'_EV_1KM_RefSB_b13' + part + '.tif',geotiff=geotiff)
	DN_solarzenith=read_tiff(input$+'_SolarZenith' + part + '.tif',geotiff=geotiff)
	muv=cos(DN_solarzenith*0.01*!dtor)
	Ref2=MODIS_CALIBRATION(DN_band2,SMARK,2,/REFLECTANCE)/muv
	Ref17=MODIS_CALIBRATION(DN_band17,SMARK,19,/REFLECTANCE)/muv
	Ref18=MODIS_CALIBRATION(DN_band18,SMARK,20,/REFLECTANCE)/muv
	Ref19=MODIS_CALIBRATION(DN_band19,SMARK,21,/REFLECTANCE)/muv
	t17=Ref17/Ref2
	t18=Ref18/Ref2
	t19=Ref19/Ref2
	w17=((0.02-alog(t17))/0.651)^2.
	w18=((0.02-alog(t18))/0.651)^2.
	w19=((0.02-alog(t19))/0.651)^2.
	w=0.192*w17+0.453*w18+0.355*w19
	write_tiff,output$+'_wv' + part + '.tif',w,geotiff=geotiff,/float
end



;确定图像上的暗目标
function get_ddv,input$,smark,cld, part
	DN_band7 = read_tiff(input$ + '_EV_500_RefSB_b4' + part + '.tif', geotiff = geotiff)
	DN_solarzenith = read_tiff(input$ + '_SolarZenith' + part + '.tif', geotiff = geotiff)
	muv = cos(DN_solarzenith * 0.01 * !dtor)
	Ref7 = MODIS_CALIBRATION(DN_band7, SMARK, 7, /REFLECTANCE) / muv
	indices = where(cld eq 1)
	if(indices[0] ne -1) then Ref7[indices] = 1.0
	Ref_ddv = {Ref1: Ref7 * 0.0, Ref3: Ref7 * 0.0}
	indices = where(Ref7 ge 0.01 and Ref7 le 0.15)
	print, part, float(n_elements(indices)) / float(n_elements(DN_band7))
	if((float(n_elements(indices)) / float(n_elements(DN_band7))) lt 0.15) then begin
		field = strsplit(input$, '\', /extract)
		str = '"' + field[n_elements(field) - 1] + '_' + part + '"图像中暗目标数量不够！'
		print, str
		caution, str
		return, -1
	endif else begin
		Ref_ddv.Ref1[indices] = Ref7[indices] / 2.
		Ref_ddv.Ref3[indices] = Ref7[indices] / 4.
		return,Ref_ddv
	endelse
end


;计算散射角
;function get_pthata,input$, part
;	DN_solarzenith = read_tiff(input$ + '_SolarZenith' + part + '.tif', geotiff = geotiff) * 0.01 * !dtor
;	DN_solarazimuth = fix(read_tiff(input$ + '_SolarAzimuth' + part + '.tif', geotiff = geotiff)) * 0.01 * !dtor
;	DN_sensorzenith = read_tiff(input$ + '_SensorZenith' + part + '.tif', geotiff = geotiff) * 0.01 * !dtor
;	DN_sensorazimuth = fix(read_tiff(input$ + '_SensorAzimuth' + part + '.tif', geotiff = geotiff)) * 0.01 * !dtor
;	deltaphi = abs(DN_solarazimuth - DN_sensorazimuth)
;	indices = where(deltaphi gt !pi)
;	if(indices[0] ne -1) then deltaphi[indices] = 2 * !pi - deltaphi[indices]
;	muthata = cos(DN_solarzenith) * cos(DN_sensorzenith) + sin(DN_solarzenith) * sin(DN_sensorzenith) * cos(deltaphi)
;	thata = acos(muthata) / !dtor
;	return, thata
;end




;根据暗目标，进行反演气溶胶光学厚度
pro aerosol_tao, input$, output$, smark, cld$, latitude$, part
	DN_band1 = read_tiff(input$ + '_EV_250_RefSB_b0' + part + '.tif', geotiff = geotiff)
	DN_band3 = read_tiff(input$ + '_EV_500_RefSB_b0' + part + '.tif', geotiff = geotiff)
	DN_solarzenith = read_tiff(input$ + '_SolarZenith' + part + '.tif', geotiff = geotiff) * 0.01
	DN_solarazimuth = fix(read_tiff(input$ + '_SolarAzimuth' + part + '.tif', geotiff = geotiff)) * 0.01
	DN_sensorzenith = read_tiff(input$ + '_SensorZenith' + part + '.tif', geotiff = geotiff) * 0.01
	DN_sensorazimuth = fix(read_tiff(input$ + '_SensorAzimuth' + part + '.tif', geotiff = geotiff)) * 0.01
	deltaphi = abs(DN_solarazimuth - DN_sensorazimuth)
	indices = where(deltaphi gt 180)
	if(indices[0] ne -1) then deltaphi[indices] = 360 - deltaphi[indices]
	muv = cos(DN_solarzenith * !dtor)
	Ref1 = MODIS_CALIBRATION(DN_band1, SMARK, 1, /REFLECTANCE) / muv
	Ref3 = MODIS_CALIBRATION(DN_band3, SMARK, 3, /REFLECTANCE) / muv
	field = strsplit(input$, '\' ,/extract)
	joy = fix(strmid(field[n_elements(field)-1], 5, 3))
	cld = read_tiff(cld$, geotiff = geotiff)

;	roi = read_tiff('data\base\CHINA_1KM_ROI' + part + '.TIF',geotiff = geotiff)
;	indices = where(roi eq 0)
;	if(indices[0] ne -1) then cld[indices] = 1

	indices = where(cld eq 1)
	if(indices[0] ne -1) then begin
		Ref1[indices] = 1.0
		Ref3[indices] = 1.0
	endif
	tao = cld * 0.0
	Ref_ddv = get_ddv(input$, smark, cld, part)
	if(size(Ref_ddv,/type) ne 2) then begin
		lat = read_tiff(latitude$, geotiff = geotiff)
		indices_mid = where(lat eq 0)
		indices_tro = where(lat eq 2)
		indices_sub = where(lat eq 1)
;		thata = get_pthata(input$, part)

		if(indices_mid[0] ne -1) then lat[indices] = 1		;把属于中纬度的地区提取出来
		if(indices_tro[0] ne -1) then lat[indices] = 0
		if(indices_sub[0] ne -1) then lat[indices] = 0
		Ref_ddv_temp = {Ref1: Ref_ddv.Ref1 * lat, Ref3: Ref_ddv.Ref3 * lat}
		indices = where(Ref_ddv_temp.Ref3 ne 0.0)
		if(indices[0] ne -1) then begin
			if(joy ge 91 and joy le 273) then begin
				lut_b = read_ascii('data\paras\LUT\MIDSUMCON_B.TXT')
				lut_b = lut_b.field1
				lut_r = read_ascii('data\paras\LUT\MIDSUMCON_R.TXT')
				lut_r = lut_r.field1
			endif else begin
				lut_b = read_ascii('data\paras\LUT\MIDWINCON_B.TXT')
				lut_b = lut_b.field1
				lut_r = read_ascii('data\paras\LUT\MIDWINCON_R.TXT')
				lut_r = lut_r.field1
			endelse
			;将D = (D1 * D1 + D2 * D2 + D3 * D3 + D4 * D4 + D5 * D5) ^ (1. / 5)分解多项式
			;并将乘方等运算移到了循环外面
			;///////////////////////////////////////////////
			;整理成一维的数组
			lut_b0 = reform(lut_b[0, *])
			lut_b1 = reform(lut_b[1, *])
			lut_b2 = reform(lut_b[2, *])
			lut_b5 = reform(lut_b[5, *])
			lut_b6 = reform(lut_b[6, *])
			;平方运算
			lut_b02 = (lut_b0 / 90.) ^ 2
			lut_b12 = (lut_b1 / 90.) ^ 2
			lut_b22 = (lut_b2 / 180.) ^ 2
			lut_b52 = lut_b5 ^ 2
			lut_b62 = lut_b6 ^ 2
			lut_ball = lut_b02 + lut_b12 + lut_b22 + lut_b52 + lut_b62
			;2 * A
			A2 = 2 * DN_solarzenith / (90.^2)
			B2 = 2 * DN_sensorzenith / (90.^2)
			C2 = 2 * deltaphi / (180. ^ 2)
			D2 = 2 * Ref_ddv_temp.Ref3
			E2 = 2 * Ref3
			;A ^ 2
			AA = (DN_solarzenith / 90.) ^ 2
			BB = (DN_sensorzenith / 90.) ^ 2
			CC = (deltaphi / 180.) ^ 2
			DD = (Ref_ddv_temp.Ref3) ^ 2
			EE = Ref3 ^ 2
			FF = AA + BB + CC + DD + EE
			;///////////////////////////////////////////////
			t = systime(/s)
			for i = 0L, n_elements(indices)-1 do begin
				D = lut_ball + FF[indices[i]] $
					- lut_b0 * A2[indices[i]] $
					- lut_b1 * B2[indices[i]] $
					- lut_b2 * C2[indices[i]] $
					- lut_b5 * D2[indices[i]] $
					- lut_b6 * E2[indices[i]]
				index = where(D eq min(D))
				tao_b = total(lut_b[3, index]) / n_elements(index)
				tao[indices[i]] = tao_b
;
				if i eq 1000 then begin
					print, systime(/s) - t
					print, 'here'
				endif
			endfor
		endif

		if(indices_mid[0] ne -1) then lat[indices] = 0
		if(indices_tro[0] ne -1) then lat[indices] = 1		;把属于热带的地区提取出来
		if(indices_sub[0] ne -1) then lat[indices] = 0
		Ref_ddv_temp = {Ref1: Ref_ddv.Ref1 * lat, Ref3: Ref_ddv.Ref3 * lat}
		indices = where(Ref_ddv_temp.Ref3 ne 0.0)
		if(indices[0] ne -1) then begin
			lut_b = read_ascii('data\paras\LUT\TROCON_B.TXT')
			lut_b = lut_b.field1
			lut_r = read_ascii('data\paras\LUT\TROCON_R.TXT')
			lut_r = lut_r.field1
			;将D = (D1 * D1 + D2 * D2 + D3 * D3 + D4 * D4 + D5 * D5) ^ (1. / 5)分解多项式
			;并将乘方等运算移到了循环外面
			;///////////////////////////////////////////////
			;整理成一维的数组
			lut_b0 = reform(lut_b[0, *])
			lut_b1 = reform(lut_b[1, *])
			lut_b2 = reform(lut_b[2, *])
			lut_b5 = reform(lut_b[5, *])
			lut_b6 = reform(lut_b[6, *])
			;平方运算
			lut_b02 = (lut_b0 / 90.) ^ 2
			lut_b12 = (lut_b1 / 90.) ^ 2
			lut_b22 = (lut_b2 / 180.) ^ 2
			lut_b52 = lut_b5 ^ 2
			lut_b62 = lut_b6 ^ 2
			;2 * A
			A2 = 2 * DN_solarzenith / (90.^2)
			B2 = 2 * DN_sensorzenith / (90.^2)
			C2 = 2 * deltaphi / (180. ^ 2)
			D2 = 2 * Ref_ddv_temp.Ref3
			E2 = 2 * Ref3
			;A ^ 2
			AA = (DN_solarzenith / 90.) ^ 2
			BB = (DN_sensorzenith / 90.) ^ 2
			CC = (deltaphi / 180.) ^ 2
			DD = (Ref_ddv_temp.Ref3) ^ 2
			EE = Ref3 ^ 2
			;///////////////////////////////////////////////
			for i = 0L, n_elements(indices)-1 do begin
				;原来的方法，实现起来比较耗费时间，把平方计算移到循环外面可节约大量时间
;				D1 = (lut_b[0, *] - DN_solarzenith[indices[i]]) / 90.
;				D2 = (lut_b[1, *] - DN_sensorzenith[indices[i]]) / 90.
;				D3 = (lut_b[2, *] - deltaphi[indices[i]]) / 180.
;				D4 = lut_b[5, *] - Ref_ddv_temp.Ref3[indices[i]]
;				D5 = lut_b[6, *] - Ref3[indices[i]]
;				D = (D1 * D1 + D2 * D2 + D3 * D3 + D4 * D4 + D5 * D5) ^ (1. / 5)
				D = lut_b02 - lut_b0 * A2[indices[i]] + AA[indices[i]] $
					+ lut_b12 - lut_b1 * B2[indices[i]] + BB[indices[i]] $
					+ lut_b22 - lut_b2 * C2[indices[i]] + CC[indices[i]] $
					+ lut_b52 - lut_b5 * D2[indices[i]] + DD[indices[i]] $
					+ lut_b62 - lut_b6 * E2[indices[i]] + EE[indices[i]]
				index = where(D eq min(D))
				tao_b = total(lut_b[3, index]) / n_elements(index)
				tao[indices[i]] = tao_b
;				D1 = (lut_r[0, *] - DN_solarzenith[indices[i]]) / 90.
;				D2 = (lut_r[1, *] - DN_sensorzenith[indices[i]]) / 90.
;				D3 = (lut_r[2, *] - deltaphi[indices[i]]) / 180.
;				D4 = lut_r[5, *] - Ref_ddv_temp.Ref1[indices[i]]
;				D5 = lut_r[6, *] - Ref1[indices[i]]
;				D = (D1 * D1 + D2 * D2 + D3 * D3 + D4 * D4 + D5 * D5) ^ (1. / 5)
;				index = where(D eq min(D))
;				tao_r = total(lut_r[3, index]) / n_elements(index)
;				tao[indices[i]] = (tao_b + tao_r) / 2.
			endfor
		endif

		;由于整个中国图像为5300×4800，所以如果进行气溶胶光学厚度插值，机器性能跟不上
		;所以，要把气溶胶光学厚度插为分辨率10公里的图像
		dims = size(tao,/dimensions)
;		dims = fix(dims / 10)
;		tao = congrid(tao, dims[0], dims[1])
		indices = where(tao ne 0.0)
		if(indices[0] ne -1) then ind = array_indices(tao, indices)
		tempx = ind[0,*]
		tempy = ind[1,*]
		tempv = transpose(tao[indices])
		tao = griddata(tempx, tempy, tempv, start = [0,0], delta = 1, dimension = dims)
		write_tiff, output$ + '_tao' + part + '.tif', tao, geotiff = geotiff,/float

		;暂时不考虑亚北极区域，中国属于此区域的地区极少，只有黑龙江北部，而现在气候变暖，亚北极区
		;实际上应该北移，则中国输入此区域的地区基本没有
	endif else begin
		write_tiff, output$ + '_tao' + part + '.tif', tao, geotiff = geotiff,/float
	endelse
end

;各种大气产品的集成
PRO ATMOSPHERE_PRODUCT, dir, part, latitude$, wv = wv, aerosol = aerosol
	input$ = dir.modis_pre + dir.date
	smark = dir.mark
	output$ = dir.modis_product + dir.date

	;不调用计算
	if(keyword_set(wv)) then water_vapor,input$, output$, smark

	if(keyword_set(aerosol)) then begin
		;land_type$='data\base\china_1km_landtype.tif' ;用于区分下垫面的文件，由0、1、2、3分别代表大陆，水体，城市、沙尘
		;latitude$ = 'D:\crop_modis_test\A2003253_LAT.tif'
		;latitude$ = 'data\base\CHINA_1KM_LAT' + part + '.TIF'  ;用于区分地区纬度，由0、1、2分别代表中纬度(温带)、低纬度(热带)、高纬度(寒带)
		cld$ = output$ + '_cld' + part + '.tif'
		if(~file_test(cld$)) then begin
			print, '请先进行云检测！'
			return
		endif
		;大气纠正
		aerosol_tao, input$, output$, smark, cld$, latitude$, part  ;, land_type$
	endif
END