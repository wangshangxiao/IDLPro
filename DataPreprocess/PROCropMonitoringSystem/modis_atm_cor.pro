PRO MODIS_ATM_COR, dir, part, latitude$
	input$ = dir.modis_pre + dir.date
	smark = dir.mark
	output$ = dir.modis_product + dir.date

	joy = GetDateOfString(dir.date, /jul) - julday(12, 31, fix(strmid(dir.date, 0, 4)) - 1)
;	latitude$ = 'D:\crop_modis_test\A2003253_LAT.tif'
;	latitude$ = 'data\base\CHINA_1KM_LAT' + part + '.TIF'  ;用于区分地区纬度，由0、1、2分别代表低纬度(热带)、中纬度(温带)、高纬度(寒带)
	cld$ = output$ + '_cld' + part + '.tif'
	tao$ = output$ + '_tao' + part + '.tif'
;	roi = read_tiff('data\base\CHINA_1KM_ROI' + part + '.TIF', geotiff = geotiff)
	if(~file_test(cld$)) then begin
		print, '请先进行云检测！'
		return
	endif
	if(~file_test(tao$)) then begin
		str = '"' + filed[1] + '_' + part + '"没有气溶胶光学厚度文件，将不进行大气校正！'
		print, str
		caution, str
		return
	endif
	DN1 = read_tiff(input$ + '_EV_250_RefSB_b0' + part + '.tif', geotiff = geotiff)
	DN2 = read_tiff(input$ + '_EV_250_RefSB_b1' + part + '.tif', geotiff = geotiff)
	DN3 = read_tiff(input$ + '_EV_500_RefSB_b0' + part + '.tif', geotiff = geotiff)
	DN4 = read_tiff(input$ + '_EV_500_RefSB_b1' + part + '.tif', geotiff = geotiff)
	DN5 = read_tiff(input$ + '_EV_500_RefSB_b2' + part + '.tif', geotiff = geotiff)
;	DN6 = read_tiff(input$ + '_EV_500_RefSB_b3' + part + '.tif', geotiff = geotiff)
	DN7 = read_tiff(input$ + '_EV_500_RefSB_b4' + part + '.tif', geotiff = geotiff)
	DN_solarzenith = read_tiff(input$ + '_SolarZenith' + part + '.tif', geotiff = geotiff) * 0.01
	DN_solarazimuth = fix(read_tiff(input$ + '_SolarAzimuth' + part + '.tif', geotiff = geotiff)) * 0.01
	DN_sensorzenith = read_tiff(input$ + '_SensorZenith' + part + '.tif', geotiff = geotiff) * 0.01
	DN_sensorazimuth = fix(read_tiff(input$ + '_SensorAzimuth' + part + '.tif', geotiff = geotiff)) * 0.01
	deltaphi = abs(DN_solarazimuth - DN_sensorazimuth)
	indices = where(deltaphi gt 180)
	if(indices[0] ne -1) then deltaphi[indices] = 360 - deltaphi[indices]
	muv = cos(DN_solarzenith * !dtor)
	dims = size(DN1, /dimensions)
	Ref = fltarr(dims[0], dims[1], 7)
	Ref[*, *, 0] = MODIS_CALIBRATION(DN1, SMARK, 1, /REFLECTANCE) / muv
	Ref[*, *, 1] = MODIS_CALIBRATION(DN2, SMARK, 2, /REFLECTANCE) / muv
	Ref[*, *, 2] = MODIS_CALIBRATION(DN3, SMARK, 3, /REFLECTANCE) / muv
	Ref[*, *, 3] = MODIS_CALIBRATION(DN4, SMARK, 4, /REFLECTANCE) / muv
	Ref[*, *, 4] = MODIS_CALIBRATION(DN5, SMARK, 5, /REFLECTANCE) / muv
;	Ref[*, *, 5] = MODIS_CALIBRATION(DN6, SMARK, 6, /REFLECTANCE) / muv
	Ref[*, *, 6] = MODIS_CALIBRATION(DN7, SMARK, 7, /REFLECTANCE) / muv
	cld = read_tiff(cld$, geotiff = geotiff)
;	dims = size(cld,/dimensions)
	if(file_test(tao$)) then begin
		tao = read_tiff(tao$, geotiff = geotiff)
	endif else begin
		tao = cld * 0.
	endelse
;	tao = congrid(tao, dims[0], dims[1])
	lat = read_tiff(latitude$, geotiff = geotiff)
	indices_mid = where(lat eq 1)
	indices_tro = where(lat eq 0)
	indices_sub = where(lat eq 2)

	if(indices_mid[0] ne -1) then lat[indices_mid] = 1		;把属于中纬度的地区提取出来
	if(indices_tro[0] ne -1) then lat[indices_tro] = 0
	if(indices_sub[0] ne -1) then lat[indices_sub] = 0
	cld_temp = (~cld) and lat ;and roi
	indices1 = where(cld_temp eq 1)

	if(indices_mid[0] ne -1) then lat[indices_mid] = 0
	if(indices_tro[0] ne -1) then lat[indices_tro] = 1		;把属于热带的地区提取出来
	if(indices_sub[0] ne -1) then lat[indices_sub] = 0
	cld_temp = (~cld) and lat ;and roi
	indices2 = where(cld_temp eq 1)

	if(indices1[0] eq -1 and indices2[0] eq -1) then begin
		print, part, '图像中没有可以进行大气校正的像元！'
;		Result = DIALOG_MESSAGE('图像中没有可以进行大气校正的像元！',/error,title='Warning!')
		for i = 0, 1 do begin
			TEMP_P0 = Ref[*,*,i] * 0.
			TEMP_T = Ref[*,*,i] * 0. + 1.
			TEMP_S = Ref[*,*,i] * 0.
			write_tiff, output$ + '_acr_b' + strtrim(string(i + 1), 2) + '_P0' + part + '.tif', TEMP_P0, geotiff = geotiff, /float
			write_tiff, output$ + '_acr_b' + strtrim(string(i + 1), 2) + '_T' + part + '.tif', TEMP_T, geotiff = geotiff, /float
			write_tiff, output$ + '_acr_b' + strtrim(string(i + 1), 2) + '_S' + part + '.tif', TEMP_S, geotiff = geotiff, /float
			write_tiff, output$ + '_acr_b' + strtrim(string(i + 1), 2) + part + '.tif', Ref[*,*,i], geotiff = geotiff, /float
		endfor
		return
	endif else begin
		indices_tao = where(tao eq 0.0, count)
		if(count / n_elements(tao ) eq 1) then begin
			for i = 0, 1 do begin
				TEMP_P0 = Ref[*,*,i] * 0.
				TEMP_T = Ref[*,*,i] * 0. + 1.
				TEMP_S = Ref[*,*,i] * 0.
				write_tiff, output$ + '_acr_b' + strtrim(string(i + 1), 2) + '_P0' + part + '.tif', TEMP_P0, geotiff = geotiff, /float
				write_tiff, output$ + '_acr_b' + strtrim(string(i + 1), 2) + '_T' + part + '.tif', TEMP_T, geotiff = geotiff, /float
				write_tiff, output$ + '_acr_b' + strtrim(string(i + 1), 2) + '_S' + part + '.tif', TEMP_S, geotiff = geotiff, /float
				write_tiff, output$ + '_acr_b' + strtrim(string(i + 1), 2) + part + '.tif', Ref[*,*,i], geotiff = geotiff, /float
			endfor
		endif else begin
			for i = 0, 1 do begin
				Ps = Ref[*,*,i]
				TEMP_P0 = Ref[*,*,i] * 0.
				TEMP_T = Ref[*,*,i] * 0. + 1.
				TEMP_S = Ref[*,*,i] * 0.
				if(indices1[0] ne -1) then begin
					if(joy ge 91 and joy le 273) then begin
						lut = read_ascii('data\paras\LUT\MIDSUM\lut'+strtrim(string(42 + i), 2)+'.txt')
						lut = lut.field1
					endif else begin
						lut = read_ascii('data\paras\LUT\MIDWIN\lut'+strtrim(string(42 + i), 2)+'.txt')
						lut = lut.field1
					endelse
					;将D = (D1 * D1 + D2 * D2 + D3 * D3 + D4 * D4) ^ (1. / 4)分解多项式
					;并将乘方等运算移到了循环外面
					;//////////////////////////////////////////////////////////////////
					;整理成一维的数组
					lut_0 = reform(lut[0, *])
					lut_1 = reform(lut[1, *])
					lut_2 = reform(lut[2, *])
					lut_3 = reform(lut[3, *])
					;平方运算
					lut_02 = lut_0 ^ 2
					lut_12 = lut_1 ^ 2
					lut_22 = lut_2 ^ 2
					lut_32 = lut_3 ^ 2
					;2 * A
					A2 = 2 * DN_solarzenith
					B2 = 2 * DN_sensorzenith
					C2 = 2 * deltaphi
					D2 = 2 * tao
					;A ^ 2
					AA = DN_solarzenith ^ 2
					BB = DN_sensorzenith ^ 2
					CC = deltaphi ^ 2
					DD = tao ^ 2
					;//////////////////////////////////////////////////////////////////
					for j = 0L, n_elements(indices1)-1 do begin
						;原来的方法，实现起来比较耗费时间，把平方计算移到循环外面可节约大量时间
;						D1 = lut[0, *] - DN_solarzenith[indices1[j]]
;						D2 = lut[1, *] - DN_sensorzenith[indices1[j]]
;						D3 = lut[2, *] - deltaphi[indices1[j]]
;						D4 = lut[3, *] - tao[indices1[j]]
;						D = (D1 * D1 + D2 * D2 + D3 * D3 + D4 * D4) ^ (1. / 4)
						D = lut_02 - lut_0 * A2[indices1[j]] + AA[indices1[j]] $
							+ lut_12 - lut_1 * B2[indices1[j]] + BB[indices1[j]] $
							+ lut_22 - lut_2 * C2[indices1[j]] + CC[indices1[j]] $
							+ lut_32 - lut_3 * D2[indices1[j]] + DD[indices1[j]]
						index = where(D eq min(D))
						n = float(n_elements(index))
						P0 = total(lut[5, index]) / n
						T = total(lut[6, index]) / n
						S = total(lut[7, index]) / n
						ind = array_indices(cld_temp, indices1[j])
						TEMP_P0[indices1[j]] = P0
						TEMP_T[indices1[j]] = T
						TEMP_S[indices1[j]] = S
						Ps[indices1[j]] = (Ref[ind[0], ind[1], i] - P0) / (T + Ref[ind[0], ind[1], i] * S - P0 * S)
					endfor
				endif
				if(indices2[0] ne -1) then begin
					lut = read_ascii('data\paras\LUT\TRO\lut'+strtrim(string(42 + i), 2)+'.txt')
					lut = lut.field1
					;将D = (D1 * D1 + D2 * D2 + D3 * D3 + D4 * D4) ^ (1. / 4)分解多项式
					;并将乘方等运算移到了循环外面
					;//////////////////////////////////////////////////////////////////
					;整理成一维的数组
					lut_0 = reform(lut[0, *])
					lut_1 = reform(lut[1, *])
					lut_2 = reform(lut[2, *])
					lut_3 = reform(lut[3, *])
					;平方运算
					lut_02 = lut_0 ^ 2
					lut_12 = lut_1 ^ 2
					lut_22 = lut_2 ^ 2
					lut_32 = lut_3 ^ 2
					;2 * A
					A2 = 2 * DN_solarzenith
					B2 = 2 * DN_sensorzenith
					C2 = 2 * deltaphi
					D2 = 2 * tao
					;A ^ 2
					AA = DN_solarzenith ^ 2
					BB = DN_sensorzenith ^ 2
					CC = deltaphi ^ 2
					DD = tao ^ 2
					;//////////////////////////////////////////////////////////////////
					for j = 0L, n_elements(indices2)-1 do begin
						;原来的方法，实现起来比较耗费时间，把平方计算移到循环外面可节约大量时间
;						D1 = lut[0, *] - DN_solarzenith[indices2[j]]
;						D2 = lut[1, *] - DN_sensorzenith[indices2[j]]
;						D3 = lut[2, *] - deltaphi[indices2[j]]
;						D2 = lut[3, *] - tao[indices2[j]]
;						D = (D1 * D1 + D2 * D2 + D3 * D3 + D4 * D4) ^ (1. / 4)
						D = lut_02 - lut_0 * A2[indices2[j]] + AA[indices2[j]] $
							+ lut_12 - lut_1 * B2[indices2[j]] + BB[indices2[j]] $
							+ lut_22 - lut_2 * C2[indices2[j]] + CC[indices2[j]] $
							+ lut_32 - lut_3 * D2[indices2[j]] + DD[indices2[j]]
						index = where(D eq min(D))
						n = float(n_elements(index))
						P0 = total(lut[5, index]) / n
						T = total(lut[6, index]) / n
						S = total(lut[7, index]) / n
						ind = array_indices(cld_temp, indices2[j])
						TEMP_P0[indices1[j]] = P0
						TEMP_T[indices1[j]] = T
						TEMP_S[indices1[j]] = S
						Ps[indices2[j]] = (Ref[ind[0], ind[1], i] - P0) / (T + Ref[ind[0], ind[1], i] * S - P0 * S)
					endfor
				endif
				write_tiff, output$ + '_acr_b' + strtrim(string(i + 1), 2) + '_P0' + part + '.tif', TEMP_P0, geotiff = geotiff, /float
				write_tiff, output$ + '_acr_b' + strtrim(string(i + 1), 2) + '_T' + part + '.tif', TEMP_T, geotiff = geotiff, /float
				write_tiff, output$ + '_acr_b' + strtrim(string(i + 1), 2) + '_S' + part + '.tif', TEMP_S, geotiff = geotiff, /float
				write_tiff, output$ + '_acr_b' + strtrim(string(i + 1), 2) + part + '.tif', Ps, geotiff = geotiff, /float
			endfor
		endelse
	endelse
END