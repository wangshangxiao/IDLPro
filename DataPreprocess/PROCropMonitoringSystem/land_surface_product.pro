;计算未经过大气校正的地表NDVI和ALBEDO
pro ndvi_albedo, input$, output$, smark, part, ndvi = ndvi, albedo = albedo
	DN_band1 = read_tiff(input$ + '_EV_250_RefSB_b0' + part + '.tif',geotiff = geotiff)
	DN_band2 = read_tiff(input$ + '_EV_250_RefSB_b1' + part + '.tif',geotiff = geotiff)
	DN_solarzenith = read_tiff(input$ + '_SolarZenith' + part + '.tif',geotiff = geotiff)
	muv = cos(DN_solarzenith * 0.01 * !dtor)
	Ref1 = MODIS_CALIBRATION(DN_band1, SMARK, 1, /REFLECTANCE) / muv
	Ref2 = MODIS_CALIBRATION(DN_band2, SMARK, 2, /REFLECTANCE) / muv
	if(keyword_set(ndvi)) then begin
		value_ndvi = Ref1 * 0
		indices = where(DN_band1 gt 0, count)
		if(count gt 0) then begin
			value_ndvi[indices] = (Ref2[indices] - Ref1[indices]) / (Ref2[indices] + Ref1[indices])
			value_ndvi[indices] = value_ndvi[indices] < 0.93
			value_ndvi[indices] = value_ndvi[indices] > (-0.2)
			value_ndvi[indices] = fix((value_ndvi[indices] + 1.) * 127, type = 1)
		endif

		indices = where((DN_band1) eq 0 and (value_ndvi eq fix(127 * 0.8)), count)
		if(count gt 0) then value_ndvi[indices] = 0.

		write_tiff, output$ + '_ndvi' + part + '.tif', value_ndvi, geotiff = geotiff
	endif
	if(keyword_set(albedo)) then begin
		DN_band3 = read_tiff(input$ + '_EV_500_RefSB_b0' + part + '.tif',geotiff = geotiff)
		DN_band4 = read_tiff(input$ + '_EV_500_RefSB_b1' + part + '.tif',geotiff = geotiff)
		DN_band5 = read_tiff(input$ + '_EV_500_RefSB_b2' + part + '.tif',geotiff = geotiff)
		DN_band6 = read_tiff(input$ + '_EV_500_RefSB_b3' + part + '.tif',geotiff = geotiff)
		DN_band7 = read_tiff(input$ + '_EV_500_RefSB_b4' + part + '.tif',geotiff = geotiff)
		weight = [0.2183, 0.1349, 0.2838, 0.2536, 0.0645, 0.0327, 0.0123]
		Ref3 = MODIS_CALIBRATION(DN_band3, SMARK, 3, /REFLECTANCE) / muv
		Ref4 = MODIS_CALIBRATION(DN_band4, SMARK, 4, /REFLECTANCE) / muv
		Ref5 = MODIS_CALIBRATION(DN_band5, SMARK, 5, /REFLECTANCE) / muv
		Ref6 = MODIS_CALIBRATION(DN_band6, SMARK, 6, /REFLECTANCE) / muv
		Ref7 = MODIS_CALIBRATION(DN_band7, SMARK, 7, /REFLECTANCE) / muv
		value_albedo = Ref1 * weight[0] + Ref2 * weight[1] + Ref3 * weight[2] + Ref4 * weight[3] + Ref5 * weight[4] + Ref6 * weight[5] + Ref7 * weight[6]
;		indices = where(value_albedo eq 0)
;		if(indices[0] ne -1) then value_albedo[indices] = -1.
		write_tiff, output$ + '_albedo' + part + '.tif', value_albedo, geotiff = geotiff, /float
	endif
end


;计算经过大气校正的地表NDVI和ALBEDO
pro	ndvi_albedo_acr, input$, output$, part, ndvi = ndvi, albedo = albedo
	Ref1 = read_tiff(input$ + '_acr_b1' + part + '.tif', geotiff = geotiff)
	Ref2 = read_tiff(input$ + '_acr_b2' + part + '.tif', geotiff = geotiff)
	if(keyword_set(ndvi)) then begin
		value_ndvi = Ref1 * 0
		indices = where(Ref1 gt 0, count)
		if(count gt 0) then begin
			value_ndvi[indices] = (Ref2[indices] - Ref1[indices]) / (Ref2[indices] + Ref1[indices])
			value_ndvi[indices] = value_ndvi[indices] < 0.93
			value_ndvi[indices] = value_ndvi[indices] > (-0.2)
			value_ndvi[indices] = fix((value_ndvi[indices] + 1.) * 127, type = 1)
		endif

		indices = where((Ref1) eq 0 and (value_ndvi eq fix(0.8 * 127)), count)
		if(count gt 0) then value_ndvi[indices] = 0.

		write_tiff, output$ + '_ndvi_acr' + part + '.tif', value_ndvi, geotiff = geotiff
	endif
	if(keyword_set(albedo)) then begin
		Ref3 = read_tiff(input$ + '_acr_b3' + part + '.tif', geotiff = geotiff)
		Ref4 = read_tiff(input$ + '_acr_b4' + part + '.tif', geotiff = geotiff)
		Ref5 = read_tiff(input$ + '_acr_b5' + part + '.tif', geotiff = geotiff)
		Ref6 = read_tiff(input$ + '_acr_b6' + part + '.tif', geotiff = geotiff)
		Ref7 = read_tiff(input$ + '_acr_b7' + part + '.tif', geotiff = geotiff)
		weight = [0.2183, 0.1349, 0.2838, 0.2536, 0.0645, 0.0327, 0.0123]
		value_albedo = Ref1 * weight[0] + Ref2 * weight[1] + Ref3 * weight[2] + Ref4 * weight[3] + Ref5 * weight[4] + Ref6 * weight[5] + Ref7 * weight[6]
;		indices = where(value_albedo eq 0)
;		if(indices[0] ne -1) then value_albedo[indices] = -1.
		write_tiff, output$ + '_albedo_acr' + part + '.tif', value_albedo, geotiff = geotiff, /float
	endif
end









;计算地表LST，采用分裂窗算法
pro l_s_t, input$, output$, smark, part
	DN_band1 = read_tiff(input$ + '_EV_250_RefSB_b0' + part + '.tif',geotiff = geotiff)
	DN_band2 = read_tiff(input$ + '_EV_250_RefSB_b1' + part + '.tif',geotiff = geotiff)
	DN_band17 = read_tiff(input$ + '_EV_1KM_RefSB_b11' + part + '.tif',geotiff = geotiff)
	DN_band18 = read_tiff(input$ + '_EV_1KM_RefSB_b12' + part + '.tif',geotiff = geotiff)
	DN_band19 = read_tiff(input$ + '_EV_1KM_RefSB_b13' + part + '.tif',geotiff = geotiff)
	DN_solarzenith = read_tiff(input$ + '_SolarZenith' + part + '.tif',geotiff = geotiff)
	DN_band31 = read_tiff(input$ + '_EV_1KM_Emissive_b10' + part + '.tif',geotiff = geotiff)
	DN_band32 = read_tiff(input$ + '_EV_1KM_Emissive_b11' + part + '.tif',geotiff = geotiff)
	muv = cos(DN_solarzenith * 0.01 * !dtor)
	Ref1 = MODIS_CALIBRATION(DN_band1, SMARK, 1, /REFLECTANCE) / muv
	Ref2 = MODIS_CALIBRATION(DN_band2, SMARK, 2, /REFLECTANCE) / muv
	ndvi = Ref1 * 0.
	indices = where(DN_band1 gt 0,count)
	if(count gt 0) then begin
		ndvi(indices) = (Ref2(indices) - Ref1(indices)) / (Ref2(indices) + Ref1(indices))
	endif
	Ref17 = MODIS_CALIBRATION(DN_band17, SMARK, 19, /REFLECTANCE) / muv
	Ref18 = MODIS_CALIBRATION(DN_band18, SMARK, 20, /REFLECTANCE) / muv
	Ref19 = MODIS_CALIBRATION(DN_band19, SMARK, 21, /REFLECTANCE) / muv
	BT31 = MODIS_CALIBRATION(DN_band31, SMARK, 33, /BT)
	BT32 = MODIS_CALIBRATION(DN_band32, SMARK, 34, /BT)
	t17 = Ref17 / Ref2
	t18 = Ref18 / Ref2
	t19 = Ref19 / Ref2
	w17 = ((0.02 - alog(t17)) / 0.651) ^ 2.
	w18 = ((0.02 - alog(t18)) / 0.651) ^ 2.
	w19 = ((0.02 - alog(t19)) / 0.651) ^ 2.
	w = 0.192 * w17 + 0.453 * w18 + 0.355 * w19
	t31 = 2.89798 - 1.88366 * exp(-1 * (w / (-21.22704)))
	t32 = -3.59289 + 4.60414 * exp(w / (-32.70639))
	mark31 = ndvi
	mark31[*] = 0
	mark32 = mark31
	Pv = mark31
	Rv = mark31
	Rs = mark31
	indices = where(ndvi lt 0)
	if(indices[0] ne -1) then begin
		mark31[indices] = 0.99683
		mark32[indices] = 0.99254
	endif
	indices = where(ndvi ge 0)
	if(indices[0] ne -1) then begin
		Pv[indices] = (NDVI[indices] - 0.15) / (0.9 - 0.15)
		minPv = Pv < (1 - Pv)
		Rv[indices] = 0.92762 + 0.07033 * Pv[indices]
		Rs[indices] = 0.99782 + 0.08362 * Pv[indices]
		mark31[indices] = Pv[indices] * Rv[indices] * 0.98672 + (1 - Pv[indices]) * Rs[indices] * 0.96767 + 0.003796 * minPv
		mark32[indices] = Pv[indices] * Rv[indices] * 0.98990 + (1 - Pv[indices]) * Rs[indices] * 0.97790 + 0.003796 * minPv
	endif
	a31 = 0.13787 * mark31 * t31
	b31 = 0.13787 * BT31 + 31.65677 * t31 * mark31 - 31.65677
	c31 = (1 - t31) * (1 + (1 - mark31) * t31) * 0.13787
	d31 = (1 - t31)*(1 + (1 - mark31) * t31) * 31.65677
	a32 = 0.11849 * mark32 * t32
	b32 = 0.11849 * BT32 + 26.50036 * t32 * mark32 - 26.50036
	c32 = (1 - t32) * (1 + (1 - mark32) * t32) * 0.11849
	d32 = (1 - t32) * (1 + (1 - mark32) * t32) * 26.50036
	Ts = (c32 * (b31 + d31) - c31 * (d32 + b32)) / (c32 * a31 - c31 * a32)
;	indices = where(Ts eq 0)
;	if(indices[0] ne -1) then Ts[indices] = -1.
;	write_tiff, output$ + '_ts' + part + '.tif', BT31, geotiff = geotiff, /float
	write_tiff, output$ + '_lst' + part + '.tif', fix(Ts * 10), geotiff = geotiff, /short
end


;各种陆地产品的集成
PRO LAND_SURFACE_PRODUCT, dir, part, ndvi = ndvi, albedo = albedo, lst = lst, acr = acr
	input$ = dir.modis_pre + dir.date
	smark = dir.mark
	output$ = dir.modis_product + dir.date
	if(keyword_set(acr)) then begin
		if(keyword_set(ndvi)) then ndvi_albedo_acr, output$, output$, part, /ndvi
		if(keyword_set(albedo)) then ndvi_albedo_acr, output$, output$, part, /albedo
	endif else begin
		if(keyword_set(ndvi)) then ndvi_albedo, input$, output$, smark, part, /ndvi
		if(keyword_set(albedo)) then ndvi_albedo, input$, output$, smark, part, /albedo
		if(keyword_set(lst)) then l_s_t, input$, output$, smark, part
	endelse
END