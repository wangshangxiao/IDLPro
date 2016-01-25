function WaterLand, Ref1, Ref2

	;Rband2/Rband1

	r=Ref2/Ref1
	indices0=where(r le 0.75)
	indices1=where(r gt 0.75)
	if(indices0[0] ne -1) then r(indices0)=0
	if(indices1[0] ne -1) then r(indices1)=1
	Water_Land=r
	return, Water_Land
end



function cloud1,BT35,BT27,BT31,Ref26,DEM

	;BTband35
	F1=BT35
	F1[*]=1
	indices=where(BT35 ge 228)
	if(indices[0] ne -1) then F1(indices)=1
	indices=where(BT35 ge 226 and BT35 lt 228)
	if(indices[0] ne -1) then F1(indices)=0.95
	indices=where(BT35 gt 224 and BT35 lt 226)
	if(indices[0] ne -1) then F1(indices)=0.66
	indices=where(BT35 le 224)
	if(indices[0] ne -1) then F1(indices)=0

	;BTband27
	F2=BT27
	F2[*]=1
	indices=where(BT27 ge 225)
	if(indices[0] ne -1) then F2(indices)=1
	indices=where(BT27 ge 220 and BT27 lt 225)
	if(indices[0] ne -1) then F2(indices)=0.95
	indices=where(BT27 gt 215 and BT27 lt 220)
	if(indices[0] ne -1) then F2(indices)=0.66
	indices=where(BT27 le 215)
	if(indices[0] ne -1) then F2(indices)=0

	;BTband31
	F3=BT31
	F3[*]=1
	indices=where(DEM gt 0 and DEM lt 20)
	if(indices[0] ne -1) then begin
		n=n_elements(indices)
		for i=0L,n-1 do begin
			if(BT31(indices[i]) gt 297.5 and BT31(indices[i]) lt 302.5) then F3(indices[i])=0.95
			if(BT31(indices[i]) le 297.5) then F3(indices[i])=0.66
		endfor
	endif

	;Rband26
	F4=Ref26
	F4[*]=1
	indices=where(Ref26 le 0.03)
	if(indices[0] ne -1) then F4[indices]=1
	indices=where(Ref26 le 0.035 and Ref26 gt 0.03)
	if(indices[0] ne -1) then F4[indices]=0.95
	indices=where(Ref26 lt 0.04 and Ref26 gt 0.035)
	if(indices[0] ne -1) then F4[indices]=0.66
	indices=where(Ref26 ge 0.04)
	if(indices[0] ne -1) then F4[indices]=0

	Fhigh=F1<F2
	Fhigh=Fhigh<F3
	Fhigh=Fhigh<F4

	return,Fhigh
end





function cloud2, Ref1,joy

	;Rband1
	F1=Ref1
	F1[*]=1
	if(joy ge 152 and joy le 275) then begin
		indices=where(Ref1 le 0.14)
		if(indices[0] ne -1) then F1[indices]=1
		indices=where(Ref1 le 0.18 and Ref1 gt 0.14)
		if(indices[0] ne -1) then F1[indices]=0.95
		indices=where(Ref1 lt 0.22 and Ref1 gt 0.18)
		if(indices[0] ne -1) then F1[indices]=0.66
		indices=where(Ref1 ge 0.22)
		if(indices[0] ne -1) then F1[indices]=0
	endif

	Fmid=F1

	return, Fmid
end






function cloud3,BT31,BT22,Water_Land

	;BTband31-BTband22
	BT=BT31-BT22
	Flow=BT
	Flow[*]=1

	BT_land=BT*Water_land
	BT_water=BT*(~Water_Land)

	indices=where(BT_water ge -6 and BT_water lt 0)
	if(indices[0] ne -1) then Flow(indices)=1
	indices=where(BT_water ge -8 and BT_water lt -6)
	if(indices[0] ne -1) then Flow(indices)=0.95
	indices=where(BT_water gt -10 and BT_water lt -8)
	if(indices[0] ne -1) then Flow(indices)=0.66
	indices=where(BT_water le -10)
	if(indices[0] ne -1) then Flow(indices)=0

	indices=where(BT_land ge -10 and BT_land lt 0)
	if(indices[0] ne -1) then Flow(indices)=1
	indices=where(BT_land ge -12 and BT_land lt -10)
	if(indices[0] ne -1) then Flow(indices)=0.95
	indices=where(BT_land gt -14 and BT_land lt -12)
	if(indices[0] ne -1) then Flow(indices)=0.66
	indices=where(BT_land le -14)
	if(indices[0] ne -1) then Flow(indices)=0

	return,Flow
end



function clear_repair1,Q,Ref4,Ref5,BT20,BT22,BT31
	;Rband5/Rband4>2.0   BTband20-BTband22<11   BTband20-BTband31<15

	R = Ref5 / Ref4
	deltaBT1 = BT20 - BT22
	deltaBT2 = BT20 - BT31

	indices=where(Q lt 0.95)
	if(indices[0] ne -1) then begin
		n=n_elements(indices)
		for i=0L,n-1 do begin
			if(R(indices[i]) gt 2.0) then begin
				if(deltaBT1(indices[i]) lt 11) then begin
					if(deltaBT2(indices[i]) lt 15) then begin
						Q(indices[i])=1
					endif
				endif
			endif
		endfor
	endif
	return, Q
end

function clear_repair2,Fhigh,NDVI,Q
	indices=where(Fhigh ne 1)
	if(indices[0] ne -1) then begin
		Fhigh(indices)=0
		NDVI=NDVI*Fhigh
		indices1=where(NDVI gt 0.40 or NDVI lt -0.18)
		if(indices1[0] ne -1) then begin
			Q(indices1)=1
		endif
	endif
	return,Q
end

;云检测功能
PRO MODIS_CLOUD_MASK, dir, part, dem$
	if(~file_test(dem$)) then begin
		print, '没有DEM文件(' + dem$ + '),不能进行云检测！'
		return
	endif else begin
		dem=read_tiff(dem$,geotiff=geotiff)
	endelse

	input$=dir.modis_pre+dir.date
	smark=dir.mark
	joy = GetDateOfString(dir.date, /jul)
;	joy=fix(strmid(field[1],5,3))
	output$=dir.modis_product+dir.date
	DN_band1=read_tiff(input$+'_EV_250_RefSB_b0' + part + '.tif',geotiff=geotiff)
	DN_band2=read_tiff(input$+'_EV_250_RefSB_b1' + part + '.tif',geotiff=geotiff)
	DN_band4=read_tiff(input$+'_EV_500_RefSB_b1' + part + '.tif',geotiff=geotiff)
	DN_band5=read_tiff(input$+'_EV_500_RefSB_b2' + part + '.tif',geotiff=geotiff)
	DN_solarzenith=read_tiff(input$+'_SolarZenith' + part + '.tif',geotiff=geotiff)
	muv=cos(DN_solarzenith*0.01*!dtor)
	DN_band26=read_tiff(input$+'_EV_Band26' + part + '.tif',geotiff=geotiff)
	DN_band20=read_tiff(input$+'_EV_1KM_Emissive_b0' + part + '.tif',geotiff=geotiff)
	DN_band22=read_tiff(input$+'_EV_1KM_Emissive_b2' + part + '.tif',geotiff=geotiff)
	DN_band27=read_tiff(input$+'_EV_1KM_Emissive_b6' + part + '.tif',geotiff=geotiff)
	DN_band31=read_tiff(input$+'_EV_1KM_Emissive_b10' + part + '.tif',geotiff=geotiff)
	DN_band35=read_tiff(input$+'_EV_1KM_Emissive_b14' + part + '.tif',geotiff=geotiff)
	Ref1 = MODIS_CALIBRATION(DN_band1,SMARK,1,/REFLECTANCE)/muv
	Ref2 = MODIS_CALIBRATION(DN_band2,SMARK,2,/REFLECTANCE)/muv
	ndvi = Ref1 * 0
	indices=where(DN_band1 gt 0,count)
	if(count gt 0) then begin
		ndvi[indices] = (Ref2[indices] - Ref1[indices]) / (Ref2[indices] + Ref1[indices])
	endif
	BT35 = MODIS_CALIBRATION(DN_band35,SMARK,37,/BT)
	BT27 = MODIS_CALIBRATION(DN_band27,SMARK,29,/BT)
	BT31 = MODIS_CALIBRATION(DN_band31,SMARK,33,/BT)
	Ref26 = MODIS_CALIBRATION(DN_band26,SMARK,28,/REFLECTANCE)/muv
	BT22 = MODIS_CALIBRATION(DN_band22,SMARK,24,/BT)
	BT22_c3 = MODIS_CALIBRATION(DN_band5,SMARK,24,/BT)
	BT20 = MODIS_CALIBRATION(DN_band20,SMARK,22,/BT)
	Ref4 = MODIS_CALIBRATION(DN_band4,SMARK,4,/REFLECTANCE)/muv
	Ref5 = MODIS_CALIBRATION(DN_band5,SMARK,5,/REFLECTANCE)/muv
	Water_Land=WaterLand(Ref1,Ref2)
	F1 = cloud1(BT35,BT27,BT31,Ref26,dem)
	F2 = cloud2(Ref1,joy)
	F3 = cloud3(BT31,BT22_c3,Water_Land)
	if(Joy ge 152 and Joy le 275) then begin
		Q=(F1*F2*F3)^(1./3)
	endif else begin
		Q=(F1*F3)^(1./2)
	endelse
	Q = clear_repair1(Q,Ref4,Ref5,BT20,BT22,BT31)
	Q = clear_repair2(F1,ndvi,Q)
	indices1 = where(Q lt 1 and Q gt 0.66)
	indices2 = where(Q le 0.66 and Q gt 0)
	if(indices1[0] ne -1) then Q(indices1) = 1
	if(indices2[0] ne -1) then Q(indices2) = 0
	indices = where(DN_band1 eq 0)
	if(indices[0] ne -1) then begin
		Q[indices] = 0
	endif
	write_tiff, output$ + '_cld' + part + '.tif', ~Q, geotiff = geotiff
END