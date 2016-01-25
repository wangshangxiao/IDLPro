;�������ܣ���һ��һά������һ��һά��������

pro	caution, str
	OK = WIDGET_BASE(UNAME = 'TABLE_BASE', xoffset=300, yoffset=300, TITLE = 'INFORMATION!', /tlb_frame_attr, /BASE_ALIGN_CENTER)
	BTN = widget_button(ok, value = str, xsize = strlen(str) / 0.15, ysize = 50)
	WIDGET_CONTROL, OK, /REALIZE
	wait, 5
	if widget_info(ok, /valid) eq 1 then widget_control, ok, /destroy
end

;��tifת����hdr��ʽ������
pro tif2hdr, dir, mark, roi$
	roi = read_tiff(roi$)
	input$=dir.modis_product+dir.date

	for i = 0, n_elements(mark) - 1 do begin
		omark = mark[i]
		if(mark[i]) eq 'lst' then omark = 'TS'
		SAVE_FILE = dir.modis_product + dir.date + omark

		tif_file = input$ + mark[i] + '.tif'

		data = read_tiff(tif_file, geotiff = geotiff)
		a = size(data)
		case mark[i] of
			'ndvi': begin
						data = fix(data, type = 1)
						DataType = 1
					end
			'lst': begin
				  	  data = fix(data)
					  DataType = 2
				  end
			'TS': begin
				  	  data = fix(data)
					  DataType = 2
				  end
			'cld': begin
					   data = data
					   DataType = 1
				   end
			'vci': begin
					   data = data
					   DataType = 2
				   end
			'tci': begin
					   data = data
					   DataType = 2
				   end
		endcase
		enter=string(byte(13))+string(byte(10))     ;�س���ASCII��ֵ
        time=systime()                              ;����ʱ��
        col =STRTRIM(a[1],2)                     ;����Ӱ������
        line=STRTRIM(a[2],2)                       ;����Ӱ������
        DataType_   = STRTRIM(DataType,2)              ;��������
        sensortype_ = 'Terra'          				   ;����������
        tulx  = STRTRIM(geotiff.modeltiepointtag[3],2)                         ;���Ͻ�X����(ALBERS110)
        tuly  = STRTRIM(geotiff.modeltiepointtag[4],2)                         ;���Ͻ�Y����(ALBERS110)
		Resolution_   = STRTRIM(geotiff.modelpixelscaletag[0],2)			;���ش�С.Ҳ���ֱ���.
		CenterMedian_ = STRTRIM(fix(geotiff.projcenterlonggeokey),2)		;��110,����105
		IF CenterMedian_ EQ '110' THEN BEGIN
		   EastingFalse = '4000000'					;��ƫ���ٹ���.
		ENDIF ELSE EastingFalse = '0.0'

        SaveFileName = SAVE_FILE
        HeadFileName = SAVE_FILE+'.hdr'

        Bandname = STRMID(SaveFileName,STRPOS(SaveFileName,'\',/REVERSE_SEARCH)+1)  ;������·�����ļ���.

        HeadInfomation='ENVI'+enter+$
        'description = {' + enter+ $
        '  Create New File Time ['+time+']}'+enter+$
        'samples = '+col+enter+$
        'lines   = '+line+enter+$
        'bands   = 1'+enter+$
        'header offset = 0'+enter+$
        'file type = ENVI Standard'+enter+$
        'data type = '+DataType_+enter+$
        'interleave = bsq'+enter+$
        'sensor type = '+sensortype_+enter+$
        'byte order = 0'+enter+$
        'map info = {chiesealbers, 1.0000, 1.0000, ' +tulx+', ' +tuly+', '+Resolution_+', '+Resolution_+', Krasovsky, units=Meters}'+enter+$
        'projection info = {9, 6378245.0, 6356863.0, 0.000000, '+CenterMedian_+', '+EastingFalse+', 0.0, 25.000000, 47.000000, PCI (ACEA), units=Meters}'+enter+$
        'pixel size = {' + STRTRIM(geotiff.modelpixelscaletag[0],2) + ',' + STRTRIM(geotiff.modelpixelscaletag[1],2) + ',' + 'units=Meters}'
;        'wavelength units = Unknown'+enter+$
;        'band names = {'+Bandname+'}'+enter

         openw,lun,HeadFileName,/get_lun     ;ע��:���ļ��Ѵ�ʱ,���д���̲������κ���ʾ,�������ļ��滻��.��ͬ.,
         writeu,lun,HeadInfomation
         free_lun,lun

         openw,lun,SaveFileName,/get_lun
         writeu,lun,data
         free_lun,lun
         ;ɾ��tif�ļ�
         file_delete, tif_file
	endfor

end


pro m_d03_rename, dir
	m_d03$ = file_search(dir + 'geolocated*.hdf', count = count)
	if(count gt 0) then begin
		for i = 0, count - 1 do begin
			field = strsplit(file_basename(m_d03$[i]), '_', /extract)
			hkm$ = file_search(dir + 'HKM_' + field[1] + '_' + field[2] + '*.hdf', count = count1)
			if(count1 eq 1) then begin
				spawn, 'rename ' + m_d03$[i] + ' geolocated' + strmid(file_basename(hkm$), 3, strlen(file_basename(hkm$)) - 3)
			endif
		endfor
	endif
end

;��������Ԫ�ظ�����ȣ����Ҹ�Ԫ��һһ��Ӧ��ֻ��λ�ò�ͬ
function sort_array,array1,array2
	array_index=array1
	array_index[*]=0
	for i=0,n_elements(array1)-1 do begin
		array_index[i]=where(array2 eq array1[i])
	endfor
	return,array_index
end

;�Բ�ͬ����ŵ�Ӱ�����mosaick��Ȼ����roi�Ĵ�С����mosaick���Ӱ�����resize
pro mosaick_resize, dir, roi$, type
	;�Զ��ļ������������
	ok=query_tiff(roi$,info,geotiff=roi_geotiff)
	if ~ok then begin
		showmsg, '����ROI�ļ�����', /log
		return
	endif

	; mark���ڱ�ʶҪmosaick���ļ��ǹ��ײ����ļ����ǵ��������ļ�
	if type eq 'GEO' then begin
		;Ҫmosaick���ļ�Ϊ�����ļ�
		;ͬʱҪ��̫��/�۲��춥�ǣ�̫��/�۲ⷽλ�Ƕ�����MOSAICK
		markname=['SolarZenith','SolarAzimuth','SensorZenith','SensorAzimuth']
		for j=0,3 do begin
			output$ = dir.modis_pre + dir.date + '_' + markname[j]+'.tif'
			rex = dir.modis_pre + 'GEO_' + dir.date + '_*_' + markname[j] + '.tif'
			fname=file_search(rex)
			if fname[0] eq '' then begin
				showmsg, markname[j] + ' - ����MOSAICK�ķֲ����ļ�û���ҵ���', /log
				return
			endif

			n=n_elements(fname)

			temp=read_tiff(fname[0])
			temp[*]=0
			for i=0,n-1 do begin
				data=read_tiff(fname[i],geotiff=geotiff)
				indices=where(data ne 0)
				if(indices[0] ne -1) then begin
					temp(indices)=data(indices)
				endif
			endfor
			x0=geotiff.modeltiepointtag[3]
			y0=geotiff.modeltiepointtag[4]
			roix0=roi_geotiff.modeltiepointtag[3]
			roiy0=roi_geotiff.modeltiepointtag[4]
			startx=fix((roix0-x0)/dir.psize)
			starty=fix((y0-roiy0)/dir.psize)
			endx=startx+info.dimensions[0]-1
			endy=starty+info.dimensions[1]-1
			data=temp[startx:endx,starty:endy]
			write_tiff,output$,data,geotiff=roi_geotiff,/short
		endfor
	endif else begin
		;�����ļ�
		;ȷ������ļ�,��0Ϊ��
		files = file_search(dir.modis_pre + type + '_' + dir.date + '_*_*.tif')
		if files[0] eq '' then begin
			showmsg, '����(' + dir.date + ')��' + type + '�ļ������𻵣����ܴ���', /log
			return
		endif

		;������д���
		ntypes = n_elements(files)
		for i=0, ntypes-1 do begin
			;��ȡ�ļ����ͱ�ʶ(EV֮��Ĳ���)
			pos = strpos(files[i], 'EV_')
			tstr = strmid(files[i], pos)
			afiles = file_search(dir.modis_pre + type + '_*_' + tstr)
			nfiles = n_elements(afiles)
			;�������һ�����͵�����
			output$ = dir.MODIS_PRE + '\' + dir.date + '_' + tstr
			for j=0, nfiles-1 do begin
				if j eq 0 then begin
					temp = read_tiff(afiles[j], geotiff=geotiff)
					temp[*, *] = 0
				endif
				data = read_tiff(afiles[j])
				indices = where(data ne 0)
				if(indices[0] ne -1) then temp[indices] = data[indices]
			endfor
			;����
			x0=geotiff.modeltiepointtag[3]
			y0=geotiff.modeltiepointtag[4]
			roix0=roi_geotiff.modeltiepointtag[3]
			roiy0=roi_geotiff.modeltiepointtag[4]
			startx=fix((roix0-x0)/dir.psize)
			starty=fix((y0-roiy0)/dir.psize)
			endx=startx+info.dimensions[0]-1
			endy=starty+info.dimensions[1]-1
			data=temp[startx:endx,starty:endy]
			write_tiff,output$,data,geotiff=roi_geotiff, /short
		endfor
	endelse
end

;��С����ļ��ϲ���һ��
pro unit, dir, date, mark, roi$, acr = acr   ;mark:(cld,tao,wv,acr_b1,acr_b2,......)
	roi = read_tiff(roi$, geotiff = geotiff)
	data = fltarr(5300, 4800)
	deltax = 1060
	deltay = 960
	for j = 0, 4 do begin
		for k = 0, 4 do begin
			file$ = strcompress(dir + date + '_' + mark + '_' + string(j) + string(k) + '.tif', /remove_all)
			if(~file_test(file$)) then begin
				if(keyword_set(acr)) then begin
					file$ = strcompress(dir + date + '_' + mark + '_acr_' + string(j) + string(k) + '.tif', /remove_all)
				endif
			endif
			temp = read_tiff(file$)
			data[k * deltax: (k + 1) * deltax - 1, j * deltay: (j + 1) * deltay - 1] = temp
		endfor
	endfor
;	if(mark eq 'cld') then begin
;		indices = where(roi eq 0)
;		if(indices[0] ne -1) then begin
;			data[indices] = 1
;		endif
;	endif
	if(mark eq 'lst') then begin
		mark = 'TS'
	endif
	output$ = dir + date + mark + '.tif'
	type = size(temp, /type)
	case type of
		1: begin
		     write_tiff, output$, data, geotiff = geotiff
		   end
		2: begin
		     write_tiff, output$, data, geotiff = geotiff, /short
		   end
		3: begin
		     write_tiff, output$, data, geotiff = geotiff, /long
		   end
		4: begin
		     write_tiff, output$, data, geotiff = geotiff, /float
		   end
		12: begin
		     write_tiff, output$, data, geotiff = geotiff, /short
		    end
	endcase
end

;�и��ļ���С��
pro segment, dir
	file$ = file_search(dir.modis_pre + '*' + dir.date + '*.tif', count = n)
	if(n ne 0) then begin
		deltax = 1060
		deltay = 960
		for i = 0, n-1 do begin
			data = read_tiff(file$[i])
			type = size(data, /type)
			for j = 0, 4 do begin
				for k = 0, 4 do begin
					temp = data[k * deltax: (k + 1) * deltax - 1, j * deltay: (j + 1) * deltay - 1]
					output$ = dir.modis_pre + file_basename(file$[i], '.tif') + '_' + strtrim(j, 2) + strtrim(k, 2) + '.tif'
					case type of
						1: begin
						     write_tiff, output$, temp
						   end
						2: begin
						     write_tiff, output$, temp, /short
						   end
						4: begin
						     write_tiff, output$, temp, /float
						   end
						else: begin
						     write_tiff, output$, temp, /long
						   end
					endcase
				endfor
			endfor

		endfor
	endif
end


pro segfile_del, dir, date, mark
	for j = 0, 4 do begin
		for k = 0, 4 do begin
			file$ = strcompress(dir + date + '_' + mark + '_' + string(j) + string(k) + '.tif', /remove_all)
			if(file_test(file$)) then begin
				file_delete, file$
			endif
		endfor
	endfor
end

;������ͶӰ��ͷ�ļ�,��MRT����ʹ��
pro creat_prm,hdf$,m_d03$,output$,prm$,band,pixel_size,geo_infor,type
	case strlowcase(type) of
		'qkm':begin
				  SDS_NAME='INPUT_SDS_NAME=EV_250_RefSB,'+strtrim(string(band[0]),2)+','+strtrim(string(band[1]),2)
				  PSIZE='OUTPUT_PIXEL_SIZE='+string(pixel_size)
			  end
		'hkm':begin
			      SDS_NAME='INPUT_SDS_NAME=EV_500_RefSB,'+strtrim(string(band[2]),2)+','+strtrim(string(band[3]),2)+','+strtrim(string(band[4]),2)+','+strtrim(string(band[5]),2)+','+strtrim(string(band[6]),2)
			      PSIZE='OUTPUT_PIXEL_SIZE='+string(pixel_size)
    	   	  end
    	'1km':begin
				  if(band[27] eq 0) then begin
					  SDS_NAME='INPUT_SDS_NAME = EV_1KM_RefSB,'+ $
					  strtrim(string(band[7]),2)+','+strtrim(string(band[8]),2)+','+strtrim(string(band[9]),2)+','+strtrim(string(band[10]),2)+',' $
					  +strtrim(string(band[11]),2)+','+strtrim(string(band[12]),2)+','+strtrim(string(band[13]),2)+','+strtrim(string(band[14]),2)+',' $
					  +strtrim(string(band[15]),2)+','+strtrim(string(band[16]),2)+','+strtrim(string(band[17]),2)+','+strtrim(string(band[18]),2)+',' $
					  +strtrim(string(band[19]),2)+','+strtrim(string(band[20]),2)+';' $
					  +'EV_1KM_Emissive,'+strtrim(string(band[21]),2)+','+strtrim(string(band[22]),2)+','+strtrim(string(band[23]),2)+',' $
					  +strtrim(string(band[24]),2)+','+strtrim(string(band[25]),2)+','+strtrim(string(band[26]),2)+','+strtrim(string(band[28]),2)+',' $
					  +strtrim(string(band[29]),2)+','+strtrim(string(band[30]),2)+','+strtrim(string(band[31]),2)+','+strtrim(string(band[32]),2)+',' $
					  +strtrim(string(band[33]),2)+','+strtrim(string(band[34]),2)+','+strtrim(string(band[35]),2)+','+strtrim(string(band[36]),2)+',' $
					  +strtrim(string(band[37]),2)
				  endif else begin
					  SDS_NAME='INPUT_SDS_NAME = EV_1KM_RefSB,'+ $
					  strtrim(string(band[7]),2)+','+strtrim(string(band[8]),2)+','+strtrim(string(band[9]),2)+','+strtrim(string(band[10]),2)+',' $
					  +strtrim(string(band[11]),2)+','+strtrim(string(band[12]),2)+','+strtrim(string(band[13]),2)+','+strtrim(string(band[14]),2)+',' $
					  +strtrim(string(band[15]),2)+','+strtrim(string(band[16]),2)+','+strtrim(string(band[17]),2)+','+strtrim(string(band[18]),2)+',' $
					  +strtrim(string(band[19]),2)+','+strtrim(string(band[20]),2)+';' $
					  +'EV_1KM_Emissive,'+strtrim(string(band[21]),2)+','+strtrim(string(band[22]),2)+','+strtrim(string(band[23]),2)+',' $
					  +strtrim(string(band[24]),2)+','+strtrim(string(band[25]),2)+','+strtrim(string(band[26]),2)+','+strtrim(string(band[28]),2)+',' $
					  +strtrim(string(band[29]),2)+','+strtrim(string(band[30]),2)+','+strtrim(string(band[31]),2)+','+strtrim(string(band[32]),2)+',' $
					  +strtrim(string(band[33]),2)+','+strtrim(string(band[34]),2)+','+strtrim(string(band[35]),2)+','+strtrim(string(band[36]),2)+',' $
					  +strtrim(string(band[37]),2)+';'+'EV_Band26'
				  endelse
				  PSIZE='OUTPUT_PIXEL_SIZE='+string(pixel_size)
    		  end
    	else: begin;03
				  SDS_NAME='INPUT_SDS_NAME=SensorZenith; SensorAzimuth; SolarZenith; SolarAzimuth'
				  PSIZE='OUTPUT_PIXEL_SIZE='+string(pixel_size)
    	endelse
	endcase
	openw,lun,prm$,/get_lun
    printf,lun,'INPUT_FILENAME='+hdf$
    printf,lun,'GEOLOCATION_FILENAME='+m_d03$
    printf,lun,SDS_NAME
    printf,lun,'OUTPUT_SPATIAL_SUBSET_TYPE=LAT_LONG'
    printf,lun,'OUTPUT_SPACE_UPPER_LEFT_CORNER (LONG LAT) = '+strtrim(string(geo_infor.ullong),2)+' '+ strtrim(string(geo_infor.ullat),2)
    printf,lun,'OUTPUT_SPACE_LOWER_RIGHT_CORNER (LONG LAT) = '+strtrim(string(geo_infor.lrlong),2)+' '+ strtrim(string(geo_infor.lrlat),2)
    printf,lun,'OUTPUT_FILENAME='+output$
    printf,lun,'OUTPUT_FILE_FORMAT=GEOTIFF_FMT'
    printf,lun,'KERNEL_TYPE (CC/BI/NN)=BI'
    printf,lun,'OUTPUT_PROJECTION_NUMBER=ALBERS'
    printf,lun,'OUTPUT_PROJECTION_PARAMETER=0.0 0.0 25.0 47.0 110.0 0.0 4000000.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0'
    printf,lun,'OUTPUT_PROJECTION_SPHERE=15'
    printf,lun,PSIZE
    if lun ne -1l then free_lun,lun
end

;��ͶӰ
function reproject, dir, index, band, geo_infor, type
	type = strupcase(type)
	case type of
		'HKM' : hdf$ = dir.file_hkm[index]
		'QKM' : hdf$ = dir.file_qkm[index]
		'GEO' : hdf$ = dir.file_geo[index]
		'1KM' : hdf$ = dir.file_1km[index]
	endcase

    output$ = dir.modis_pre + type + '_' + dir.date + '_' + strtrim(index, 2)

    prm$ = output$ + '.prm'

	;ΪMRT����׼�������ű�
	creat_prm,hdf$,dir.file_geo[index],output$,prm$,band,dir.psize,geo_infor,type

	if file_test(prm$) then begin
		print,'MRT reprojecting:',hdf$
	    print,'prm fileanme: ',prm$
	    spawn,'cmd/E:ON/c "set MRTDATADIR='+dir.mrtswathdata_dir+'&&'+dir.mrtswath_dir+'swath2grid -pf='+prm$+'"',result,errresult
		if n_elements(result) le 1 then ans = '' else begin
           len=n_elements(result)
           ans=result[len-2]
        endelse
        if ans eq 'Finished processing!' then begin
        	print,'Status: done!'
        	return, 0
        endif else begin
        	print,'Status: ERROR!'
            print,result
            showmsg, 'Ԥ�������' + hdf$, /log
            return, 1
        endelse
	endif
end

;ModisԤ����������
pro MODIS_IRSA_AUTO, Day1B, AC_STATUS, OUTPUT,MRTSwath,MRTSwathData
	;ԭʼ����׼��
	PRINT, Day1B.date
	date = GetDateOfString(Day1B.date)
	year = date.year
	month = date.month
	day = date.day

	date_ul = getDateWithUL(year, month, day)

	;syspath = getsyspath()
	mrtswath=MRTSwath
	mrtswathdata=MRTSwathData
	if(file_test(mrtswath) eq 0) then begin
		print, '�����mrtswath·������ָ����ȷ��MRTSwath·����'
	endif
;	modis_pre='F:\drought\modis_pre\'
;	modis_product ='F:\drought\Data\modis_index\'
    MIDOUT=OUTPUT
    modis_pre=OUTPUT
    modis_product=OUTPUT
	dir={$
		mrtswath_dir:     mrtswath , $			;mrtswath\bin��·��
		mrtswathdata_dir: mrtswathdata, $	;MRTSWATH\DATA��·��
		modis_pre:        modis_pre, $				;���ң������Ԥ������
		modis_product:    modis_product, $		;MODIS��Ʒ(NDVI,CLD,TS)���·��
		;
		psize:1000, $
		mark : Day1B.modis_type, $

		file_1km : Day1B.list_1km, $
		file_hkm : Day1B.list_hkm, $
		file_qkm : Day1B.list_qkm, $
		file_geo : Day1B.list_geo,$
		date:Day1B.date,  $
		count:Day1B.Count $
		}

	geo_info={$
			   ullong: 70.0, $ 	;	   ullong: 70.0,$
			   ullat: 55.0, $	;	   ullat: 55.0,$
			   lrlong: 135.0, $	;	   lrlong: 135.0,$
			   lrlat: 10.0 $	;	   lrlat: 10.0 $
			  }
	band = [1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1,0,0,0,1,1,0,0,0,1,1,0,0,1,0]
	acr = 0	;����������0������1��

	roi$ = 'data\base\CHINA_1KM_ROI.TIF'
	mark = ['ndvi', 'TS', 'cld']

	;��һ����,������ν�����ͶӰ����
	typelist = ['HKM', '1KM', 'QKM', 'GEO']
	for i=0, dir.count - 1 do begin
		for ti =0,3 do begin
			;��ͶӰ
			re = reproject(dir, i, band, geo_info, typelist[ti])
			if re eq 1 then break
		endfor

		;�������ʧ��,��˹�����ȫ������,�Ѵ������������Ҳɾ��
		if ti ne 4 then begin
			showmsg, 'Modis������ݳ���:' + Day1B.date
			;ɾ���ֿ��TIF�ļ�
			deletename=file_search(dir.modis_pre + '???_' + dir.date + '_' + strtrim(i, 2) + '*.tif', count=n)
			if(n ne 0) then begin
				file_delete, deletename
			endif

			;ɾ��prm�ļ�
			deletename=file_search(dir.modis_pre+'*.prm',count=n)
			if(n ne 0) then begin
				file_delete, deletename
			endif
		endif
	endfor


	;������Ч������
	for ti=0, 3 do begin
		;ƴ�Ӳ����г���ROIͬ��С���ļ�
		mosaick_resize, dir, roi$, typelist[ti]

		;ɾ���ֹ����TIF�ļ�
		deletename=file_search(dir.modis_pre + typelist[ti] + '_' + dir.date + '*.tif', count=n)
		if(n ne 0) then begin
			file_delete, deletename
		endif
	endfor

	;ɾ��prm�ļ�
	deletename=file_search(dir.modis_pre + '*.prm',count=n)
	if(n ne 0) then begin
		file_delete, deletename
	endif

	showMsg, '--��ͶӰ���', /log

    RSDroughtIndexRun,AC_STATUS,MIDOUT
end
