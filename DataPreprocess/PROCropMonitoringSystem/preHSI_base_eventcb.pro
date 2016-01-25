;
; IDL Event Callback Procedures
; preHSI_base_eventcb
;
; Generated on:	11/17/2009 15:20.59
;
;
; Empty stub procedure used for autoloading.
;
pro preHSI_base_eventcb
end

pro img_browse, Event

	PICK_FILE,Event

end

pro calculate_hsi, Event

   group_id = event.top
	widget_control, group_id, get_uvalue=winfo
	widget_control, winfo.input1, get_value = inputfile
	widget_control, winfo.output1, get_value = outputfile

	if file_test(inputfile) eq 0 then begin
		Result = DIALOG_MESSAGE('HSI数据文件不存在，无法计算！' + inputfile, /CENTER)
		return
	end

	if query_tiff(inputfile) eq 0 then begin
		Result = DIALOG_MESSAGE('HSI数据文件格式错误！', /CENTER)
		return
	end

	if strcompress(file_basename(outputfile, '.tif'), /REMOVE_ALL) eq '' then begin
		Result = DIALOG_MESSAGE('输出文件名称不正确！' + outputfile, /CENTER)
		return
	end

	tempfile=strsplit(outputfile,'.',/extract)
	outputfile=tempfile[0]+'.tif'

	common_log,'开始计算'

	preprocessing_hsi,inputfile,outputfile

end

pro calculate_hyp, Event

   	group_id = event.top
	widget_control, group_id, get_uvalue=winfo
	widget_control, winfo.input2, get_value = inputfile
	widget_control, winfo.output2, get_value = outputfile

	if file_test(inputfile) eq 0 then begin
		Result = DIALOG_MESSAGE('Hyperion数据所在文件夹未指定!', /CENTER)
		return
	end

	if file_test(outputfile) eq 0 then begin
		Result = DIALOG_MESSAGE('处理结果输出文件夹未指定!', /CENTER)
		return
	end

	inputfile=common_filepath(inputfile)
	outputfile=common_filepath(outputfile)

	common_log,'GST值设为'+strtrim(string(winfo.WID_SLIDER_GST),2)
	common_log,'BFNP值设为'+strtrim(string(winfo.WID_SLIDER_BFNP),2)

	GST=float(winfo.WID_SLIDER_GST)/10.0
	BFNP=float(winfo.WID_SLIDER_BFNP)/100.0

	common_log,'开始处理hyperion数据'

	preprocessing_hyp,inputfile,outputfile,GST,BFNP

end

pro EXIT_PROGRAM,Event

	widget_control, event.top, /destroy

end