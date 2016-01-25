pro Common_log,operation_text

	log_text=string(systime())+' '+operation_text
	log_file='text\common_log.txt'

	openw,lun,log_file,/get_lun,/append
	printf,lun,log_text
	free_lun,lun

end

