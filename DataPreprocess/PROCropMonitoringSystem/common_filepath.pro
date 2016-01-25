Function common_filepath,filepath

	pos1=strpos(filepath,'\',/reverse_search)
	pos2=strlen(filepath)-1
	if pos1 eq pos2 then begin
		return,filepath
	endif else begin
		return,filepath+'\'
	endelse
end