pro example_envi_select

compile_opt IDL2

envi, /restore_base_save_files 

envi_batch_init, log_file='batch.txt'


name = envi_pickfile(title='Pick a text file', $ 

   filter='*.img') 

if (name eq '') then return 

print, 'The selected filename is: ', name


ENVI_OPEN_FILE, name, R_FID=fid 

IF (fid EQ -1) THEN RETURN 

ENVI_FILE_QUERY, fid, DIMS=dims, NB=nb



;envi_open_file,file,r_fid=fid
ENVI_SELECT, fid=fid,dims=dims,pos=pos
if (fid[0] eq -1) then return


; Exit ENVI Classic

envi_batch_exit 

end