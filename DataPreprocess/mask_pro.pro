; MASK_PRO.PRO - written by Jennifer Kay (jenkay@u.washington.edu)
; This file applies the same mask to a series of images.

; 1. OPEN IDL (without ENVI)
; 2. Initiate Batch Mode in ENVI (Note: You have to do this every time!)
; IDL> envi, /restore_base_save_files
; 3. Change path names - edit program
; 4. Compile your code by typing IDL> .compile C:/data/modis/idl/mask_pro
; 5. type 'mask_pro' at the command prompt

pro mask_pro

mask2=bytarr(120,120)

openr,1,'C:\data\compare\ssmi\ssmi_mask2'
readu,1,mask2
close,1

temp=intarr(120,120)
temp(*,*)=1
mask=mask2*temp


file_name=FindFile('C:\data\compare\ssmi\sss_*composite',count=numfiles)

FOR j=0,numfiles-1 DO BEGIN
data=intarr(120,120)
openr,1,file_name(j)
readu,1,data
close,1

output=intarr(120,120)

FOR zzz=0,120-1 DO BEGIN ; columns
FOR zz=0,120-1 DO BEGIN ; rows

IF (mask(zzz,zz) eq 255)  THEN $
output(zzz,zz)=255 else $
output(zzz,zz)=data(zzz,zz)

ENDFOR ; block columns loop (zzz)
ENDFOR ; block rows loop (zz)

openw,1,file_name(j) + '_masked'
writeu,1,output
close,1

ENDFOR; file loop

end