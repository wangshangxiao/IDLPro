; mosaic_maker_jg v 0.1
;
;	This is designed to bypass the ENVI Mosaicking GUI which can have problems (or is just very slow)
;	with very large mosaic datasets.  Simply compile (or put in your save_add directory),
;	type into the ENVI command line:

;		mosaic_maker_jg[,BACKGROUND=value,RASTERFILENAMES=string_array,OUTPUT=string]

;	BACKGROUND=desired background value (default is 0).
;	RASTERFILENAMES=string array of filenames, e.g. rasterfilenames=['file1.img','file2.img'].
;		Not using "RASTERFILENAMES" will bring up a file selection dialog (which is probably preferred).
;	OUTPUT=string path to desired output mosaic file, e.g. output='/pathtomosaic/mosaicname.img'
;		Not using "OUTPUT" will bring up a file selection dialog.

;   mosaic_maker_jg is Copyright (C) 2008 Jonathan A. Greenberg

;   This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.

;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.

;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.


; GEOREF_MOSAIC_SETUP is copyright ITT Visual Information Solutions
; From ENVI Tech Tip #3336
;	http://www.ittvis.com/services/techtip.asp?ttid=3336

; program to estimate mosaic parameters

pro georef_mosaic_setup, fids=fids, dims=dims, out_ps=out_ps, $
  xsize=xsize, ysize=ysize, x0=x0, y0=y0, map_info=map_info
compile_opt strictarr, hidden

; some basic error checking
;
if keyword_set(dims) then $
  if n_elements(fids) ne n_elements(dims[0,*]) then dims=0
;
if n_elements(fids) lt 2 then begin
  xsize = -1
  ysize = -1
  x0 = -1
  y0 = -1
  return
endif

; if no DIMS passed in
;
nfiles = n_elements(fids)
if (keyword_set(dims) eq 0) then begin
  dims = fltarr(5, nfiles)
  for i=0, nfiles-1 do begin
    envi_file_query, fids[i], ns=ns, nl=nl
    dims[*,i] = [-1L, 0, ns-1, 0, nl-1]
  endfor
endif

; - compute the size of the output mosaic (xsize and ysize)
; - store the map coords of the UL corner of each image since you'll need it later
;
UL_corners_X = dblarr(nfiles)
UL_corners_Y = dblarr(nfiles)
east = -1e34
west = 1e34
north = -1e34
south = 1e34
for i=0,nfiles-1 do begin
  pts = [ [dims[1,i], dims[3,i]],   $ 	; UL
          [dims[2,i], dims[3,i]],   $	; UR
          [dims[1,i], dims[4,i]],   $	; LL
          [dims[2,i], dims[4,i]] ]		; LR
  envi_convert_file_coordinates, fids[i], pts[0,*], pts[1,*], xmap, ymap, /to_map
  UL_corners_X[i] = xmap[0]
  UL_corners_Y[i] = ymap[0]
  east  = east > max(xmap)
  west = west < min(xmap)
  north = north > max(ymap)
  south = south < min(ymap)
endfor
xsize = east - west
ysize = north - south
xsize_pix = round( xsize/out_ps[0] )
ysize_pix = round( ysize/out_ps[1] )

; to make things easy, create a temp image that's got a header
; that's the same as the output mosaic image
;
proj = envi_get_projection(fid=fids[0])
map_info = envi_map_info_create(proj=proj, mc=[0,0,west,north], ps=out_ps)
temp = bytarr(10,10)
envi_enter_data, temp, map_info=map_info, /no_realize, r_fid=tmp_fid

; find the x and y offsets for the images
;
x0 = lonarr(nfiles)
y0 = lonarr(nfiles)
for i=0,nfiles-1 do begin
  envi_convert_file_coordinates, tmp_fid, xpix, ypix, UL_corners_X[i], UL_corners_Y[i]
  x0[i] = xpix
  y0[i] = ypix
endfor

; delete the tmp file

envi_file_mng, id=tmp_fid, /remove, /no_warning

end


pro mosaic_maker_jg,pos=pos,background=background,rasterfilenames=rasterfilenames,output=output

	if n_elements(background) eq 0 then background=0

	if n_elements(rasterfilenames) eq 0 then begin
		rasterfilenames=envi_pickfile(title='Please select input raster(s):',/multiple_files)
	    if rasterfilenames[0] eq '' then return
	endif
	numfiles=n_elements(rasterfilenames)
	rasterfids=lonarr(numfiles)

	if n_elements(output) eq 0 then begin
	    output=envi_pickfile(title='Output Mosaick Filename:')
	    if output eq '' then return
	endif

	envi_open_file, rasterfilenames[0], r_fid=tempfid
   	rasterfids[0]=tempfid
   	envi_file_query,tempfid,nb=nb,ns=tempns,nl=tempnl,data_type=data_type
	map_info = envi_get_map_info(fid=tempfid)
	out_ps=map_info.ps[0:1]

   	if n_elements(pos) eq 0 OR n_elements(pos) gt nb then pos=lindgen(nb)
	posarr=lonarr(n_elements(pos),numfiles)
	for i=0,numfiles-1 do posarr[*,i]=pos

	dimsarr=lonarr(5,numfiles)
	dimsarr[*,0]=[-1,0, tempns-1,0, tempnl-1]

	use_see_through = lonarr(numfiles)

	for i=1,numfiles-1 do begin
		envi_open_file, rasterfilenames[i], r_fid=tempfid
	   	rasterfids[i]=tempfid
	   	envi_file_query,tempfid,nb=nb,ns=tempns,nl=tempnl
	   	dimsarr[*,i]=[-1,0, tempns-1,0, tempnl-1]
	endfor

	georef_mosaic_setup, fids=rasterfids, out_ps=out_ps, dims=dimsarr, xsize=xsize, ysize=ysize,$
		x0=x0, y0=y0, map_info=map_info

  envi_doit, 'mosaic_doit', fid=rasterfids, pos=posarr, $
    dims=dimsarr, out_name=output, xsize=xsize, $
    ysize=ysize, x0=x0, y0=y0, georef=1, $
    out_dt=data_type, pixel_size=out_ps, $
    background=background,use_see_through=use_see_through,$
    map_info=map_info

end