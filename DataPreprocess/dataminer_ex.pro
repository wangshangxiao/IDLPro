PRO dataminer_ex, READALL = readall, QUERY = query

; check if dataminer is licensed
If (db_exists() eq 0) then message, 'ERROR: Database access not available'

; Create a new database object
oDB = Obj_New('IDLdbDatabase')

; Set the Use_Cursor_Lib property (Needed for MS Access)
oDB->SetProperty,/Use_Cursor_Lib

; Use dialogs to connect the object to a database
connect = Dialog_DBConnect(oDB)
if (not connect) then return

; get a list of the tables in the file
tables = oDB->GetTables()

tbl = where(stregex(tables.name,'gvalues', /fold_case) ne -1)
if (tbl[0] eq -1) then begin
	message,'ERROR: no tables matched'
endif

; Create a recordset object and connect to the table
oRS = obj_new('IDLdbRecordSet', oDB, table=tables[tbl[0]].name)

; Get the first and last records in the dataset to determine limits
; the map
a = oRS->MoveCursor(/First)
data = oRS->GetRecord()
elat = data.latitude
blon = data.longitude
a = oRS->MoveCursor(/Last)
data = oRS->GetRecord()
blat = data.latitude
elon = data.longitude

map_set, /CYL, LIMIT = [blat, blon, elat, elon], /GRID, /CONT
a = oRS->MoveCursor(/FIRST)

if (keyword_set(readall)) then begin
	eot = 0
	while not eot do begin
		xdata = fltarr(1000)
		ydata = fltarr(1000)
		npts = 0L
		for i = 0L, 999 do begin
			data = oRS->GetRecord()
			xdata[i]=data.longitude
			ydata[i]=data.latitude
			npts = npts+1
	    	if (not oRS->MoveCursor(/NEXT)) then begin
	    		eot=1
	    		goto, breakout
	    	endif
		endfor
		breakout:
		device, decompose = 1
		oplot, xdata[0:npts-1], ydata[0:npts-1], color = 'ff0000'xl, PSYM=3
	endwhile
endif

if (keyword_set(query)) then begin
	; find all the records where the height is below 0.0
	query1 = 'CREATE TABLE heightlt0 (latitude DECIMAL(8,2), longitude DECIMAL(8,2), height DECIMAL(8,2))'
	query2 = 'INSERT INTO heightlt0 SELECT latitude, longitude, height FROM gvalues WHERE height<0.0'
	oRS1 = obj_new('IDLdbRecordSet', oDB, SQL = query1)
	oDB->ExecuteSQL, query2
	oDB->ExecuteSQL, 'UPDATE'
	obj_destroy, oRS1
endif

Obj_Destroy, oRS
Obj_Destroy, oDB

End
