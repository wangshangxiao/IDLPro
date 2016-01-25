PRO BuildTire, points, conn, ddist
;
;build one wall
;
pts = FLTARR(3,28)
pts[*,0] = [2.00,1.50,0]
pts[*,1] = [1.60,1.50,0]
pts[*,2] = [1.40,1.42,0]
pts[*,3] = [1.33,1.27,0]
pts[*,4] = [1.28,1.08,0]
pts[*,5] = [1.30,0.85,0]
pts[*,6] = [1.36,0.65,0]
pts[*,7] = [1.37,0.55,0]
pts[*,8] = [1.51,0.55,0]
pts[*,9] = [1.51,0.68,0]
pts[*,10] = [1.45,0.87,0]
pts[*,11] = [1.42,1.10,0]
pts[*,12] = [1.50,1.25,0]
pts[*,13] = [1.62,1.35,0]
pts[*,14] = [2.00,1.35,0]
pts[0,*] = pts[0,*]-2.0
;
;mirror to the other side
;
for i=15,27 do begin
    pts[*,i] = pts[*,28-i]
    pts[0,i] = -pts[0,i]
    end
pts[1,*] = 0.75 + pts[1,*]
;
;rotate the cross section
;
n = 24.0
inc = 360.0/n
ang = 0.0
points = FLTARR(3,28*n)
ddist = FLTARR(28*n)
defl = ddist
for i=0,n-1 do begin
    ca = COS(ang*(!PI/180.0))
    sa = SIN(ang*(!PI/180.0))
    for j=0,27 do begin
        pt = [0.0,0.0,0.0]
        pt[0] = pts[0,j]
        pt[1] = pts[1,j]*ca + pts[2,j]*sa
        pt[2] = pts[1,j]*sa + pts[2,j]*ca
        points[*,i*28+j] = pt
        ddist[i*28+j] = SQRT(TOTAL(pt^2))
        end
    ang = ang + inc
    end
;
;create the connectivity array using quads
;
conn = LONARR(28L*n*5)
k = 0
for i=0,n-1 do begin
    s1 = i*28
    s2 = s1 + 28
    if (i EQ n-1) then s2 = 0
    for j=0,27 do begin
        l = j + 1
        if (j EQ 27) then l = 0
        conn[k] = 4
        conn[k+1] = j+s2
        conn[k+2] = j+s1
        conn[k+3] = l+s1
        conn[k+4] = l+s2
        k = k + 5
        end
    end
end
