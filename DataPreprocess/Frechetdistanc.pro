FUNCTION Calfrechetdistanc,p1,n,p2,m
  tempdis=Make_array(m,n,value=-1.0)
  distance=Cal(p1,p2,tempdis,m-1,n-1)
  Return,distance
END

FUNCTION Cal,p1,p2,tempdis,i,j
  IF(tempdis[i,j] GT -1.0) THEN BEGIN
    Return,tempdis[i,j]
  ENDIF ELSE IF(i EQ 0 AND j EQ 0) THEN BEGIN
    tempdis[i,j] = Dis(p1,p2,0,0)
  ENDIF ELSE IF(i GT 0 AND j EQ 0) THEN BEGIN
    tempdis[i,j] = Max([Cal(p1,p2,tempdis,i-1,0),Dis(p1,p2,i,0)])
  ENDIF ELSE IF(i EQ 0 AND j GT 0) THEN BEGIN
    tempdis[i,j] = Max([Cal(p1,p2,tempdis,0,j-1),Dis(p1,p2,0,j)])
  ENDIF ELSE IF(i GT 0 AND j GT 0) THEN BEGIN
    tempdis[i,j] = Max([Min([Cal(p1,p2,tempdis,i-1,j),Cal(p1,p2,tempdis,i-1,j-1),Cal(p1,p2,tempdis,i,j-1)]),Dis(p1,p2,i,j)])
  ENDIF ELSE BEGIN
    tempdis[i,j] = 9999999
  ENDELSE
  Return,tempdis[i,j]
END

FUNCTION Dis,p1,p2,i,j
  Return,Sqrt((p1[0,j]-p2[0,i])^2 + (p1[1,j]-p2[1,i])^2)
END

pro Frechetdistanc
  ;test data
data1= [[1.0,2.0],[2.0,3.0],[3.0,4.0],[4.0,5.0],[5.0,6.0]]
data2= [[1.0,-2.0],[2.0,-3.0],[3.0,-4.0],[4.0,-5.0],[5.0,-6.0],[6.0,-7.0],[7.0,-8.0]]
;  
n=5
m=7
  distance=Double(Calfrechetdistanc(data1,n,data2,m))
  print,distance
END
