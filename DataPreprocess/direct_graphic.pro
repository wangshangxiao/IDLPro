pro direct_graphic

size=40
peak=shift(dist(size),size/2+5.,size/2-5.)
peak=exp(-(peak/15)^2)
shade_surf,peak,background=255,color=100
;wait,3
;shade_surf,peak,ax=210,background=255,color=100

end
