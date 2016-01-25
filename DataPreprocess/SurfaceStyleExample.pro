PRO SurfaceStyleExample

a = findgen(40)
a = sin(a/5) / exp(a/50)
wave = a # a

gp = {bottom:'Blue', color:'Brown', shading:1, style:2}
shade_surf,wave
;surfstyle = live_style('surface', GRAPHIC_PROPERTIES = gp)
live_surface, wave, STYLE = surfstyle
;surface,wave

END