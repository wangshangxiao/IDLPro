pro widget3

tlb=widget_base(title='My first Widget',xoffset=200,yoffset=100,/column)
base1=widget_base(tlb,/row)


label=widget_label(base1,value='Name: ',/dynamic_resize)
text1=widget_text(base1,xsize=15,ysize=1,/edit, $
	event_pro='widget3_text')
draw=widget_draw(tlb,xsize=300,ysize=300)
xslider=widget_slider(tlb,min=-90,max=90,value=30,	$
		title='X Rotation',event_pro='widget3_slider')
zslider=widget_slider(tlb,min=-180,max=180,value=30,	$
		title='z Rotation',event_pro='widget3_slider')


button1=widget_button(tlb,value='Exit',event_pro='widget3_exit')

widget_control,tlb,/realize
wait,3
widget_control,label,set_value='IDL Command: '
widget_control,draw,get_value=draw_index
help,draw_index
wset,draw_index
data=dist(40)
shade_surf,data

state={xslider:xslider, zslider:zslider, data:data}
widget_control,tlb,set_uvalue=state
xmanager,'widget3',tlb

end

pro widget3_exit,event

widget_control,event.top,/destroy
print,'-Exiting widget3'

end

pro widget3_text,event
widget_control,event.id,get_value=temp

a=execute(temp[0])
end

pro widget3_slider,event

widget_control,	event.top, get_uvalue=state1
widget_control,	state1.xslider, get_value=x
widget_control,	state1.zslider, get_value=z
shade_surf,state1.data, ax=x, az=z
widget_control, event.top, set_uvalue=state1

end