pro widget1  ;������
   	base=widget_base(/column)
    button1=widget_button(base,value='��',uvalue='op')
    draw=widget_draw(base,uvalue='dra',xsize=800,ysize=600);, $
    ;x_scroll_size=1000,y_scroll_size=1000)
    button2=widget_button(base,value='�˳�',uvalue='ex')

    widget_control,base,/realize
    xmanager,'widget1',base;,event='widget1_event'
end

pro widget1_event, ev ;�¼��������
widget_control,ev.id, get_uvalue=uv
case uv of
'op':begin
	shade_surf,dist(100)
end
'ex':begin
	widget_control,ev.top,/destroy
	end
endcase
end
