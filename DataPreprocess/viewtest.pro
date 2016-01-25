pro viewtest,DrawID
widget_control,DrawID,get_value=state,/no_copy
;
state.oWindow->DisplayImage,state.oView

end

pro DisplayImage,oView
  ;oView = Obj_new('IDLgrView',viewPlane_rect=[0,0,800,600])
  oModel=Obj_new('IDLgrModel')
  oImg=Obj_new('IDLgrImage')
  ;
  oModel.ADD,oImg
  oView.ADD,oModel
  ;
  read_jpeg,self.file,data
  oImg.Setproperty,data=data
  ;
  self.oWin.draw,oView
end