pro drawexapmle
  wTlb = Widget_base()
  wDraw=Widget_Draw(wtlb,xsize=800,ysize=600,graphics_level=2)
  widget_control,wtlb,/realize
  objDraw =Obj_new('DisplayImage',wDraw)
  objDraw.openFile
  objDraw.Display
end

pro DisplayImage::Display
  oView = Obj_new('IDLgrView',viewPlane_rect=[0,0,800,600])
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


pro DisplayImage::openFile
  filter = ['*.tif','*.jpg','*.bmp']
  title = '´ò¿ªÓ°Ïñ'
  self.file=DIALOG_PICKFILE(title=title, filter=filter);
end

Function DisplayImage::init,wDraw
  widget_control,wDraw,get_value=oWin
  self.oWin=oWin
  return,1
End

pro DisplayImage__define
  strl={DisplayImage,oWin:Obj_new(),file:''}
end