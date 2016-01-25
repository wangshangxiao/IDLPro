;
pro myObject::show
  void=dialog_message(self.input,/information)
end

;
function myObject::init,in
  ;
  self.input=in
  return,1
end

;
pro myObject__DEFINE
  ;
  str = { myObject,input:'' }
end

pro testmyobject
  s=obj_new('myObject','abc')
  s.show
end