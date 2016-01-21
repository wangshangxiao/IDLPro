;-------
;员工父类
;-------
Pro Employee::StartWork
;
print,self._name+'开始工作:--- '
End
;
;员工类初始化(构建)
;
Function Employee::Init,name

    if n_elements(name) ne 1 then name='*'
    self._name=name
return,1
End
;
;员工类定义
;
Pro Employee__define
Var = { Employee     ,$
             _name   : ''   $ 
   }
End
;-------
;经理类
;-------
Pro Manager::StartWork
;
self->Employee::StartWork ;调用父类地方
print,self._name+'给员工下达任务'
End
;
;经理类定义
;
Pro Manager__define
    Var = { Manager     ,$
    Inherits   Employee   $     ;继承父类
          }

End
;-------
;秘书类
;-------
Pro Secretary::StartWork
;
self->Employee::StartWork   ;调用父类方法
print,self._name+'协助经理'
End
;
;秘书类定义
;
Pro Secretary__define
    Var = { Secretary     ,$
    Inherits   Employee   $      ;继承父类
          }

End
;-------
;销售类
;-------
Pro Seller::StartWork
;
self->Employee::StartWork            ;调用父类方法
print,self._name+'销售产品'
End
;
;销售类定义
;
Pro Seller__define
    Var = { Seller     ,$
    Inherits   Employee   $      ;继承父类
          }

End
;-------
;主函数入口
;-------
Pro TestPolymorphism
;
;初始化员工对象
;
Employee = objarr(4)
Employee[0] = obj_new('Manager','张三')
Employee[1] = obj_new('Secretary','李四')
Employee[2] = obj_new('Seller','王五')
Employee[3] = obj_new('Seller','马六')
;
Print,'---早上开始工作----'
;
For i=0,N_elements(Employee)-1 do begin
  
   Employee[i]->StartWork ;多态体现:每个员工都要StartWork，但是工作内容是多样的
EndFor
End

;--------------------------------------------------------------------------------------------------------

;每个类都继承了父类Employee  


