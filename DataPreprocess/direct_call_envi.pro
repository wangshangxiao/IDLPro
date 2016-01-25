pro direct_call_envi
  ;
  wTlb = widget_base()

;Open Image File 为ENVI menu的对应名称
  wButton = widget_button(wtlb,value='ROI Tool', $

    uvalue='roi tool', $

    event_pro = 'envi_menu_event')

  widget_control,wtlb,/realize

  ;初始化envi

  envi,/restore_base_save_Files

  envi_batch_init

  ;启动事件关联

  xmanager,'direct_Call_Envi',wtlb,/no_block

end
