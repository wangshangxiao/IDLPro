pro direct_call_envi
  ;
  wTlb = widget_base()

;Open Image File ΪENVI menu�Ķ�Ӧ����
  wButton = widget_button(wtlb,value='ROI Tool', $

    uvalue='roi tool', $

    event_pro = 'envi_menu_event')

  widget_control,wtlb,/realize

  ;��ʼ��envi

  envi,/restore_base_save_Files

  envi_batch_init

  ;�����¼�����

  xmanager,'direct_Call_Envi',wtlb,/no_block

end
