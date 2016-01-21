pro xxx
  
  
  ENVI,/RESTORE_BASE_SAVE_FILES
    ENVI_BATCH_INIT,/NO_STATUS_WINDOW,LOG_FILE="C:\envi_Preprocessing.Log"

envi_report_init, ['数据加载中......'], title='进度条演示', base=base

n=10000
envi_report_inc, base, n

FOR i=1,n DO BEGIN
   envi_report_stat, base, i, n
   envi_report_inc, base,float(n)/i
ENDFOR

envi_report_init, base=base, /finish
end