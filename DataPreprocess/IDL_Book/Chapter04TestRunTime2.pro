; Chapter04TestRunTime2.pro
PRO Chapter04TestRunTime2
����MyVariable = DIST(1000, 1000)
����Sum = 0.0
����StartTime = SYSTIME(/SECONDS)
����Sum = TOTAL(MyVariable)
����EndTime = SYSTIME(/SECONDS)
����PRINT, '����ĺͣ�', Sum
����PRINT, 'ʹ��ʱ�䣺' , EndTime - StartTime
END