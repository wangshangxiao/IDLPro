PRO test_FXSegmentation
COMPILE_OPT idl2
; 启动 ENVI 5.2
e = ENVI()
; 打开遥感图像
File = FILEPATH('qb_boulder_msi', Subdir=['data'], $
Root_Dir=e.ROOT_DIR)
Raster = e.OpenRaster(File)
; 初始化 FXSegmentation Task
Task = ENVITASK('FXSegmentation')
; 设置输入参数：输入图像、分割尺度、合并尺度
Task.INPUT_RASTER = Raster
Task.SEGMENT_VALUE = 50.0
Task.MERGE_VALUE = 90.0
; 设置输出栅格路径（位于ENVI临时目录下）
Task.OUTPUT _MEANS_RASTER_URI = e.GetTemporaryFilename()
; 执行面向对象分割
Task.Execute
; 将分割结果添加到 Data Manager 中
DataColl = e.DATA
DataColl.Add, Task.OUTPUT _MEANS_RASTER
; 在ENVI中显示分割结果
View1 = e.GetView()
Layer1 = View1.CreateLayer(Task.OUTPUT _MEANS_RASTER)
END