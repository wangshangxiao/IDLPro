; Chapter09CropWorld.pro
PRO Chapter09CropWorld
world = READ_PNG(FILEPATH('avhrr.png', $
SUBDIRECTORY = ['examples', 'data']), R,G,B)
DEVICE, DECOMPOSED = 0, RETAIN = 2
TVLCT, R, G, B
worldSize = SIZE(world, /DIMENSIONS)
WINDOW, 0, XSIZE = worldSize[0], YSIZE = worldSize[1]
TV, world
; ����ʹ�ù�꺯��CURSORѡȡ�ü�����������������
africa = world [312:475, 103:264]
WINDOW, 2, XSIZE =(475-312 + 1), YSIZE =(264-103 + 1)
TV, africa
END