; Chapter09RemoveBridges.pro
PRO Chapter09RemoveBridges
  DEVICE, DECOMPOSED = 0, RETAIN = 2
  LOADCT, 0
  xsize = 768  &  ysize = 512
  img = READ_BINARY(FILEPATH('nyny.dat', $
    SUBDIRECTORY = ['examples', 'data']), DATA_DIMS = [xsize, ysize])
  ; ��ǿͼ��Աȶȣ�Ȼ����ʾͼ��
  img = BYTSCL(img)  &  WINDOW, 0  &  TVSCL, img
  ; ������ֵ����ͼ��
  maskImg = img LT 70
  ; ���������νṹԪ��
  side = 3
  strucElem = DIST(side) LE side
  maskImg = MORPH_OPEN(maskImg, strucElem)
  maskImg = MORPH_CLOSE(maskImg, strucElem)
  WINDOW, 1, title='Mask After Opening and Closing'
  TVSCL, maskImg
  ; ��ȡ����ͼ������
  labelImg = LABEL_REGION(maskImg)
  ; ��ȥ���б���������ͼ������
  regions = labelImg[WHERE(labelImg NE 0)]
  mainRegion = WHERE(HISTOGRAM(labelImg) EQ MAX(HISTOGRAM(regions)))
  maskImg = labelImg EQ mainRegion[0]
  ; ��ʾ����ͼ��
  Window, 3, TITLE = 'Final Masked Image'
  TVSCL, maskImg
  newImg = MORPH_OPEN(img, strucElem, /GRAY)
  newImg[WHERE(maskImg EQ 0)] = img[WHERE(maskImg EQ 0)]
  PRINT, 'Hit any key to end program.'
  WINDOW, 2, XSIZE = xsize, YSIZE = ysize, TITLE = 'Hit Any Key to End'
  ; ��˸�Ա�ԭͼ��ͷָ�ͼ��
  FLICK, img, newImg
END