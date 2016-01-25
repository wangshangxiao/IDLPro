PRO ResizeIt, image
image = Congrid(image, 150, 150)
Window, 0, XSize=150, YSize=150
TVSCL, image
END
