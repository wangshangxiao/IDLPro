PRO ImageBar, image, EraseFirst=erasefirst, $
   NColors=ncolors, Bottom=bottom

   ; Let the user open an image data set if one is not supplied.
   ; This is only available on device that supports windows.

IF N_Params() EQ 0 THEN BEGIN
   IF (!D.Flags AND 256) NE 0 THEN BEGIN
      image = GetImage(Cancel=cancelled, 'm51.dat', $
         XSize=340, YSize=440)
      IF cancelled THEN RETURN
   ENDIF ELSE BEGIN
      Message, 'Please supply an image argument.', /Continue
      RETURN
   ENDELSE
ENDIF

   ; Make sure image is 2D.

s = Size(image)
IF s[0] NE 2 THEN BEGIN
   Message, 'Image argument must be 2D.', /Continue
   RETURN
ENDIF

x = FIndGen(s[1])
y = FIndGen(s[2])

   ; Check keywords. Define defaults if necessary.

IF N_Elements(ncolors) EQ 0 THEN ncolors = !D.Table_Size
IF N_Elements(bottom) EQ 0 THEN bottom = 0
drawColor = ncolors - 1 + bottom

   ; Display the image in the window in device-independent way.

imagePos = [0.15, 0.15, 0.9, 0.75]
IF Keyword_Set(erasefirst) THEN Erase
TVImage, BytScl(image, Top=ncolors) + bottom, $
   Position=imagePos

   ; Draw axes around the image.

Plot, x, y, /NoData, XStyle=1, YStyle=1, Position=imagePos, $
   /NoErase, Color=drawColor

   ; Draw a colorbar next to the image.

barPos = [imagepos[0], (imagepos[3] + 0.15), imagepos[2], $
   (imagepos[3] + 0.15) + 0.05]
Colorbar, Position=barPos, NColors=ncolors, $
   Bottom=bottom, Color=drawColor

END