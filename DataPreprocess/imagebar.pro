PRO ImageBar, image, EraseFirst=erasefirst, $
   NColors=ncolors, Bottom=bottom, $
   Keep_Aspect_Ratio=keepaspect, _Extra=extra

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

IF N_Elements(ncolors) EQ 0 THEN $
   ncolors = (!D.N_Colors < 256)
IF N_Elements(bottom) EQ 0 THEN bottom = 0
drawColor = ncolors - 1 + bottom

IF !D.NAME EQ 'PS' THEN drawColor = !P.Color

   ; Display the image in the window in device-independent way.

imagePos = [0.15, 0.15, 0.9, 0.75]
IF Keyword_Set(erasefirst) THEN Erase
thisSize = Str_Size('A Default String', 0.25)

TVImage, BytScl(image, Top=ncolors-1) + bottom, $
   Position=imagePos, $
   Keep_Aspect_Ratio=Keyword_Set(keepaspect)

   ; Draw axes around the image.

Plot, x, y, /NoData, XStyle=1, YStyle=1, $
   Position=imagePos, /NoErase, Color=drawColor, $
   _Extra=extra, Charsize=thisSize

   ; Draw a colorbar next to the image.

barPos = [imagepos[0], (imagepos[3] + 0.15), $
   imagepos[2], (imagepos[3] + 0.15) + 0.05]

Colorbar, Position=barPos, NColors=ncolors, $
   Bottom=bottom, Color=drawColor, _Extra=extra, $
   Charsize=thisSize*0.75

END