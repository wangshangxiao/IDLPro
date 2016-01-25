FUNCTION CIRCLE, xcenter, ycenter, radius
points = (2 * !PI / 99.0) * FIndGen(100)
x = xcenter + radius * Cos(points)
y = ycenter + radius * Sin(points)
RETURN, Transpose([[x],[y]])
END
