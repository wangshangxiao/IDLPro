; Chapter04CubeVolumeMain.pro
PRO  Chapter04CubeVolumeMain
������TempVolume = 0
������READ,  PROMPT = "�����볤����ĳ� x = ?", x
������READ,  PROMPT = "�����볤����Ŀ� y = ?", y
������READ,  PROMPT = "�����볤����ĸ� z = ?", z
������Chapter04CubeVolume, x, y, z, TempVolume
������PRINT, 'Cube Volume = ', TempVolume
END