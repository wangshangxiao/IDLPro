PRO RPC_ORTHO

InputImage = ''
OutPutImage= ''

;重采样方法: 0=NearestNeighbor 1=Bilinear 2=CubicConvolution
IMAGE_RESAMPLING = 1

;背景值
BACKGROUND = 0

;高度值：如果使用了DEM，设置为-999；
;或设置平均DEM值，如果不确定，可设置为0或为空
INPUT_HEIGHT = -999.0

;DEM_FILE: DEM文件绝对路径
DEM_FILE = 'C:\Program Files\Exelis\ENVI51\data\GMTED2010.jp2'

;DEM重采样方法: 0=NearestNeighbor 1=Bilinear 2=CubicConvolution
DEM_RESAMPLING = 1

;GCP_FILE: GCP点
;格式为：
;    <longitude>    <latitude>      <elevation>    <image x>      <image y>
;For example:
;    17.84719387    29.54761461     343.70000     607.240000     201.780000
;经纬度点是WGS-84坐标系

GCP_FILE ='C:\temp\testEnvi\data\Spot\SCENE01\IMAGERY2.pts'

;GEOID_OFFSET: DEM偏移误差，如不确定或位置设置为0或空

GEOID_OFFSET = 0.0

;输出文件格式: 0=ENVI 1=GeoTIFF

OUTPUT_FORMAT = 1

END