PRO RPC_ORTHO

InputImage = ''
OutPutImage= ''

;�ز�������: 0=NearestNeighbor 1=Bilinear 2=CubicConvolution
IMAGE_RESAMPLING = 1

;����ֵ
BACKGROUND = 0

;�߶�ֵ�����ʹ����DEM������Ϊ-999��
;������ƽ��DEMֵ�������ȷ����������Ϊ0��Ϊ��
INPUT_HEIGHT = -999.0

;DEM_FILE: DEM�ļ�����·��
DEM_FILE = 'C:\Program Files\Exelis\ENVI51\data\GMTED2010.jp2'

;DEM�ز�������: 0=NearestNeighbor 1=Bilinear 2=CubicConvolution
DEM_RESAMPLING = 1

;GCP_FILE: GCP��
;��ʽΪ��
;    <longitude>    <latitude>      <elevation>    <image x>      <image y>
;For example:
;    17.84719387    29.54761461     343.70000     607.240000     201.780000
;��γ�ȵ���WGS-84����ϵ

GCP_FILE ='C:\temp\testEnvi\data\Spot\SCENE01\IMAGERY2.pts'

;GEOID_OFFSET: DEMƫ�����粻ȷ����λ������Ϊ0���

GEOID_OFFSET = 0.0

;����ļ���ʽ: 0=ENVI 1=GeoTIFF

OUTPUT_FORMAT = 1

END