;--------------------------------------------------------------
pro transfor
;--------------------------------------------------------------
    ;=======设置批处理环境，初始化，并开始批处理模式=====
    envi, /restore_base_save_files
    envi_batch_init, log_file='batch.txt'

    ;=====定义输入文件路径=====

    inpath='C:\My_work\ChinaVGTChange\GIMMS\Data\Data\'

    ;=====定义输出文件路径=====

    outpath='C:\My_work\ChinaVGTChange\GIMMS\Data\Data\output\'

    ;定义批处理文件名列表

    filename  = [$
'00apr15a.n14-VIg_data.tif', $
'00apr15b.n14-VIg_data.tif', $
'00aug15a.n14-VIg_data.tif', $
'00aug15b.n14-VIg_data.tif', $
'00dec15a.n16-VIg_data.tif', $
'00dec15b.n16-VIg_data.tif', $
'00feb15a.n14-VIg_data.tif', $
'00feb15b.n14-VIg_data.tif', $
'00jan15a.n14-VIg_data.tif', $
'00jan15b.n14-VIg_data.tif', $
'00jul15a.n14-VIg_data.tif', $
'00jul15b.n14-VIg_data.tif', $
'00jun15a.n14-VIg_data.tif', $
'00jun15b.n14-VIg_data.tif', $
'00mar15a.n14-VIg_data.tif', $
'00mar15b.n14-VIg_data.tif', $
'00may15a.n14-VIg_data.tif', $
'00may15b.n14-VIg_data.tif', $
'00nov15a.n16-VIg_data.tif', $
'00nov15b.n16-VIg_data.tif', $
'00oct15a.n14-VIg_data.tif', $
'00oct15b.n14-VIg_data.tif', $
'00sep15a.n14-VIg_data.tif', $
'00sep15b.n14-VIg_data.tif', $
'01apr15a.n16-VIg_data.tif', $
'01apr15b.n16-VIg_data.tif', $
'01aug15a.n16-VIg_data.tif', $
'01aug15b.n16-VIg_data.tif', $
'01dec15a.n16-VIg_data.tif', $
'01dec15b.n16-VIg_data.tif', $
'01feb15a.n16-VIg_data.tif', $
'01feb15b.n16-VIg_data.tif', $
'01jan15a.n16-VIg_data.tif', $
'01jan15b.n16-VIg_data.tif', $
'01jul15a.n16-VIg_data.tif', $
'01jul15b.n16-VIg_data.tif', $
'01jun15a.n16-VIg_data.tif', $
'01jun15b.n16-VIg_data.tif', $
'01mar15a.n16-VIg_data.tif', $
'01mar15b.n16-VIg_data.tif', $
'01may15a.n16-VIg_data.tif', $
'01may15b.n16-VIg_data.tif', $
'01nov15a.n16-VIg_data.tif', $
'01nov15b.n16-VIg_data.tif', $
'01oct15a.n16-VIg_data.tif', $
'01oct15b.n16-VIg_data.tif', $
'01sep15a.n16-VIg_data.tif', $
'01sep15b.n16-VIg_data.tif', $
'02apr15a.n16-VIg_data.tif', $
'02apr15b.n16-VIg_data.tif', $
'02aug15a.n16-VIg_data.tif', $
'02aug15b.n16-VIg_data.tif', $
'02dec15a.n16-VIg_data.tif', $
'02dec15b.n16-VIg_data.tif', $
'02feb15a.n16-VIg_data.tif', $
'02feb15b.n16-VIg_data.tif', $
'02jan15a.n16-VIg_data.tif', $
'02jan15b.n16-VIg_data.tif', $
'02jul15a.n16-VIg_data.tif', $
'02jul15b.n16-VIg_data.tif', $
'02jun15a.n16-VIg_data.tif', $
'02jun15b.n16-VIg_data.tif', $
'02mar15a.n16-VIg_data.tif', $
'02mar15b.n16-VIg_data.tif', $
'02may15a.n16-VIg_data.tif', $
'02may15b.n16-VIg_data.tif', $
'02nov15a.n16-VIg_data.tif', $
'02nov15b.n16-VIg_data.tif', $
'02oct15a.n16-VIg_data.tif', $
'02oct15b.n16-VIg_data.tif', $
'02sep15a.n16-VIg_data.tif', $
'02sep15b.n16-VIg_data.tif', $
'03apr15a.n16-VIg_data.tif', $
'03apr15b.n16-VIg_data.tif', $
'03aug15a.n16-VIg_data.tif', $
'03aug15b.n16-VIg_data.tif', $
'03dec15a.n16-VIg_data.tif', $
'03dec15b.n16-VIg_data.tif', $
'03feb15a.n16-VIg_data.tif', $
'03feb15b.n16-VIg_data.tif', $
'03jan15a.n16-VIg_data.tif', $
'03jan15b.n16-VIg_data.tif', $
'03jul15a.n16-VIg_data.tif', $
'03jul15b.n16-VIg_data.tif', $
'03jun15a.n16-VIg_data.tif', $
'03jun15b.n16-VIg_data.tif', $
'03mar15a.n16-VIg_data.tif', $
'03mar15b.n16-VIg_data.tif', $
'03may15a.n16-VIg_data.tif', $
'03may15b.n16-VIg_data.tif', $
'03nov15a.n16-VIg_data.tif', $
'03nov15b.n16-VIg_data.tif', $
'03oct15a.n16-VIg_data.tif', $
'03oct15b.n16-VIg_data.tif', $
'03sep15a.n16-VIg_data.tif', $
'03sep15b.n16-VIg_data.tif', $
'81aug15a.n07-VIg_data.tif', $
'81aug15b.n07-VIg_data.tif', $
'81dec15a.n07-VIg_data.tif', $
'81dec15b.n07-VIg_data.tif', $
'81jul15a.n07-VIg_data.tif', $
'81jul15b.n07-VIg_data.tif', $
'81nov15a.n07-VIg_data.tif', $
'81nov15b.n07-VIg_data.tif', $
'81oct15a.n07-VIg_data.tif', $
'81oct15b.n07-VIg_data.tif', $
'81sep15a.n07-VIg_data.tif', $
'81sep15b.n07-VIg_data.tif', $
'82apr15a.n07-VIg_data.tif', $
'82apr15b.n07-VIg_data.tif', $
'82aug15a.n07-VIg_data.tif', $
'82aug15b.n07-VIg_data.tif', $
'82dec15a.n07-VIg_data.tif', $
'82dec15b.n07-VIg_data.tif', $
'82feb15a.n07-VIg_data.tif', $
'82feb15b.n07-VIg_data.tif', $
'82jan15a.n07-VIg_data.tif', $
'82jan15b.n07-VIg_data.tif', $
'82jul15a.n07-VIg_data.tif', $
'82jul15b.n07-VIg_data.tif', $
'82jun15a.n07-VIg_data.tif', $
'82jun15b.n07-VIg_data.tif', $
'82mar15a.n07-VIg_data.tif', $
'82mar15b.n07-VIg_data.tif', $
'82may15a.n07-VIg_data.tif', $
'82may15b.n07-VIg_data.tif', $
'82nov15a.n07-VIg_data.tif', $
'82nov15b.n07-VIg_data.tif', $
'82oct15a.n07-VIg_data.tif', $
'82oct15b.n07-VIg_data.tif', $
'82sep15a.n07-VIg_data.tif', $
'82sep15b.n07-VIg_data.tif', $
'83apr15a.n07-VIg_data.tif', $
'83apr15b.n07-VIg_data.tif', $
'83aug15a.n07-VIg_data.tif', $
'83aug15b.n07-VIg_data.tif', $
'83dec15a.n07-VIg_data.tif', $
'83dec15b.n07-VIg_data.tif', $
'83feb15a.n07-VIg_data.tif', $
'83feb15b.n07-VIg_data.tif', $
'83jan15a.n07-VIg_data.tif', $
'83jan15b.n07-VIg_data.tif', $
'83jul15a.n07-VIg_data.tif', $
'83jul15b.n07-VIg_data.tif', $
'83jun15a.n07-VIg_data.tif', $
'83jun15b.n07-VIg_data.tif', $
'83mar15a.n07-VIg_data.tif', $
'83mar15b.n07-VIg_data.tif', $
'83may15a.n07-VIg_data.tif', $
'83may15b.n07-VIg_data.tif', $
'83nov15a.n07-VIg_data.tif', $
'83nov15b.n07-VIg_data.tif', $
'83oct15a.n07-VIg_data.tif', $
'83oct15b.n07-VIg_data.tif', $
'83sep15a.n07-VIg_data.tif', $
'83sep15b.n07-VIg_data.tif', $
'84apr15a.n07-VIg_data.tif', $
'84apr15b.n07-VIg_data.tif', $
'84aug15a.n07-VIg_data.tif', $
'84aug15b.n07-VIg_data.tif', $
'84dec15a.n07-VIg_data.tif', $
'84dec15b.n07-VIg_data.tif', $
'84feb15a.n07-VIg_data.tif', $
'84feb15b.n07-VIg_data.tif', $
'84jan15a.n07-VIg_data.tif', $
'84jan15b.n07-VIg_data.tif', $
'84jul15a.n07-VIg_data.tif', $
'84jul15b.n07-VIg_data.tif', $
'84jun15a.n07-VIg_data.tif', $
'84jun15b.n07-VIg_data.tif', $
'84mar15a.n07-VIg_data.tif', $
'84mar15b.n07-VIg_data.tif', $
'84may15a.n07-VIg_data.tif', $
'84may15b.n07-VIg_data.tif', $
'84nov15a.n07-VIg_data.tif', $
'84nov15b.n07-VIg_data.tif', $
'84oct15a.n07-VIg_data.tif', $
'84oct15b.n07-VIg_data.tif', $
'84sep15a.n07-VIg_data.tif', $
'84sep15b.n07-VIg_data.tif', $
'85apr15a.n09-VIg_data.tif', $
'85apr15b.n09-VIg_data.tif', $
'85aug15a.n09-VIg_data.tif', $
'85aug15b.n09-VIg_data.tif', $
'85dec15a.n09-VIg_data.tif', $
'85dec15b.n09-VIg_data.tif', $
'85feb15a.n07-VIg_data.tif', $
'85feb15b.n07-VIg_data.tif', $
'85jan15a.n07-VIg_data.tif', $
'85jan15b.n07-VIg_data.tif', $
'85jul15a.n09-VIg_data.tif', $
'85jul15b.n09-VIg_data.tif', $
'85jun15a.n09-VIg_data.tif', $
'85jun15b.n09-VIg_data.tif', $
'85mar15a.n09-VIg_data.tif', $
'85mar15b.n09-VIg_data.tif', $
'85may15a.n09-VIg_data.tif', $
'85may15b.n09-VIg_data.tif', $
'85nov15a.n09-VIg_data.tif', $
'85nov15b.n09-VIg_data.tif', $
'85oct15a.n09-VIg_data.tif', $
'85oct15b.n09-VIg_data.tif', $
'85sep15a.n09-VIg_data.tif', $
'85sep15b.n09-VIg_data.tif', $
'86apr15a.n09-VIg_data.tif', $
'86apr15b.n09-VIg_data.tif', $
'86aug15a.n09-VIg_data.tif', $
'86aug15b.n09-VIg_data.tif', $
'86dec15a.n09-VIg_data.tif', $
'86dec15b.n09-VIg_data.tif', $
'86feb15a.n09-VIg_data.tif', $
'86feb15b.n09-VIg_data.tif', $
'86jan15a.n09-VIg_data.tif', $
'86jan15b.n09-VIg_data.tif', $
'86jul15a.n09-VIg_data.tif', $
'86jul15b.n09-VIg_data.tif', $
'86jun15a.n09-VIg_data.tif', $
'86jun15b.n09-VIg_data.tif', $
'86mar15a.n09-VIg_data.tif', $
'86mar15b.n09-VIg_data.tif', $
'86may15a.n09-VIg_data.tif', $
'86may15b.n09-VIg_data.tif', $
'86nov15a.n09-VIg_data.tif', $
'86nov15b.n09-VIg_data.tif', $
'86oct15a.n09-VIg_data.tif', $
'86oct15b.n09-VIg_data.tif', $
'86sep15a.n09-VIg_data.tif', $
'86sep15b.n09-VIg_data.tif', $
'87apr15a.n09-VIg_data.tif', $
'87apr15b.n09-VIg_data.tif', $
'87aug15a.n09-VIg_data.tif', $
'87aug15b.n09-VIg_data.tif', $
'87dec15a.n09-VIg_data.tif', $
'87dec15b.n09-VIg_data.tif', $
'87feb15a.n09-VIg_data.tif', $
'87feb15b.n09-VIg_data.tif', $
'87jan15a.n09-VIg_data.tif', $
'87jan15b.n09-VIg_data.tif', $
'87jul15a.n09-VIg_data.tif', $
'87jul15b.n09-VIg_data.tif', $
'87jun15a.n09-VIg_data.tif', $
'87jun15b.n09-VIg_data.tif', $
'87mar15a.n09-VIg_data.tif', $
'87mar15b.n09-VIg_data.tif', $
'87may15a.n09-VIg_data.tif', $
'87may15b.n09-VIg_data.tif', $
'87nov15a.n09-VIg_data.tif', $
'87nov15b.n09-VIg_data.tif', $
'87oct15a.n09-VIg_data.tif', $
'87oct15b.n09-VIg_data.tif', $
'87sep15a.n09-VIg_data.tif', $
'87sep15b.n09-VIg_data.tif', $
'88apr15a.n09-VIg_data.tif', $
'88apr15b.n09-VIg_data.tif', $
'88aug15a.n09-VIg_data.tif', $
'88aug15b.n09-VIg_data.tif', $
'88dec15a.n11-VIg_data.tif', $
'88dec15b.n11-VIg_data.tif', $
'88feb15a.n09-VIg_data.tif', $
'88feb15b.n09-VIg_data.tif', $
'88jan15a.n09-VIg_data.tif', $
'88jan15b.n09-VIg_data.tif', $
'88jul15a.n09-VIg_data.tif', $
'88jul15b.n09-VIg_data.tif', $
'88jun15a.n09-VIg_data.tif', $
'88jun15b.n09-VIg_data.tif', $
'88mar15a.n09-VIg_data.tif', $
'88mar15b.n09-VIg_data.tif', $
'88may15a.n09-VIg_data.tif', $
'88may15b.n09-VIg_data.tif', $
'88nov15a.n11-VIg_data.tif', $
'88nov15b.n11-VIg_data.tif', $
'88oct15a.n09-VIg_data.tif', $
'88oct15b.n09-VIg_data.tif', $
'88sep15a.n09-VIg_data.tif', $
'88sep15b.n09-VIg_data.tif', $
'89apr15a.n11-VIg_data.tif', $
'89apr15b.n11-VIg_data.tif', $
'89aug15a.n11-VIg_data.tif', $
'89aug15b.n11-VIg_data.tif', $
'89dec15a.n11-VIg_data.tif', $
'89dec15b.n11-VIg_data.tif', $
'89feb15a.n11-VIg_data.tif', $
'89feb15b.n11-VIg_data.tif', $
'89jan15a.n11-VIg_data.tif', $
'89jan15b.n11-VIg_data.tif', $
'89jul15a.n11-VIg_data.tif', $
'89jul15b.n11-VIg_data.tif', $
'89jun15a.n11-VIg_data.tif', $
'89jun15b.n11-VIg_data.tif', $
'89mar15a.n11-VIg_data.tif', $
'89mar15b.n11-VIg_data.tif', $
'89may15a.n11-VIg_data.tif', $
'89may15b.n11-VIg_data.tif', $
'89nov15a.n11-VIg_data.tif', $
'89nov15b.n11-VIg_data.tif', $
'89oct15a.n11-VIg_data.tif', $
'89oct15b.n11-VIg_data.tif', $
'89sep15a.n11-VIg_data.tif', $
'89sep15b.n11-VIg_data.tif', $
'90apr15a.n11-VIg_data.tif', $
'90apr15b.n11-VIg_data.tif', $
'90aug15a.n11-VIg_data.tif', $
'90aug15b.n11-VIg_data.tif', $
'90dec15a.n11-VIg_data.tif', $
'90dec15b.n11-VIg_data.tif', $
'90feb15a.n11-VIg_data.tif', $
'90feb15b.n11-VIg_data.tif', $
'90jan15a.n11-VIg_data.tif', $
'90jan15b.n11-VIg_data.tif', $
'90jul15a.n11-VIg_data.tif', $
'90jul15b.n11-VIg_data.tif', $
'90jun15a.n11-VIg_data.tif', $
'90jun15b.n11-VIg_data.tif', $
'90mar15a.n11-VIg_data.tif', $
'90mar15b.n11-VIg_data.tif', $
'90may15a.n11-VIg_data.tif', $
'90may15b.n11-VIg_data.tif', $
'90nov15a.n11-VIg_data.tif', $
'90nov15b.n11-VIg_data.tif', $
'90oct15a.n11-VIg_data.tif', $
'90oct15b.n11-VIg_data.tif', $
'90sep15a.n11-VIg_data.tif', $
'90sep15b.n11-VIg_data.tif', $
'91apr15a.n11-VIg_data.tif', $
'91apr15b.n11-VIg_data.tif', $
'91aug15a.n11-VIg_data.tif', $
'91aug15b.n11-VIg_data.tif', $
'91dec15a.n11-VIg_data.tif', $
'91dec15b.n11-VIg_data.tif', $
'91feb15a.n11-VIg_data.tif', $
'91feb15b.n11-VIg_data.tif', $
'91jan15a.n11-VIg_data.tif', $
'91jan15b.n11-VIg_data.tif', $
'91jul15a.n11-VIg_data.tif', $
'91jul15b.n11-VIg_data.tif', $
'91jun15a.n11-VIg_data.tif', $
'91jun15b.n11-VIg_data.tif', $
'91mar15a.n11-VIg_data.tif', $
'91mar15b.n11-VIg_data.tif', $
'91may15a.n11-VIg_data.tif', $
'91may15b.n11-VIg_data.tif', $
'91nov15a.n11-VIg_data.tif', $
'91nov15b.n11-VIg_data.tif', $
'91oct15a.n11-VIg_data.tif', $
'91oct15b.n11-VIg_data.tif', $
'91sep15a.n11-VIg_data.tif', $
'91sep15b.n11-VIg_data.tif', $
'92apr15a.n11-VIg_data.tif', $
'92apr15b.n11-VIg_data.tif', $
'92aug15a.n11-VIg_data.tif', $
'92aug15b.n11-VIg_data.tif', $
'92dec15a.n11-VIg_data.tif', $
'92dec15b.n11-VIg_data.tif', $
'92feb15a.n11-VIg_data.tif', $
'92feb15b.n11-VIg_data.tif', $
'92jan15a.n11-VIg_data.tif', $
'92jan15b.n11-VIg_data.tif', $
'92jul15a.n11-VIg_data.tif', $
'92jul15b.n11-VIg_data.tif', $
'92jun15a.n11-VIg_data.tif', $
'92jun15b.n11-VIg_data.tif', $
'92mar15a.n11-VIg_data.tif', $
'92mar15b.n11-VIg_data.tif', $
'92may15a.n11-VIg_data.tif', $
'92may15b.n11-VIg_data.tif', $
'92nov15a.n11-VIg_data.tif', $
'92nov15b.n11-VIg_data.tif', $
'92oct15a.n11-VIg_data.tif', $
'92oct15b.n11-VIg_data.tif', $
'92sep15a.n11-VIg_data.tif', $
'92sep15b.n11-VIg_data.tif', $
'93apr15a.n11-VIg_data.tif', $
'93apr15b.n11-VIg_data.tif', $
'93aug15a.n11-VIg_data.tif', $
'93aug15b.n11-VIg_data.tif', $
'93dec15a.n11-VIg_data.tif', $
'93dec15b.n11-VIg_data.tif', $
'93feb15a.n11-VIg_data.tif', $
'93feb15b.n11-VIg_data.tif', $
'93jan15a.n11-VIg_data.tif', $
'93jan15b.n11-VIg_data.tif', $
'93jul15a.n11-VIg_data.tif', $
'93jul15b.n11-VIg_data.tif', $
'93jun15a.n11-VIg_data.tif', $
'93jun15b.n11-VIg_data.tif', $
'93mar15a.n11-VIg_data.tif', $
'93mar15b.n11-VIg_data.tif', $
'93may15a.n11-VIg_data.tif', $
'93may15b.n11-VIg_data.tif', $
'93nov15a.n11-VIg_data.tif', $
'93nov15b.n11-VIg_data.tif', $
'93oct15a.n11-VIg_data.tif', $
'93oct15b.n11-VIg_data.tif', $
'93sep15a.n11-VIg_data.tif', $
'93sep15b.n11-VIg_data.tif', $
'94apr15a.n11-VIg_data.tif', $
'94apr15b.n11-VIg_data.tif', $
'94aug15a.n11-VIg_data.tif', $
'94aug15b.n11-VIg_data.tif', $
'94dec15a.n09-VIg_data.tif', $
'94dec15b.n09-VIg_data.tif', $
'94feb15a.n11-VIg_data.tif', $
'94feb15b.n11-VIg_data.tif', $
'94jan15a.n11-VIg_data.tif', $
'94jan15b.n11-VIg_data.tif', $
'94jul15a.n11-VIg_data.tif', $
'94jul15b.n11-VIg_data.tif', $
'94jun15a.n11-VIg_data.tif', $
'94jun15b.n11-VIg_data.tif', $
'94mar15a.n11-VIg_data.tif', $
'94mar15b.n11-VIg_data.tif', $
'94may15a.n11-VIg_data.tif', $
'94may15b.n11-VIg_data.tif', $
'94nov15a.n09-VIg_data.tif', $
'94nov15b.n09-VIg_data.tif', $
'94oct15a.n09-VIg_data.tif', $
'94oct15b.n09-VIg_data.tif', $
'94sep15a.n09-VIg_data.tif', $
'94sep15b.n09-VIg_data.tif', $
'95apr15a.n14-VIg_data.tif', $
'95apr15b.n14-VIg_data.tif', $
'95aug15a.n14-VIg_data.tif', $
'95aug15b.n14-VIg_data.tif', $
'95dec15a.n14-VIg_data.tif', $
'95dec15b.n14-VIg_data.tif', $
'95feb15a.n14-VIg_data.tif', $
'95feb15b.n14-VIg_data.tif', $
'95jan15a.n09-VIg_data.tif', $
'95jan15b.n14-VIg_data.tif', $
'95jul15a.n14-VIg_data.tif', $
'95jul15b.n14-VIg_data.tif', $
'95jun15a.n14-VIg_data.tif', $
'95jun15b.n14-VIg_data.tif', $
'95mar15a.n14-VIg_data.tif', $
'95mar15b.n14-VIg_data.tif', $
'95may15a.n14-VIg_data.tif', $
'95may15b.n14-VIg_data.tif', $
'95nov15a.n14-VIg_data.tif', $
'95nov15b.n14-VIg_data.tif', $
'95oct15a.n14-VIg_data.tif', $
'95oct15b.n14-VIg_data.tif', $
'95sep15a.n14-VIg_data.tif', $
'95sep15b.n14-VIg_data.tif', $
'96apr15a.n14-VIg_data.tif', $
'96apr15b.n14-VIg_data.tif', $
'96aug15a.n14-VIg_data.tif', $
'96aug15b.n14-VIg_data.tif', $
'96dec15a.n14-VIg_data.tif', $
'96dec15b.n14-VIg_data.tif', $
'96feb15a.n14-VIg_data.tif', $
'96feb15b.n14-VIg_data.tif', $
'96jan15a.n14-VIg_data.tif', $
'96jan15b.n14-VIg_data.tif', $
'96jul15a.n14-VIg_data.tif', $
'96jul15b.n14-VIg_data.tif', $
'96jun15a.n14-VIg_data.tif', $
'96jun15b.n14-VIg_data.tif', $
'96mar15a.n14-VIg_data.tif', $
'96mar15b.n14-VIg_data.tif', $
'96may15a.n14-VIg_data.tif', $
'96may15b.n14-VIg_data.tif', $
'96nov15a.n14-VIg_data.tif', $
'96nov15b.n14-VIg_data.tif', $
'96oct15a.n14-VIg_data.tif', $
'96oct15b.n14-VIg_data.tif', $
'96sep15a.n14-VIg_data.tif', $
'96sep15b.n14-VIg_data.tif', $
'97apr15a.n14-VIg_data.tif', $
'97apr15b.n14-VIg_data.tif', $
'97aug15a.n14-VIg_data.tif', $
'97aug15b.n14-VIg_data.tif', $
'97dec15a.n14-VIg_data.tif', $
'97dec15b.n14-VIg_data.tif', $
'97feb15a.n14-VIg_data.tif', $
'97feb15b.n14-VIg_data.tif', $
'97jan15a.n14-VIg_data.tif', $
'97jan15b.n14-VIg_data.tif', $
'97jul15a.n14-VIg_data.tif', $
'97jul15b.n14-VIg_data.tif', $
'97jun15a.n14-VIg_data.tif', $
'97jun15b.n14-VIg_data.tif', $
'97mar15a.n14-VIg_data.tif', $
'97mar15b.n14-VIg_data.tif', $
'97may15a.n14-VIg_data.tif', $
'97may15b.n14-VIg_data.tif', $
'97nov15a.n14-VIg_data.tif', $
'97nov15b.n14-VIg_data.tif', $
'97oct15a.n14-VIg_data.tif', $
'97oct15b.n14-VIg_data.tif', $
'97sep15a.n14-VIg_data.tif', $
'97sep15b.n14-VIg_data.tif', $
'98apr15a.n14-VIg_data.tif', $
'98apr15b.n14-VIg_data.tif', $
'98aug15a.n14-VIg_data.tif', $
'98aug15b.n14-VIg_data.tif', $
'98dec15a.n14-VIg_data.tif', $
'98dec15b.n14-VIg_data.tif', $
'98feb15a.n14-VIg_data.tif', $
'98feb15b.n14-VIg_data.tif', $
'98jan15a.n14-VIg_data.tif', $
'98jan15b.n14-VIg_data.tif', $
'98jul15a.n14-VIg_data.tif', $
'98jul15b.n14-VIg_data.tif', $
'98jun15a.n14-VIg_data.tif', $
'98jun15b.n14-VIg_data.tif', $
'98mar15a.n14-VIg_data.tif', $
'98mar15b.n14-VIg_data.tif', $
'98may15a.n14-VIg_data.tif', $
'98may15b.n14-VIg_data.tif', $
'98nov15a.n14-VIg_data.tif', $
'98nov15b.n14-VIg_data.tif', $
'98oct15a.n14-VIg_data.tif', $
'98oct15b.n14-VIg_data.tif', $
'98sep15a.n14-VIg_data.tif', $
'98sep15b.n14-VIg_data.tif', $
'99apr15a.n14-VIg_data.tif', $
'99apr15b.n14-VIg_data.tif', $
'99aug15a.n14-VIg_data.tif', $
'99aug15b.n14-VIg_data.tif', $
'99dec15a.n14-VIg_data.tif', $
'99dec15b.n14-VIg_data.tif', $
'99feb15a.n14-VIg_data.tif', $
'99feb15b.n14-VIg_data.tif', $
'99jan15a.n14-VIg_data.tif', $
'99jan15b.n14-VIg_data.tif', $
'99jul15a.n14-VIg_data.tif', $
'99jul15b.n14-VIg_data.tif', $
'99jun15a.n14-VIg_data.tif', $
'99jun15b.n14-VIg_data.tif', $
'99mar15a.n14-VIg_data.tif', $
'99mar15b.n14-VIg_data.tif', $
'99may15a.n14-VIg_data.tif', $
'99may15b.n14-VIg_data.tif', $
'99nov15a.n14-VIg_data.tif', $
'99nov15b.n14-VIg_data.tif', $
'99oct15a.n14-VIg_data.tif', $
'99oct15b.n14-VIg_data.tif', $
'99sep15a.n14-VIg_data.tif', $
'99sep15b.n14-VIg_data.tif' ]

    n = N_ELEMENTS(filename)

    ;==========批处理=====================
    FOR i=0,n-1 DO BEGIN


        in_name=inpath+filename[i]
        out_name=outpath+filename[i] + '_envi.img'


        envi_open_file, in_name, r_fid=fid
        if (fid eq -1) then begin
            envi_batch_exit
            return
        endif

        envi_output_to_external_format, fid=fid, dims=[-1,3472,4007,557,810], pos=0, out_name=out_name, /ENVI
        ;envi_doit, 'resize_doit', fid=fid, pos=pos, dims=[-1, 476,1095,1,600], interp=0, rfact=[1,1],out_name=out_name, r_fid=r_fid

    ENDFOR

    ;=============================退出批处理环境=======================
    envi_batch_exit

end
