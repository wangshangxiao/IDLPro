

PRO DC_PCA_Result_EVENT,EVENT
	WTarget=Widget_info(EVENT.ID,/NAME) EQ 'TREE'? WIDGET_INFO(EVENT.ID,/TREE_ROOT):EVENT.ID
	Wwidget= EVENT.TOP

	WIDGET_CONTROL,Event.top,GET_UVALUE=PA
	CASE WTarget OF
		WIDGET_INFO(Wwidget,FIND_BY_UNAME='SaveBu'):BEGIN
			WIDGET_CONTROL,(*PA).CeoTable,GET_VALUE=Ceo,GET_UVALUE=VarName & Ceo=STRTRIM(Ceo,2)
			WIDGET_CONTROL,(*PA).EvalTable,GET_VALUE=Eval,GET_UVALUE=Ehead & Eval=STRTRIM(Eval,2)
			WIDGET_CONTROL,(*PA).LoadTable,GET_VALUE=Lval,GET_UVALUE=Lhead & Lval=STRTRIM(Lval,2)
			WIDGET_CONTROL,(*PA).ScoreTable,GET_VALUE=Sval,GET_UVALUE=Shead

			NumVar = N_ELEMENTS(VarName)
			NumPCA = N_ELEMENTS(Lval)/NumVar				;�������غɵ����ɷֵĸ���
			NumCounty = N_ELEMENTS(Sval)/(NumPCA+3)			;���ĸ��������,Ҳ������

			EvalRowName = TRANSPOSE('F'+STRTRIM(INDGEN(NumVar)+1,2))

			SaveData = STRARR((NumVar+1)+(NumPCA+1)+(3+1),(NumVar+1)+(NumCounty+1))   ;����д��ǿ�ɶ���

			VvarName = TRANSPOSE(VarName)

			SaveData[*,0]        = ['����',VarName,'ԭ����',Lhead,'���ɷ�',Ehead]
			SaveData[*,1:NumVar] = [VvarName,Ceo,VvarName,Lval,EvalRowName,Eval]
			SaveData[0:NumPCA+3-1,NumVar+1]   = Shead
			SaveData[0:NumPCA+3-1,NumVar+2:*] = Sval
			DC_SaveTextData,SaveData,EVENT.ID,FILENAME='���ɷַ������.txt'
		END

		WIDGET_INFO(Wwidget,FIND_BY_UNAME='CloseBu'):WIDGET_CONTROL,EVENT.TOP,/DESTROY
		ELSE:
	ENDCASE
END

;*************************������***********************************
PRO DC_PCA_Result,PCAData,GROUP_LEADER=group_leader,PCA_TLB=ItsTLB

	TLB=WIDGET_BASE(UNAME='TLB',GROUP_LEADER=group_leader,TITLE='���ɷַ��������',/COL $
				   ,XOFFSET=100,YOFFSET=200,XPAD=1,YPAD=1, TLB_FRAME_ATTR=1)
	;----------------------------------------------------
	TableBase = WIDGET_BASE(TLB,/ROW,XPAD=0,YPAD=0,SPACE=8)
		Lwidth = 400 & Wwidth = 160
		LeftBase = WIDGET_BASE(TableBase,/COLUMN,/FRAME,/BASE_ALIGN_LEFT,XPAD=0,SPACE=5)
			CeoLable  = WIDGET_LABEL(LeftBase,VALUE='1�����ϵ������',/ALIGN_CENTER,YOFFSET=100)
			CeoTable  = WIDGET_TABLE(LeftBase,/FRAME,SCR_XSIZE=Lwidth,YSIZE=10,SCR_YSIZE=Wwidth)

			EvalLable = WIDGET_LABEL(LeftBase,VALUE='2������ֵ�����ɷֹ����ʺ��ۼƹ�����',/ALIGN_CENTER)
			EvalTable = WIDGET_TABLE(LeftBase,/FRAME,SCR_XSIZE=Lwidth,YSIZE=10,SCR_YSIZE=Wwidth)

		RightBase = WIDGET_BASE(TableBase,/COLUMN,/FRAME,/BASE_ALIGN_LEFT,XPAD=0,SPACE=5)
			LoadLable  = WIDGET_LABEL(RightBase,VALUE='3�����ɷ��غɾ���',/ALIGN_CENTER)
			LoadTable  = WIDGET_TABLE(RightBase,/FRAME,SCR_XSIZE=Lwidth,YSIZE=10,SCR_YSIZE=Wwidth)

			ScoreLable = WIDGET_LABEL(RightBase,VALUE='4�����ز������ɷֵ÷�',/ALIGN_CENTER)
			ScoreTable = WIDGET_TABLE(RightBase,/FRAME,SCR_XSIZE=Lwidth,YSIZE=10,SCR_YSIZE=Wwidth)

	;---------------------------------------------------
	ButtonBase = WIDGET_BASE(TLB,/ROW,/FRAME,SPACE=250,XPAD=150,/GRID_LAYOUT)
		Widths = 120
		SaveBu  = WIDGET_BUTTON(ButtonBase,VALUE='��  ��',UNAME='SaveBu',/ALIGN_CENTER,SCR_X=Widths)
		CloseBu = WIDGET_BUTTON(ButtonBase,VALUE='��  ��',UNAME='CloseBu',/ALIGN_CENTER,SCR_X=Widths)
	;----------------------------------------------------------------------------------
	;;PcaData = {Para	: RE,VarName : *((*PA).EstiTypeID),Countyname:Countyname}
	;;	 RETURN,{Cor_Coe        :	Cor_Coe,					$	;���ϵ������
	;;	 		 Evalue			:	TRANSPOSE(Evalue),			$	;����ֵ,ת�ó���,��FLOAT��,��ͬ
	;;	 		 Contribute		:	TRANSPOSE(Contribute),		$	;������
	;;	 		 Cum_Contribute :	TRANSPOSE(Cum_Contribute),	$	;�ۻ�������
	;;	 		 LoadVal		:	TRANSPOSE(LoadVal),			$	;ǰ�������ɷֵ��غ�(���±������̵�ϵ��)
	;;	 		 PCAScore		:	PCAScore					$	;ǰ�������ɷ�,�ۺϵ÷ֺ�����(��Ӧ�������)
	;;	 		 }

	VarName = PcaData.VarName	&	NumVar = N_ELEMENTS(PcaData.VarName)  ;NumVar��ָԭ�����ĸ���
	PCA     = PcaData.Para				;���ɷֲ���
	CountyName = PcaData.CountyName		;����

	WIDGET_CONTROL,CeoTable,TABLE_XSIZE=NumVar,TABLE_YSIZE=NumVar,SET_VALUE=PCA.Cor_Coe $
				  ,ROW_LABELS=VarName,COLUMN_LABELS=VarName,COLUMN_WIDTHS=55  $
				  ,SET_TABLE_SELECT=[-1,-1,-1,-1],SET_UVALUE=VarName

	EvalColName = ['����ֵ','������%','�ۼƹ�����%']
	EvalRowName = 'F'+STRTRIM(INDGEN(NumVar)+1,2)
	WIDGET_CONTROL,EvalTable,TABLE_XSIZE=3,TABLE_YSIZE=NumVar,COLUMN_LABELS=EvalColName $
				  ,ROW_LABELS=EvalRowName,SET_VALUE=[PCA.Evalue,PCA.Contribute,PCA.Cum_Contribute] $
			      ,SET_TABLE_SELECT=[-1,-1,-1,-1],COLUMN_WIDTHS=100,ALIGNMENT=1,SET_UVALUE=EvalColName

	NumPCA = N_ELEMENTS(PCA.LoadVal)/NumVar		;��ָ�Ѽ�������ɷֵĸ���
	LoadColName = '��'+STRTRIM(INDGEN(NumPCA)+1,2)+'���ɷ�'+EvalRowName[0:NumPCA-1]
	MaxCoe = MAX(PCA.LoadVal[0,*],MaxId)		;MaxId���ص��ǵ�һ���ɷ�(��һ��)�غ�ֵ�����±�
	WIDGET_CONTROL,LoadTable,TABLE_XSIZE=NumPCA,TABLE_YSIZE=NumVar,COLUMN_LABELS=LoadColName $
				  ,ROW_LABELS=VarName,SET_VALUE=PCA.LoadVal,COLUMN_WIDTHS=150 $
				  ,ALIGNMENT=1,SET_TABLE_SELECT=[-1,-1,-1,-1],SET_UVALUE=LoadColName
	WIDGET_CONTROL,LoadTable,USE_TABLE_SELECT=[0,MaxId,0,MaxId],BACKGROUND_COLOR=[0,255,0]

	ScoreColName = ['����',LoadColName,'�ۺϵ÷֦�F','λ��']
	CountyNum = N_ELEMENTS(CountyName)
	PCAScore  = STRTRIM(PCA.PCAScore,2)
	PCAScore[NumPCA+2-1,*] = STRTRIM(FIX(PCAScore[NumPCA+2-1,*]),2)
	WIDGET_CONTROL,ScoreTable,TABLE_XSIZE=NumPCA+3,TABLE_YSIZE=CountyNum,COLUMN_WIDTHS=80   $
				  ,COLUMN_LABELS=ScoreColName,ROW_LABELS=STRTRIM(INDGEN(CountyNum)+1,2) $
				  ,SET_VALUE=[CountyName,PCAScore],SET_TABLE_SELECT=[-1,-1,-1,-1],ALIGNMENT=1 $
				  ,SET_UVALUE=ScoreColName

	WIDGET_CONTROL,TLB,/REALIZE

	IF ARG_PRESENT(ItsTLB) THEN ItsTLB = TLB

	State = {CeoTable	:	CeoTable,	$
			 EvalTable	:	EvalTable,	$
			 LoadTable	:	LoadTable,	$
			 ScoreTable	:	ScoreTable	$
			}

	WIDGET_CONTROL,TLB,SET_UVALUE=PTR_NEW(State,/NO_COPY)

	XMANAGER,'DC_PCA_Result',TLB,/NO_BLOCK,CLEANUP='DC_CleanAllHeap'

END