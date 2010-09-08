-module(indent).

test_1({Name,{ue_network_capability,
			  <<Eea0:1,Eea1:1,Eea2:1,Eea3:1,Eea4:1,Eea5:1,Eea6:1,Eea7:1,
				_:1,Eia1:1,Eia2:1,Eia3:1,Eia4:1,Eia5:1,Eia6:1,Eia7:1,
				R1/bytes>>}}) ->
	ieGroup(Name,
			[ieGroup(?ie_epsEncryptionAlgs,
					 [ie(?ie_eea0_128,value(?ie_eea0_128,Eea0)),
					  ie(?ie_eea1_128,value(?ie_eea1_128,Eea1)),
					  ie(?ie_eea2_128,value(?ie_eea2_128,Eea2)),
					  ie(?ie_eea3,value(?ie_eea3,Eea3)),
					  ie(?ie_eea4,value(?ie_eea4,Eea4)),
					  ie(?ie_eea5,value(?ie_eea5,Eea5)),
					  ie(?ie_eea6,value(?ie_eea6,Eea6)),
					  ie(?ie_eea7,value(?ie_eea7,Eea7))],2),
			 ieGroup(?ie_epsIntegrityAlgs,
					 [ie(?ie_eia1_128,value(?ie_eia1_128,Eia1)),
					  ie(?ie_eia2_128,value(?ie_eia2_128,Eia2)),
					  ie(?ie_eia3,value(?ie_eia3,Eia3)),
					  ie(?ie_eia4,value(?ie_eia4,Eia4)),
					  ie(?ie_eia5,value(?ie_eia5,Eia5)),
					  ie(?ie_eia6,value(?ie_eia6,Eia6)),
					  ie(?ie_eia7,value(?ie_eia7,Eia7))],2) |
				 case R1 of
					 <<Uea0:1,Uea1:1,Uea2:1,Uea3:1,Uea4:1,Uea5:1,Uea6:1,Uea7:1,
					   Ucs2:1,Uia1:1,Uia2:1,Uia3:1,Uia4:1,Uia5:1,Uia6:1,Uia7:1,
					   R2/bytes>> ->
						 [ieGroup(?ie_umtsEncryptionAlgs,
								  [ie(?ie_uea0,value(?ie_uea0,Uea0)),
								   ie(?ie_uea0,value(?ie_uea0,Uea1)),
								   ie(?ie_uea2,value(?ie_uea2,Uea2)),
								   ie(?ie_uea3,value(?ie_uea3,Uea3)),
								   ie(?ie_uea4,value(?ie_uea4,Uea4)),
								   ie(?ie_uea5,value(?ie_uea5,Uea5)),
								   ie(?ie_uea6,value(?ie_uea6,Uea6)),
								   ie(?ie_uea7,value(?ie_uea7,Uea7))],2),
						  ie(?ie_ucs2Support,value(?ie_ucs2Support,Ucs2)),
						  ieGroup(?ie_umtsIntegrityAlgs,
								  [ie(?ie_uia1,value(?ie_uia1,Uia1)),
								   ie(?ie_uia2,value(?ie_uia2,Uia2)),
								   ie(?ie_uia3,value(?ie_uia3,Uia3)),
								   ie(?ie_uia4,value(?ie_uia4,Uia4)),
								   ie(?ie_uia5,value(?ie_uia5,Uia5)),
								   ie(?ie_uia6,value(?ie_uia6,Uia6)),
								   ie(?ie_uia7,value(?ie_uia7,Uia7))],2) |
							  case R2 of
								  <<_:6,Vcc:1,_/bits>> ->
									  [ie(?ie_srvccCapability,
										  value(?ie_srvccCapability,Vcc))];
								  _->
									  []
							  end];
					 _ ->
						 []
				 end]).

