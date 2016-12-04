




model3<-lm(SalePrice ~ LotFrontage + LotArea + Neighborhood : GrLivArea + BsmtFinSF1 +
             BsmtUnfSF + X1stFlrSF + X2ndFlrSF + GarageCars + 
             WoodDeckSF + EnclosedPorch + ScreenPorch + MiscFeature + 
             MSSub20 + MSSub60 + MSSub160 + MSSub90 + MSZRL + MSZRM + 
             MSZFV + STR + LOTSHREG + LNDCNTRBNK + UTILALL + LOTCFGIN + 
             LOTCFGCRN + LOTCFGCUL + SLOPEGTL + SLOPEMOD + COND1NORM + 
             COND2NORM + COND2FEED + X1VYNL + X1HDBRD + X1MTLSD + X1WDSD + 
             X1PLY + X1CEMBD + X1BRKF + X1WDSHNG + X1STCO + X1ASBSHNG + 
             X2PLY + X2BRKF + XQAVG + XQGD + XQEX + XCOAVG + XCOGD + XCOFA + 
             FOUNDBRTL + BQUALAVG + BQUALGD + BQUALEX + BQUALFA + BMEXGD + 
             BFT1GLQ + BFT1ALQ + BFT1BLQ + BFT1REC + BFT2UNF + BFT2BLQ + 
             BFT2REC + BFT2LOW + HQCEX + CENTAIRY + KITCHQEX + FUNCTYP + 
             FUNCMN2 + FUNCMN1 + GTYPATTCH + GTYDET + GTYBI + GQUALTA + 
             GQUALFA + GARCOTA + MSJUN + MSJUL + MSMAY + SLTYPWD + SLTYPCOD + 
             SLCONDNORM + SLCONDAB + OAQUAL3 + OAQUAL4 + OAQUAL5 + OAQUAL6 + 
             OAQUAL7 + OAQUAL8 + OAQUAL9 + OAQUAL10 + OACOND2 + OACOND3 + 
             OACOND4 + OACOND5 + OACOND7 + OACOND8 + OACOND9 + YB30 + 
             YB50 + YB60 + YB70 + YB80 + YB90 + YB00 + YRM90 + BMFB1 + 
             BMFB2 + BMHB1 + FB2 + FB3 + HB1 + KAG2 + FRPL1 + FRPL2 + 
             YRSOLD2009 + BTPTH + GFINUNF + BRAG4, data=hdatatrain)

summary(model3)