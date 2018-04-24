setwd("C:/Users/rvasi/Desktop/R Activities/Using Secondary Data")
install.packages("foreign")
install.packages("survey")
install.packages("readxl")
library(readxl)
library(foreign)
library(survey)

##Importing Data

#AtlasPlus Data (Incidence of Congenital Syphilis in Louisiana Counties for 2015)
  #go to https://www.cdc.gov/nchhstp/Atlas/ and select "STD" and "Tables"
  #for indicator, check "Congenital Syphilis" and click next
  #for geography, click "county". Scroll down and check off the "Louisiana" box to select all Louisiana counties
  #for year, scroll down and check "2015"    
  #Continue clicking next (do not group by geography) and click "Create My Table"
  #Click "underlying data" to see the expanded table and then click "export" to download.
  #Move this file to your current working directory and import it by the appropriate file name
    CS_INCIDENCE<-read.csv("AtlasPlusTableData.csv")
#County Health Rankings for Louisiana Counties in 2015
    download.file("http://www.countyhealthrankings.org/sites/default/files/state/downloads/2015%20County%20Health%20Rankings%20Louisiana%20Data%20-%20v3.xls",destfile = "./2015 County Health Rankings Louisiana Data.xls", mode="wb")
      #citation:https://stackoverflow.com/questions/15250257/r-download-file-issue-with-excel-workbook
    COUNTY_RANKS<-read_excel("2015 County Health Rankings Louisiana Data.xls",sheet="Outcomes & Factors Rankings")
    RANKED_MEAS<-read_excel("2015 County Health Rankings Louisiana Data.xls",sheet="Ranked Measure Data")  
    ADD_MEAS<-read_excel("2015 County Health Rankings Louisiana Data.xls", sheet="Additional Measure Data")

##Cleaning Up Imported Data
    
#Congenital Syphilis Incidence
  colnames(CS_INCIDENCE)=c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11")
  CS_INCIDENCE<-CS_INCIDENCE[,!names(CS_INCIDENCE) %in% c("x1","x2","x4","x5","x6","x7","x8","x10","x11"), drop=F]
    #citation: week 3 R activity
  #note: standardizing county names will come later in data merge
  colnames(CS_INCIDENCE)=c("COUNTY","RATE")
  CS_INCIDENCE=CS_INCIDENCE[-2,]
  CS_INCIDENCE=CS_INCIDENCE[-1,]
  #changing factor to numeric
  CS_INCIDENCE$RATE<-as.numeric(levels(CS_INCIDENCE$RATE))[CS_INCIDENCE$RATE]
    #citation: https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information
  
#County Overall Rankings
  colnames(COUNTY_RANKS)<-c("FIPS","STATE","COUNTY","HO_Z-SCORE","RANK_HEALTHOUT","HF_Z-SCORE","RANK_HEALTHFAC")
  COUNTY_RANKS<-COUNTY_RANKS[-2,]
  COUNTY_RANKS<-COUNTY_RANKS[-1,]
  COUNTY_RANKS$"HO_Z-SCORE"<-NULL
  COUNTY_RANKS$"HF_Z-SCORE"<-NULL
  COUNTY_RANKS$"FIPS"<-NULL
  COUNTY_RANKS$"STATE"<-NULL
  COUNTY_RANKS$"RANK_HEALTHFAC"<-NULL
  COUNTY_RANKS$RANK_HEALTHOUT<-as.numeric(COUNTY_RANKS$RANK_HEALTHOUT)
  #standardizing county names
  CS_INCIDENCE$COUNTY<-COUNTY_RANKS$COUNTY
  
#Ranked Measures
  RANKED_MEAS<-RANKED_MEAS[,c(3,74)]
  colnames(RANKED_MEAS)<-c("COUNTY","RATIO_PCP")
  RANKED_MEAS[2,"COUNTY"]="LOUISIANA_OVERALL"
  RANKED_MEAS<-RANKED_MEAS[-1,]
  RANKED_MEAS<-RANKED_MEAS[-67,]
  RANKED_MEAS<-RANKED_MEAS[-66,]
  
#Saving Louisiana overall data
  LA_OVERALL<-data.frame()
  LA_OVERALL<-RANKED_MEAS[0:1,2]
  #now removing LA Overall data from county-specific data
  RANKED_MEAS<-RANKED_MEAS[-1,]
  
#Additional Measures
  ADD_MEAS<-ADD_MEAS[,c(3,4,58,67,71)]
  colnames(ADD_MEAS)<-c("COUNTY","POP_COUNTY","PCT_UNINSURED","PCT_CNSDDTC","RATIO_OTHERPCP")
  ADD_MEAS<-ADD_MEAS[-1,]
  ADD_MEAS[1,"COUNTY"]="LOUISIANA_OVERALL"
  LA_OVERALL<-merge(LA_OVERALL,ADD_MEAS[0:1,2:5])
  ADD_MEAS<-ADD_MEAS[-1,]
    #NOTE: "'^This data was updated on March 29, 2017. Please see http://www.countyhealthrankings.org/content/data-changes for more information." under OTHER PCP
  ADD_MEAS<-ADD_MEAS[-66,]
  ADD_MEAS<-ADD_MEAS[-65,]
  
##Putting Together Data
  DATA<-merge(CS_INCIDENCE,COUNTY_RANKS)
  DATA<-merge(DATA,RANKED_MEAS)
  DATA<-merge(DATA,ADD_MEAS)
  
##Cleaning Up Environment
  remove(ADD_MEAS)
  remove(COUNTY_RANKS)
  remove(CS_INCIDENCE)
  remove(RANKED_MEAS)

##Imputation (Single)
  #PCT_CNSDDTC
  DATA$PCT_CNSDDTC[is.na(DATA$PCT_CNSDDTC)]<-LA_OVERALL$PCT_CNSDDTC
  #RATIO_PCP
  DATA$RATIO_PCP[is.na(DATA$RATIO_PCP)]<-LA_OVERALL$RATIO_PCP
  
##Variable Creation [NUMERICAL]
  #PCP and OtherPCP Combined Ratio
  #In order to figure out the combined ratio, we need to look at the county pop/ratio number to get the number of PCP and OtherPCP
  #Then the combined ratio, standardized to 1, will be County Pop:(PCP number + OtherPCP)
  RATIO_PCP_NUM<-0
  for(i in 1:64) {
    RATIO_PCP_NUM[i]<-strsplit(DATA$RATIO_PCP,':')[[i]][1]
  }
  RATIO_PCP_NUM<-as.numeric(RATIO_PCP_NUM)
  
  RATIO_OTHERPCP_NUM<-0
  for(i in 1:64) {
    RATIO_OTHERPCP_NUM[i]<-strsplit(DATA$RATIO_OTHERPCP,':')[[i]][1]
  }
  RATIO_OTHERPCP_NUM<-as.numeric(RATIO_OTHERPCP_NUM)
  
  DATA$POP_COUNTY<-as.numeric(DATA$POP_COUNTY)
  NUMBER_PCP<-DATA$POP_COUNTY/RATIO_PCP_NUM
  NUMBER_OTHERPCP<-DATA$POP_COUNTY/RATIO_OTHERPCP_NUM
  
  #Creating combined ratio, standardizing to 1
  DATA$RATIO_PCPCOMBINED<-DATA$POP_COUNTY/(NUMBER_PCP+NUMBER_OTHERPCP)

##Variable Creation [CATEGORICAL]
  #Level of % of Adults(<65) Who Could Not See Doctor Due to Cost in Last 12 Months
  #Low, Middle, High
  labels_levels<-c("Low","Medium","High")
  DATA$PCT_CNSDDTC<-as.numeric(as.character(DATA$PCT_CNSDDTC))
  table(DATA$LEVEL_CNSDDTC<-cut(DATA$PCT_CNSDDTC,quantile(DATA$PCT_CNSDDTC, probs=seq(0,1,1/3),na.rm=TRUE),include.lowest=TRUE,labels=labels_levels))
  
  #Level of Non-Zero Congenital Syphilis Incidence
  #Low, Medium, High
    #Limitation: Counties with small populations are more likely to have unstable rates for rare events
  RATE_NONZERO<-DATA$RATE[DATA$RATE>0]; DATA$RATE
  #using midway points of non-zero incidence, we can create four breaks for levels of incidence overall: 
  #zero incidence="Zero", below or equal to Q1="Low", above Q1 and less than or equal to Q3="Medium", above Q3="High"
  summary(RATE_NONZERO)
  quantile(RATE_NONZERO,probs=seq(0,1,1/3))
  #Q1=45.45, Q3=265.80
  breaks<-c(0,0.1,45.45,265.80,800)
  labels2=c("Zero","Low","Medium","High")
  DATA$LEVEL_INCIDENCE<-cut(DATA$RATE,breaks,include.lowest = TRUE,labels=labels2)
  table(DATA$LEVEL_INCIDENCE)
  
  #Level of Percentage Uninsured
  #Low, Middle, High
  DATA$PCT_UNINSURED<-as.numeric(DATA$PCT_UNINSURED)
  DATA$LEVEL_UNINSURED<-cut(DATA$PCT_UNINSURED,quantile(DATA$PCT_UNINSURED,probs=seq(0,1,1/3),na.rm=TRUE),include.lowest=TRUE,labels=labels_levels)
  table(DATA$LEVEL_UNINSURED)
  
  #Level of PCP Combined Ratio
  DATA$LEVEL_PCPCOMBINED<-cut(DATA$RATIO_PCPCOMBINED,quantile(DATA$RATIO_PCPCOMBINED,probs=seq(0,1,1/3),na.rm=TRUE),include.lowest=TRUE,labels=labels_levels)
  table(DATA$LEVEL_PCPCOMBINED)
  
  #Rank of Incidence Rates
  DATA$RANK_INCIDENCE=rank(DATA$RATE)
  
##Significance Tests
  #Correlation tests for ranks: Spearman (incidence vs rank)
  cor.test(DATA$RANK_HEALTHOUT,DATA$RANK_INCIDENCE,method="spearman",exact=TRUE) #p-value=0.5282
  
  #T-tests (means of incidence across levels)
  #CNSDDTC Creating subsets of each combination of levels
    CNSDDTC_LM=subset(DATA,DATA$LEVEL_CNSDDTC=="Low"|DATA$LEVEL_CNSDDTC=="Medium")
    CNSDDTC_LH=subset(DATA,DATA$LEVEL_CNSDDTC=="Low"|DATA$LEVEL_CNSDDTC=="High")
    CNSDDTC_MH=subset(DATA,DATA$LEVEL_CNSDDTC=="Medium"|DATA$LEVEL_CNSDDTC=="High")
  
    t.test(CNSDDTC_LM$RATE~CNSDDTC_LM$LEVEL_CNSDDTC) #p-value=0.8934
    t.test(CNSDDTC_LH$RATE~CNSDDTC_LH$LEVEL_CNSDDTC) #p-value=0.5534
    t.test(CNSDDTC_MH$RATE~CNSDDTC_MH$LEVEL_CNSDDTC) #p=value=0.6053
  
  #PCP Combined Level
    PCPCOMBINED_LM=subset(DATA,DATA$LEVEL_PCPCOMBINED=="Low"|DATA$LEVEL_PCPCOMBINED=="Medium")
    t.test(PCPCOMBINED_LM$RATE~PCPCOMBINED_LM$LEVEL_PCPCOMBINED) #0.1726

    PCPCOMBINED_LH=subset(DATA,DATA$LEVEL_PCPCOMBINED=="Low"|DATA$LEVEL_PCPCOMBINED=="High")
    t.test(PCPCOMBINED_LH$RATE~PCPCOMBINED_LH$LEVEL_PCPCOMBINED) #0.02359

    PCPCOMBINED_MH=subset(DATA,DATA$LEVEL_PCPCOMBINED=="Medium"|DATA$LEVEL_PCPCOMBINED=="High")
    t.test(PCPCOMBINED_MH$RATE~PCPCOMBINED_MH$LEVEL_PCPCOMBINED) #0.09572

  #Uninsured Level
    UNINSURED_LM=subset(DATA,DATA$LEVEL_UNINSURED=="Low"|DATA$LEVEL_UNINSURED=="Medium")
    t.test(UNINSURED_LM$RATE~UNINSURED_LM$LEVEL_UNINSURED) #0.9473
    
    UNINSURED_LH=subset(DATA,DATA$LEVEL_UNINSURED=="Low"|DATA$LEVEL_UNINSURED=="High")
    t.test(UNINSURED_LH$RATE~UNINSURED_LH$LEVEL_UNINSURED) #0.47
    
    UNINSURED_MH=subset(DATA,DATA$LEVEL_UNINSURED=="Medium"|DATA$LEVEL_UNINSURED=="High")
    t.test(UNINSURED_MH$RATE~UNINSURED_MH$LEVEL_UNINSURED) #0.4747
  
  #Incidence Level
    INCIDENCE_LM=subset(DATA,DATA$LEVEL_INCIDENCE=="Low"|DATA$LEVEL_INCIDENCE=="Medium")
    t.test(INCIDENCE_LM$RANK_HEALTHOUT~INCIDENCE_LM$LEVEL_INCIDENCE) #p=0.465
    t.test(INCIDENCE_LM$POP_COUNTY~INCIDENCE_LM$LEVEL_INCIDENCE) #p=0.4366
    t.test(INCIDENCE_LM$PCT_UNINSURED~INCIDENCE_LM$LEVEL_INCIDENCE) #p=0.4152
    t.test(INCIDENCE_LM$PCT_CNSDDTC~INCIDENCE_LM$LEVEL_INCIDENCE) #p=0.5437
    t.test(INCIDENCE_LM$RATIO_PCPCOMBINED~INCIDENCE_LM$LEVEL_INCIDENCE) #p=0.5569
    
    INCIDENCE_LH=subset(DATA,DATA$LEVEL_INCIDENCE=="Low"|DATA$LEVEL_INCIDENCE=="High")
    t.test(INCIDENCE_LH$RANK_HEALTHOUT~INCIDENCE_LH$LEVEL_INCIDENCE) #p=0.1831
    t.test(INCIDENCE_LH$POP_COUNTY~INCIDENCE_LH$LEVEL_INCIDENCE) #p=0.05036
    t.test(INCIDENCE_LH$PCT_UNINSURED~INCIDENCE_LH$LEVEL_INCIDENCE) #0.1445
    t.test(INCIDENCE_LH$PCT_CNSDDTC~INCIDENCE_LH$LEVEL_INCIDENCE)#0.201
    t.test(INCIDENCE_LH$RATIO_PCPCOMBINED~INCIDENCE_LH$LEVEL_INCIDENCE) #0.6675
    
    INCIDENCE_MH=subset(DATA,DATA$LEVEL_INCIDENCE=="Medium"|DATA$LEVEL_INCIDENCE=="High")
    t.test(INCIDENCE_MH$RANK_HEALTHOUT~INCIDENCE_MH$LEVEL_INCIDENCE) #0.3869
    t.test(INCIDENCE_MH$POP_COUNTY~INCIDENCE_MH$LEVEL_INCIDENCE) #0.1154
    t.test(INCIDENCE_MH$PCT_UNINSURED~INCIDENCE_MH$LEVEL_INCIDENCE) #0.04601
    t.test(INCIDENCE_MH$PCT_CNSDDTC~INCIDENCE_MH$LEVEL_INCIDENCE) #0.2516
    t.test(INCIDENCE_MH$RATIO_PCPCOMBINED~INCIDENCE_MH$LEVEL_INCIDENCE) #0.9722
    
    INCIDENCE_ZL=subset(DATA,DATA$LEVEL_INCIDENCE=="Zero"|DATA$LEVEL_INCIDENCE=="Low")
    t.test(INCIDENCE_ZL$RANK_HEALTHOUT~INCIDENCE_ZL$LEVEL_INCIDENCE) #0.1595
    t.test(INCIDENCE_ZL$POP_COUNTY~INCIDENCE_ZL$LEVEL_INCIDENCE) #0.03491
    t.test(INCIDENCE_ZL$PCT_UNINSURED~INCIDENCE_ZL$LEVEL_INCIDENCE) #0.2325
    t.test(INCIDENCE_ZL$PCT_CNSDDTC~INCIDENCE_ZL$LEVEL_INCIDENCE) #0.005581
    t.test(INCIDENCE_ZL$RATIO_PCPCOMBINED~INCIDENCE_ZL$LEVEL_INCIDENCE) #0.0004884
    
    INCIDENCE_ZM=subset(DATA,DATA$LEVEL_INCIDENCE=="Zero"|DATA$LEVEL_INCIDENCE=="Medium")
    t.test(INCIDENCE_ZM$RANK_HEALTHOUT~INCIDENCE_ZM$LEVEL_INCIDENCE) #0.3605
    t.test(INCIDENCE_ZM$POP_COUNTY~INCIDENCE_ZM$LEVEL_INCIDENCE) #0.0465
    t.test(INCIDENCE_ZM$PCT_UNINSURED~INCIDENCE_ZM$LEVEL_INCIDENCE) #0.04034
    t.test(INCIDENCE_ZM$PCT_CNSDDTC~INCIDENCE_ZM$LEVEL_INCIDENCE) #0.003615
    t.test(INCIDENCE_ZM$RATIO_PCPCOMBINED~INCIDENCE_ZM$LEVEL_INCIDENCE) #0.001168
    
    INCIDENCE_ZH=subset(DATA,DATA$LEVEL_INCIDENCE=="Zero"|DATA$LEVEL_INCIDENCE=="High")
    t.test(INCIDENCE_ZH$RANK_HEALTHOUT~INCIDENCE_ZH$LEVEL_INCIDENCE) #0.7186
    t.test(INCIDENCE_ZH$POP_COUNTY~INCIDENCE_ZH$LEVEL_INCIDENCE) #0.529
    t.test(INCIDENCE_ZH$PCT_UNINSURED~INCIDENCE_ZH$LEVEL_INCIDENCE) #0.4207
    t.test(INCIDENCE_ZH$PCT_CNSDDTC~INCIDENCE_ZH$LEVEL_INCIDENCE) #0.642
    t.test(INCIDENCE_ZH$RATIO_PCPCOMBINED~INCIDENCE_ZH$LEVEL_INCIDENCE) #0.01631
  
  #Chi-Squared Test
  chisq.test(DATA$LEVEL_INCIDENCE,DATA$LEVEL_CNSDDTC) #0.0331
  chisq.test(DATA$LEVEL_INCIDENCE,DATA$LEVEL_UNINSURED) #0.2429  
  chisq.test(DATA$LEVEL_INCIDENCE,DATA$LEVEL_PCPCOMBINED) #0.04332