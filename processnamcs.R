# =============================================================================
# Import
# =============================================================================

source('/Users/sk792/DropboxHarvard/Projects/MassDPH/Code/icd.R') 

icdmakedec <- function(x){
	gsub("-","",paste0(substr(x,1,3),".",substr(x,4,5))) 
}
icdmakeraw <- function(x){
	gsub("-","",x) 
}

namcs02 <- haven::read_spss("namcs02-spss.sav")
namcs03 <- haven::read_spss("namcs03-spss.sav")
namcs04 <- haven::read_spss("namcs04-spss.sav")
namcs05 <- haven::read_spss("namcs05-spss.sav")
namcs06 <- haven::read_spss("namcs06-spss.sav")
namcs07 <- haven::read_spss("namcs07-spss.sav")
namcs08 <- haven::read_spss("namcs08-spss.sav")
namcs09 <- haven::read_spss("namcs09-spss.sav")
namcs10 <- haven::read_spss("namcs2010-spss.sav")
namcs11 <- haven::read_spss("namcs2011-spss.sav")
namcs12 <- haven::read_spss("namcs2012-spss.sav")
namcs13 <- haven::read_spss("namcs2013-spss.sav")
namcs14 <- haven::read_spss("namcs2014-spss.sav")
namcs15 <- haven::read_spss("namcs2015-spss.sav")

overallpopsizes <- c(
	283086156, #2002
	285519866, #2003
	288375857, #2004
	291155919, #2005
	294180154, #2006
	296302408, #2007
	298688159, #2008
	301570342, #2009
	303627880, #2010
	306378752, #2011
	308698958, #2012
	310929426, #2013
	313672860, #2014
	316286252  #2015
	)

popsizes <- data.frame(DT_YEAR=2002:2015, OVERALL=overallpopsizes)

# =============================================================================
# Select and format useful columns for NAMCS data
# =============================================================================

namcs02cut <- namcs02 %>% select(vmonth, age, sex, diag1, diag2, diag3, med, med1, med2, med3, med4, med5, med6, nummed, patwt, region) %>% 
	mutate(DT_YEAR=2002) %>% 
	rename(DT_MONTH=vmonth, AGE=age, SEX=sex, DIAG1=diag1, DIAG2=diag2, DIAG3=diag3, MED=med, MED1=med1, MED2=med2, MED3=med3, MED4=med4, MED5=med5, MED6=med6, NUMMED=nummed, PATWT=patwt, REGION=region)

namcs03cut <- namcs03 %>% select(vmonth, age, sex, diag1, diag2, diag3, med, med1, med2, med3, med4, med5, med6, nummed, patwt, region) %>%
	mutate(DT_YEAR=2003) %>%
	rename(DT_MONTH=vmonth, AGE=age, SEX=sex, DIAG1=diag1, DIAG2=diag2, DIAG3=diag3, MED=med, MED1=med1, MED2=med2, MED3=med3, MED4=med4, MED5=med5, MED6=med6, NUMMED=nummed, PATWT=patwt, REGION=region)

namcs04cut <- namcs04 %>% select(vmonth, age, sex, diag1, diag2, diag3, med, med1, med2, med3, med4, med5, med6, nummed, patwt, region) %>%
	mutate(DT_YEAR=2004) %>%
	rename(DT_MONTH=vmonth, AGE=age, SEX=sex, DIAG1=diag1, DIAG2=diag2, DIAG3=diag3, MED=med, MED1=med1, MED2=med2, MED3=med3, MED4=med4, MED5=med5, MED6=med6, NUMMED=nummed, PATWT=patwt, REGION=region)

namcs05cut <- namcs05 %>% select(VMONTH, AGE, SEX, DIAG1, DIAG2, DIAG3, MED, MED1, MED2, MED3, MED4, MED5, MED6, NUMMED, PATWT, REGION) %>%
	mutate(DT_YEAR=2005) %>%
	rename(DT_MONTH=VMONTH)

namcs06cut <- namcs06 %>% select(VMONTH, AGE, SEX, DIAG1, DIAG2, DIAG3, MED, MED1, MED2, MED3, MED4, MED5, MED6, NUMMED, PATWT, REGION) %>%
	mutate(DT_YEAR=2006) %>%
	rename(DT_MONTH=VMONTH)

namcs07cut <- namcs07 %>% select(VMONTH, AGE, SEX, DIAG1, DIAG2, DIAG3, MED, MED1, MED2, MED3, MED4, MED5, MED6, NUMMED, PATWT, REGION) %>%
	mutate(DT_YEAR=2007) %>%
	rename(DT_MONTH=VMONTH)

namcs08cut <- namcs08 %>% select(VMONTH, AGE, SEX, DIAG1, DIAG2, DIAG3, MED, MED1, MED2, MED3, MED4, MED5, MED6, NUMMED, PATWT, REGION) %>%
	mutate(DT_YEAR=2008) %>%
	rename(DT_MONTH=VMONTH)

namcs09cut <- namcs09 %>% select(VMONTH, AGE, SEX, DIAG1, DIAG2, DIAG3, MED, MED1, MED2, MED3, MED4, MED5, MED6, NUMMED, PATWT, REGION) %>%
	mutate(DT_YEAR=2009) %>%
	rename(DT_MONTH=VMONTH)

namcs10cut <- namcs10 %>% select(VMONTH, AGE, SEX, DIAG1, DIAG2, DIAG3, MED, MED1, MED2, MED3, MED4, MED5, MED6, NUMMED, PATWT, REGION) %>%
	mutate(DT_YEAR=2010) %>%
	rename(DT_MONTH=VMONTH)

namcs11cut <- namcs11 %>% select(VMONTH, AGE, SEX, DIAG1, DIAG2, DIAG3, MED, MED1, MED2, MED3, MED4, MED5, MED6, NUMMED, PATWT, REGION) %>%
	mutate(DT_YEAR=2011) %>%
	rename(DT_MONTH=VMONTH)

namcs12cut <- namcs12 %>% select(VMONTH, AGE, SEX, DIAG1, DIAG2, DIAG3, MED, MED1, MED2, MED3, MED4, MED5, MED6, NUMMED, PATWT, REGIONOFF) %>%
	mutate(DT_YEAR=2012) %>%
	rename(DT_MONTH=VMONTH, REGION=REGIONOFF)

namcs13cut <- namcs13 %>% select(VMONTH, AGE, SEX, DIAG1, DIAG2, DIAG3, MED, MED1, MED2, MED3, MED4, MED5, MED6, NUMMED, PATWT, REGIONOFF) %>%
	mutate(DT_YEAR=2013) %>%
	rename(DT_MONTH=VMONTH, REGION=REGIONOFF)

namcs14cut <- namcs14 %>% select(VMONTH, AGE, SEX, DIAG1, DIAG2, DIAG3, MED, MED1, MED2, MED3, MED4, MED5, MED6, NUMMED, PATWT, REGIONOFF) %>%
	mutate(DT_YEAR=2014) %>%
	rename(DT_MONTH=VMONTH, REGION=REGIONOFF)

namcs15cut <- namcs15 %>% select(VMONTH, AGE, SEX, DIAG1, DIAG2, DIAG3, MED, MED1, MED2, MED3, MED4, MED5, MED6, NUMMED, PATWT, REGIONOFF) %>%
	mutate(DT_YEAR=2015) %>%
	rename(DT_MONTH=VMONTH, REGION=REGIONOFF)

# =============================================================================
# Bind NAMCS data into a single data frame
# =============================================================================

namcs <- rbind(namcs02cut, 
	namcs03cut, 
	namcs04cut, 
	namcs05cut, 
	namcs06cut, 
	namcs07cut, 
	namcs08cut, 
	namcs09cut, 
	namcs10cut, 
	namcs11cut, 
	namcs12cut, 
	namcs13cut, 
	namcs14cut, 
	namcs15cut)
namcs$DT_MONTH <- as.numeric(namcs$DT_MONTH)
namcs$AGE <- as.numeric(namcs$AGE)
namcs$SEX <- as.numeric(namcs$SEX)
namcs$DIAG1 <- as.character(namcs$DIAG1)
namcs$DIAG2 <- as.character(namcs$DIAG2)
namcs$DIAG3 <- as.character(namcs$DIAG3)
namcs$MED <- as.numeric(namcs$MED)
namcs$MED1 <- as.character(namcs$MED1)
namcs$MED2 <- as.character(namcs$MED2)
namcs$MED3 <- as.character(namcs$MED3)
namcs$MED4 <- as.character(namcs$MED4)
namcs$MED5 <- as.character(namcs$MED5)
namcs$MED6 <- as.character(namcs$MED6)
namcs$NUMMED <- as.numeric(namcs$NUMMED)
namcs$PATWT <- as.numeric(namcs$PATWT)
namcs$REGION <- as.numeric(namcs$REGION)
namcs$DT_YEAR <- as.numeric(namcs$DT_YEAR)
namcs <- namcs %>% 
	mutate(DIAG1=icdmakeraw(DIAG1)) %>%
	mutate(DIAG2=icdmakeraw(DIAG2)) %>%
	mutate(DIAG3=icdmakeraw(DIAG3)) %>%
	mutate(DIAG1=case_when(DIAG1=="00000"~"",TRUE~DIAG1)) %>%
	mutate(DIAG2=case_when(DIAG2=="00000"~"",TRUE~DIAG2)) %>%
	mutate(DIAG3=case_when(DIAG3=="00000"~"",TRUE~DIAG3)) %>%
	left_join(fd_df, by=c("DIAG1"="ICD")) %>%
	rename(FDDX1=COND) %>%
	mutate(FDDX1=case_when(is.na(FDDX1)~"All other codes",TRUE~FDDX1)) %>%
	left_join(fd_df, by=c("DIAG2"="ICD")) %>%
	rename(FDDX2=COND) %>%
	mutate(FDDX2=case_when(is.na(FDDX2)~"All other codes",TRUE~FDDX2)) %>%
	left_join(fd_df, by=c("DIAG3"="ICD")) %>%
	rename(FDDX3=COND) %>%
	mutate(FDDX3=case_when(is.na(FDDX3)~"All other codes",TRUE~FDDX3)) %>% 
	left_join(popsizes, by="DT_YEAR") %>%
	rename(POPSIZE=OVERALL)

# =============================================================================
# Clean up workspace
# =============================================================================

rm(namcs02)
rm(namcs03)
rm(namcs04)
rm(namcs05)
rm(namcs06)
rm(namcs07)
rm(namcs08)
rm(namcs09)
rm(namcs10)
rm(namcs11)
rm(namcs12)
rm(namcs13)
rm(namcs14)
rm(namcs15)

rm(namcs02cut)
rm(namcs03cut)
rm(namcs04cut)
rm(namcs05cut)
rm(namcs06cut)
rm(namcs07cut)
rm(namcs08cut)
rm(namcs09cut)
rm(namcs10cut)
rm(namcs11cut)
rm(namcs12cut)
rm(namcs13cut)
rm(namcs14cut)
rm(namcs15cut)
