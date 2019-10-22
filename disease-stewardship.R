# Supplemental code for "Reductions in observed disease and improved antibiotic stewardship contributed to a decline in outpatient antibiotic prescribing in Massachusetts between 2011 and 2015", by SM Kissler, RM Klevens, ML Barnett, and YH Grad. 

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 3.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


# =============================================================================
# Import
# =============================================================================

library(tidyverse,quietly = T)
library(scales)
library(lubridate)
library(icd)

load("em_red.RData")
load("em_red_exec.RData")
load("members_red.RData")
load("abx_claims_red.RData")

restricttimespan <- function(x){
	x %>%
	filter(DT_YEAR >= 2011 & DT_YEAR <= 2015) %>%
	filter(DT_YEAR <= 2014 | (DT_YEAR==2015 & DT_MONTH<=9))
}

members_red <- members_red %>% filter(AGEGRP!="65+") %>% restricttimespan
abx_claims_red <- abx_claims_red %>% filter(AGEGRP!="65+") %>% restricttimespan

members_red_overall <- members_red %>% 
	group_by(DT_MONTH, DT_YEAR) %>%
	summarise(n_memb=sum(n_memb)) 

namcs02 <- read.csv('namcs02.csv')
namcs03 <- read.csv('namcs03.csv')
namcs04 <- read.csv('namcs04.csv')
namcs05 <- read.csv('namcs05.csv')
namcs06 <- read.csv('namcs06.csv')
namcs07 <- read.csv('namcs07.csv')
namcs08 <- read.csv('namcs08.csv')
namcs09 <- read.csv('namcs09.csv')
namcs10 <- read.csv('namcs10.csv')
namcs11 <- read.csv('namcs11.csv')
namcs12 <- read.csv('namcs12.csv')
namcs13 <- read.csv('namcs13.csv')
namcs14 <- read.csv('namcs14.csv')
namcs15 <- read.csv('namcs15.csv')

namcs <- rbind(namcs02,
	namcs03,
	namcs04,
	namcs05,
	namcs06,
	namcs07,
	namcs08,
	namcs09,
	namcs10,
	namcs11,
	namcs12,
	namcs13,
	namcs14,
	namcs15)

source('icd.R') 

# =============================================================================
# Define key variables and functions
# =============================================================================

fdnames <- c("Miscellaneous bacterial infections",
	"Pneumonia",
	"Urinary tract infections (UTI)",
	"Acne",
	"Gastrointestinal infections",
	"Pharyngitis",
	"Sinusitis",
	"Skin cutaneous and mucosal infections",
	"Suppurative otitis media",
	"Asthma allergy",
	"Brochitis bronchiolitis",
	"Influenza",
	"Miscellaneous other infections",
	"Non-suppurative otitis media",
	"Other gastrointestinal conditions",
	"Other skin cutaneous and mucosal conditions",
	"Other genitourinary conditions",
	"Viral pneumonia",
	"Viral upper respiratory infection (URI)",
	"Other respiratory conditions",
	"All other codes")

fdnames_cleaned <- c("Misc. bacterial infections",
	"Pneumonia",
	"Urinary tract infections (UTI)",
	"Acne",
	"Gastrointestinal infections",
	"Pharyngitis",
	"Sinusitis",
	"Skin, cutaneous and mucosal infections",
	"Suppurative otitis media",
	"Asthma, allergy",
	"Bronchitis/bronchiolitis",
	"Influenza",
	"Miscellaneous other infections",
	"Non-suppurative otitis media",
	"Other gastrointestinal conditions",
	"Other skin, cutaneous and mucosal conditions",
	"Other genitourinary conditions",
	"Viral pneumonia",
	"Viral upper respiratory infection (URI)",
	"Other respiratory conditions",
	"All other codes")

ipdinds <- c(2, 7, 9, 11, 14)

makeYMD <- function(x){
	x %>%
	mutate(dummyday=1) %>%
	mutate(YMD=ymd(paste(DT_YEAR,"/",DT_MONTH,"/",dummyday,sep=""))) %>%
	select(-dummyday)
}

minimize <- list(theme_minimal(), 
		theme(text=element_text(size=18)),
		theme(axis.text.x=element_text(size=16))
		)

groupages <- function(x){ 
	x %>% 
		mutate(AGEGRP=case_when(
			AGEGRP=="00-02"~"00-05",
			AGEGRP=="03-05"~"00-05",
			AGEGRP=="06-09"~"06-19",
			AGEGRP=="10-14"~"06-19",
			AGEGRP=="15-19"~"06-19",
			AGEGRP=="20-29"~"20-49",
			AGEGRP=="30-39"~"20-49",
			AGEGRP=="40-49"~"20-49",
			AGEGRP=="50-59"~"50-64",
			AGEGRP=="60-64"~"50-64",
			AGEGRP=="65+"~"65+",
			TRUE~"Unknown"
			))
}

DTtoindex<- data.frame(
  DT_YEAR=unlist(map(2011:2015,rep,12)), 
  DT_MONTH=rep(1:12,5), 
  index=1:60)

DTtoindex_0 <- data.frame(
  DT_YEAR=unlist(map(2011:2015,rep,12)), 
  DT_MONTH=rep(1:12,5), 
  index=0:59)


slopeci <- function(x){
	x %>% 
		predict(interval="confidence") %>% 
		as_tibble() %>%
		filter(row_number() %in% c(1,n())) %>% 
		mutate(pt=c("first","last")) %>%
		gather("estimate","value",fit,lwr,upr,-pt) %>%
		mutate(slopeestimate=case_when(
			estimate=="fit"~"mean",
			pt=="first" & estimate=="upr"~"lower",
			pt=="last" & estimate=="lwr"~"lower",
			pt=="first" & estimate=="lwr"~"upper",
			pt=="last" & estimate=="upr"~"upper",
			)) %>%
		split(.$slopeestimate) %>%
		map(~ select(., pt, value)) %>%
		map(~ spread(., pt, value)) %>%
		map(~ mutate(., pctchange=(last-first)/first*100)) %>%
		map(~ select(., pctchange)) %>%
		bind_rows(.id="estimate")
}

slopeci_raw <- function(x){
	x %>% 
		predict(interval="confidence") %>% 
		as_tibble() %>%
		filter(row_number() %in% c(1,n())) %>% 
		mutate(pt=c("first","last")) %>%
		gather("estimate","value",fit,lwr,upr,-pt) %>%
		mutate(slopeestimate=case_when(
			estimate=="fit"~"mean",
			pt=="first" & estimate=="upr"~"lower",
			pt=="last" & estimate=="lwr"~"lower",
			pt=="first" & estimate=="lwr"~"upper",
			pt=="last" & estimate=="upr"~"upper",
			)) %>%
		split(.$slopeestimate) %>%
		map(~ select(., pt, value)) %>%
		map(~ spread(., pt, value)) %>%
		map(~ mutate(., change=last-first)) %>%
		map(~ select(., change)) %>%
		bind_rows(.id="estimate")
}

bpll <- function(df, t, xcol, ycol){
	x <- df[,xcol]
	y <- df[,ycol]
	z <- optim(c(1, 0, 0), bpfunc_sse, t=t, x=x, y=y)
	sseval <- bpfunc_sse(z$par,t,x,y)
	return(-sseval)
}

bpfunc <- function(params,t,x){
	b0 <- params[1]
	b1 <- params[2]
	b2 <- params[3]
	out <- b0 + b1*x + b2*(x-t)*as.numeric(x>t)
	return(out)
}

bpfunc_sse <- function(params,t,x,y){
	out <- sum((y - bpfunc(params,t,x))^2)
}

bpfit <- function(df, t, xcol, ycol){
	z <- optim(c(1, 0, 0), bpfunc_sse, t=t, x=df[,xcol], y=df[,ycol])
	xfitvals <- seq(min(df[,xcol]), max(df[,xcol]), length.out=100)
	yfitvals <- bpfunc(z$par, t, xfitvals)
	fitdf <- data.frame(xcoltemp=xfitvals, ycoltemp=yfitvals) %>% mutate(data.type="fit")
	names(fitdf)[1] <- xcol
	names(fitdf)[2] <- ycol
	df <- df %>% mutate(data.type="raw") %>% bind_rows(fitdf)
	return(df)
}

bpfit_lite <- function(df, t, xcol, ycol){
	z <- optim(c(1, 0, 0), bpfunc_sse, t=t, x=df[,xcol], y=df[,ycol])
	xfitvals <- seq(min(df[,xcol]), max(df[,xcol]), length.out=100)
	yfitvals <- bpfunc(z$par, t, xfitvals)
	fitdf <- data.frame(xcoltemp=xfitvals, ycoltemp=yfitvals)
	names(fitdf)[1] <- xcol
	names(fitdf)[2] <- ycol
	return(fitdf)
}

# =============================================================================
# Antibiotic prescribing rate, overall and by age group
# =============================================================================

figabx_overall <- abx_claims_red %>% 
	ungroup() %>%
	group_by(DT_YEAR, DT_MONTH) %>%
	summarise(NRX=sum(NRX)) %>%
	left_join(members_red_overall, by=c("DT_YEAR","DT_MONTH")) %>%
	mutate(rxpkp=NRX/n_memb*1000) %>%
	makeYMD %>%
	ggplot(aes(x=YMD, y=rxpkp)) + 
		geom_point(alpha=0.3) + 
		geom_line(alpha=0.3) + 
		geom_line(stat="smooth", method="loess", span=0.2, size=0.8, alpha=0.8) +
		geom_line(stat="smooth", method="lm") + 
		expand_limits(y=0) + 
		minimize + 
		# theme(text=element_text(size=16)) + 
		scale_x_date(labels = date_format("%Y"), breaks='1 year', limits=c(as.Date("2011-01-01"), as.Date("2016-01-01"))) + 
		labs(x="Month", y="Fills per 1,000 beneficiaries")

figabx_age <- abx_claims_red %>% 
	ungroup() %>%
	groupages %>%
	group_by(DT_YEAR, DT_MONTH, AGEGRP) %>%
	summarise(NRX=sum(NRX)) %>%
	left_join((members_red %>% ungroup() %>% groupages %>% group_by(DT_MONTH, DT_YEAR, AGEGRP) %>% summarise(n_memb=sum(n_memb))), by=c("DT_YEAR","DT_MONTH","AGEGRP")) %>%
	mutate(rxpkp=NRX/n_memb*1000) %>%
	makeYMD %>%
	rename(Age=AGEGRP) %>% 
	ggplot(aes(x=YMD, y=rxpkp, col=Age)) + 
		geom_point(alpha=0.3) + 
		geom_line(alpha=0.3) + 
		geom_line(stat="smooth", method="loess", span=0.2, size=0.8, alpha=0.8) +
		expand_limits(y=0) + 
		minimize + 
		scale_x_date(labels = date_format("%Y"), breaks=c(as.Date("2011-01-01"),as.Date("2012-01-01"),as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01")), limits=c(as.Date("2011-01-01"), as.Date("2016-01-01"))) + 
		labs(x="", y="Fills per 1,000 beneficiaries per age group") + 
		scale_color_manual(values=c("blue","red","black","magenta"))

# =============================================================================
# Executive diagnosis rates with/without detrending
# =============================================================================

# ---------- Plot incidence of observed disease, no detrending: ---------- 

# Find regression slopes and significances: 
siglabels <- list()
for(cond in 1:length(fdnames)){
	em_red_formatted <- em_red %>% 
		filter(FDDX==fdnames[[cond]]) %>% 
		group_by(DT_YEAR, DT_MONTH) %>% 
		summarise(NVISITS=sum(NVISITS)) %>% 
		left_join(members_red_overall, by=c("DT_YEAR","DT_MONTH")) %>%
		mutate(vpkp=NVISITS/n_memb*1000) %>% 
		left_join(DTtoindex, by=c("DT_YEAR", "DT_MONTH"))

	em_lm <- lm(vpkp~index, data=em_red_formatted)
	pval <- summary(em_lm)$coefficients[8]
	if(pval < 0.05){
		pctchange <- paste0("Change of ",as.character(round((last(predict(em_lm)) - first(predict(em_lm))) / (first(predict(em_lm))) * 100, digits=1)),"%")	
	} else {
		pctchange <- "No significant change"
	}
	siglabels[[cond]] <- paste0(pctchange,"\n(p = ", signif(pval, digits=2), "; ")
}

# Generate confidence intervals for the delcines:
slopelabels <- list()
for(cond in 1:length(fdnames)){
	em_red_formatted <- em_red %>% 
		filter(FDDX==fdnames[[cond]]) %>% 
		group_by(DT_YEAR, DT_MONTH) %>% 
		summarise(NVISITS=sum(NVISITS)) %>% 
		left_join(members_red_overall, by=c("DT_YEAR","DT_MONTH")) %>%
		mutate(vpkp=NVISITS/n_memb*1000) %>% 
		left_join(DTtoindex, by=c("DT_YEAR", "DT_MONTH"))

	em_lm <- lm(vpkp~index, data=em_red_formatted)
	luslope <- slopeci(em_lm)
	slopelabels[[cond]] <- paste0("95% CI: (",round(filter(luslope, estimate=="lower")$pctchange, digits=1),"%, ",round(filter(luslope, estimate=="upper")$pctchange, digits=1),"%)")
}


# Plot obserbed disease incidence for each condition:
fdplots <- list()
for(cond in 1:length(fdnames)){
	fdplots[[cond]] <- em_red %>% 
		filter(FDDX==fdnames[cond]) %>% 
		group_by(DT_YEAR, DT_MONTH) %>%
		summarise(NVISITS=sum(NVISITS)) %>%
		merge(members_red_overall, by=c("DT_MONTH","DT_YEAR")) %>% 
		makeYMD %>%
		ggplot(aes(x=YMD, y=NVISITS/n_memb*1000)) + #
			geom_point(alpha=0.3) + 
			geom_line(alpha=0.3) + 
			geom_line(stat="smooth",method="loess",span=0.2,size=0.8,col="black") +
			geom_line(stat="smooth",method="lm",size=0.8,col="black") + 
			expand_limits(y=0) + 
			scale_x_date(labels = date_format("%Y"), breaks='1 year', limits=c(as.Date("2011-01-01"), as.Date("2016-01-01"))) + 
			labs(x="Month", y="Visits per 1,000 beneficiaries", title=fdnames_cleaned[cond], subtitle=paste0(siglabels[[cond]]," ", slopelabels[[cond]],")")) + 
			minimize
}

# ---------- Now, repeat with detrending: ---------- 

# Generate a functon to easily mutate visits per 1,000 people (vpkp):
make_vpkp <- function(x){
	x %>%
	group_by(DT_YEAR, DT_MONTH, EXECDX) %>% 
	summarise(NVISITS=sum(NVISITS)) %>% 
	restricttimespan %>%
	left_join(members_red_overall, by=c("DT_YEAR","DT_MONTH")) %>%
	mutate(vpkp=NVISITS/n_memb*1000) %>%
	select(DT_YEAR, DT_MONTH, EXECDX, vpkp)
}

# Calculate linear regressions for observed disease slopes:
disease_lms <- em_red_exec %>% 
	# Reduce to visits per 1000 people per year/month/condition:
	make_vpkp %>%
	# Rename the empty condition to "None", to play nicely with lists below:
	mutate(EXECDX=case_when(EXECDX==""~"None",TRUE~EXECDX)) %>%
	# Add an index for months, for lm (starting with 0 for 1 Jan 2011):
	left_join(DTtoindex_0, by=c("DT_YEAR", "DT_MONTH")) %>%
	# Split into lists by executive diagnosis:
	split(.$EXECDX) %>%
	# Regress visits on month for each condition:
	map(~ lm(vpkp ~ index, data=.)) %>%
	# Represent lm output as a data frame
	map_dfr(broom::tidy, .id="EXECDX") %>%
	# Rename regression outputs
	mutate(term=case_when(term=="(Intercept)"~"intercept", term=="index"~"slope")) %>%
	# Convert the "None" diagnosis back to "":
	mutate(EXECDX=case_when(EXECDX=="None"~"",TRUE~EXECDX)) %>%
	# Select just the diagnosis, coefficient name, best-fit value, and p-value:
	select(EXECDX, term, estimate, p.value)

# Calculate mean predictions for the disease slopes: 
disease_predictions <- em_red_exec %>% 
	# Reduce to visits per 1000 people per year/month/condition:
	make_vpkp %>%
	# Rename the empty condition to "None", to play nicely with lists below:
	mutate(EXECDX=case_when(EXECDX==""~"None",TRUE~EXECDX)) %>%
	# Add an index for months, for lm (starting with 0 for 1 Jan 2011):
	left_join(DTtoindex_0, by=c("DT_YEAR", "DT_MONTH")) %>%
	# Split into lists by executive diagnosis:
	split(.$EXECDX) %>%
	# Store linear model predictions for each condition:
	map(~ data.frame(
		index=.$index, 
		prediction=predict(lm(vpkp ~ index, data=.))
		)) %>%
	# Convert the month indices back to DT_YEAR and DT_MONTH:
	map(~ merge(.,DTtoindex_0, by="index")) %>%
	# Convert list into data frame:
	bind_rows(.id="EXECDX") %>%
	# Select year, month, diagnosis, and predicted value from the linear model:
	select(DT_YEAR, DT_MONTH, EXECDX, prediction) %>%
	# Convert the "None" diagnosis back to "":
	mutate(EXECDX=case_when(EXECDX=="None"~"", TRUE~EXECDX)) 

# Detrend the mean predictions for the slope:
disease_predictions_detrended <- disease_lms %>% 
	# Rename the empty condition to "None", to play nicely with lists below:
	mutate(EXECDX=case_when(EXECDX==""~"None",TRUE~EXECDX)) %>%
	# Set all significant slopes to 0 (to detrend):
	mutate(estimate=case_when(term=="slope" & p.value<=0.05 ~ 0,
							  TRUE ~ estimate)) %>%
	# Don't need the p-value anymore, so get rid of it: 
	select(-p.value) %>%
	# Give the slope and intercept their own columns:
	spread(key=term, value=estimate) %>%
	# Split into lists by executive diagnosis:
	split(.$EXECDX) %>%
	# Calculate the de-trended predictions for each condition:
	map( ~ data.frame(index=DTtoindex_0$index, prediction_detrended=.$intercept + DTtoindex_0$index*.$slope)) %>%
	# Convert the month indices back to DT_YEAR and DT_MONTH:
	map(~ merge(.,DTtoindex_0, by="index")) %>%
	# Convert list into data frame:
	bind_rows(.id="EXECDX") %>%
	# Select year, month, diagnosis, and detrended predicted value:
	select(DT_YEAR, DT_MONTH, EXECDX, prediction_detrended) %>%
	# Convert the "None" diagnosis back to "":
	mutate(EXECDX=case_when(EXECDX=="None"~"", TRUE~EXECDX)) %>%
	# Ensure that all dates are between Jan 2011 and Sep 2015:
	restricttimespan

# Gather raw and detrended data into a single data frame:
disease_df <- em_red_exec %>%
	make_vpkp %>%
	left_join(disease_predictions, by=c("DT_YEAR","DT_MONTH","EXECDX")) %>%
	left_join(disease_predictions_detrended, by=c("DT_YEAR","DT_MONTH","EXECDX")) %>% 
	mutate(vpkp_detrended = vpkp + (prediction_detrended - prediction)) %>%
	select(DT_YEAR, DT_MONTH, EXECDX, vpkp, vpkp_detrended)

# Find regression slopes and significances: 
siglabels_detrended <- list()
for(cond in 1:length(fdnames)){
	disease_df_formatted <- disease_df %>% 
		filter(EXECDX==fdnames[[cond]]) %>% 
		left_join(DTtoindex, by=c("DT_YEAR", "DT_MONTH"))

	disease_lm <- lm(vpkp~index, data=disease_df_formatted)
	pval <- summary(disease_lm)$coefficients[8]
	if(pval < 0.05){
		pctchange <- paste0("Change of ",as.character(round((last(predict(disease_lm)) - first(predict(disease_lm))) / (first(predict(disease_lm))) * 100, digits=1)),"%")	
	} else {
		pctchange <- "No significant change"
	}
	siglabels_detrended[[cond]] <- paste0(pctchange,"\n(p = ", signif(pval, digits=2), "; ")
}

# Generate confidence intervals for the delcines:
slopelabels_detrended <- list()
for(cond in 1:length(fdnames)){
	disease_df_formatted <- disease_df %>% 
		filter(EXECDX==fdnames[[cond]]) %>% 
		left_join(DTtoindex, by=c("DT_YEAR", "DT_MONTH"))

	disease_lm <- lm(vpkp~index, data=disease_df_formatted)
	luslope <- slopeci(disease_lm)
	slopelabels_detrended[[cond]] <- paste0("95% CI: (",round(filter(luslope, estimate=="lower")$pctchange, digits=1),"%, ",round(filter(luslope, estimate=="upper")$pctchange, digits=1),"%)")
}

# Plot incidence of observed disease for each condition, with detrending:
fdplots_detrended <- list()
for(cond in 1:length(fdnames)){
	fdplots_detrended[[cond]] <- disease_df %>% 
		filter(EXECDX==fdnames[cond]) %>% 
		gather(key="LEVEL", value="vpkp", vpkp, vpkp_detrended) %>%
		mutate(LEVEL=case_when(LEVEL=="vpkp"~"Raw", LEVEL=="vpkp_detrended"~"Detrended")) %>%
		makeYMD %>%
		ggplot(aes(x=YMD, y=vpkp, col=LEVEL)) + #
			geom_point(alpha=0.3) + 
			geom_line(alpha=0.3) + 
			geom_line(stat="smooth",method="loess",span=0.2,size=0.8) +
			geom_line(stat="smooth",method="lm",size=0.8) + 
			expand_limits(y=0) + 
			scale_x_date(labels = date_format("%Y"), breaks='1 year', limits=c(as.Date("2011-01-01"), as.Date("2016-01-01"))) + 
			labs(x="Month", y="Visits per 1,000 beneficiaries", title=fdnames_cleaned[cond], subtitle=paste0(siglabels_detrended[[cond]]," ", slopelabels_detrended[[cond]],")")) + 
			minimize + 
			theme(legend.title=element_blank(), legend.text=element_text(size=18)) + 
			scale_color_manual(values=c("blue","black"))
}

# =============================================================================
# Per-visit prescribing rates with/without detrending
# =============================================================================

# ---------- Plot per-visit prescribing rates with detrending: ---------- 

# Useful function for generating number of prescriptions per visit:
make_rxfrac <- function(x){
	x %>%
	group_by(DT_YEAR, DT_MONTH, EXECDX) %>% 
	summarise(NVISITS=sum(NVISITS), NRX=sum(NRX))  %>% 
	restricttimespan %>%
	mutate(rxfrac=NRX/NVISITS) %>%
	select(DT_YEAR, DT_MONTH, EXECDX, rxfrac)
}

stewardship_lms <- em_red_exec %>% 
	make_rxfrac %>%
	mutate(EXECDX=case_when(EXECDX==""~"None",TRUE~EXECDX)) %>%
	left_join(DTtoindex_0, by=c("DT_YEAR", "DT_MONTH")) %>%
	split(.$EXECDX) %>%
	map(~ lm(rxfrac ~ index, data=.)) %>%
	map_dfr(broom::tidy, .id="EXECDX") %>%
	mutate(term=case_when(term=="(Intercept)"~"intercept", term=="index"~"slope")) %>%
	mutate(EXECDX=case_when(EXECDX=="None"~"",TRUE~EXECDX)) %>%
	select(EXECDX, term, estimate, p.value)

stewardship_predictions <- em_red_exec %>% 
	make_rxfrac %>%
	mutate(EXECDX=case_when(EXECDX==""~"None",TRUE~EXECDX)) %>%
	left_join(DTtoindex_0, by=c("DT_YEAR", "DT_MONTH")) %>%
	split(.$EXECDX) %>%
	map(~ data.frame(
		index=.$index, 
		prediction=predict(lm(rxfrac ~ index, data=.))
		)) %>%
	map(~ merge(.,DTtoindex_0, by="index")) %>%
	bind_rows(.id="EXECDX") %>%
	select(DT_YEAR, DT_MONTH, EXECDX, prediction) %>%
	mutate(EXECDX=case_when(EXECDX=="None"~"", TRUE~EXECDX)) 

stewardship_predictions_detrended <- stewardship_lms %>% 
	mutate(EXECDX=case_when(EXECDX==""~"None",TRUE~EXECDX)) %>%
	mutate(estimate=case_when(term=="slope" & p.value<=0.05 ~ 0,
							  TRUE ~ estimate)) %>%
	select(-p.value) %>%
	spread(key=term, value=estimate) %>%
	split(.$EXECDX) %>%
	map( ~ data.frame(index=DTtoindex_0$index, prediction_detrended=.$intercept + DTtoindex_0$index*.$slope)) %>%
	map(~ merge(.,DTtoindex_0, by="index")) %>%
	bind_rows(.id="EXECDX") %>%
	select(DT_YEAR, DT_MONTH, EXECDX, prediction_detrended) %>%
	mutate(EXECDX=case_when(EXECDX=="None"~"", TRUE~EXECDX)) %>%
	restricttimespan

stewardship_df <- em_red_exec %>%
	make_rxfrac %>%
	left_join(stewardship_predictions, by=c("DT_YEAR","DT_MONTH","EXECDX")) %>%
	left_join(stewardship_predictions_detrended, by=c("DT_YEAR","DT_MONTH","EXECDX")) %>% 
	mutate(rxfrac_detrended = rxfrac + (prediction_detrended - prediction)) %>%
	select(DT_YEAR, DT_MONTH, EXECDX, rxfrac, rxfrac_detrended)

# Find regression slopes and significances: 
siglabels_detrended_stewardship <- list()
for(cond in 1:length(fdnames)){
	stewardship_df_formatted <- stewardship_df %>% 
		filter(EXECDX==fdnames[[cond]]) %>% 
		left_join(DTtoindex, by=c("DT_YEAR", "DT_MONTH"))

	stewardship_lm <- lm(rxfrac~index, data=stewardship_df_formatted)
	pval <- summary(stewardship_lm)$coefficients[8]
	if(pval < 0.05){
		pctchange <- paste0("Change of ",as.character(round((last(predict(stewardship_lm)) - first(predict(stewardship_lm)))*100, digits=2)),"%")	
	} else {
		pctchange <- "No significant change"
	}
	siglabels_detrended_stewardship[[cond]] <- paste0(pctchange,"\n(p = ", signif(pval, digits=2), "; ")
}

# Generate confidence intervals for the delcines:
slopelabels_detrended_stewardship <- list()
for(cond in 1:length(fdnames)){
	stewardship_df_formatted <- stewardship_df %>% 
		filter(EXECDX==fdnames[[cond]]) %>% 
		left_join(DTtoindex, by=c("DT_YEAR", "DT_MONTH"))

	stewardship_lm <- lm(rxfrac~index, data=stewardship_df_formatted)
	luslope <- slopeci_raw(stewardship_lm)
	slopelabels_detrended_stewardship[[cond]] <- paste0("95% CI: (",round(100*filter(luslope, estimate=="lower")$change, digits=2),"%, ",round(100*filter(luslope, estimate=="upper")$change, digits=2),"%)")
}

stewardshipplots_detrended <- list()
for(cond in 1:length(fdnames)){
	stewardshipplots_detrended[[cond]] <- stewardship_df %>% 
		filter(EXECDX==fdnames[cond]) %>% 
		gather(key="LEVEL", value="rxfrac", rxfrac, rxfrac_detrended) %>%
		mutate(LEVEL=case_when(LEVEL=="rxfrac"~"Raw", LEVEL=="rxfrac_detrended"~"Detrended")) %>%
		makeYMD %>%
		ggplot(aes(x=YMD, y=rxfrac*100, col=LEVEL)) + #
			geom_point(alpha=0.3) + 
			geom_line(alpha=0.3) + 
			geom_line(stat="smooth",method="loess",span=0.2,size=0.8) +
			geom_line(stat="smooth",method="lm",size=0.8) + 
			expand_limits(y=c(0,100)) + 
			scale_x_date(labels = date_format("%Y"), breaks='1 year', limits=c(as.Date("2011-01-01"), as.Date("2016-01-01"))) + 
			labs(x="Month", y="Percentage of visits followed by an antibiotic prescription", title=fdnames_cleaned[cond], subtitle=paste0(siglabels_detrended_stewardship[[cond]]," ", slopelabels_detrended_stewardship[[cond]],")")) + 
			minimize + 
			theme(legend.title=element_blank(),legend.text=element_text(size=18)) + 
			scale_color_manual(values=c("blue","black"))
}


# ---------- Plot per-visit prescribing rates, no detrending: ---------- 

stewardshipplots <- list()
for(cond in 1:length(fdnames)){
	stewardshipplots[[cond]] <- stewardship_df %>% 
		filter(EXECDX==fdnames[cond]) %>% 		
		makeYMD %>%
		ggplot(aes(x=YMD, y=rxfrac*100)) + #
			geom_point(alpha=0.3) + 
			geom_line(alpha=0.3) + 
			geom_line(stat="smooth",method="loess",span=0.2,size=0.8) +
			geom_line(stat="smooth",method="lm",size=0.8) + 
			expand_limits(y=c(0,100)) + 
			scale_x_date(labels = date_format("%Y"), breaks='1 year', limits=c(as.Date("2011-01-01"), as.Date("2016-01-01"))) + 
			labs(x="Month", y="Percentage of visits followed by an antibiotic prescription", title=fdnames_cleaned[cond], subtitle=paste0(siglabels_detrended_stewardship[[cond]]," ", slopelabels_detrended_stewardship[[cond]],")")) + 
			minimize
}

# =============================================================================
# Measure avoided prescriptions
# =============================================================================

combined_df <- disease_df %>%
	inner_join(stewardship_df, by=c("DT_YEAR","DT_MONTH","EXECDX")) %>%
	mutate(rxpkp=rxfrac*vpkp) %>%
	mutate(rxpkp_stewardshipdetrended=rxfrac_detrended*vpkp) %>%
	mutate(rxpkp_diseasedetrended=rxfrac*vpkp_detrended) %>%
	mutate(rxpkp_bothdetrended=rxfrac_detrended*vpkp_detrended)

rxavoided_df <- combined_df %>% 
	mutate(rxpkp_avoided_ste=rxpkp_stewardshipdetrended-rxpkp) %>%
	mutate(rxpkp_avoided_dis=rxpkp_diseasedetrended-rxpkp) %>%
	mutate(rxpkp_avoided_both = rxpkp_bothdetrended-rxpkp) %>%
	group_by(EXECDX) %>%
	summarise(rxpkp=sum(rxpkp),
		rxpkp_avoided_dis=sum(rxpkp_avoided_dis), 
		rxpkp_avoided_ste=sum(rxpkp_avoided_ste), 
		rxpkp_avoided_both=sum(rxpkp_avoided_both)) %>% 
	arrange(desc(rxpkp_avoided_both)) %>%
	filter(EXECDX != "") %>%
	filter(EXECDX != "All other codes")

rxavoided_totals <- rxavoided_df %>%
	ungroup() %>%
	summarise(rxpkp=sum(rxpkp), 
	rxpkp_avoided_dis=sum(rxpkp_avoided_dis),
	rxpkp_avoided_ste=sum(rxpkp_avoided_ste),
	rxpkp_avoided_both=sum(rxpkp_avoided_both))

# =============================================================================
# NAMCS plots
# =============================================================================

# Do a breakpoint plot for the NAMCS tier 1/2 diseases - - - - - - - - - - - - 

# Set the conditions: 
condlist <- fdnames[1:9]

# Make a dataframe of cases per person per year:
datdf <- namcs %>% 
	group_by(DT_YEAR) %>%
	filter(FDDX1 %in% condlist | 
		FDDX2 %in% condlist | 
		FDDX3 %in% condlist) %>%
	select(DT_YEAR, PATWT, POPSIZE) %>%
	group_by(DT_YEAR) %>%
	summarise(NCASES=sum(PATWT), CASERATE=NCASES/first(POPSIZE), POPSIZE=first(POPSIZE))  %>% 
	select(DT_YEAR, CASERATE) 

# Run a regression on the data:
tier12namcs_lm <- lm(CASERATE ~ DT_YEAR, data=datdf)
tier12namcs_slopes <- slopeci(tier12namcs_lm) 

# Find the optimal breakpoint, using log likelihood: 
lldf <- data.frame(t=c(), mlval=c())
for(t in seq(2003, 2014, 0.5)){
	llval <- bpll(datdf, t, "DT_YEAR", "CASERATE")
	lldf <- rbind(lldf, data.frame(t=t, llval=llval))
}
ggplot(lldf, aes(x=t, y=llval)) + geom_point() + geom_line() 

# The optimal breakpoint is in 2010. Generate a figure using that breakpoint: 

plotdf <- namcs %>% 
	group_by(DT_YEAR) %>%
	filter(FDDX1 %in% condlist | 
		FDDX2 %in% condlist | 
		FDDX3 %in% condlist) %>%
	select(DT_YEAR, PATWT, POPSIZE) %>%
	group_by(DT_YEAR) %>%
	summarise(NCASES=sum(PATWT), CASERATE=NCASES/first(POPSIZE), POPSIZE=first(POPSIZE)) 

breakpoint <- 2010
figtier12_namcs <- ggplot(data=datdf, aes(x=DT_YEAR, y=CASERATE*1000)) + 
	geom_point(alpha=0.3) + 
	geom_line(alpha=0.3) + 
	geom_line(stat="smooth", method="lm") + 
	geom_line(data=bpfit_lite(plotdf, breakpoint, "DT_YEAR", "CASERATE") %>% mutate(CASERATE=CASERATE*1000), aes(x=DT_YEAR, y=CASERATE), linetype="dashed") + 
	expand_limits(y=0) +
	minimize + 
	scale_x_continuous(breaks=2002:2015) + 
	theme(axis.text.x=element_text(angle=90, vjust=.5)) + 
	labs(title="Antibiotic-meriting conditions, US NAMCS", x="", y="Visits per 1,000 people", subtitle="22.2% decline \n(p<0.001, 95% CI=(-32.8%, -10.1%))")


# Do a breakpoint plot for the NAMCS pneumococcal conditions - - - - - - - - - 

condlist <- fdnames[ipdinds]

# Make a dataframe of cases per person per year:
datdf <- namcs %>% 
	group_by(DT_YEAR) %>%
	filter(FDDX1 %in% condlist | 
		FDDX2 %in% condlist | 
		FDDX3 %in% condlist) %>%
	select(DT_YEAR, PATWT, POPSIZE) %>%
	group_by(DT_YEAR) %>%
	summarise(NCASES=sum(PATWT), CASERATE=NCASES/first(POPSIZE), POPSIZE=first(POPSIZE))  %>% 
	select(DT_YEAR, CASERATE) 

# Run a regression on the data:
ipdnamcs_lm <- lm(CASERATE ~ DT_YEAR, data=datdf)
ipdnamcs_slopes <- slopeci(ipdnamcs_lm) 

# Find the optimal breakpoint, using log likelihood: 
lldf <- data.frame(t=c(), mlval=c())
for(t in seq(2003, 2014, 0.5)){
	llval <- bpll(datdf, t, "DT_YEAR", "CASERATE")
	lldf <- rbind(lldf, data.frame(t=t, llval=llval))
}
ggplot(lldf, aes(x=t, y=llval)) + geom_point() + geom_line() 

# The optimal breakpoint is in 2009. Generate a figure using that breakpoint: 

plotdf <- namcs %>% 
	group_by(DT_YEAR) %>%
	filter(FDDX1 %in% condlist | 
		FDDX2 %in% condlist | 
		FDDX3 %in% condlist) %>%
	select(DT_YEAR, PATWT, POPSIZE) %>%
	group_by(DT_YEAR) %>%
	summarise(NCASES=sum(PATWT), CASERATE=NCASES/first(POPSIZE), POPSIZE=first(POPSIZE)) 

breakpoint <- 2009
figipd_namcs <- ggplot(data=datdf, aes(x=DT_YEAR, y=CASERATE*1000)) + 
	geom_point(alpha=0.3) + 
	geom_line(alpha=0.3) + 
	geom_line(stat="smooth", method="lm") + 
	geom_line(data=bpfit_lite(plotdf, breakpoint, "DT_YEAR", "CASERATE") %>% mutate(CASERATE=CASERATE*1000), aes(x=DT_YEAR, y=CASERATE), linetype="dashed") + 
	expand_limits(y=0) +
	minimize + 
	scale_x_continuous(breaks=2002:2015) + 
	theme(axis.text.x=element_text(angle=90, vjust=.5)) + 
	labs(title="Possible pneumoccus, US NAMCS", x="", y="Visits per 1,000 people", subtitle="42.9% decline \n(p<.0001, 95% CI=(-30.7%, -53.5%))")


# Do a regular linear regression for all NAMCS conditions - - - - - - - -

# Generate a data frame of cases per person per year:
everything_df <- namcs %>% 
	group_by(DT_YEAR) %>%
	select(DT_YEAR, PATWT, POPSIZE) %>%
	group_by(DT_YEAR) %>%
	summarise(NCASES=sum(PATWT), CASERATE=NCASES/first(POPSIZE), POPSIZE=first(POPSIZE)) 

# Run linear regressions on the data:
everything_lm <- lm(CASERATE~DT_YEAR, data=everything_df)
everything_slopes <- slopeci(everything_lm) 

figeverything_namcs <- namcs %>% 
	group_by(DT_YEAR) %>%
	select(DT_YEAR, PATWT, POPSIZE) %>%
	group_by(DT_YEAR) %>%
	summarise(NCASES=sum(PATWT), CASERATE=NCASES/first(POPSIZE), POPSIZE=first(POPSIZE)) %>%
	ggplot(aes(x=DT_YEAR, y=CASERATE*1000)) + 
		geom_point(alpha=0.3) + 
		geom_line(alpha=0.3) + 
		geom_line(stat="smooth", method="lm") + 
		scale_x_continuous(breaks=2002:2015) + 
		labs(title="All conditions, US NAMCS", x="", y="Visits per 1,000 people", subtitle="No significant change\n(p=0.17, 95% CI=(-16.0%, 4.96%))") + 
		minimize + 
		theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
		expand_limits(y=0)


