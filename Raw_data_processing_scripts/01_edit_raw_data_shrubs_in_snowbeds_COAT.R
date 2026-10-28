
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# Background  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# The script first reads in selected datafiles on shrub counts in snowbeds, based on template written by Eeva in 2017.
# (Template snowbed vegetation data.xlsx)
# Data from Komag and from Vestre Jakobselv are run through a spell check part separately, and combined afterwards.
# In the end the script produces a raw-data file with potential corrections, so the file is ready to be saved at COAT data portal. 

## last update Eeva 17.10.2023

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# start script  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

rm(list=ls())
objects()

################################# set working directories

# data.dir = where the data to be checked are  
data.dir <- "/Users//Library/CloudStorage/Box-Box/COAT/Modules/Small rodent module/data/V_snowbed_shrub_abundance_intensive/raw data from field/" 

# out.dir = where the cleaned files should be saved 
out.dir <-"/Users//Library/CloudStorage/Box-Box/COAT/Modules/Small rodent module/data/V_snowbed_shrub_abundance_intensive/edited files to data portal/"  

################################# get functions and libraries

'%!in%' <- function(x,y)!('%in%'(x,y))

IsDate <- function(mydate, date.format = "%d.%m.%y") {
  tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}

library(tidyverse) # arrange, gather, spread, pivot_longer


################################# get data

# write here the original file name 
filename<-"snowbed_shrubs_VJ_2026.txt"
#filename<-"snowbed_shrubs_KO_2026.txt"

# read in data
myfile<-read.delim(paste(data.dir, filename, sep = ""), header=TRUE, sep="\t")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# check column titles   ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


titles<-c("date", "locality", "site", "plot", "treatment", "subplot", 
          "emp_nig", "vac_myr",	"vac_uli",	"bet_nan",	"vac_vit",	"phy_cae",	"nar_str",	"hup_sel",	"sal_her", 
          "comment", "observer")


## check that names in the file are identical with names that should be in the file
identical(titles, names(myfile))
cbind.data.frame(titles, names(myfile)) ## see both
names(myfile); head(myfile)

# getting rid of additional empty columns if they appear
#myfile<-select(myfile, -contains("X"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# check spelling of study design variables and additional info ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


## check and correct spelling here
str(myfile)
myfile <- myfile %>% mutate(across(where(is_character),as_factor)) # change character variables to factors



################ dates
levels(myfile$date) ## check that these are within the range of appropriate  dates
table(myfile$date, useNA="ifany")
all(IsDate(myfile$date[!is.na(myfile$date)])) 

# if date is typed in format year-month-day 
#myfile$new_date <- as.Date(myfile$date, "%Y-%m-%d")

# if date is typed in format day.month.year
myfile$new_date <- as.Date(myfile$date, "%d.%m.%Y")

table(myfile$new_date, useNA="ifany")

# 
# ################ locality
levels(myfile$locality)
potential.locality<-c("vestre_jakobselv", "komagdalen")
all(levels(myfile$locality) %in% potential.locality)
# 
# ################ site
levels(myfile$site)
potential.site<-c("ko_kj_sn_1","ko_kj_sn_2","ko_kj_sn_3","ko_kj_sn_4","ko_kj_sn_5","ko_kj_sn_6","ko_kj_sn_7","ko_kj_sn_8","ko_kj_sn_9","ko_kj_sn_10",
                  "ko_kj_sn_11","ko_kj_sn_12","ko_kj_sn_13","ko_kj_sn_14","ko_kj_sn_15","ko_kj_sn_16","ko_kj_sn_17","ko_kj_sn_18","ko_kj_sn_19",
                  "ko_ru_sn_1","ko_ru_sn_2","ko_ru_sn_3","ko_ru_sn_4","ko_ru_sn_5","ko_ru_sn_6","ko_ru_sn_7","ko_ru_sn_8","ko_ru_sn_9","ko_ru_sn_10",
                  "ko_ru_sn_11","ko_ru_sn_12","ko_ru_sn_13","ko_ru_sn_14","ko_ru_sn_15","ko_ru_sn_16","ko_ru_sn_17","ko_ru_sn_18","ko_ru_sn_19","ko_ru_sn_20",
                  "vj_be_sn_1","vj_be_sn_2","vj_be_sn_3","vj_be_sn_4","vj_be_sn_5","vj_be_sn_6","vj_be_sn_7","vj_be_sn_8","vj_be_sn_11","vj_be_sn_12",
                  "vj_be_sn_13","vj_be_sn_14","vj_be_sn_15","vj_be_sn_17","vj_be_sn_19","vj_be_sn_20","vj_be_sn_21","vj_be_sn_22","vj_be_sn_24","vj_be_sn_25",
                  "vj_to_sn_2","vj_to_sn_3","vj_to_sn_4","vj_to_sn_5","vj_to_sn_6","vj_to_sn_8","vj_to_sn_9","vj_to_sn_12","vj_to_sn_13","vj_to_sn_14","vj_to_sn_15",
                  "vj_to_sn_16","vj_to_sn_19","vj_to_sn_20","vj_to_sn_21","vj_to_sn_22","vj_to_sn_24","vj_to_sn_25","vj_to_sn_26","vj_to_sn_27")

all(levels(myfile$site) %in% potential.site)


############# plot
levels(as.factor(myfile$plot)) # should be numbers from 1 to 4
table(myfile$site, myfile$plot) # all should be 12
unique(table(myfile$site, myfile$plot)) # all should be 12


# ############# treatment
levels(as.factor(myfile$treatment)) # should be control

# ############# subplot
levels(as.factor(myfile$subplot))
table(myfile$site, myfile$subplot) # all should be 4
unique(table(myfile$site, myfile$subplot)) # all should be 4

# 
# ############# observer
levels(as.factor(myfile$observer))
summary(myfile$observer)

# potential corrections
#levels(myfile$observer)[which(levels(myfile$observer)=="ems tb")] = "ems_tb" ; levels(myfile$observer)
#levels(myfile$observer)[which(levels(myfile$observer)=="EFK")] = "efk" ; levels(myfile$observer)
#myfile$observer<-rep("mld", times=length(myfile$observer))

#comments
levels(as.factor(myfile$comment))
levels(myfile$comment_new)
#myfile$comment<-myfile$comment_new
#levels(myfile$comment)[which(levels(myfile$comment)=="Dekket av sn?")] = "plot covered by snow" ; levels(myfile$comment)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# check values in plant data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# # values in plant variables should be only one or zero
table(myfile$emp_nig)
table(myfile$vac_myr)
table(myfile$vac_uli)
table(myfile$bet_nan)
table(myfile$vac_vit)
table(myfile$phy_cae)
table(myfile$nar_str)
table(myfile$hup_sel)
table(myfile$sal_her)

# check raw data and replace when needed
# myfile[myfile$emp_nig==3,]

# 
#toto <- myfile %>%                               # Replacing values
#   mutate(emp_nig = replace(emp_nig, emp_nig == 3, 0))
# table(toto$emp_nig)
# toto[toto$site=="vj_be_sn_17" & toto$plot==1,]

# myfile <- myfile %>%                               
#      mutate(emp_nig = replace(emp_nig, emp_nig == 3, 0))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# combine files from KO and VJ ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

head(myfile)

# when you have checked the data from Komag, run this
myfile_komag<-myfile
names(myfile_komag)
myfile_komag <- myfile_komag[, c("date", "locality", "site", "plot",  "treatment", "subplot", 
                                 "emp_nig",   "vac_myr" ,  "vac_uli",   "bet_nan" , "vac_vit",   "phy_cae",   "nar_str",   "hup_sel"  , "sal_her"   ,
                                 "observer", "comment")]

names(myfile_komag)<-c("date", "locality", "site", "plot",  "treatment", "subplot",  "emp_nig",   "vac_myr" ,  "vac_uli",   "bet_nan" , "vac_vit",   "phy_cae",   "nar_str",   "hup_sel"  , "sal_her"   , "observer", "comment")


# when you have checked the data from Vestre Jakobselv, run this
myfile_vj<-myfile
myfile_vj <- myfile_vj[, c("date", "locality", "site", "plot",  "treatment", "subplot", 
                           "emp_nig",   "vac_myr" ,  "vac_uli",   "bet_nan" , "vac_vit",   "phy_cae",   "nar_str",   "hup_sel"  , "sal_her"   ,
                           "observer", "comment")]
names(myfile_vj)<-c("date", "locality", "site", "plot",  "treatment", "subplot",  "emp_nig",   "vac_myr" ,  "vac_uli",   "bet_nan" , "vac_vit",   "phy_cae",   "nar_str",   "hup_sel"  , "sal_her"   , "observer", "comment")


# when both of the above have been done, run this
new_file<-rbind.data.frame(myfile_komag, myfile_vj)
summary(new_file)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# reformat column content  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#spatial variables
new_file$sn_region<-rep("varanger", nrow(new_file))
new_file$sn_locality<-new_file$locality
new_file$sn_section<-substr(new_file$site, 4,5)
new_file$sn_section<- ifelse(new_file$sn_section == "kj", "kjoltindan", 
                             ifelse(new_file$sn_section == "ru", "ruossachokka",       
                                    ifelse(new_file$sn_section == "be", "bearalveaijohka", 
                                           ifelse(new_file$sn_section == "to", "torvhaugdalen", 
                                                  NA))))


new_file$sn_site<-new_file$site
new_file$sn_plot<-new_file$plot
new_file$sc_plot_treatment<-new_file$treatment
new_file$sn_subplot<-new_file$subplot


#temporal variables
new_file$t_date<-new_file$date

# comments
levels(as.factor(new_file$comment))
summary(new_file$comment)
new_file$v_comment<-new_file$comment
str(new_file)


#observer
new_file$v_observer<-new_file$observer


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# reshape data    ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# which variables to include?
names(new_file)
summary(new_file)

# select variables
var<-c("sn_region", "sn_locality", "sn_section","sn_site","sn_plot","sc_plot_treatment","sn_subplot",
       "t_date",
       "emp_nig",   "vac_myr" ,  "vac_uli",   "bet_nan" , "vac_vit",   "phy_cae",   "nar_str",   "hup_sel"  , "sal_her",
       "v_comment", "v_observer")
new_file_temp<-new_file[,which(colnames(new_file) %in% var)]

# pivot longer
myfile_long<-pivot_longer(new_file_temp, cols=c("emp_nig",   "vac_myr" ,  "vac_uli",   "bet_nan" , "vac_vit",   "phy_cae",   "nar_str",   "hup_sel"  , "sal_her" ), 
                          names_to =  "v_species", values_to = "v_presence") 

## check that the melting worked; each species should come once in each subplot 
myfile_long[myfile_long$sn_site=="ko_kj_sn_1" & myfile_long$sn_plot=="1" & myfile_long$sn_subplot=="1",]
myfile_long[myfile_long$sn_site=="vj_be_sn_11" & myfile_long$sn_plot=="1" & myfile_long$sn_subplot=="1",]


#no species = 9, no subplots = 12; each plot should have 108 rows
unique(table(myfile_long$sn_site, myfile_long$sn_plot)) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# minor editing and formatting   ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# # set empty comments to NA
str(myfile_long)
myfile_long$v_comment <- as.character(myfile_long$v_comment)
myfile_long$v_comment[myfile_long$v_comment==""] <- "NA"
myfile_long$v_comment<-as.factor(myfile_long$v_comment)
summary(myfile_long)
head(myfile_long)

# re-order columns
myfile_long<-relocate(myfile_long, c("v_observer", "v_comment"), .after="v_presence")
head(myfile_long)

# order by site/plot/subplot/species
myfile_long<-arrange(myfile_long, sn_site, sn_plot, sn_subplot, v_species)
head(myfile_long)

# add column with year
myfile_long <- add_column(myfile_long, t_year = substr(myfile_long$t_date, 1, 4), .before = "t_date")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################# write out file for server ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# write here the current year
year<-2025

filename<-paste("V_snowbed_shrub_abundance_intensive", "_", year, ".txt", sep = "")
write.table(myfile_long, paste(out.dir, filename, sep = ""), sep=";",row.names = F)


