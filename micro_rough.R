## Microplastic summary statistics
### Created by Ryan Helcoski, 2_19_2019
### Goal is to create 3 tables including basic summary tables and ANOVA with raw microplastic data

#### Load 
install.packages("dplyr")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("emmeans")
install.packages("multcompView")

library(dplyr)
library(ggplot2)
library(Hmisc)
library(emmeans)
library(multcompView)

#### Load data
#change the green text to your input location. Make sure you have the raw data master file. 
m2 <- read.csv("C:/Users/hp/Desktop/Years/Feb 2018/Micro/Micro_Final/analysis/Microplastic_data.csv")
debris <- read.csv("C:/Users/hp/Desktop/Years/Feb 2018/Micro/Micro_Final/analysis/debris.csv")
mud <- read.csv("C:/Users/hp/Desktop/Years/Feb 2018/Micro/Micro_Final/analysis/mudflat.csv")
channel <- read.csv("C:/Users/hp/Desktop/Years/Feb 2018/Micro/Micro_Final/analysis/channel_edge.csv")
dense <- read.csv("C:/Users/hp/Desktop/Years/Feb 2018/Micro/Micro_Final/analysis/dense_veg.csv")


#set_outpath
outpath <- "C:/Users/hp/Desktop/Years/Feb 2018/Micro/Micro_Final/analysis"


###remove columns from master data for easier use
m3 <- m2[ -c(1:4, 10, 14)]




#### First analysis, creating table 1  and table 2####

#mean soil characersitic by habitat
debrism <- colMeans(debris)
debrism
mudm <- colMeans(mud)
mudm
channelm <- colMeans(channel)
channelm
densem <- colMeans(dense)
densem
m3m <- colMeans(m3)
m3m

#merge means into one table
means <- rbind(debrism,mudm,channelm,densem,m3m)
means

#export table of means
write.csv(means, file=outpath)



#standard error
debris_se <- sapply(debris,function(x)sd(x)/sqrt(length(x)))
debris_se
mud_se <- sapply(mud,function(x)sd(x)/sqrt(length(x)))
mud_se
channel_se <- sapply(channel,function(x)sd(x)/sqrt(length(x)))
channel_se
dense_se <- sapply(dense,function(x)sd(x)/sqrt(length(x)))
dense_se
m3_se <- sapply(m3,function(x)sd(x)/sqrt(length(x)))
m3_se

#merge
se <- rbind(debris_se,mud_se,channel_se,dense_se,m3_se)
se

#export table
write.csv(se, file=outpath)



#### Finishing table 1 and 2 with ANOVA ####
########ANOVA, habitat type and soil characteristics, microplastics. 
# Bartlett test of homogeneity of variances
bartlett.test(m2$SOM, m2$hab_Type, data=m2)
#p-value > .05 accept the null hypothesis H0 (variances homogeneity)

bartlett.test(m2$bulk_density_dry, m2$hab_Type, data=m2)
#p-value > .05 accept the null hypothesis H0 (variances homogeneity)

bartlett.test(m2$stem_count, m2$hab_Type, data=m2)
# p-value < .05, full stop. THERE IS ALREADY SIGNIFICANT VARIANCES

bartlett.test(m2$perc_cover, m2$hab_Type, data=m2)
# p-value < .05, full stop. THERE IS ALREADY SIGNIFICANT VARIANCES

fligner.test(m2$num_plastics_m2, m2$hab_Type, data=m2)
# p-value < .05, full stop. THERE IS ALREADY SIGNIFICANT VARIANCES

fligner.test(m2$num_fibers_m2, m2$hab_Type, data=m2)
# p-value < .05, full stop. THERE IS ALREADY SIGNIFICANT VARIANCES

fligner.test(m2$num_shards_m2, m2$hab_Type, data=m2)
#p-value > .05 accept the null hypothesis H0 (variances homogeneity)

fligner.test(m2$mg_plastic_m2, m2$hab_Type, data=m2)
#p-value > .05 accept the null hypothesis H0 (variances homogeneity)

#### CONCLUSION
#There is already significant variation between stem count and habitat, cover class and habitat, 
# number of plastics and habitat, and fibers and habitat


#### ANOVA                                          FIX THIS! MAKE SURE TO BRING ALL ANOVA INTO ONE MAJOR FILE
# SOM ~ habitat type
som_hab= lm(formula = m2$SOM ~ m2$hab_Type)
anova(som_hab)
som_hab_aov <- aov(formula = m2$SOM ~ m2$hab_Type)
som_hab_aov_em <- emmeans(som_hab_aov, ~hab_Type)
cld(som_hab_aov_em, adjust="none")


# bulk_density ~ habitat type
bulk_hab = lm(formula = m2$bulk_density_dry ~ m2$hab_Type)
anova(bulk_hab)
bulk_hab_aov <- aov(formula = m2$bulk_density_dry ~ m2$hab_Type)
bulk_hab_aov_em <- emmeans(bulk_hab_aov, ~hab_Type)
cld(bulk_hab_aov_em, adjust="none")


#stem_densisty ~ habitat type
stem_hab = lm(formula =  m2$stem_count ~ m2$hab_Type)
anova(stem_hab)
stem_hab_aov <- aov(formula =  m2$stem_count ~ m2$hab_Type)
stem_hab_aov_em <- emmeans(stem_hab_aov, ~hab_Type)
cld(stem_hab_aov_em, adjust="none")




# total_cover ~ habitat type
cover_hab = lm(formula =  m2$perc_cover ~ m2$hab_Type)
anova(cover_hab)
cove_hab_aov <- aov(formula =  m2$perc_cover ~ m2$hab_Type)
cove_hab_aov_em <- emmeans(cove_hab_aov, ~hab_Type)
cld(cove_hab_aov_em, adjust="none")




#fibers ~ habitat type
fiber_hab = lm(formula =m2$num_fibers_m2 ~ m2$hab_Type)
anova(fiber_hab)
fiber_hab_aov <- aov(formula =m2$num_fibers_m2 ~ m2$hab_Type)
fiber_hab_aov_em <- emmeans(fiber_hab_aov, ~hab_Type)
cld(fiber_hab_aov_em, adjust="none")



#fragments ~ habitat type
fragment_hab = lm(formula = m2$num_shards_m2 ~ m2$hab_Type)
anova(fragment_hab)
shards_hab_aov <- aov(formula =m2$num_shards_m2 ~ m2$hab_Type)
shards_hab_aov_em <- emmeans(shards_hab_aov, ~hab_Type)
cld(shards_hab_aov_em, adjust="none")



# total plastics ~ habitat type
total_hab = lm(formula = m2$num_plastics_m2 ~ m2$hab_Type)
anova(total_hab)
total_hab_aov <- aov(formula =m2$num_plastics_m2 ~ m2$hab_Type)
total_hab_aov_em <- emmeans(total_hab_aov, ~hab_Type)
cld(total_hab_aov_em, adjust="none")




#mg plastics ~ habitat type
mg_hab = lm(formula = m2$mg_plastic_m2 ~ m2$hab_Type)
anova(mg_hab)
mg_hab_aov <- aov(formula =m2$mg_plastic_m2 ~ m2$hab_Type)
mg_hab_aov_em <- emmeans(mg_hab_aov, ~hab_Type)
cld(mg_hab_aov_em, adjust="none")







#### Table 3, correlation of everything vs everything ####
#general correlation
cor_all <- cor(m3)
cor_all
mycor <- rcorr(as.matrix(m3), type="pearson")
# mycor$r shows the correlation matrix, mycor$p the matrix with corresponding p-values.

