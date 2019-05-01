## Microplastic summary statistics ##
### Created by Ryan Helcoski, 2_19_2019 - 5_1_2019      ###
### Statistical analyses for creating tables 1,2,3      ###

##### Load packages ###

install.packages("dplyr")
install.packages("Hmisc")
install.packages("emmeans")
install.packages("multcompView")

library(dplyr)
library(Hmisc)
library(emmeans)
library(multcompView)


##### Load data ###
#change the green text to your input location. Make sure you have the raw data master file provided on github
m2 <- read.csv("/Microplastic_data.csv")

##### set_outpath ###
# use the space between the quotations to write up the location you want your data exported to
outpath <- " "

##### seperate data for easier analysis
#remove columns from master data for individual habitat types
m3 <- m2[ -c(1:4, 10, 14)]

#Create individual data frame for each habitat for easier analysis
#debris line
debris <- m3 [-c(1:18), ]
#mudflat
mud <- m3 [-c(7:24), ]
#channel edge
channel <- m3 [-c(1:6, 13:24), ]
#dense vegetation
dense <- m3 [-c(1:12, 19:24), ]



##### First analysis, creating table 1  and table 2. Summary statistics and standard error ####

### mean soil characersitic by habitat
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



#### standard error
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


#### ANOVA  #### Table 1
# SOM ~ habitat type
som_hab= lm(formula = m2$SOM ~ m2$hab_Type)
a_som_hab <- anova(som_hab)
a_s_h <- as.data.frame(a_som_hab)
som_hab_aov <- aov(formula = m2$SOM ~ m2$hab_Type)
som_hab_aov_em <- emmeans(som_hab_aov, ~hab_Type)
cld_som_hab <- cld(som_hab_aov_em, adjust="none")
c_s_h <- as.data.frame(cld_som_hab)

# bulk_density ~ habitat type
bulk_hab = lm(formula = m2$bulk_density_dry ~ m2$hab_Type)
a_bulk_hab <- anova(bulk_hab)
a_b_h <- as.data.frame(a_bulk_hab)
bulk_hab_aov <- aov(formula = m2$bulk_density_dry ~ m2$hab_Type)
bulk_hab_aov_em <- emmeans(bulk_hab_aov, ~hab_Type)
cld_bulk_hab <- cld(bulk_hab_aov_em, adjust="none")
c_b_h <- as.data.frame(cld_bulk_hab)

#stem_densisty ~ habitat type
stem_hab = lm(formula =  m2$stem_count ~ m2$hab_Type)
a_stem_hab <- anova(stem_hab)
a_st_h <- as.data.frame(a_stem_hab)
stem_hab_aov <- aov(formula =  m2$stem_count ~ m2$hab_Type)
stem_hab_aov_em <- emmeans(stem_hab_aov, ~hab_Type)
cld_stem_hab <- cld(stem_hab_aov_em, adjust="none")
c_st_h <- as.data.frame(cld_stem_hab)

# total_cover ~ habitat type
cover_hab = lm(formula =  m2$perc_cover ~ m2$hab_Type)
a_cover_hab <- anova(cover_hab)
a_c_h <- as.data.frame(a_cover_hab)
cove_hab_aov <- aov(formula =  m2$perc_cover ~ m2$hab_Type)
cove_hab_aov_em <- emmeans(cove_hab_aov, ~hab_Type)
cld_cover_hab <- cld(cove_hab_aov_em, adjust="none")
c_c_h <- as.data.frame(cld_cover_hab)

#merge 
an_1 <- list(a_s_h, a_b_h, a_st_h, a_c_h)
anova_soil_veg <- do.call(rbind, unname(an_1))
cld_1 <- list(c_s_h, c_b_h, c_st_h, c_c_h)
cld_soil_veg <- do.call(rbind, unname(cld_1))

#export tables
write.csv(anova_soil_veg, file=outpath)
write.csv(cld_soil_veg, file=outpath)


#### ANOVA  #### Table 2
#fibers ~ habitat type
fiber_hab = lm(formula =m2$num_fibers_m2 ~ m2$hab_Type)
a_fiber_hab <- anova(fiber_hab)
a_f_h <- as.data.frame(a_fiber_hab)
fiber_hab_aov <- aov(formula =m2$num_fibers_m2 ~ m2$hab_Type)
fiber_hab_aov_em <- emmeans(fiber_hab_aov, ~hab_Type)
cld_fiber_hab <- cld(fiber_hab_aov_em, adjust="none")
c_f_h <- as.data.frame(cld_fiber_hab)

#fragments ~ habitat type
fragment_hab = lm(formula = m2$num_shards_m2 ~ m2$hab_Type)
a_fragmanet_hab <- anova(fragment_hab)
a_fr_h <- as.data.frame(a_fragmanet_hab)
shards_hab_aov <- aov(formula =m2$num_shards_m2 ~ m2$hab_Type)
shards_hab_aov_em <- emmeans(shards_hab_aov, ~hab_Type)
cld_fiber_hab <- cld(shards_hab_aov_em, adjust="none")
c_fr_h <- as.data.frame(cld_fiber_hab)

# total plastics ~ habitat type
total_hab = lm(formula = m2$num_plastics_m2 ~ m2$hab_Type)
a_total_hab <- anova(total_hab)
a_t_h <- as.data.frame(a_total_hab)
total_hab_aov <- aov(formula =m2$num_plastics_m2 ~ m2$hab_Type)
total_hab_aov_em <- emmeans(total_hab_aov, ~hab_Type)
cld_total_hab <- cld(total_hab_aov_em, adjust="none")
c_t_h <- as.data.frame(cld_total_hab)

#mg plastics ~ habitat type
mg_hab = lm(formula = m2$mg_plastic_m2 ~ m2$hab_Type)
a_mg_hab <- anova(mg_hab)
a_m_h <- as.data.frame(a_mg_hab)
mg_hab_aov <- aov(formula =m2$mg_plastic_m2 ~ m2$hab_Type)
mg_hab_aov_em <- emmeans(mg_hab_aov, ~hab_Type)
cld_mg_hab <- cld(mg_hab_aov_em, adjust="none")
c_m_h <- as.data.frame(cld_mg_hab)


#merge 
an_2 <- list(a_f_h, a_fr_h, a_t_h, a_m_h)
anova_plastic <- do.call(rbind, unname(an_2))
cld_2 <- list(c_f_h, c_fr_h, c_t_h, c_m_h)
cld_plastic <- do.call(rbind, unname(cld_2))

#export tables
write.csv(anova_plastic, file=outpath)
write.csv(cld_plastic, file=outpath)





#### Table 3, correlation of everything vs everything ####
mycor <- rcorr(as.matrix(m3), type="pearson")
m_r <- mycor$r
correlation_r <- m_r [-c(5:8), -c(1:4)]
m_P <- mycor$P
correlation_P <- m_P [-c(5:8), -c(1:4)]

#export tables
write.csv(correlation_r, file=outpath)
write.csv(correlation_P, file=outpath)
