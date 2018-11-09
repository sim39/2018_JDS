#filename: JDS_2018_SEM_TSC.R
#
#description: description for piecewise structural equation modeling analysis 
#for thermophilic spores for our 2018 JDS manuscript in R

#load dataset
load("JDS_2018_wide_data_tsc.rdata")

# VIFs were investigated by fitting with lm() and using the car::vif() function
# All looked acceptable except m_ubed, and the VIFs were brought into an
# acceptable range by removing one variable (lacthous:scrpalley). 

###Linear mixed-effects models
library(lme4)
#LMM-I for TSC
m_nbed <- lmer(NBED_log_density ~ (1|FarmID) +
                 typeOR +
                 # type/bedmat
                 typeOR.bedmatMS +
                 typeOR.bedmatSt +
                 typeOR.bedmatOther +
                 typeIOR.bedmatRS+
                 typeIOR.bedmatLimestone +
                 # type/bedmat/NSwashed_yn
                 bedmatNS.NSwashed_ynyes +
                 # type/bedmat/RSrecycled
                 bedmatRS.RSrecycledPLs +
                 bedmatRS.RSrecycledOther +
                 RSorMStimeinstor_num +
                 # type/bedmat/RSstorage
                 bedmatRS.RSstorageUC +
                 # type/bedmat/RSreclaimuse
                 bedmatRS.RSreclaimusemost +
                 # type/bedmat/MSsolidpress_yn
                 bedmatMS.MSsolidpress_ynyes +
                 # type/bedmat/MSmechdried_yn
                 bedmatMS.MSmechdried_ynyes +
                 # lacthous/addnewbed (added after testing for missing paths)
                 lacthousDL.addnewbed +
                 lacthousB.addnewbed +
                 lacthousTS.addnewbed,
               data = wide_data_tsc)

#LMM-II for TSC
m_ubed <- lmer(UBED_log_density ~ (1|FarmID) +
                 typeOR +
                 # type/bedmat
                 typeOR.bedmatMS +
                 typeOR.bedmatSt +
                 typeOR.bedmatOther +
                 typeIOR.bedmatRS +
                 typeIOR.bedmatLimestone +
                 # lacthous 
                 lacthousDL +
                 lacthousB +
                 lacthousTS +
                 # lacthous/scrpman
                 lacthousDL.scrpman +
                 lacthousB.scrpman +
                 lacthousTS.scrpman +
                 #lacthous/scrpalley+ # Removed because VIFs are large when included 
                 # lacthous/addnewbed
                 lacthousDL.addnewbed +
                 lacthousB.addnewbed +
                 lacthousTS.addnewbed +
                 surfraked_num +
                 compremyes +
                 stallbodylength_num +
                 cowsperstall_num +
                 # paddock_yn
                 paddock_ynyes +
                 # bedcond_yn
                 bedcond_ynyes +
                 NBED_log_density,
               data = wide_data_tsc)

#LMM-III for TSC
m_btm <- lme4::lmer( BTM_log_density ~ (1|FarmID) +
                 typeOR +
                 # type/bedmat
                 typeOR.bedmatMS +
                 typeOR.bedmatSt +
                 typeOR.bedmatOther +
                 typeIOR.bedmatRS+
                 typeIOR.bedmatLimestone +
                 milksched +
                 washmilkunityes +
                 #prediptype
                 prediptypeHydrogen.peroxide +
                 prediptypeChlorinated.product +
                 prediptypeno.pre.dip +
                 prediptypeOther +
                 #postdiptype
                 postdiptypeno.post.dip +
                 postdiptypenon.Iodine +
                 #teatscrub_yn
                 teatscrub_ynyes +
                 #forestrip_yn
                 forestrip_ynyes +
                 #clipudder_yn
                 clipudder_ynyes +
                 #taildock_yn
                 taildock_yntrimswitches +
                 taildock_ynyes +
                 #lacthous
                 lacthousDL +
                 lacthousB +
                 lacthousTS +
                 #paddock_yn
                 paddock_ynyes +
                 #per_hyg
                 per_hyg2 +
                 per_hyg3 +
                 per_hyg4 +
                 per_clnmas +
                 UBED_log_density,
               data = wide_data_tsc)
###Piecewise structural equation model for TSC
library(piecewiseSEM)
#
sem_coefs_TSC <- sem.coefs(modelList = list(m_nbed,m_ubed,m_btm),data=wide_data_tsc)
standardized_sem_coefs_TSC <- sem.coefs(modelList = list(m_nbed,m_ubed,m_btm),data=wide_data_tsc, standardize = "scale")

# Plot
coef.table_TSC <- standardized_sem_coefs_TSC[c(41,22,6,18,3,7,1,4,20,2,5,19,8,21,24,23,42,40),]
sem.plot(coef.table = coef.table_TSC) 

# Fisher's C doesn't reject the null of "no missing paths"
sem.fit(list(m_nbed,m_ubed,m_btm),data=wide_data_tsc)

# Marginal and conditional R^2s 
#(conditional R^2 "excludes" the variability coming from the random effect of farm)
rsquared(list(m_nbed,m_ubed,m_btm))
