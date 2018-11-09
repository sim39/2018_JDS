#filename: sem_analysis_msc.R
#
#description: description for piecewise structural equation modeling analysis 
#for mesophilic spores for our 2018 JDS manuscript in R

#load dataset
load("sem_data_msc.rdata")

# VIFs were investigated by fitting with lm() and using the car::vif() function
# All looked acceptable except m_ubed, and the VIFs were brought into an
# acceptable range by removing one variable (lacthous:scrpalley)

###Linear mixed-effects models
library(lme4)
#LMM-I for MSC
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
               data = sem_data_msc)

#LMM-II for MSC
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
               data = sem_data_msc)

#LMM-III for MSC
m_btm <- lmer( BTM_log_density ~ (1|FarmID) +
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
               data = sem_data_msc)
###Piecewise structural equation model for MSC
library(piecewiseSEM)
library(dplyr)
#
sem_coefs_MSC <- sem.coefs(modelList = list(m_nbed,m_ubed, m_btm),data=sem_data_msc)
standardized_sem_coefs_MSC <- sem.coefs(modelList = list(m_nbed,m_ubed, m_btm),data=sem_data_msc, standardize = "scale")

# Plot
coef.table_MSC <- standardized_sem_coefs_MSC[c(41,20,18,4,1,19,3,2,5,42,40),]
sem.plot(coef.table = coef.table_MSC) 

# Fisher's C doesn't reject the null of "no missing paths"
sem.fit(list(m_nbed,m_ubed,m_btm),data=sem_data_msc)

# Marginal and conditional R^2s
#(conditional R^2 "excludes" the variability coming from the random effect of farm)
rsquared(list(m_nbed,m_ubed,m_btm))

# Indirect effects
names(sem_coefs_MSC)[6] <- "ok"

twopaths_MSC <- sem_coefs_MSC %>% 
  filter(response == "NBED_log_density") %>%
  select(predictor, estimate, std.error) %>%
  mutate(est_from_UBED =
           sem_coefs_MSC$estimate[sem_coefs_MSC$response=="UBED_log_density" &
                                    sem_coefs_MSC$predictor=="NBED_log_density"],
         se_from_UBED =
           sem_coefs_MSC$std.error[sem_coefs_MSC$response=="UBED_log_density" &
                                     sem_coefs_MSC$predictor=="NBED_log_density"],
         est_from_BTM =
           sem_coefs_MSC$estimate[sem_coefs_MSC$response=="BTM_log_density" &
                                    sem_coefs_MSC$predictor=="UBED_log_density"],
         se_from_BTM =
           sem_coefs_MSC$std.error[sem_coefs_MSC$response=="BTM_log_density" &
                                     sem_coefs_MSC$predictor=="UBED_log_density"])

twopaths_MSC <- twopaths_MSC %>%
  mutate(est_ab = estimate * est_from_UBED,
         se_ab = sqrt((est_from_UBED^2*std.error^2)+
                        (estimate^2*se_from_UBED^2))) %>%
  mutate(est_abc = est_ab * est_from_BTM, 
         se_abc = sqrt((est_from_BTM^2*se_ab^2)+
                         (est_ab^2*se_from_BTM^2))) %>%
  mutate(z = est_abc/se_abc,
         pvalue = 2*(1-pnorm(abs(z))))

onepath_NBEDtoUBED_MSC <- sem_coefs_MSC %>% 
  filter(response == "NBED_log_density") %>%
  select(predictor, estimate, std.error) %>%
  mutate(est_from_UBED =
           sem_coefs_MSC$estimate[sem_coefs_MSC$response=="UBED_log_density" & 
                                    sem_coefs_MSC$predictor=="NBED_log_density"],
         se_from_UBED =
           sem_coefs_MSC$std.error[sem_coefs_MSC$response=="UBED_log_density" &
                                     sem_coefs_MSC$predictor=="NBED_log_density"])
onepath_NBEDtoUBED_MSC <- onepath_NBEDtoUBED_MSC %>%
  mutate(est_ab = estimate * est_from_UBED,
         se_ab = sqrt((est_from_UBED^2*std.error^2)+
                        (estimate^2*se_from_UBED^2))) %>%
  mutate(z = est_ab/se_ab,
         pvalue = 2*(1-pnorm(abs(z))))

onepath_UBEDtoBTM_MSC <- sem_coefs_MSC %>% 
  filter(response == "UBED_log_density") %>%
  select(predictor, estimate, std.error) %>%
  mutate(est_from_BTM =
           sem_coefs_MSC$estimate[sem_coefs_MSC$response=="BTM_log_density" &
                                    sem_coefs_MSC$predictor=="UBED_log_density"],
         se_from_BTM =
           sem_coefs_MSC$std.error[sem_coefs_MSC$response=="BTM_log_density" &
                                     sem_coefs_MSC$predictor=="UBED_log_density"])
onepath_UBEDtoBTM_MSC <- onepath_UBEDtoBTM_MSC %>%
  mutate(est_ab = estimate * est_from_BTM,
         se_ab = sqrt((est_from_BTM^2*std.error^2)+
                        (estimate^2*se_from_BTM^2))) %>%
  mutate(z = est_ab/se_ab,
         pvalue = 2*(1-pnorm(abs(z))))