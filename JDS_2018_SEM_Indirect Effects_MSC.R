source("JDS_2018_SEM_MSC.R")
library(dplyr)
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