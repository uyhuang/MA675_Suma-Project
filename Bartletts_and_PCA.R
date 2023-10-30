library(psych)
library(ltm)
# Run data_cleaning.R first.

#For SL data alone:

SL_var_data<- SL_data %>% select(SL_goals_awareness_q,
                                 SL_service_quality_q,
                                 SL_session_affordability_a,
                                 SL_parents_can_observe_q,
                                 SL_receive_home_activities_q,
                                 SL_material_affordability_a,
                                 SL_duration_f,
                                 SL_days_per_week_f
) %>% dplyr::rename(
  goals_awareness = SL_goals_awareness_q,
  service_quality=SL_service_quality_q,
  session_affordability = SL_session_affordability_a,
  parent_observation = SL_parents_can_observe_q,
  home_activities= SL_receive_home_activities_q,
  material_affordability = SL_material_affordability_a,
  session_duration = SL_duration_f,
  weekly_attendance = SL_days_per_week_f
) %>% mutate(
  weekly_attendance = abs(weekly_attendance-7)
)

corr_mat_SL<- cor(SL_var_data)

bartlett_SL<- cortest.bartlett(corr_mat_SL, n = nrow(SL_var_data))

#Just the NSL data:

NSL_var_data<- NSL_data_no_missing %>% select(
  NSL_not_affordable_a,
  NSL_no_distance_q,
  NSL_not_helping_q,
  NSL_therapy_not_good_q,
  NSL_SL_unavailable_child_old_a) %>% 
  mutate(
    affordable= abs(NSL_not_affordable_a-7),
    services_close = abs(NSL_no_distance_q-7),
    helping = abs(NSL_not_helping_q-7),
    quality = abs(NSL_therapy_not_good_q-7),
    available_despite_age = 
      NSL_SL_unavailable_child_old_a) %>% select(-c(NSL_not_affordable_a,
                                                    NSL_no_distance_q,
                                                    NSL_not_helping_q,
                                                    NSL_therapy_not_good_q,
                                                    NSL_SL_unavailable_child_old_a))

corr_mat_NSL<- cor(NSL_var_data)

bartlett_NSL<- cortest.bartlett(corr_mat_NSL, n = nrow(NSL_var_data))

#Combining both SL and NSL:

#Lets just take the data as is and run the test and then try  wizardry

#We are taking days and duration for SL out because these throw NAs in our corr_mat

bart_data<- data_of_int %>% mutate(
  NSL_affordable= ifelse(NSL_not_affordable_a!=0,abs(NSL_not_affordable_a-7),0),
  NSL_services_close = ifelse(NSL_no_distance_q!=0,abs(NSL_no_distance_q-7),0),
  NSL_helping = ifelse(NSL_not_helping_q!=0,abs(NSL_not_helping_q-7),0),
  NSL_quality = ifelse(NSL_therapy_not_good_q!=0,abs(NSL_therapy_not_good_q-7),0),
  NSL_available_despite_age = 
    ifelse(NSL_SL_unavailable_child_old_a!=0,
  abs(NSL_SL_unavailable_child_old_a-7),0))%>% select(-c(NSL_not_affordable_a,
                                            NSL_no_distance_q,
                                            NSL_not_helping_q,
                                            NSL_therapy_not_good_q,
                                            NSL_SL_unavailable_child_old_a)) %>% 
                            select(SL_goals_awareness_q,
                                          SL_service_quality_q,
                                          SL_session_affordability_a,
                                          SL_parents_can_observe_q,
                                          SL_receive_home_activities_q,
                                          SL_material_affordability_a,
                                          NSL_affordable,
                                          NSL_services_close,
                                          NSL_helping,
                                          NSL_quality,
                                          NSL_available_despite_age)

corr_mat<- cor(bart_data)

bartlett<- cortest.bartlett(corr_mat, n = nrow(bart_data))


#A brief rationale on what follows: 
#The SL data has 68 observations. NSL has 40. So im going to try and repeatedly
#sample from SL and take the average p value from all the bartlett's test. 

#sampling from the dataset
i<-1
bartlett<-c()
while(i<=1000){
SL_40<- SL_data[sample(nrow(SL_data), 40, replace=FALSE),]%>% select(SL_goals_awareness_q,
                                                        SL_service_quality_q,
                                                        SL_session_affordability_a,
                                                        SL_parents_can_observe_q,
                                                        SL_receive_home_activities_q,
                                                        SL_material_affordability_a)

NSL_cov<- NSL_data_no_missing %>% mutate(
  NSL_affordable= ifelse(NSL_not_affordable_a!=0,abs(NSL_not_affordable_a-7),0),
  NSL_services_close = ifelse(NSL_no_distance_q!=0,abs(NSL_no_distance_q-7),0),
  NSL_helping = ifelse(NSL_not_helping_q!=0,abs(NSL_not_helping_q-7),0),
  NSL_quality = ifelse(NSL_therapy_not_good_q!=0,abs(NSL_therapy_not_good_q-7),0),
  NSL_available_despite_age = 
    ifelse(NSL_SL_unavailable_child_old_a!=0,
           abs(NSL_SL_unavailable_child_old_a-7),0)) %>% select(
                                         NSL_affordable,
                                         NSL_services_close,
                                         NSL_helping,
                                         NSL_quality,
                                         NSL_available_despite_age)


bart_frame<- data.frame(SL_40,NSL_cov)

cor_bart<- cor(bart_frame)

bartlett[i]<- cortest.bartlett(cor_bart, n = nrow(bart_frame))$p.value
i<-i+1
}
#Is this necessary? 



#PCA
PCA_data<- data_of_int %>% mutate(
  NSL_affordable= ifelse(NSL_not_affordable_a!=0,abs(NSL_not_affordable_a-7),0),
  NSL_services_close = ifelse(NSL_no_distance_q!=0,abs(NSL_no_distance_q-7),0),
  NSL_helping = ifelse(NSL_not_helping_q!=0,abs(NSL_not_helping_q-7),0),
  NSL_quality = ifelse(NSL_therapy_not_good_q!=0,abs(NSL_therapy_not_good_q-7),0),
  NSL_available_despite_age = 
    ifelse(NSL_SL_unavailable_child_old_a!=0,
           abs(NSL_SL_unavailable_child_old_a-7),0))%>% select(-c(NSL_not_affordable_a,
                                                                  NSL_no_distance_q,
                                                                  NSL_not_helping_q,
                                                                  NSL_therapy_not_good_q,
                                                                  NSL_SL_unavailable_child_old_a)) %>% 
  select(
         SL_goals_awareness_q,
         SL_service_quality_q,
         SL_session_affordability_a,
         SL_parents_can_observe_q,
         SL_receive_home_activities_q,
         SL_material_affordability_a,
         NSL_affordable,
         NSL_services_close,
         NSL_helping,
         NSL_quality,
         NSL_available_despite_age)

p<- princomp(PCA_data, cor=TRUE)
n<- princomp(PCA_data, cor=FALSE)
p$loadings
n$loadings
