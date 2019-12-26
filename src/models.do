cd"C:\Users\borqu\Documents\reentry-data-pipeline\Stata"
use df.dta, clear
drop if time!=1
drop if reg_folio==10016
drop if reg_folio==10083
drop if reg_folio==10097
drop if reg_folio==10248
drop if reg_folio==20020
drop if reg_folio==20120
drop if reg_folio==20191
drop if reg_folio==20289
drop if reg_folio==20298
drop if reg_folio==30025
drop if reg_folio==30148
drop if reg_folio==30159
drop if reg_folio==40267
drop if reg_folio==40280
drop if reg_folio==50080
drop if reg_folio==50131
drop if reg_folio==50163
drop if reg_folio==50242
drop if reg_folio==50245

// Merge with clusters
merge 1:1 reg_folio using cluster_membership.dta

// Dummy for cluster
tabulate cluster_job_ind_4, generate(clus_job)
tabulate cluster_jobv2_4, generate(clus_job_crime)

// Models for Job Clusters
logit clus_job1 age i.only_primary i.any_previous_work i.early_crime i.class previous_sentences nchildren total_months_in_prison i.drug_depabuse mental_health self_efficacy desire_change, or
outreg2 using job_clus.xls, lab eform nor2 addstat(Log likelihood, e(ll), gl, e(df_m), Pseudo R2, e(r2_p), N, e(N)) 

logit clus_job2 age i.only_primary i.any_previous_work i.early_crime i.class previous_sentences nchildren total_months_in_prison i.drug_depabuse mental_health self_efficacy desire_change, or
outreg2 using job_clus.xls, lab eform nor2 addstat(Log likelihood, e(ll), gl, e(df_m), Pseudo R2, e(r2_p), N, e(N)) 

logit clus_job3 age i.only_primary i.any_previous_work i.early_crime i.class previous_sentences nchildren total_months_in_prison i.drug_depabuse mental_health self_efficacy desire_change, or
outreg2 using job_clus.xls, lab eform nor2 addstat(Log likelihood, e(ll), gl, e(df_m), Pseudo R2, e(r2_p), N, e(N)) 

logit clus_job4 age i.only_primary i.any_previous_work i.early_crime i.class previous_sentences nchildren total_months_in_prison i.drug_depabuse mental_health self_efficacy desire_change, or
outreg2 using job_clus.xls, lab eform nor2 addstat(Log likelihood, e(ll), gl, e(df_m), Pseudo R2, e(r2_p), N, e(N)) 

// Models for Job-Crime Clusters
logit clus_job_crime1 age i.only_primary i.any_previous_work i.early_crime i.class previous_sentences nchildren total_months_in_prison i.drug_depabuse mental_health self_efficacy desire_change, or
outreg2 using job_crime_clus.xls, lab eform nor2 addstat(Log likelihood, e(ll), gl, e(df_m), Pseudo R2, e(r2_p), N, e(N)) 

logit clus_job_crime2 age i.only_primary i.any_previous_work i.early_crime i.class previous_sentences nchildren total_months_in_prison i.drug_depabuse mental_health self_efficacy desire_change, or
outreg2 using job_crime_clus.xls, lab eform nor2 addstat(Log likelihood, e(ll), gl, e(df_m), Pseudo R2, e(r2_p), N, e(N)) 

logit clus_job_crime3 age i.only_primary i.any_previous_work i.early_crime i.class previous_sentences nchildren total_months_in_prison i.drug_depabuse mental_health self_efficacy desire_change, or
outreg2 using job_crime_clus.xls, lab eform nor2 addstat(Log likelihood, e(ll), gl, e(df_m), Pseudo R2, e(r2_p), N, e(N)) 

logit clus_job_crime4 age i.only_primary i.any_previous_work i.early_crime i.class previous_sentences nchildren total_months_in_prison i.drug_depabuse mental_health self_efficacy desire_change, or
outreg2 using job_crime_clus.xls, lab eform nor2 addstat(Log likelihood, e(ll), gl, e(df_m), Pseudo R2, e(r2_p), N, e(N)) 
