#############################################
# sequence analysis
# reentry work paper
# author: sebastian daza
#############################################


# libraries
library(data.table)
library(PST)
library(WeightedCluster)
library(TraMineRextras)
library(xtable)

# set seed to reproduce imputation
set.seed(210013)

# relative directory of the paper
path_paper = "reports/paper-work-lifecourse/"

source(paste0(path_paper, "src/utils.R"))

# all missing ids
all_missing_ids = c(10016, 10083, 10097, 10248, 20020, 20120, 20191,
                    20289, 20298, 30025, 30148, 30159, 40267, 50080,
                    50131, 50163, 50242, 50245)

# read data
dat = fread("output/bases/calendario_11_meses.csv")
class = fread("data/clases_latentes.csv")
dat = dat[reg_muestra == 1][!reg_folio %in% all_missing_ids]

setnames(class, c("FOLIO_2", "predclass3G"),
         c("reg_folio", "class"))

dat = merge(dat, class[, .(reg_folio, class)],
            by = "reg_folio", x.all = TRUE)

n = length(unique(dat$reg_folio))
print(paste0("Number of valid cases: ", n))

# create data.table object to save cluster membership
cluster_membership = data.table::copy(unique(dat[, .(reg_folio)]))

# job

# 1 = 0 = cuenta propia informal
# 2 = 1 = cuenta propia formal
# 3 = 10 = dependiente informal
# 4 = 11 = dependiente formal

work_columns = c("jobtype_1_oc", "jobtype_2_oc",
                 "jobtype_3_oc", "jobtype_4_oc")
dat[, (work_columns) := lapply(.SD, as.numeric),
    .SDcols = work_columns]

new_work_columns = c("jobtype_1", "jobtype_2",
                     "jobtype_3", "jobtype_4")

dat[, (new_work_columns) := lapply(.SD, function(x) ifelse(x > 0, 1, 0)),
    .SDcols = work_columns]

# job search
job_search = c("busco_trab2", "busco_trab")

dat[, (job_search) := lapply(.SD, as.numeric),
    .SDcols = job_search]

dat[busco_trab == 0, busco_trab2 :=
      ifelse(is.na(busco_trab2), 0, busco_trab2)]

dat[, jobsearch := busco_trab2]
table(dat$jobsearch)

# prison
prison_vars = c("carcel_dias", "carcel_prision_preventiva",
                "carcel_nueva_condena" )
dat[, (prison_vars) := lapply(.SD, as.numeric),
    .SDcols = prison_vars]

dat[carcel_prision_preventiva == 0 & carcel_nueva_condena == 0,
    carcel_dias := ifelse(is.na(carcel_dias), 0, carcel_dias)]

table(dat$carcel_prision_preventiva)
table(dat$carcel_nueva_condena)

dat[, prison := ifelse(carcel_dias > 0, 1, 0)]
table(dat$prison)
dat[, anyprison := getMax(prison), reg_folio]
prop.table(table(dat[, getMax(prison), reg_folio]$V1))


# crime
# here you define the variables to be added in a sequences
crime_vars = c("robo_habitado_congente_oc","robo_habitado_singente_oc", "robo_nohabitado_oc",
               "robo_cajeauto_oc", "robo_vehiculo_oc", "robo_en_vehiculo_oc", "robo_hurto_oc",
               "robo_robo_sorpresa_oc", "robo_robo_intimida_amenaza_oc", "robo_robo_intimida_arma_oc",
               "robo_robo_con_violencia_oc", "lesion_grave_oc", "homicidio_oc","amenazas_oc",
               "drogas_no_ventas_oc", "drogas_ventas_oc", "actividades_ilegales_oc", "receptacion_oc",
                "vif_oc","vandalismo_oc", "estafas_oc","porte_armas_oc")

no_income_crime = c("homicidio_oc", "amenazas_oc", "vif_oc", "vandalismo_oc",
                    "porte_armas_oc", "lesion_grave_oc")

crime_vars = crime_vars[!crime_vars %in% no_income_crime]

nc_crime_vars = gsub("_oc", "", crime_vars)

dat[, (c(crime_vars, nc_crime_vars, no_income_crime)) := lapply(.SD, as.numeric),
    .SDcols = c(crime_vars, nc_crime_vars, no_income_crime)]

for (i in seq_along(crime_vars)) {
    dat[get(nc_crime_vars[i]) == 0, (crime_vars[i]) :=
        ifelse(is.na(get(crime_vars[i])), 0, get(crime_vars[i]))]
}

# no income crimes
dat[, "no_income_crime" := apply(.SD, 1, flag_positive_values),
    .SDcols = no_income_crime]
dat[, no_income_crime := ifelse(is.na(no_income_crime), 0, no_income_crime)]
prop.table(table(dat[, getMax(no_income_crime), reg_folio]$V1))

# crime
dat[, "crime" := apply(.SD, 1, flag_positive_values),
    .SDcols = crime_vars]

# explore samples
ids = unique(dat$reg_folio)
dat[reg_folio == sample(ids, 1),
    c("reg_folio", "month_index",
      new_work_columns), with = FALSE]

# work
ids = unique(dat$reg_folio)
dat[reg_folio == sample(ids, 1),
   .(month_index, jobtype_4, jobtype_3, jobtype_2, jobtype_1)]

# 1 = 0 = cuenta propia informal
# 2 = 1 = cuenta propia formal
# 3 = 10 = dependiente informal
# 4 = 11 = dependiente formal

# recode combinations
# impute with zero when there is at least one valid value
dat[, jobtype_valid := apply(.SD, 1, function(x) any(!is.na(x))),
    .SDcols = new_work_columns]
dat[, (new_work_columns) := lapply(.SD,
                                   function(x) ifelse(jobtype_valid & is.na(x), 0, x)),
   .SDcols = new_work_columns]

dat[reg_folio == sample(ids, 1),
   .(month_index, jobtype_4, jobtype_3, jobtype_2, jobtype_1)]

dat[, jobtype := jobtype_4 * 1000 + jobtype_3 * 100 +
    jobtype_2 * 10 + jobtype_1]

dat[, anyjob := as.numeric(jobtype > 0)]
table(dat$anyjob)

dat[jobtype %in% c(1111, 1101, 1100, 1001, 1000), njobtype := 4]
dat[jobtype %in% c(110, 101, 100), njobtype := 3]
dat[jobtype %in% c(11, 10), njobtype := 2]
dat[jobtype == 1, njobtype := 1]
dat[jobtype == 0, njobtype := 0]

table(dat$njobtype)

# prison
seq_data_prison = create_sequences(
    data = dat,
    seq_variable = "prison",
    seq_labels = c("No Prison", "Prison"),
    columns = 3:13
  )

seqstatd(seq_data_prison)
seqmeant(seq_data_prison, prop = FALSE, serr = TRUE)

# job and searching
dat[, jobsearching := njobtype * 10 + jobsearch]
table(dat$jobsearching)

# 1 = 0 = cuenta propia informal
# 2 = 1 = cuenta propia formal
# 3 = 10 = dependiente informal
# 4 = 11 = dependiente formal

dat[jobsearching %in% c(10, 11), jobsearching := 2]
dat[jobsearching %in% c(20, 21), jobsearching := 2]
dat[jobsearching %in% c(30, 31), jobsearching := 3]
dat[jobsearching %in% c(40, 41), jobsearching := 4]

prop.table(table(dat[, any(jobsearching == 1), reg_folio]$V1))

dat[, anyjobsearch := getMax(jobsearch), reg_folio]

seq_data_search = create_sequences(
    data = dat,
    seq_variable = "jobsearching",
    seq_labels = c("None", "Searched",
                   "Self-employed",
                   "Employed U",
                   "Employed L"),
    columns = 3:13
  )

seqstatd(seq_data_search)
seqdplot(seq_data_search, cex.legend = 0.6,
             with.legend = "auto" )

# transition rates table
states_in = paste0("> ", c("None", "Searched",
                           "Self-employed",
                           "Employed U",
                           "Employed L"))
states_out = paste0(c("None", "Searched",
                      "Self-employed",
                      "Employed U",
                      "Employed L"), " >")

jobsearched.trate = seqtrate(seq_data_search)
jobsearched.trate
rownames(jobsearched.trate) = states_out
colnames(jobsearched.trate) = states_in

caption = paste0("Transition rates job search and employment (N = ", n, ")")
label = "tab:transition_rates_job_search"
comment = "Probability to switch at a given position from state $s_i$ to state $s_j$. U = Under-the-table, L = Legitimate."

add_notes_table(jobsearched.trate ,
                align = "lccccc",
                tabcolsep = 10,
                caption = caption,
                label = label,
                comment = comment,
                filename = paste0(path_paper, "output/transition_rates_job_search.tex")
                )

seqmeant(seq_data_search, prop = FALSE, serr = TRUE)

# any job
prop.table(table(dat[, getMax(anyjob), reg_folio]$V1))

seq_data_anyjob = create_sequences(
    data = dat,
    seq_variable = "anyjob",
    seq_labels = c("None", "Employed"),
    columns = 3:13
  )

seqstatd(seq_data_anyjob)
seqmeant(seq_data_anyjob, prop = TRUE, serr = TRUE)
seqmeant(seq_data_anyjob, prop = FALSE, serr = TRUE)

# all jobs (4 categories)
seq_data_jobs = create_sequences(
    data = dat,
    seq_variable = "njobtype",
    seq_labels = c("None", "Self-employed U",
                   "Self-employed L",
                   "Employed U",
                   "Employed L"),
    columns = 3:13
  )

seqstatd(seq_data_jobs)
savepdf(paste0(path_paper, "output/seq_dist_jobs"))
    seqdplot(seq_data_jobs, cex.legend = 0.6,
             with.legend = "right")
dev.off()

# jobs (3 categories)
dat[njobtype == 1, independent_job := 1]
dat[njobtype == 2, independent_job := 1]
dat[njobtype == 3, independent_job := 2]
dat[njobtype == 4, independent_job := 3]
dat[njobtype == 0, independent_job := 0]

table(dat$independent_job)

seq_data_jobs_se = create_sequences(
    data = dat,
    seq_variable = "independent_job",
    seq_labels = c("None", "Self-employed",
                   "Employed U",
                   "Employed L"),
    columns = 3:13)

# transition rates table
states_in = paste0("> ", c("None", "Self-employed",
              "Employed U",
              "Employed L"))
states_out = paste0(c("None", "Self-employed",
              "Employed U",
              "Employed L"), " >")

selfemployed.trate = seqtrate(seq_data_jobs_se)
rownames(selfemployed.trate) = states_out
colnames(selfemployed.trate) = states_in

caption = paste0("Transition rates jobs (N = ", n, ")")
label = "tab:transition_rates_jobs"
comment = "Probability to switch at a given position from state $s_i$ to state $s_j$. U = Under-the-table, L = Legitimate."

add_notes_table(selfemployed.trate,
                align = "lcccc",
                tabcolsep = 10,
                caption = caption,
                label = label,
                comment = comment,
                filename = paste0(path_paper, "output/transition_rates_job.tex")
                )

# compare clusters solutions
seq_data_jobs_se_distance = seqdist(seq_data_jobs_se, norm = "auto", method = "HAM")
benchmark_clusters = wcKMedRange(seq_data_jobs_se_distance, 2:6)

# create benchmark table for jobs
tcl = benchmark_clusters$stats
tcl = tcl[, c("ASW", "HG", "PBC")]
row.names(tcl) = paste0(2:6, " clusters")

caption = paste0("Employement quality measures for cluster solutions (N = ", n, ")")
label = "tab:quality_clusters_job"
comment = "ASW = Average Silhouette width, HG = Hubert's Gamma, PBC = Point Biserial Correlation."

add_notes_table(tcl,
                align = "lccc",
                tabcolsep = 35,
                caption = caption,
                label = label,
                comment = comment,
                filename = paste0(path_paper, "output/cluster_quality_job.tex")
                )

# 4 clusters seems the best solution
plot(benchmark_clusters, stat = c("ASW", "HG", "PBC"))

# create plots (self-employed = se)
cl_jobs_se_4 = create_clusters(seq_data_jobs_se, nclusters = 4,
                               method = "HAM")

cluster_labels_jobs_se = c("Unemployed", "Under-the-table",
                           "Legitimate employed",
                           "Self-employed")

create_plots(seq_data_jobs_se, cl_jobs_se_4[[1]],
             cluster_labels = cluster_labels_jobs_se,
             paste0(path_paper, "output/seq_job_se_4_clusters"), order = "sql")

cluster_membership[, cluster_job_se_4 := cl_jobs_se_4[["c4"]][[1]]]

# add crime
dat[, work_crime := independent_job]
table(dat$work_crime)
dat[, work_crime := work_crime * 10 + crime]
table(dat$work_crime)

dat[work_crime == 10, work_crime := 2]
dat[work_crime == 11, work_crime := 3]
dat[work_crime == 20, work_crime := 4]
dat[work_crime == 21, work_crime := 5]
dat[work_crime == 30, work_crime := 6]
dat[work_crime == 31, work_crime := 7]
table(dat$work_crime)

seq_data_jobs_crime = create_sequences(data = dat,
                            seq_variable = "work_crime",
                            seq_labels = c("None", "Crime",
                                           "Self-employed", "Self-employed - Crime",
                                           "Employed U", "Employed U - Crime",
                                           "Employed L", "Employed L - Crime"),
                            columns = 3:13)

savepdf(paste0(path_paper, "output/seq_dist_jobs_crime"))
    seqdplot(seq_data_jobs_crime, cex.legend=0.6,
             with.legend = "right")
dev.off()

# crime and employed vs self-employed
dat[njobtype == 1, type_job_em_se := 1]
dat[njobtype == 2, type_job_em_se := 1]
dat[njobtype == 3, type_job_em_se := 2]
dat[njobtype == 4, type_job_em_se := 2]
dat[njobtype == 0, type_job_em_se := 0]
table(dat$type_job_em_se)

dat[, work_crime_em_se := type_job_em_se * 10 + crime]
table(dat$work_crime_em_se)

dat[work_crime_em_se == 10, work_crime_em_se := 2]
dat[work_crime_em_se == 11, work_crime_em_se := 3]
dat[work_crime_em_se == 20, work_crime_em_se := 4]
dat[work_crime_em_se == 21, work_crime_em_se := 5]
table(dat$work_crime_em_se)

seq_data_job_crime_em_se = create_sequences(data = dat,
                                  seq_variable = "work_crime_em_se",
                                  seq_labels = c("None", "Crime",
                                                 "Self-employed", "Self-employed - Crime",
                                                 "Employed", "Employed - Crime"),
                                  columns = 3:13)

# compare clusters solutions
seq_data_job_crime_em_se_distance = seqdist(seq_data_job_crime_em_se,
                                            norm = "auto", method = "HAM")
benchmark_clusters = wcKMedRange(seq_data_job_crime_em_se_distance, 2:6)

# create benchmark table for jobs
tcl = benchmark_clusters$stats
tcl = tcl[, c("ASW", "HG", "PBC")]
row.names(tcl) = paste0(2:6, " clusters")

caption = paste0("Employement and crime quality measures for cluster solutions (N = ", n, ")")
label = "tab:quality_clusters_job_crime"
comment = "ASW = Average Silhouette width, HG = Hubert's Gamma, PBC = Point Biserial Correlation."

add_notes_table(tcl,
                align = "lccc",
                tabcolsep = 35,
                caption = caption,
                label = label,
                comment = comment,
                filename = paste0(path_paper, "output/cluster_quality_job_crime.tex")
                )

# 4 clusters seems the best solution
plot(benchmark_clusters, stat = c("ASW", "HG", "PBC"))

# 4-cluster solution
cl_job_crime_em_se_4 = create_clusters(seq_data_job_crime_em_se,
                                       nclusters = 4,
                                       method = "HAM",
                                       norm_distance = "auto")

cluster_labels_job_crime = c("Offenders", "Unemployed",
                             "Employed", "Self-employed")
create_plots(seq_data_job_crime_em_se, cl_job_crime_em_se_4[[1]],
             cluster_labels = cluster_labels_job_crime,
             paste0(path_paper, "output/seq_job_crime_em_se_4_clusters"),
             order = "sql")

cluster_membership[, cluster_job_crime_em_se_4 := cl_job_crime_em_se_4[["c4"]][[1]]]

# transition rates table
states_in = paste0("> ", c("None", "Crime","SE", "SE-Crime",
                            "E", "E-Crime"))
states_out = paste0(c("None", "Crime","SE", "SE-Crime",
                        "E", "E-Crime"), " >")

jobs_crime.trate = seqtrate(seq_data_job_crime_em_se)
rownames(jobs_crime.trate) = states_out
colnames(jobs_crime.trate) = states_in

caption = paste0("Transition rates jobs-crime (N = ", n, ")")
label = "tab:transition_rates_jobs_crime"
comment = "Probability to switch at a given position from state $s_i$ to state $s_j$. SE = Self-employed, E = Employed."

add_notes_table(jobs_crime.trate,
                align = "lcccccc",
                tabcolsep = 10,
                caption = caption,
                label = label,
                comment = comment,
                filename = paste0(path_paper, "output/transition_rates_job_crime.tex")
                )

# any job and crime
dat[, anyjob_crime := anyjob * 10 + crime]
table(dat$anyjob_crime)

seq_data_anyjobcrime = create_sequences(
    data = dat,
    seq_variable = "anyjob_crime",
    seq_labels = c("None", "Crime",
                   "Job",
                   "Job-Crime"),
    columns = 3:13
  )

# transition rates table
states_in = paste0("> ", c("None", "Crime",
                           "Job", "Job-Crime"))
states_out = paste0(c("None", "Crime",
                      "Job", "Job-Crime"), " >")


anyjobcrime.trate = seqtrate(seq_data_anyjobcrime)
rownames(anyjobcrime.trate) = states_out
colnames(anyjobcrime.trate) = states_in

caption = paste0("Transition rates job-crime (N = ", n, ")")
label = "tab:transition_rates_anyjob_crime"
comment = "Probability to switch at a given position from state $s_i$ to state $s_j$."

add_notes_table(anyjobcrime.trate,
                align = "lcccc",
                tabcolsep = 10,
                caption = caption,
                label = label,
                comment = comment,
                filename = paste0(path_paper, "output/transition_rates_anyjob_crime.tex")
                )
# save cluster membership
fwrite(cluster_membership,
       file = paste0(path_paper, "output/cluster_membership.csv"),
       row.names = FALSE)

# save sequence data
ccovs = c("anyjob", "anyjobsearch" , "anyprison")
saveRDS(list(cluster_labels_jobs_se, cluster_labels_job_crime),
        file = paste0(path_paper, "output/cluster_labels.rd"))
saveRDS(dat[, lapply(.SD, getMax), reg_folio, .SDcols = ccovs],
        file = paste0(path_paper, "output/calendar_covs.rd"))
saveRDS(seq_data_jobs_se, file = paste0(path_paper, "output/seq_data_job.rd"))
saveRDS(seq_data_jobs_se_distance, file = paste0(path_paper, "output/seq_data_job_distance.rd"))
saveRDS(seq_data_job_crime_em_se, file = paste0(path_paper, "output/seq_data_job_crime_em_se.rd"))
saveRDS(seq_data_job_crime_em_se_distance, file = paste0(path_paper, "output/seq_data_job_crime_em_se_distance.rd"))