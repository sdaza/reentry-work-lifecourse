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

# crime
crime_vars = c("robo_habitado_congente_oc","robo_habitado_singente_oc", "robo_nohabitado_oc",
               "robo_cajeauto_oc", "robo_vehiculo_oc", "robo_en_vehiculo_oc", "robo_hurto_oc",
               "robo_robo_sorpresa_oc", "robo_robo_intimida_amenaza_oc", "robo_robo_intimida_arma_oc",
               "robo_robo_con_violencia_oc", "lesion_grave_oc", "homicidio_oc","amenazas_oc",
               "drogas_no_ventas_oc", "drogas_ventas_oc", "actividades_ilegales_oc", "receptacion_oc",
                "vif_oc","vandalismo_oc", "estafas_oc","porte_armas_oc")

nc_crime_vars = gsub("_oc", "", crime_vars)

dat[, (c(crime_vars, nc_crime_vars)) := lapply(.SD, as.numeric),
    .SDcols = c(crime_vars, nc_crime_vars)]

for (i in seq_along(crime_vars)) {
    dat[get(nc_crime_vars[i]) == 0, (crime_vars[i]) :=
        ifelse(is.na(get(crime_vars[i])), 0, get(crime_vars[i]))]
}

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

dat[jobtype %in% c(1111, 1101, 1100, 1001, 1000), njobtype := 4]
dat[jobtype %in% c(110, 101, 100), njobtype := 3]
dat[jobtype %in% c(11, 10), njobtype := 2]
dat[jobtype == 1, njobtype := 1]
dat[jobtype == 0, njobtype := 0]

table(dat$njobtype)

# jobs
seq_data_jobs = create_sequences(
    data = dat,
    seq_variable = "njobtype",
    seq_labels = c("None", "Independent informal",
                   "Independent formal",
                   "Dependent informal",
                   "Dependent formal"),
    columns = 3:13
  )

savepdf(paste0(path_paper, "output/seq_dist_jobs"))
    seqdplot(seq_data_jobs, cex.legend = 0.6,
             with.legend = "auto" )
dev.off()

# transition rates table
states_in = paste0("-> ", c("None", "Ind informal",
              "Ind formal",
              "Dep informal",
              "Dep formal"))
states_out = paste0(c("None", "Ind informal",
              "Ind formal",
              "Dep informal",
              "Dep formal"), " ->")


jobs.trate = seqtrate(seq_data_jobs)
rownames(jobs.trate) = states_out
colnames(jobs.trate) = states_in

caption = paste0("Transition rates jobs (N = ", n, ")")
label = "tab:transition_rates_jobs"
comment = "Probability to switch at a given position from state $s_i$ to state $s_j$. Ind = Independent, Dep = Dependent."

add_notes_table(jobs.trate,
                align = "lccccc",
                tabcolsep = 10,
                caption = caption,
                label = label,
                comment = comment,
                filename = paste0(path_paper, "output/transtion_rates_job.tex")
                )

# independent job
dat[njobtype == 1, independent_job := 1]
dat[njobtype == 2, independent_job := 1]
dat[njobtype == 3, independent_job := 2]
dat[njobtype == 4, independent_job := 3]
dat[njobtype == 0, independent_job := 0]

table(dat$independent_job)

seq_data_jobs_ind = create_sequences(
    data = dat,
    seq_variable = "independent_job",
    seq_labels = c("None", "Independent",
                   "Dependent informal",
                   "Dependent formal"),
    columns = 3:13)

# compare clusters solutions
seq_data_jobs_ind_distance = seqdist(seq_data_jobs_ind, method = "HAM")
benchmark_clusters = wcKMedRange(seq_data_jobs_ind_distance, 2:6)

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

# create plots
cl_jobs_ind = create_clusters(seq_data_jobs_ind, nclusters = 3:6)
create_plots(seq_data_jobs_ind, cl_jobs_ind,
             paste0(path_paper, "output/seq_job_all_clusters"),
             order = "sql")

cl_jobs_ind_4 = create_clusters(seq_data_jobs_ind, nclusters = 4)
create_plots(seq_data_jobs_ind, cl_jobs_ind_4,
             paste0(path_paper, "output/seq_job_4_clusters"), order = "sql")

cluster_membership[, cluster_job_ind_4 := cl_jobs_ind_4[["c4"]][[1]]]

# add crime
dat[, work_crime := independent_job]
table(dat$work_crime)
dat[, work_crime := work_crime * 10 + crime]
table(dat$work_crime)

dat[, nwork_crime := work_crime]
dat[work_crime == 10, nwork_crime := 2]
dat[work_crime == 11, nwork_crime := 3]
dat[work_crime == 20, nwork_crime := 4]
dat[work_crime == 21, nwork_crime := 5]
dat[work_crime == 30, nwork_crime := 6]
dat[work_crime == 31, nwork_crime := 7]
table(dat$nwork_crime)

seq_data_jobs_crime = create_sequences(data = dat,
                            seq_variable = "nwork_crime",
                            seq_labels = c("None", "Crime",
                                           "Independent", "Independent-Crime",
                                           "Dep informal", "Dep informal-Crime",
                                           "Dep formal", "Dep formal-Crime"),
                            columns = 3:13)

savepdf(paste0(path_paper, "output/seq_dist_jobs_crime"))
    seqdplot(seq_data_jobs_crime, cex.legend=0.6,
             with.legend = "right")
dev.off()

# crime and independent + dependent informal
dat[njobtype == 1, type_job_1 := 1]
dat[njobtype == 2, type_job_1 := 1]
dat[njobtype == 3, type_job_1 := 1]
dat[njobtype == 4, type_job_1 := 2]
dat[njobtype == 0, type_job_1 := 0]
table(dat$type_job_1)

dat[, work_crime_1 := type_job_1]
table(dat$work_crime_1)
dat[, work_crime_1 := work_crime_1 * 10 + crime]
table(dat$work_crime_1)

dat[, nwork_crime_1 := work_crime_1]
dat[work_crime_1 == 10, nwork_crime_1 := 2]
dat[work_crime_1 == 11, nwork_crime_1 := 3]
dat[work_crime_1 == 20, nwork_crime_1 := 4]
dat[work_crime_1 == 21, nwork_crime_1 := 5]
table(dat$nwork_crime_1)

# trying to simplify labels
seq_data_job_crime_v1 = create_sequences(data = dat,
                                seq_variable = "nwork_crime_1",
                                seq_labels = c("None", "Crime",
                                               "Other jobs", "Other jobs-Crime",
                                               "Dep formal", "Dep formal-Crime"),
                                columns = 3:13)

# compare clusters solutions
seq_data_job_crime_v1_distance = seqdist(seq_data_job_crime_v1, method = "HAM")
benchmark_clusters = wcKMedRange(seq_data_job_crime_v1_distance, 2:6)

# create table job
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

# create plots
cl_job_crime_v1 = create_clusters(seq_data_job_crime_v1, nclusters = 3:6)
create_plots(seq_data_job_crime_v1, cl_job_crime_v1,
             paste0(path_paper, "output/seq_jobv1_all_clusters"),
             order = "sql")

cl_job_crime_v1_4 = create_clusters(seq_data_job_crime_v1, nclusters = 4)
create_plots(seq_data_job_crime_v1, cl_job_crime_v1_4,
             paste0(path_paper, "output/seq_jobv1_4_clusters"),
             order = "sql")

cluster_membership[, cluster_jobv1_4 := cl_job_crime_v1_4[["c4"]][[1]]]

# crime and dependent formal + dependent informal
dat[njobtype == 1, type_job_2 := 1]
dat[njobtype == 2, type_job_2 := 1]
dat[njobtype == 3, type_job_2 := 2]
dat[njobtype == 4, type_job_2 := 2]
dat[njobtype == 0, type_job_2 := 0]
table(dat$type_job_2)

dat[, work_crime_2 := type_job_2]
table(dat$work_crime_2)
dat[, work_crime_2 := work_crime_2 * 10 + crime]
table(dat$work_crime_2)

dat[, nwork_crime_2 := work_crime_2]
dat[work_crime_2 == 10, nwork_crime_2 := 2]
dat[work_crime_2 == 11, nwork_crime_2 := 3]
dat[work_crime_2 == 20, nwork_crime_2 := 4]
dat[work_crime_2 == 21, nwork_crime_2 := 5]
table(dat$nwork_crime_2)

seq_data_job_crime_v2 = create_sequences(data = dat,
                                  seq_variable = "nwork_crime_2",
                                  seq_labels = c("None", "Crime",
                                                 "Independent", "Independent-Crime",
                                                 "Dependent", "Dependent-Crime"),
                                  columns = 3:13)

# compare clusters solutions
seq_data_job_crime_v2_distance = seqdist(seq_data_job_crime_v2, method = "HAM")
benchmark_clusters = wcKMedRange(seq_data_job_crime_v2_distance, 2:6)

# 4 clusters seems the best solution
plot(benchmark_clusters, stat = c("ASW", "HG", "PBC"))

# create table

# create plots
cl_job_crime_v2 = create_clusters(seq_data_job_crime_v2, nclusters = 3:6)
create_plots(seq_data_job_crime_v2, cl_job_crime_v2,
             paste0(path_paper, "output/seq_jobv2_all_clusters"),
             order = "sql")

cl_job_crime_v2_4 = create_clusters(seq_data_job_crime_v2, nclusters = 4)

create_plots(seq_data_job_crime_v2, cl_job_crime_v2_4,
             paste0(path_paper, "output/seq_jobv2_4_clusters"),
             order = "sql")

cluster_membership[, cluster_jobv2_4 := cl_job_crime_v2_4[["c4"]][[1]]]


# transition rates table
states_in = paste0("-> ", c("None", "Crime","Ind", "Ind-Crime",
                            "Dep", "Dep-Crime"))
states_out = paste0(c("None", "Crime","Ind", "Ind-Crime",
                            "Dep", "Dep-Crime"), " ->")

jobs_crime.trate = seqtrate(seq_data_job_crime_v2)
rownames(jobs_crime.trate) = states_out
colnames(jobs_crime.trate) = states_in

caption = paste0("Transition rates jobs-crime (N = ", n, ")")
label = "tab:transition_rates_jobs_crime"
comment = "Probability to switch at a given position from state $s_i$ to state $s_j$. Ind = independent, Dep = Dependent."

add_notes_table(jobs_crime.trate,
                align = "lcccccc",
                tabcolsep = 10,
                caption = caption,
                label = label,
                comment = comment,
                filename = paste0(path_paper, "output/transtion_rates_job_crime.tex")
                )


# save cluster membership
fwrite(cluster_membership,
       file = paste0(path_paper, "output/cluster_membership.csv"),
       row.names = FALSE)

# save sequence data
saveRDS(seq_data_jobs_ind, file = paste0(path_paper, "output/seq_data_job.rd"))
saveRDS(seq_data_jobs_ind_distance, file = paste0(path_paper, "output/seq_data_job_distance.rd"))
saveRDS(seq_data_job_crime_v2, file = paste0(path_paper, "output/seq_data_job_crime_v2.rd"))
saveRDS(seq_data_job_crime_v2_distance, file = paste0(path_paper, "output/seq_data_job_crime_v2_distance.rd"))