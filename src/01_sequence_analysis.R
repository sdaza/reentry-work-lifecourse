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
source("src/calendario/utils.R")

# all missing ids
all_missing_ids = c(10016, 10083, 10097, 10248, 20020, 20120, 20191,
    20289, 20298, 30025, 30148, 30159, 40267, 50080,
    50131, 50163, 50242, 50245)

colors5 = c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac")
colors6 = c("#ffff99", "#e31a1c",  "#a6cee3", "#fb9a99", "#1f78b4", "#beaed4")
colors7 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
    "#e31a1c", "#fdbf6f")
colors8 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
    "#e31a1c", "#fdbf6f", "#ff7f00")

# read data
dat = readRDS(paste0(path_paper, "output/job_calendar.rds"))
class = fread("data/clases_latentes.csv")
setnames(class, c("FOLIO_2", "predclass3G"),
         c("reg_folio", "class"))

dat = dat[reg_muestra == 1][!reg_folio %in% all_missing_ids]
dat = merge(dat, class[, .(reg_folio, class)],
            by = "reg_folio", x.all = TRUE)

n = length(unique(dat$reg_folio))
print(paste0("Number of valid cases: ", n))

# create data.table object to save cluster membership
cluster_membership = data.table::copy(unique(dat[, .(reg_folio)]))

# check 12 months
table(dat[month_index == 12, days_month])
ids = dat[month_index == 12 & days_month == 0, reg_folio]

wave_summary = createWaveSummary(
    path_to_data = "output/bases/base_general_wide.csv")

table(wave_summary[reg_folio %in% ids & wave == 4, sequence_waves])
table(wave_summary[sequence_waves %in% c("001", "011", "101", "111") &
    wave == 4, cum_months] < 11.75)

# 7 dropouts
# 12 before month 12
# 78 with less than 3/4 of the month

# jobs

# 1 = cuenta propia informal
# 2 = cuenta propia formal
# 3 = dependiente informal
# 4 = dependiente formal

work_columns = paste0("trabajo", 1:7, "_oc")

# get max of jobs by time
dat[, job := apply(.SD, 1, getMax), .SDcols = work_columns]
table(dat$job)

# # checking
# dat[reg_folio == sample(unique(dat$reg_folio), 1),
#     c("reg_folio", "month_index", work_columns, "job"), with = FALSE]

# job search
table(dat$trab_busco_oc)

# prison, check filter
table(dat$jst_carcel_dias)
dat[, prison := ifelse(jst_carcel_dias > 0, 1, 0)]
table(dat$prison)
dat[, anyprison := getMax(prison), reg_folio]
prop.table(table(dat[, getMax(prison), reg_folio]$V1))

# crime
# here you define the variables to be added in a sequences
crime_vars = c("rb_habitgente_oc", "rb_habitsin_oc", "rb_nohabit_oc",
    "rb_atm_oc", "rb_vehi_oc", "rb_en_vehi_oc", "rb_hurto_oc",
    "rb_sorpresa_oc", "rb_intim_ame_oc", "rb_intim_arma_oc",
    "rb_violencia_oc", "lesion_oc", "homi_oc", "amenazas_oc",
    "drgs_prep_oc", "drgs_ventas_oc", "act_ilegal_oc", "recepta_oc",
    "vif_oc", "vandalismo_oc", "estafas_oc", "armas_oc")
no_income_crime = c("homi_oc", "amenazas_oc", "vif_oc", "vandalismo_oc",
    "armas_oc", "lesion_oc")

crime_vars = crime_vars[!crime_vars %in% no_income_crime]
nc_crime_vars = gsub("_oc", "", crime_vars)

# crime
dat[, "crime" := apply(.SD, 1, flag_positive_values),
    .SDcols = crime_vars]

# any job
dat[, anyjob := as.numeric(job > 0)]
table(dat$anyjob)

dat[is.na(days_month), days_month := 0]
length(unique(dat[month_index == 12 & (days_month <= 15), reg_folio])) /
    length(unique(dat[month_index == 12, reg_folio]))

 dat
# prison
seq_data_prison = create_sequences(
    data = dat,
    seq_variable = "prison",
    seq_labels = c("No Prison", "Prison")
  )

seqstatd(seq_data_prison)
seqmeant(seq_data_prison, prop = FALSE, serr = TRUE)

# job and searching
dat[, jobsearching := job * 10 + trab_busco_oc]
table(dat$jobsearching)

# 1 = 0 = cuenta propia informal
# 2 = 1 = cuenta propia formal
# 3 = 10 = dependiente informal
# 4 = 11 = dependiente formal

dat[jobsearching %in% c(10, 11), jobsearching := 2]
dat[jobsearching %in% c(20, 21), jobsearching := 2]
dat[jobsearching %in% c(30, 31), jobsearching := 3]
dat[jobsearching %in% c(40, 41), jobsearching := 4]
table(dat$jobsearching)

prop.table(table(dat[, any(jobsearching == "Searched"), reg_folio]$V1))
dat[, anyjobsearch := getMax(trab_busco_oc), reg_folio]

labs = c("None", "Searched", "Self-employed", "Employed U", "Employed L")

seq_data_search = create_sequences(
    data = dat,
    seq_variable = "jobsearching",
    seq_labels = labs,
    colors = colors5
)

savepdf(paste0(path_paper, "output/seq_dist_job_search"))
seqdplot(seq_data_search,
    with.legend = FALSE, ltext = labs)
mtext(side = 1, line = 2.3, "Months", font = 1, cex = 1)
legend(1.5, 1.08, legend = labs, cex = 0.7, pch = 15, col = colors5,
    horiz = TRUE,  bty = "n", inset = c(0, 2), xpd = TRUE,
    text.width=c(0,0.7,0.9,1.2,1.3))
dev.off()

# transition rates table
states_in = paste0("> ", c("None", "Searched", "Self-employed",
    "Employed U", "Employed L"))
states_out = paste0(c("None", "Searched","Self-employed","Employed U",
    "Employed L"), " >")

jobsearched.trate = seqtrate(seq_data_search)
rownames(jobsearched.trate) = states_out
colnames(jobsearched.trate) = states_in

caption = paste0("Transition rates between job search and job categories of women inmates \\newline
    during the first 12 months following their release (N = ", n, " $\\times$ 11)")
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
)

seqstatd(seq_data_anyjob)
seqmeant(seq_data_anyjob, prop = TRUE, serr = TRUE)
seqmeant(seq_data_anyjob, prop = FALSE, serr = TRUE)

# all job categories (4)
labs = c("None", "Self-employed U", "Self-employed L", "Employed U", "Employed L")
seq_data_jobs = create_sequences(
    data = dat,
    seq_variable = "job",
    seq_labels = labs,
    colors = colors5
)
seqstatd(seq_data_jobs)

savepdf(paste0(path_paper, "output/seq_dist_jobs"))
seqdplot(seq_data_jobs,
    with.legend = FALSE, ltext = labs)
mtext(side = 1, line = 2.3, "Months", font = 1, cex = 1)
legend(1.1, 1.08, legend = labs, cex = 0.7, pch = 15, col = colors5,
    horiz = TRUE,  bty = "n", inset = c(0, 2), xpd = TRUE,
    text.width=c(0,0.7,1.3,1.5,1.5))
dev.off()

# jobs (3 categories)
dat[job == 1, independent_job := 1]
dat[job == 2, independent_job := 1]
dat[job == 3, independent_job := 2]
dat[job == 4, independent_job := 3]
dat[job == 0, independent_job := 0]

table(dat$independent_job)
labs = c("None", "Self-employed", "Employed U", "Employed L")
seq_data_jobs_se = create_sequences(
    data = dat,
    seq_variable = "independent_job",
    seq_labels = labs,
    colors = colors5[2:5]
)

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

caption = paste0("Transition rates between job categories of women inmates \\newline
    during the first 12 months following their release (N = ", n, " $\\times$ 11)")
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
cluster_labels_jobs_se = c("Under-the-table", "Legitimate employed",
    "Self-employed", "Unemployed")
# cluster_levels_jobs_se   = cluster_labels_jobs_se
cluster_levels_jobs_se = c("Unemployed", "Self-employed",
                           "Under-the-table", "Legitimate employed")

cl_jobs_se_4 = create_clusters(seq_data_jobs_se, nclusters = 4,
    method = "HAM",
    cluster_labels = cluster_labels_jobs_se,
    cluster_levels = cluster_levels_jobs_se,
)

create_plots(seq_data_jobs_se, cl_jobs_se_4[[1]],
    paste0(path_paper, "output/seq_job_se_4_clusters"),
    order = "sql"
)

cluster_membership[, cluster_job_se_4 := cl_jobs_se_4[["c4"]][[1]]]

# explore specific clusters
select_cluster = "Under-the-table"

temp = data.table(
    by(seq_data_jobs_se,
        cluster_membership$cluster_job_se_4,
        seqistatd)[[select_cluster]]
    )
setnames(temp, names(temp), paste0("c", 1:4))

temp = temp[, lapply(.SD, function(x) ifelse(x > 0, 1, 0))]
temp[, otherthan := apply(.SD, 1, sum), .SDcols = c("c1", "c2", "c4")]
temp
temp[, only := ifelse(c3 == 1 & otherthan == 0, 1, 0)]
prop.table(table(temp$only))
by(seq_data_jobs_se, cluster_membership$cluster_job_se_4, seqstatf)[[select_cluster]]
by(seq_data_jobs_se, cluster_membership$cluster_job_se_4, seqstatd)[[select_cluster]]
by(seq_data_jobs_se, cluster_membership$cluster_job_se_4, seqmeant, prop = FALSE)[[select_cluster]]

# job and crime
dat[, work_crime := independent_job]
table(dat$crime)
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

labs = c("None", "Crime","Self-employed", "Self-employed - Crime",
    "Employed U", "Employed U - Crime", "Employed L", "Employed L - Crime")
seq_data_jobs_crime = create_sequences(data = dat,
    seq_variable = "work_crime",
    seq_labels = labs,
    colors = colors8
)

savepdf(paste0(path_paper, "output/seq_dist_jobs_crime"),
    mar = c(3.3,3.6,2.5,1.1), mgp = c(2.7,0.45,0))
seqdplot(seq_data_jobs_crime,
    with.legend = FALSE, ltext = labs)
mtext(side = 1, line = 2.3, "Months", font = 1, cex = 1)
legend(1.2, 1.15, legend = labs, cex = 0.7, pch = 15, col = colors8,
    ncol = 4, bty = "n", inset = c(0, 2), xpd = TRUE,
    text.width=c(0,0.7,0.7,0.7,1.7, 1.7,1.9,1.9)
    )
dev.off()

seqstatf(seq_data_jobs_crime)
seqstatd(seq_data_jobs_crime)
seqmeant(seq_data_jobs_crime)

tt = apply(
    seqstatd(seq_data_jobs_crime)[[1]][c(4,6,8),],
    2,
    sum
)

mean(tt)
min(tt)
temp = data.table(seqistatd(seq_data_jobs_crime))

setnames(temp, names(temp), paste0("c", 1:8))
temp = temp[, lapply(.SD, function(x) ifelse(x > 0, 1, 0))]
temp[, otherthan := apply(.SD, 1, sum), .SDcols = paste0("c", c(4,6,8))]
prop.table(table(temp$otherthan > 0))

# crime and employed vs self-employed
dat[job == 1, type_job_em_se := 1]
dat[job == 2, type_job_em_se := 1]
dat[job == 3, type_job_em_se := 2]
dat[job == 4, type_job_em_se := 2]
dat[job == 0, type_job_em_se := 0]
table(dat$type_job_em_se)

dat[, work_crime_em_se := type_job_em_se * 10 + crime]
table(dat$work_crime_em_se)

dat[work_crime_em_se == 10, work_crime_em_se := 2]
dat[work_crime_em_se == 11, work_crime_em_se := 3]
dat[work_crime_em_se == 20, work_crime_em_se := 4]
dat[work_crime_em_se == 21, work_crime_em_se := 5]
table(dat$work_crime_em_se)

labs = c("None", "Crime", "Self-employed", "Self-employed - Crime",
    "Employed", "Employed - Crime")

seq_data_job_crime_em_se = create_sequences(
    data = dat,
    seq_variable = "work_crime_em_se",
    seq_labels = labs,
    colors = colors7[1:6]
)

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
cluster_labels_job_crime = c("Employed", "Unemployed",
    "Self-employed", "Offenders")
# cluster_labels_job_crime = cluster_levels_job_crime
cluster_levels_job_crime = c("Unemployed", "Offenders",
    "Self-employed", "Employed")

cl_job_crime_em_se_4 = create_clusters(seq_data_job_crime_em_se,
    nclusters = 4,
    method = "HAM",
    norm_distance = "auto",
    cluster_labels = cluster_labels_job_crime,
    cluster_levels = cluster_levels_job_crime
)

create_plots(seq_data_job_crime_em_se, cl_job_crime_em_se_4[[1]],
    paste0(path_paper, "output/seq_job_crime_em_se_4_clusters"),
    order = "sql")

cluster_membership[, cluster_job_crime_em_se_4 := cl_job_crime_em_se_4[["c4"]][[1]]]

# select_cluster = "Legitimate employed"
select_cluster = "Self-employed"
select_cluster = "Employed"

temp = data.table(
    by(seq_data_job_crime_em_se,
        cluster_membership$cluster_job_crime_em_se_4, seqistatd)[[select_cluster]]
)

setnames(temp, names(temp), paste0("c", 1:6))

temp = temp[, lapply(.SD, function(x) ifelse(x > 0, 1, 0))]
temp[, otherthan := apply(.SD, 1, sum), .SDcols = paste0("c", c(2))]

temp[, otherthan := apply(.SD, 1, sum), .SDcols = paste0("c", c(3,5))]
temp[, only := ifelse(c1 == 1 & otherthan == 0, 1, 0)]
prop.table(table(temp$only))
prop.table(table(temp$otherthan > 0))

by(seq_data_job_crime_em_se, cluster_membership$cluster_job_crime_em_se_4,
    seqstatf)[[select_cluster]]
by(seq_data_job_crime_em_se, cluster_membership$cluster_job_crime_em_se_4,
    seqstatd)[[select_cluster]]
by(seq_data_job_crime_em_se, cluster_membership$cluster_job_crime_em_se_4,
    seqmeant, prop = FALSE)[[select_cluster]]
by(seq_data_search, cluster_membership$cluster_job_crime_em_se_4,
    seqtrate)[[select_cluster]]

# transition rates table
states_in = paste0("> ", c("None", "Crime","SE", "SE-Crime", "E", "E-Crime"))
states_out = paste0(c("None", "Crime","SE", "SE-Crime", "E", "E-Crime"), " >")

jobs_crime.trate = seqtrate(seq_data_job_crime_em_se)
rownames(jobs_crime.trate) = states_out
colnames(jobs_crime.trate) = states_in

caption = paste0("Transition rates between job and crime of women inmates \\newline
    during the first 12 months following their release (N = ", n, " $\\times$ 11)")
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
    seq_labels = c("None", "Crime", "Job", "Job-Crime"),
  )

# transition rates table
states_in = paste0("> ", c("None", "Crime", "Job", "Job-Crime"))
states_out = paste0(c("None", "Crime", "Job", "Job-Crime"), " >")

anyjobcrime.trate = seqtrate(seq_data_anyjobcrime)
rownames(anyjobcrime.trate) = states_out
colnames(anyjobcrime.trate) = states_in

caption = paste0("Transition rates between job and crime of women inmates \\newline
    during the first 12 months following their release (N = ", n, " $\\times$ 11)")
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
saveRDS(list(cluster_levels_jobs_se, cluster_levels_job_crime),
    file = paste0(path_paper, "output/cluster_labels.rds"))
saveRDS(dat[, lapply(.SD, getMax), reg_folio, .SDcols = ccovs],
    file = paste0(path_paper, "output/calendar_covs.rds"))
saveRDS(seq_data_jobs_se, file = paste0(path_paper, "output/seq_data_job.rds"))
saveRDS(seq_data_jobs_se_distance,
    file = paste0(path_paper, "output/seq_data_job_distance.rds"))
saveRDS(seq_data_job_crime_em_se,
    file = paste0(path_paper, "output/seq_data_job_crime_em_se.rds"))
saveRDS(seq_data_job_crime_em_se_distance,
    file = paste0(path_paper, "output/seq_data_job_crime_em_se_distance.rds"))