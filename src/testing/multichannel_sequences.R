
## 4. Examples with life course data
## 4.1. Sequence data

library(seqHMM)

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

wave_summary = createWaveSummary(
    path_to_data = "output/bases/base_general_wide.csv")

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

# prison
seq_prison = create_sequences(
    data = dat,
    seq_variable = "prison",
    seq_labels = c("No Prison", "Prison"),
    states = c("No prison", "Prison")
  )

attr(seq_prison, "cpal") <- c("lightblue", "red3")
attr(seq_prison, "labels")

seqstatd(seq_prison)
seqmeant(seq_prison, prop = FALSE, serr = TRUE)

# searching
dat[, searching := trab_busco_oc]
seq_search = create_sequences(
    data = dat,
    seq_variable = "searching",
    seq_labels = c("No search", "Search"),
    states = c("NS", "S")
)

attr(seq_search, "cpal") <- c("darkseagreen1", "coral3")

seqstatd(seq_search)
seqmeant(seq_search, prop = FALSE, serr = TRUE)

# job
labs = c("None", "Self-employed U", "Self-employed L", "Employed U", "Employed L")
seq_jobs = create_sequences(
    data = dat,
    seq_variable = "job",
    seq_labels = labs,
    states = c("No Job", "Self under", "Self legal", "Employed under", "Employed L"),
    colors = colors5
)

seqstatd(seq_jobs)
seqmeant(seq_jobs, prop = FALSE, serr = TRUE)

# crime
labs = c("No crime", "Crime")
seq_crime = create_sequences(
    data = dat,
    seq_variable = "crime",
    seq_labels = labs,
    states = c("No crime", "Crime")
)

seqstatd(seq_crime)
seqmeant(seq_crime, prop = FALSE, serr = TRUE)

# data("biofam", package = "TraMineR")
# biofam_seq <- seqdef(biofam[, 10:25], start = 15, labels = c("parent",
#   "left", "married", "left+marr", "child", "left+child", "left+marr+ch",
#   "divorced"))

# data("biofam3c", package = "seqHMM")
# marr_seq <- seqdef(biofam3c$married, start = 15, alphabet = c("single",
#   "married", "divorced"))
# child_seq <- seqdef(biofam3c$children, start = 15,
#   alphabet = c("childless", "children"))
# left_seq <- seqdef(biofam3c$left, start = 15, alphabet = c("with parents",
#   "left home"))

# attr(marr_seq, "cpal") <- c("violetred2", "darkgoldenrod2", "darkmagenta")
# attr(child_seq, "cpal") <- c("darkseagreen1", "coral3")
# attr(left_seq, "cpal") <- c("lightblue", "red3")

## all sequences plotted together
list_sequences = list(seq_crime, seq_prison, seq_jobs)
ssplot(list_sequences)

ssplot(list_sequences,
  type = "I", sortv = "mds.obs", legend.pos = "bottom", legend.pos2 = "top",
  row.prop = c(0.65, 0.35),
  ylab.pos = c(1, 2, 1), xtlab = 1:12, ylab = c("Crime", "Prison", "Search", "Job"))

# gridplot(list(ssp_f, ssp_m), ncol = 2, nrow = 2, byrow = TRUE,
#   legend.pos = "bottom", legend.pos2 = "top", row.prop = c(0.65, 0.35))


## hidden markov models

# ## Multi-channel model
# mc_init = c(0.9, 0.05, 0.02, 0.02, 0.01)

# mc_trans = matrix(c(0.80, 0.10, 0.05, 0.03, 0.02, 0, 0.90, 0.05, 0.03,
#   0.02, 0, 0, 0.90, 0.07, 0.03, 0, 0, 0, 0.90, 0.10, 0, 0, 0, 0, 1),
#   nrow = 5, ncol = 5, byrow = TRUE)

# mc_emiss_marr = matrix(c(0.90, 0.05, 0.05, 0.90, 0.05, 0.05, 0.05, 0.90,
#   0.05, 0.05, 0.90, 0.05, 0.30, 0.30, 0.40), nrow = 5, ncol = 3,
#   byrow = TRUE)

# mc_emiss_child = matrix(c(0.9, 0.1, 0.9, 0.1, 0.1, 0.9, 0.1, 0.9, 0.5,
#   0.5), nrow = 5, ncol = 2, byrow = TRUE)

# mc_emiss_left = matrix(c(0.9, 0.1, 0.1, 0.9, 0.1, 0.9, 0.1, 0.9, 0.5,
#   0.5), nrow = 5, ncol = 2, byrow = TRUE)

mc_obs = list_sequences

bic = NULL
for (i in 2:20) {
    print(paste("iteration ", i, " ::::::::"))
    mc_initmod = build_hmm(observations = mc_obs, n_states = i,
        channel_names = c("Crime", "Prison", "Search", "Job"))
    mc_fit = fit_model(mc_initmod, em_step = TRUE, local_step = TRUE,
        threads = 4,  control_em = list(restart = list(times = 100)))
    hmm_job = mc_fit$model
    bic = c(bic, BIC(hmm_job))
}

mc_initmod = build_hmm(observations = mc_obs, n_states = 5,
    channel_names = c("Crime", "Prison", "Job"))
mc_fit = fit_model(mc_initmod, em_step = FALSE, local_step = TRUE,
        threads = 4,  control_em = list(restart = list(times = 100)))

hmm_job = mc_fit$model
BIC(hmm_job)

hmm_job

## visualizing
plot(hmm_job)

plot(hmm_job, vertex.size = 50, vertex.label.dist = 4.5, with.legend = "right",  legend.prop = 0.3,
     combined.slice.label = "States with prob. < 0.05",
     vertex.label.pos = c("left", "top", "bottom", "right", "left"),)


vertex_layout <- matrix(c(1, 2, 2, 3, 1, 0, 0.5, -0.5, 0, -1),
                        ncol = 2)

savepdf("output/multichannel_test")
plot(hmm_job, layout = vertex_layout, xlim = c(0.5, 3.5),
     ylim = c(-1.5, 1), rescale = FALSE, vertex.size = 50,
     vertex.label.pos = c("left", "top", "bottom", "right", "left"),
     edge.curved = FALSE, edge.width = 1, edge.arrow.size = 1,
     with.legend = "left", legend.prop = 0.4, label.signif = 1,
     combine.slices = 0)
dev.off()

# create distance matrix
mcdist = seqdistmc(channels= mc_obs, method="HAM")
benchmark_clusters = wcKMedRange(mcdist, 2:10)
benchmark_clusters

test = create_clusters(mc_obs, nclusters = 6,
    method = "HAM", multichannel = TRUE,
        cluster_labels = 1:6,
    cluster_levels = 1:6,
)

table(test$c6[[1]])

## clustering and mixture hidden Markov models
# mc_init2 <- c(0.9, 0.05, 0.03, 0.02)

# mc_trans2 <- matrix(c(0.85, 0.05, 0.05, 0.05, 0, 0.90, 0.05, 0.05, 0, 0,
#   0.95, 0.05, 0, 0, 0, 1), nrow = 4, ncol = 4, byrow = TRUE)

# mc_emiss_marr2 <- matrix(c(0.90, 0.05, 0.05, 0.90, 0.05, 0.05, 0.05,
#   0.85, 0.10, 0.05, 0.80, 0.15), nrow = 4, ncol = 3, byrow = TRUE)

# mc_emiss_child2 <- matrix(c(0.9, 0.1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
#   nrow = 4, ncol = 2, byrow = TRUE)

# mc_emiss_left2 <- matrix(c(0.9, 0.1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
#   nrow = 4, ncol = 2, byrow = TRUE)

# mhmm_init <- list(mc_init, mc_init2)

# mhmm_trans <- list(mc_trans, mc_trans2)

# mhmm_emiss <- list(list(mc_emiss_marr, mc_emiss_child, mc_emiss_left),
#   list(mc_emiss_marr2, mc_emiss_child2, mc_emiss_left2))

# biofam3c$covariates$cohort <- cut(biofam3c$covariates$birthyr,
#   c(1908, 1935, 1945, 1957))
# biofam3c$covariates$cohort <- factor(biofam3c$covariates$cohort,
#   labels=c("1909-1935", "1936-1945", "1946-1957"))

init_mhmm <- build_mhmm(observations = mc_obs, n_states = 5,
  channel_names = c("Crime", "Prison", "Search", "Job"))

set.seed(1011)
mhmm_fit <- fit_model(init_mhmm, local_step = TRUE, threads = 4,
  control_em = list(restart = list(times = 100)))
mhmm <- mhmm_fit$model

summary(mhmm, conditional_se = FALSE)

## 4.4. Visualizing hidden Markov models
plot(hmm_biofam,
    combined.slice.label = "pr<0.05")

## Figure 4
plot(hmm_biofam, vertex.size = 50, vertex.label.dist = 4.5,
     edge.curved = c(0, 0.6, -0.8, 0.6, 0, 0.6, 0), legend.prop = 0.3,
     combined.slice.label = "States with prob. < 0.05")

## Figure 5
vertex_layout <- matrix(c(1, 2, 2, 3, 1, 0, 0.5, -0.5, 0, -1),
                        ncol = 2)
plot(hmm_biofam, layout = vertex_layout, xlim = c(0.5, 3.5),
     ylim = c(-1.5, 1), rescale = FALSE, vertex.size = 50,
     vertex.label.pos = c("left", "top", "bottom", "right", "left"),
     edge.curved = FALSE, edge.width = 1, edge.arrow.size = 1,
     with.legend = "left", legend.prop = 0.4, label.signif = 1,
     combine.slices = 0, cpal = colorpalette[[30]][14:5])

## Figure 6
ssplot(hmm_biofam, plots = "both", type = "I", sortv = "mds.hidden",
       title = "Observed and hidden state sequences", xtlab = 15:30,
       xlab = "Age")

## 4.5. Visualizing mixture hidden Markov models

## MHMM plot in a grid
plot(mhmm, interactive = FALSE, nrow = 2, legend.prop = 0.45,
  vertex.size = 50, vertex.label.cex = 1.3, cex.legend = 1.3,
  edge.curved = 0.65, edge.label.cex = 1.3, cex.edge.width = 0.8,
  edge.arrow.size = 0.8)

## Interactive MHMM plot
if (interactive()) {
    mssplot(mhmm, ask = TRUE)
}

## 4. Examples with life course data
## 4.1. Sequence data

library("seqHMM")

data("biofam", package = "TraMineR")
biofam_seq <- seqdef(biofam[, 10:25], start = 15, labels = c("parent",
  "left", "married", "left+marr", "child", "left+child", "left+marr+ch",
  "divorced"))

data("biofam3c", package = "seqHMM")
marr_seq <- seqdef(biofam3c$married, start = 15, alphabet = c("single",
  "married", "divorced"))
child_seq <- seqdef(biofam3c$children, start = 15,
  alphabet = c("childless", "children"))
left_seq <- seqdef(biofam3c$left, start = 15, alphabet = c("with parents",
  "left home"))

attr(marr_seq, "cpal") <- c("violetred2", "darkgoldenrod2", "darkmagenta")
attr(child_seq, "cpal") <- c("darkseagreen1", "coral3")
attr(left_seq, "cpal") <- c("lightblue", "red3")

## Figure 2
ssplot(list("Marriage" = marr_seq, "Parenthood" = child_seq,
  "Residence" = left_seq))

## Figure 1
seq_data <- list(biofam_seq[1:10, ], marr_seq[1:10, ], child_seq[1:10, ],
  left_seq[1:10, ])
ssplot(seq_data, type = "I", sortv = "from.start", sort.channel = 1,
  ylab = c("Original", "Marriage", "Parenthood", "Residence"),
  xtlab = 15:30, xlab = "Age", ylab.pos = c(1, 1.5), title.n = FALSE,
  title = "Ten first sequences", legend.prop = 0.63,
  ncol.legend = c(3, 1, 1, 1))

## Figure 3
ssp_f <- ssp(list(marr_seq[biofam3c$covariates$sex == "woman", ],
    child_seq[biofam3c$covariates$sex == "woman", ],
    left_seq[biofam3c$covariates$sex == "woman", ]),
  type = "I", sortv = "mds.obs", with.legend = FALSE, title = "Women",
  ylab.pos = c(1, 2, 1), xtlab = 15:30, ylab = c("Married", "Children",
    "Residence"))

ssp_m <- update(ssp_f, title = "Men",
  x = list(marr_seq[biofam3c$covariates$sex == "man", ],
    child_seq[biofam3c$covariates$sex == "man", ],
    left_seq[biofam3c$covariates$sex == "man", ]))

gridplot(list(ssp_f, ssp_m), ncol = 2, nrow = 2, byrow = TRUE,
  legend.pos = "bottom", legend.pos2 = "top", row.prop = c(0.65, 0.35))


## 4.2. Hidden Markov models

## Single-channel model with automatic starting values
sc_initmod_random <- build_hmm(observations = biofam_seq, n_states = 5)

## Single-channel model with user-defined starting values
sc_init <- c(0.9, 0.06, 0.02, 0.01, 0.01)

sc_trans <- matrix(c(0.80, 0.10, 0.05, 0.03, 0.02, 0.02, 0.80, 0.10,
  0.05, 0.03, 0.02, 0.03, 0.80, 0.10, 0.05, 0.02, 0.03, 0.05, 0.80, 0.10,
  0.02, 0.03, 0.05, 0.05, 0.85), nrow = 5, ncol = 5, byrow = TRUE)

sc_emiss <- matrix(NA, nrow = 5, ncol = 8)
sc_emiss[1, ] <- seqstatf(biofam_seq[, 1:4])[, 2] + 0.1
sc_emiss[2, ] <- seqstatf(biofam_seq[, 5:7])[, 2] + 0.1
sc_emiss[3, ] <- seqstatf(biofam_seq[, 8:10])[, 2] + 0.1
sc_emiss[4, ] <- seqstatf(biofam_seq[, 11:13])[, 2] + 0.1
sc_emiss[5, ] <- seqstatf(biofam_seq[, 14:16])[, 2] + 0.1
sc_emiss <- sc_emiss / rowSums(sc_emiss)

rownames(sc_trans) <- colnames(sc_trans) <- rownames(sc_emiss) <-
  paste("State", 1:5)

colnames(sc_emiss) <- attr(biofam_seq, "labels")

sc_trans
round(sc_emiss, 3)

sc_initmod <- build_hmm(observations = biofam_seq, initial_probs = sc_init,
  transition_probs = sc_trans, emission_probs = sc_emiss)

sc_fit <- fit_model(sc_initmod)

sc_fit$logLik

sc_fit$model

## Multi-channel model
mc_init <- c(0.9, 0.05, 0.02, 0.02, 0.01)

mc_trans <- matrix(c(0.80, 0.10, 0.05, 0.03, 0.02, 0, 0.90, 0.05, 0.03,
  0.02, 0, 0, 0.90, 0.07, 0.03, 0, 0, 0, 0.90, 0.10, 0, 0, 0, 0, 1),
  nrow = 5, ncol = 5, byrow = TRUE)

mc_emiss_marr <- matrix(c(0.90, 0.05, 0.05, 0.90, 0.05, 0.05, 0.05, 0.90,
  0.05, 0.05, 0.90, 0.05, 0.30, 0.30, 0.40), nrow = 5, ncol = 3,
  byrow = TRUE)

mc_emiss_child <- matrix(c(0.9, 0.1, 0.9, 0.1, 0.1, 0.9, 0.1, 0.9, 0.5,
  0.5), nrow = 5, ncol = 2, byrow = TRUE)

mc_emiss_left <- matrix(c(0.9, 0.1, 0.1, 0.9, 0.1, 0.9, 0.1, 0.9, 0.5,
  0.5), nrow = 5, ncol = 2, byrow = TRUE)

mc_obs <- list(marr_seq, child_seq, left_seq)

mc_emiss <- list(mc_emiss_marr, mc_emiss_child, mc_emiss_left)

mc_initmod <- build_hmm(observations = mc_obs, initial_probs = mc_init,
  transition_probs = mc_trans, emission_probs = mc_emiss,
  channel_names = c("Marriage", "Parenthood", "Residence"))

mc_initmod

mc_fit <- fit_model(mc_initmod, em_step = FALSE, local_step = TRUE,
  threads = 4)

hmm_biofam <- mc_fit$model
BIC(hmm_biofam)

## 4.3. Clustering and mixture hidden Markov models
mc_init2 <- c(0.9, 0.05, 0.03, 0.02)

mc_trans2 <- matrix(c(0.85, 0.05, 0.05, 0.05, 0, 0.90, 0.05, 0.05, 0, 0,
  0.95, 0.05, 0, 0, 0, 1), nrow = 4, ncol = 4, byrow = TRUE)

mc_emiss_marr2 <- matrix(c(0.90, 0.05, 0.05, 0.90, 0.05, 0.05, 0.05,
  0.85, 0.10, 0.05, 0.80, 0.15), nrow = 4, ncol = 3, byrow = TRUE)

mc_emiss_child2 <- matrix(c(0.9, 0.1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
  nrow = 4, ncol = 2, byrow = TRUE)

mc_emiss_left2 <- matrix(c(0.9, 0.1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
  nrow = 4, ncol = 2, byrow = TRUE)

mhmm_init <- list(mc_init, mc_init2)

mhmm_trans <- list(mc_trans, mc_trans2)

mhmm_emiss <- list(list(mc_emiss_marr, mc_emiss_child, mc_emiss_left),
  list(mc_emiss_marr2, mc_emiss_child2, mc_emiss_left2))

biofam3c$covariates$cohort <- cut(biofam3c$covariates$birthyr,
  c(1908, 1935, 1945, 1957))
biofam3c$covariates$cohort <- factor(biofam3c$covariates$cohort,
  labels=c("1909-1935", "1936-1945", "1946-1957"))

init_mhmm <- build_mhmm(observations = mc_obs, initial_probs = mhmm_init,
  transition_probs = mhmm_trans, emission_probs = mhmm_emiss,
  formula = ~sex + cohort, data = biofam3c$covariates,
  channel_names = c("Marriage", "Parenthood", "Residence"),
  cluster_names = c("Cluster 1", "Cluster 2"))

set.seed(1011)
mhmm_fit <- fit_model(init_mhmm, local_step = TRUE, threads = 4,
  control_em = list(restart = list(times = 100)))
mhmm <- mhmm_fit$model

summary(mhmm, conditional_se = FALSE)

## 4.4. Visualizing hidden Markov models
plot(hmm_biofam)

## Figure 4
plot(hmm_biofam, vertex.size = 50, vertex.label.dist = 4.5,
     edge.curved = c(0, 0.6, -0.8, 0.6, 0, 0.6, 0), legend.prop = 0.3,
     combined.slice.label = "States with prob. < 0.05")

## Figure 5
vertex_layout <- matrix(c(1, 2, 2, 3, 1, 0, 0.5, -0.5, 0, -1),
                        ncol = 2)
plot(hmm_biofam, layout = vertex_layout, xlim = c(0.5, 3.5),
     ylim = c(-1.5, 1), rescale = FALSE, vertex.size = 50,
     vertex.label.pos = c("left", "top", "bottom", "right", "left"),
     edge.curved = FALSE, edge.width = 1, edge.arrow.size = 1,
     with.legend = "left", legend.prop = 0.4, label.signif = 1,
     combine.slices = 0, cpal = colorpalette[[30]][14:5])

## Figure 6
ssplot(hmm_biofam, plots = "both", type = "I", sortv = "mds.hidden",
       title = "Observed and hidden state sequences", xtlab = 15:30,
       xlab = "Age")

## 4.5. Visualizing mixture hidden Markov models

## MHMM plot in a grid
plot(mhmm, interactive = FALSE, nrow = 2, legend.prop = 0.45,
  vertex.size = 50, vertex.label.cex = 1.3, cex.legend = 1.3,
  edge.curved = 0.65, edge.label.cex = 1.3, cex.edge.width = 0.8,
  edge.arrow.size = 0.8)

## Interactive MHMM plot
if (interactive()) {
    mssplot(mhmm, ask = TRUE)
}

