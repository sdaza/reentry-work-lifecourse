#################################################
# regression models to predict cluster membership
# reentry work paper
# author: sebastian daza
#################################################


# libraries
library(texreg)
library(mfx)
library(TraMineR)

# relative directory of the paper
path_paper = "reports/paper-work-lifecourse/"
source(paste0(path_paper, "src/utils.R"))

# load data
covs = readRDS(paste0(path_paper, "output/baseline_covariates.rd"))
clusters = fread(paste0(path_paper, "output/cluster_membership.csv"))
dat = merge(covs, clusters, by = "reg_folio")
dim(dat)

# model cluster job ind 4
table(dat$cluster_job_ind_4)

models_job_ind_4 = list()
for (i in unique(dat$cluster_job_ind_4)) {

    models_job_ind_4[[i]] = logitmfx(
       (cluster_job_ind_4 == i) ~ c_age + h_school +
                                  any_previous_work + c_nchildren +
                                  c_previous_sentences +
                                  drug_depabuse + mental_health,
       data = dat,
       atmean = FALSE,
       robust = TRUE)
}


names.map = list(c_age = "Age",
                 h_school = "High school",
                 any_previous_work = "Worked before prison",
                 c_nchildren = "Number of children",
                 c_previous_sentences = "Number of previous sentences",
                 drug_depabuse = "Dependence / abuse of drugs",
                 mental_health = "Mental health problems")

texreg(models_job_ind_4,
       # stars = 0,
       # custom.model.names = c("M1", "M1 MSM", "M2", "M2 MSM"),
       # groups = list("Race (ref. White)" = 4:5, "Education (ref. $<$ HS)" = 7:9),
       custom.coef.map = names.map,
       custom.note = "%stars. Robust standard errors in parenthesis.",
       booktabs = TRUE,
       dcolumn = TRUE,
       use.packages = FALSE,
       label = "tab:models_job_ind_4",
       caption = paste0("Marginal effects of logistics models of employment cluster membership \\newline based on solution in Figure \\ref{fig:sequences_job_clusters_4}"),
       caption.above = TRUE,
       fontsize = "footnotesize",
       float.pos = "htp",
       file = paste0(path_paper, "output/models_job_ind_4.tex")
)

# model cluster job crime v2
table(dat$cluster_jobv2_4)

models_job_crime_v2_4 = list()
for (i in unique(dat$cluster_jobv2_4)) {

    models_job_crime_v2_4[[i]] = logitmfx(
       (cluster_jobv2_4 == i) ~ c_age + h_school +
                                  any_previous_work + c_nchildren +
                                  c_previous_sentences +
                                  drug_depabuse + mental_health,
       data = dat,
       atmean = FALSE,
       robust = TRUE)
}


names.map = list(c_age = "Age",
                 h_school = "High school",
                 any_previous_work = "Worked before prison",
                 c_nchildren = "Number of children",
                 c_previous_sentences = "Number of previous sentences",
                 drug_depabuse = "Dependence / abuse of drugs",
                 mental_health = "Mental health problems")

texreg(models_job_crime_v2_4,
       # stars = 0,
       # custom.model.names = c("M1", "M1 MSM", "M2", "M2 MSM"),
       # groups = list("Race (ref. White)" = 4:5, "Education (ref. $<$ HS)" = 7:9),
       custom.coef.map = names.map,
       custom.note = "%stars. Robust standard errors in parenthesis.",
       booktabs = TRUE,
       dcolumn = TRUE,
       use.packages = FALSE,
       label = "tab:models_job_crime_4",
       caption = paste0("Marginal effects of logistics models for employment-crime cluster membership \\newline based on solution in Figure \\ref{fig:sequences_job_crime_clusters_4_v2}"),
       caption.above = TRUE,
       fontsize = "footnotesize",
       float.pos = "htp",
       file = paste0(path_paper, "output/models_job_crime_v2_4.tex")
)

# descriptive table

# jobs

vars = c("age", "h_school", "nchildren", "any_previous_work",
        "previous_sentences", "drug_depabuse", "mental_health")

tab = dat[, lapply(.SD, mean, na.rm = TRUE), cluster_job_ind_4,
          .SDcols = vars]

tab = tab[, data.table(t(.SD), keep.rownames=TRUE), .SDcols=-"cluster_job_ind_4"]
setnames(tab, names(tab), c("Variable", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"))
tab$Variable = c("Age*", "High school", "Number of children*", "Worked before prison",
                 "Number of previous sentences*", "Dependence / abuse of drugs",
                 "Mental health problems*")

n = nrow(dat)

caption = paste0("Descriptive statistics by employment clusters \\newline based on solution in Figure \\ref{fig:sequences_job_clusters_4} (N = ",
                 n, ")")
label = "tab:descriptive_job_clusters_4"
ptab = print(xtable(tab, caption = caption, label = label, align = "llcccc"),
             include.rownames=FALSE,
             caption.placement = "top",
             table.placement = "htp")

comment = "All the values are proportions except for * that are averages."
ptab = gsub("begin\\{table\\}\\[htp\\]\\n",
            "begin\\{table\\}\\[htp\\]\\\n\\\\footnotesize\\\n\\\\setlength\\{\\\\tabcolsep\\}\\{10pt\\}\\\n\\\\renewcommand\\{\\\\arraystretch\\}\\{1.3\\}\\\n\\\\begin\\{threeparttable\\}\\\n",
            ptab)
ptab = gsub("end\\{tabular\\}\\n",
            paste0("end\\{tabular\\}\\\n\\\\begin{tablenotes}\\\n\\\\scriptsize\\\n\\\\item ",
                   comment,
                   "\\\n\\\\end{tablenotes}\\\n\\\\end{threeparttable}\\\n"),
            ptab)
cat(ptab, file = paste0(path_paper, "output/descriptives_job_clusters_4.tex"))
rm(ptab, tab)

# jobs and crime

tab = dat[, lapply(.SD, mean, na.rm = TRUE), cluster_jobv2_4,
          .SDcols = vars]

tab = tab[, data.table(t(.SD), keep.rownames=TRUE), .SDcols=-"cluster_jobv2_4"]
setnames(tab, names(tab), c("Variable", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"))
tab$Variable = c("Age*", "High school", "Number of children*", "Worked before prison",
                 "Number of previous sentences*", "Dependence / abuse of drugs",
                 "Mental health problems*")

n = nrow(dat)

caption = paste0("Descriptive statistics by employment-crime clusters \\newline based on solution in Figure \\ref{fig:sequences_job_crime_clusters_4_v2} (N = ",
                 n, ")")
label = "tab:descriptive_job_crime_clusters_4"
ptab = print(xtable(tab, caption = caption, label = label, align = "llcccc"),
             include.rownames=FALSE,
             caption.placement = "top",
             table.placement = "htp")

comment = "All the values are proportions except for * that are averages."
ptab = gsub("begin\\{table\\}\\[htp\\]\\n",
            "begin\\{table\\}\\[htp\\]\\\n\\\\footnotesize\\\n\\\\setlength\\{\\\\tabcolsep\\}\\{10pt\\}\\\n\\\\renewcommand\\{\\\\arraystretch\\}\\{1.3\\}\\\n\\\\begin\\{threeparttable\\}\\\n",
            ptab)
ptab = gsub("end\\{tabular\\}\\n",
            paste0("end\\{tabular\\}\\\n\\\\begin{tablenotes}\\\n\\\\scriptsize\\\n\\\\item ",
                   comment,
                   "\\\n\\\\end{tablenotes}\\\n\\\\end{threeparttable}\\\n"),
            ptab)
cat(ptab, file = paste0(path_paper, "output/descriptives_job_crime_clusters_4.tex"))
rm(ptab, tab)

# discrepancy analysis

seq_data_jobs_ind = readRDS(paste0(path_paper, "output/seq_data_job.rd"))
seq_data_jobs_ind_distance  = readRDS(paste0(path_paper, "output/seq_data_job_distance.rd"))
seq_data_job_crime_v2 = readRDS(paste0(path_paper, "output/seq_data_job_crime_v2.rd"))
seq_data_job_crime_v2_distance = readRDS(paste0(path_paper, "output/seq_data_job_crime_v2_distance.rd"))

st = seqtree(seq_data_jobs_ind ~ age + only_primary + h_school +
                                  any_previous_work + nchildren +
                                  previous_sentences + early_crime +
                                  drug_depabuse + mental_health + sentence_length +
                                  total_months_in_prison + family_conflict +
                                  self_efficacy + desire_change ,
                                  data = dat, R = 10000, diss = seq_data_jobs_ind_distance,
             weight.permutation = "diss")

seqtreedisplay(st, type = "d",
               border = NA,
               filename = paste0(path_paper, "output/reg_tree_job.png"))

st = seqtree(seq_data_job_crime_v2 ~ age + only_primary + h_school +
                                  any_previous_work + nchildren +
                                  previous_sentences + early_crime +
                                  drug_depabuse + mental_health + sentence_length +
                                  total_months_in_prison + family_conflict +
                                  self_efficacy + desire_change ,
                                  data = dat, R = 10000, diss = seq_data_job_crime_v2_distance,
             weight.permutation = "diss")

seqtreedisplay(st, type = "d",
               border = NA,
               filename = paste0(path_paper, "output/reg_tree_job_crime.png"))