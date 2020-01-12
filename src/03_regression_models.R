#################################################
# regression models to predict cluster membership
# reentry work paper
# author: sebastian daza
#################################################


# libraries
library(texreg)
library(mfx)
library(TraMineR)
library(PST)
library(xtable)
library(hash)

# relative directory of the paper
path_paper = "reports/paper-work-lifecourse/"
source(paste0(path_paper, "src/utils.R"))

# load data
covs = readRDS(paste0(path_paper, "output/baseline_covariates.rd"))
cal_covs = readRDS(paste0(path_paper, "output/calendar_covs.rd"))

clusters = fread(paste0(path_paper, "output/cluster_membership.csv"))
dat = merge(covs, clusters, by = "reg_folio")
dat = merge(dat, cal_covs, by = "reg_folio")
names(dat)

# clusters labels
cluster_labels = readRDS(paste0(path_paper, "output/cluster_labels.rd"))
cluster_labels_jobs_se = cluster_labels[[1]]
cluster_labels_job_crime = cluster_labels[[2]]

# model cluster job 4
table(dat$cluster_job_se_4)

models_job_se_4 = list()
for (i in unique(cluster_labels_jobs_se)) {

    models_job_se_4[[i]] = logitmfx(
       (cluster_job_se_4 == i) ~ c_age + h_school +
                                  any_previous_work + c_nchildren +
                                  c_previous_sentences +
                                  c_sentence_length +
                                  drug_depabuse + mental_health +
                                  anyprison,
       data = dat,
       atmean = FALSE,
       robust = TRUE)
}

names.map = list(c_age = "Age",
                 h_school = "High school",
                 any_previous_work = "Worked before prison",
                 c_nchildren = "Number of children",
                 c_previous_sentences = "Number of previous sentences",
                 c_sentence_length = "Sentence length in months",
                 drug_depabuse = "Dependence / abuse of drugs",
                 mental_health = "Mental health problems",
                 anyprison = "Prison during follow-up")

texreg(models_job_se_4,
       custom.model.names = cluster_labels_jobs_se,
       custom.coef.map = names.map,
       custom.note = "%stars. Robust standard errors in parenthesis.",
       booktabs = TRUE,
       dcolumn = TRUE,
       use.packages = FALSE,
       label = "tab:models_job_4",
       caption = paste0("Marginal effects of logistics models of employment cluster membership \\newline based on solution in Figure \\ref{fig:sequences_job_4}"),
       caption.above = TRUE,
       fontsize = "footnotesize",
       float.pos = "htp",
       file = paste0(path_paper, "output/models_job_4.tex")
)

# model cluster job crime em se
table(dat$cluster_job_crime_em_se_4)

models_job_crime_em_se_4 = list()
for (i in unique(cluster_labels_job_crime)) {

    models_job_crime_em_se_4[[i]] = logitmfx(
       (cluster_job_crime_em_se_4 == i) ~ c_age + h_school +
                                  any_previous_work + c_nchildren +
                                  c_previous_sentences +
                                  c_sentence_length +
                                  drug_depabuse + mental_health +
                                  anyprison,
       data = dat,
       atmean = FALSE,
       robust = TRUE)
}

names.map = list(c_age = "Age",
                 h_school = "High school",
                 any_previous_work = "Worked before prison",
                 c_nchildren = "Number of children",
                 c_previous_sentences = "Number of previous sentences",
                 c_sentence_length = "Sentence length in months",
                 drug_depabuse = "Dependence / abuse of drugs",
                 mental_health = "Mental health problems",
                 # anyjobsearch = "Searched for jobs follow-up",
                 anyprison = "Prison during follow-up")

texreg(models_job_crime_em_se_4,
       custom.model.names = cluster_labels_job_crime,
       custom.coef.map = names.map,
       custom.note = "%stars. Robust standard errors in parenthesis.",
       booktabs = TRUE,
       dcolumn = TRUE,
       use.packages = FALSE,
       label = "tab:models_job_crime_4",
       caption = paste0("Marginal effects of logistics models for employment-crime cluster membership \\newline based on solution in Figure \\ref{fig:sequences_job_crime_4}"),
       caption.above = TRUE,
       fontsize = "footnotesize",
       float.pos = "htp",
       file = paste0(path_paper, "output/models_job_crime_4.tex")
)

# descriptive table

# jobs

name_vars = c("age", "h_school", "nchildren", "any_previous_work",
               "previous_sentences", "sentence_length", "drug_depabuse", "mental_health",
               "anyjobsearch", "anyprison")

label_vars = c("Age*", "High school", "Number of children*", "Worked before prison",
                "Number of previous sentences*", "Sentence length in months*", "Dependence / abuse of drugs",
                "Mental health problems*", "Searched for jobs follow-up",
                "Prison during follow-up")

dict_vars = hash(name_vars, label_vars)

tab = dat[, lapply(.SD, mean, na.rm = TRUE), cluster_job_se_4,
          .SDcols = name_vars]
tab = tab[order(match(cluster_job_se_4, cluster_labels_jobs_se))]
tab = tab[, data.table(t(.SD), keep.rownames=TRUE), .SDcols= -"cluster_job_se_4"]
setnames(tab, names(tab), c("Variable", cluster_labels_jobs_se))
tab[, Variable := values(dict_vars, name_vars)]

n = nrow(dat)

tab
caption = paste0("Descriptive statistics by employment clusters \\newline based on solution in Figure \\ref{fig:sequences_job_4} (N = ",
                 n, ")")
label = "tab:descriptives_job_4"
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
cat(ptab, file = paste0(path_paper, "output/descriptives_job_4.tex"))
rm(ptab, tab)

# jobs and crime

tab = dat[, lapply(.SD, mean, na.rm = TRUE), cluster_job_crime_em_se_4,
          .SDcols = name_vars]
tab = tab[order(match(cluster_job_crime_em_se_4, cluster_labels_job_crime))]
tab = tab[, data.table(t(.SD), keep.rownames=TRUE), .SDcols=-"cluster_job_crime_em_se_4"]
setnames(tab, names(tab), c("Variable", cluster_labels_job_crime))
tab[, Variable := values(dict_vars, name_vars)]

n = nrow(dat)

caption = paste0("Descriptive statistics by employment-crime clusters \\newline based on solution in Figure \\ref{fig:sequences_job_crime_4} (N = ",
                 n, ")")
label = "tab:descriptives_job_crime_4"
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
cat(ptab, file = paste0(path_paper, "output/descriptives_job_crime_4.tex"))
rm(ptab, tab)

# discrepancy analysis

seq_data_jobs_se = readRDS(paste0(path_paper, "output/seq_data_job.rd"))
seq_data_jobs_se_distance  = readRDS(paste0(path_paper, "output/seq_data_job_distance.rd"))
seq_data_job_crime = readRDS(paste0(path_paper, "output/seq_data_job_crime_em_se.rd"))
seq_data_job_crime_distance = readRDS(paste0(path_paper, "output/seq_data_job_crime_em_se_distance.rd"))

# jobs

st = seqtree(seq_data_jobs_se ~ age + h_school +
                                  any_previous_work + nchildren +
                                  previous_sentences +
                                  drug_depabuse + mental_health + sentence_length + anyprison,
                                  data = dat, R = 10000, diss = seq_data_jobs_se_distance,
             weight.permutation = "diss",
             min.size = 0.05,
             max.depth = 5)

seqtreedisplay(st, type = "d",
               border = NA,
               filename = paste0(path_paper, "output/reg_tree_job.png"))

# multifactor table
job.mfac = dissmfacw(seq_data_jobs_se_distance ~ age + h_school +
                     any_previous_work + nchildren +
                     previous_sentences +
                     drug_depabuse + mental_health + sentence_length + anyprison,
                     data = dat, R = 1000)

job.mfac$mfac[order(-job.mfac$mfac$PseudoR2), ]

# job and crime

st = seqtree(seq_data_job_crime_em_se ~ age + h_school +
                                  any_previous_work + nchildren +
                                  previous_sentences +
                                  drug_depabuse + mental_health +
                                  sentence_length + anyprison,
                                  data = dat, R = 10000, diss = seq_data_job_crime_em_se_distance,
             weight.permutation = "diss",
             min.size = 0.05,
             max.depth = 5)

seqtreedisplay(st, type = "d",
               border = NA,
               sortv = "from.start",
               filename = paste0(path_paper, "output/reg_tree_job_crime.png"))


# multifactor table
jobcrime.mfac = dissmfacw(seq_data_job_crime_em_se_distance ~ age + h_school +
                          any_previous_work + nchildren +
                          previous_sentences +
                          drug_depabuse + mental_health + sentence_length +
                          anyprison,
                     data = dat, R = 1000)

tt = data.table(jobcrime.mfac$mfac[order(-jobcrime.mfac$mfac$PseudoR2), ])

total = tt[Variable == "Total"]
rest = tt[Variable != "Total"]

name_vars = c("age", "h_school", "nchildren", "any_previous_work",
               "previous_sentences", "sentence_length", "drug_depabuse",
               "mental_health", "anyprison")

label_vars = c("Age", "High school", "Number of children", "Worked before prison",
                "Number of previous sentences", "Sentence length in months",
                "Dependence / abuse of drugs",
                "Mental health problems", "Prison during follow-up")

dict_vars = hash(name_vars, label_vars)

rest[, Variable := values(dict_vars, rest[["Variable"]])]

tt = rbind(rest, total)

setnames(tt,
         names(tt),
         c("Covariate", "Pseudo F", "Pseudo $R^2$", "p-value")
         )

caption = paste0("Multi-factor discrepancy analysis for job and crime sequences (N = ",
                 n-3, ")")
label = "tab:discrepancy_crime_job"
ptab = print(xtable(tt, caption = caption, label = label, align = "llccc"),
             include.rownames = FALSE,
             caption.placement = "top",
             table.placement = "htp",
             sanitize.text.function=function(x) {x})

comment = "Estimation based on Hamming distances of sequences."
ptab = gsub("begin\\{table\\}\\[htp\\]\\n",
            "begin\\{table\\}\\[htp\\]\\\n\\\\footnotesize\\\n\\\\setlength\\{\\\\tabcolsep\\}\\{10pt\\}\\\n\\\\renewcommand\\{\\\\arraystretch\\}\\{1.3\\}\\\n\\\\begin\\{threeparttable\\}\\\n",
            ptab)
ptab = gsub("end\\{tabular\\}\\n",
            paste0("end\\{tabular\\}\\\n\\\\begin{tablenotes}\\\n\\\\scriptsize\\\n\\\\item ",
                   comment,
                   "\\\n\\\\end{tablenotes}\\\n\\\\end{threeparttable}\\\n"),
            ptab)
cat(ptab, file = paste0(path_paper, "output/discrepancy_job_crime.tex"))
