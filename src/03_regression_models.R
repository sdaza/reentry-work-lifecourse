#################################################
# regression models to predict cluster membership
# reentry work paper
# author: sebastian daza
#################################################


# libraries
library(texreg)
library(mfx)

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
       # custom.note = "Robust standard errors in parenthesis.",
       booktabs = TRUE,
       dcolumn = TRUE,
       use.packages = FALSE,
       label = "models_job_ind_4",
       caption = paste0("Marginal effects of logistics models of cluster membership \\newline Employment Sequences (see Figure \\ref{test})"),
       caption.above = TRUE,
       fontsize = "footnotesize",
       float.pos = "htp",
       file = paste0(path_paper, "output/models_job_ind_4.tex")
)

