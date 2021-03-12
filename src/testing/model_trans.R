library(msm)

jobs_se_msm = tranSequence(seq_data_jobs_se, covs)

q = matrix(rep(0.1, 4*4), 4, 4, byrow = TRUE)
colnames(q) = labs
rownames(q) = labs

m0 = msm(state ~ time, subject = id, data = jobs_se_msm, gen.inits = TRUE, qmatrix = q, 
    method = "BFGS", control = list(fnscale = 4000, maxit = 10000))

hazard.msm(m0)
pmatrix.msm(m0, t = 1, ci = "normal")
pmatrix.msm(m0, t = 50, ci = "normal")
sojourn.msm(m0)
