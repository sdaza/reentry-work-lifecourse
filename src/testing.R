
# testing computation of transition rates

path_paper = "reports/paper-work-lifecourse/"
seqdata = readRDS(paste0(path_paper, "output/seq_data_job.rd"))

sel.states = attr(seqdata,"alphabet")
sel.states

weights = rep(1, nrow(seqdata))
nbetat = length(sel.states)
sdur = ncol(seqdata)
lag = 1

alltransition = 1:(sdur-lag)
numtransition = length(alltransition)

tmat = matrix(0, nrow=nbetat, ncol=nbetat)
row.names(tmat) = paste("[",sel.states," ->]",sep="")
colnames(tmat) = paste("[-> ",sel.states,"]",sep="")

x = 1
y = 1

colxcond <- seqdata[, alltransition]== sel.states[x]
PA <- sum(weights * rowSums(colxcond))
PAB <- sum(weights * (colxcond & seqdata[,alltransition+lag]==sel.states[y]))

PAB/PA