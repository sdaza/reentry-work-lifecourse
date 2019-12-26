##########################
# utility functions
# author: sebastian daza
##########################

renameColumns = function(dat, hash) {
    oldnames = as.vector(hash::keys(hash))
    newnames = as.vector(hash::values(hash))

    if (length(oldnames) != length(newnames)) {
        stop("Vector of names should have the same length")
    }

    setnames(dat, oldnames, newnames)
}

flag_positive_values = function(x) {
    x = na.omit(x)
    return( ifelse(
                   length(x) > 0,
                   as.numeric(any(x > 0)),
                   NA)
    )
}

lookvar  <- function(dat, varnames) {
    n  <- names(dat)
    nn  <- list()
        for (i in 1:length(varnames)) {
            nn[[i]]  <- grep(varnames[i],n)
        }

    nn  <- unlist(nn)

    if ( length(nn) >0 )
        {
         r  <- n[nn]
         return(r)
        }
    else
    { return("No variables found")}
}

toFirstDayOfWeek = function(date) {
    return(date - (wday(date) - 2))
}

# get first values

getFirstValue = function(x) {
    return(head(na.omit(x), 1))
}

getMax = function(x) {
    x = na.omit(x)
    if (length(x) == 0) {
        return(NA_real_)
    } else {
        return(max(x))
    }
}

getMin = function(x) {
    x = na.omit(x)
    if (length(x) == 0) {
        return(NA_real_)
    } else {
        return(min(x))
    }
}

table = function (...) base::table(..., useNA = 'ifany')

cor = function (...) stats::cor(..., use = "complete.obs")

savepdf <- function(file, width=16, height=10)
{
  fname <- paste0(file, ".pdf")
  pdf(fname, width=width/2.54, height=height/2.54,
      pointsize=10)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
}

create_sequences = function(data, seq_variable, seq_labels, columns) {
    temp = data.table::copy(data[, c("reg_folio",
                         "month_index", "class", seq_variable), with = FALSE])
    print(names(temp))
    temp = dcast(temp, reg_folio + class ~ month_index,
                 value.var = seq_variable)
    s1 = seqdef(temp, columns, right = NA,
                labels = seq_labels)
    m1 = pstree(s1, L = 10)
    s1i = impute(m1, s1, method = "prob")
    return(s1i)
}

create_clusters = function(seq_data, method = "HAM", nclusters = 2:5) {

    list_clusters = list()
    distance = seqdist(seq_data, method = method)

    for (i in nclusters) {
        assign(paste0("c", i), wcKMedoids(distance,
                                          k = i,
                                          cluster.only = TRUE))
        assign(paste0("sil", i), wcSilhouetteObs(distance,
                                                 get(paste0("c", i)),
                                                 measure="ASWw"))

        list_clusters[[paste0("c", i)]] = list(data.table(c = get(paste0("c", i)))[
                                                         , cc := .GRP, c][
                                                         , cc],
                                               get(paste0("sil", i))
                                               )
    }
    return(list_clusters)
}