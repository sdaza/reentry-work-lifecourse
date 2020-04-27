#############################################
# utility functions
# reentry work paper
# author: sebastian daza
#############################################


any_values = function(x, values) as.numeric(any(x %in% values, na.rm = TRUE))


amount_higher_zero = function(x, values) as.numeric(any(x > values))


reverse = function(x) (max(x, na.rm = TRUE) + 1) - x


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


lookvar  = function(dat, varnames) {
    n  = names(dat)
    nn  = list()
        for (i in 1:length(varnames)) {
            nn[[i]]  = grep(varnames[i],n)
        }

    nn  = unlist(nn)

    if ( length(nn) >0 )
        {
         r  = n[nn]
         return(r)
        }
    else
    { return("No variables found")}
}


toFirstDayOfWeek = function(date) {
    return(date - (wday(date) - 2))
}


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


savepdf = function(file, width = 16, height = 10, mgp = c(2.2,0.45,0),
    tcl = -0.4, mar = c(3.3,3.6,1.1,1.1)) {
    fname = paste0(file, ".pdf")
    pdf(fname, width=width/2.54, height=height/2.54,
        pointsize=10)
    par(mgp = mgp, tcl = tcl, mar = mar)
}


countmis  = function(dat, vars = NULL, pct = TRUE, exclude.complete = TRUE) {

    if (is.null(vars)) {
        vars = names(dat)
    }

    mis = sort(sapply(dat[, vars, with = FALSE],
                       function(x) sum(is.na(x))), decreasing = TRUE)

    if (exclude.complete == TRUE) {
        mis = mis[ mis > 0]
    }

    if (pct == FALSE)
      { return(mis) }

    else if ( pct == TRUE ) {
        return( round(mis / nrow(dat), 3))
    }

    return(mis)
}


create_sequences = function(data, seq_variable, seq_labels = NULL, colors = NULL,
    time_index = "month_index",
    time_invariant = c("reg_folio", "month_index", "class")
    ) {

    temp = data.table::copy(data[, c(time_invariant, seq_variable), with = FALSE])
    time = as.character(order(unique(na.omit(temp$month_index))))
    temp = dcast(temp, reg_folio + class ~ month_index,
        value.var = seq_variable)
    print(paste0("Names of columns: ", paste0(names(temp), collapse = ",")))
    columns = which(names(temp) %in% time)
    print(paste0("Columns selected: ", paste0(columns, collapse = ",")))
    s1 = seqdef(temp, columns, right = NA,
        labels = seq_labels, cpal = colors)
    m1 = pstree(s1, L = 10)
    s1i = impute(m1, s1, method = "prob")
    return(s1i)
}


create_clusters = function(seq_data, method = "HAM", norm_distance = "auto",
                           nclusters = 2:5,
                           cluster_labels = NULL,
                           cluster_levels = NULL) {

    list_clusters = list()
    distance = seqdist(seq_data, method = method, norm = norm_distance)

    for (i in nclusters) {
        assign(paste0("c", i), wcKMedoids(distance,
                                          k = i,
                                          cluster.only = TRUE))
        assign(paste0("sil", i), wcSilhouetteObs(distance,
                                                 get(paste0("c", i)),
                                                 measure = "ASWw"))

        if (!is.null(cluster_labels)) {
            assign(paste0("c", i), factor(get(paste0("c", i)), labels = cluster_labels))
        }

        if (!is.null(cluster_levels)) {
            assign(paste0("c", i), factor(get(paste0("c", i)), levels = cluster_levels))
        }

        list_clusters[[paste0("c", i)]] = list(get(paste0("c", i)),
                                               get(paste0("sil", i))
                                               )
    }
    return(list_clusters)
}


create_plots = function(seq_data,
                        cl,
                        filepath,
                        order = "from.start",
                        method_distance = "HAM",
                        norm_distance = "auto") {

    distance = seqdist(seq_data, norm = norm_distance, method = method_distance)

    clusters = cl[[1]]

    savepdf(filepath)

    ifelse(order == "from.start",
        seqIplot(seq_data, group = group.p(clusters), sortv = "from.start"),
        seqIplot(seq_data, group = group.p(clusters), sortv = cl[[2]])
    )

    seqdplot(seq_data, group = group.p(clusters))
    seqmtplot(seq_data, group = group.p(clusters))
    seqrplot(seq_data, group = group.p(clusters),
             dist.matrix = distance, border = NA)
    TraMineRextras::seqplot.tentrop(seq_data, group = group.p(clusters))
    dev.off()
}


add_notes_table = function(tab, caption, label, align,
                           comment = "",
                           floating.environment = "table",
                           fontsize = "footnotesize",
                           arraystretch = 1.3,
                           tabcolsep = 25,
                           filename = "") {

    ptcl = print(xtable(tab, caption = caption, label = label, align = align),
             caption.placement = "top",
             floating.environment = floating.environment,
             table.placement = "htp")
    ptcl = gsub("begin\\{table\\}\\[htp\\]\\n",
                paste0("begin\\{table\\}\\[htp\\]\\\n\\\\", fontsize,
                       "\\\n\\\\setlength\\{\\\\tabcolsep\\}\\{", tabcolsep,
                       "pt\\}\\\n\\\\renewcommand\\{\\\\arraystretch\\}\\{",
                       arraystretch,
                       "\\}\\\n\\\\begin\\{threeparttable\\}\\\n"),
                 ptcl)

    ptcl = gsub("end\\{tabular\\}\\n",
                paste0("end\\{tabular\\}\\\n\\\\begin{tablenotes}\\\n\\\\scriptsize\\\n\\\\item ",
                       comment,
                       "\\\n\\\\end{tablenotes}\\\n\\\\end{threeparttable}\\\n"),
                ptcl)

    cat(ptcl, file = filename)
}

