library(GUniFrac)

##############
metaMDS2 <- function (comm, distance = "bray", tree = NULL, k = 2, try = 20, trymax = 20, 
    engine = c("monoMDS", "isoMDS"), autotransform = TRUE, noshare = (engine == 
        "isoMDS"), wascores = TRUE, expand = TRUE, trace = 1, 
    plot = FALSE, previous.best, ...) 
{
    engine <- match.arg(engine)
    trymax <- max(trymax, try)
    commname <- deparse(substitute(comm), width.cutoff = 500L)
    if (length(commname) > 1L) {
        paste(commname, collapse = "", sep = "")
        commname <- gsub("[ ]{2,}", " ", commname)
    }
    if (any(autotransform, noshare > 0, wascores) && any(comm < 
        0, na.rm = TRUE)) {
        warning("'comm' has negative data: 'autotransform', 'noshare' and 'wascores' set to FALSE")
        wascores <- FALSE
        autotransform <- FALSE
        noshare <- FALSE
    }
    if (inherits(comm, "dist")) {
        dis <- comm
        if (is.null(attr(dis, "method"))) 
            attr(dis, "method") <- "user supplied"
        wascores <- FALSE
    }
    else if ((is.matrix(comm) || is.data.frame(comm)) && isSymmetric(unname(as.matrix(comm)))) {
        dis <- as.dist(comm)
        attr(dis, "method") <- "user supplied"
        wascores <- FALSE
    }
    else {
        if (trace > 2) 
            cat(">>> Calculation of dissimilarities\n")
        dis <- metaMDSdist2(comm, distance = distance, tree = tree, autotransform = autotransform, 
            noshare = noshare, trace = trace, commname = commname, 
            ...)
    }
    if (missing(previous.best)) 
        previous.best <- NULL
    if (trace > 2) 
        cat(">>> NMDS iterations\n")
    out <- metaMDSiter(dis, k = k, try = try, trymax = trymax, 
        trace = trace, plot = plot, previous.best = previous.best, 
        engine = engine, ...)
    if (out$stress < 0.001) {
        warning("Stress is (nearly) zero - you may have insufficient data")
    }
    if (trace > 2) 
        cat(">>> Post-processing NMDS\n")
    points <- postMDS(out$points, dis, plot = max(0, plot - 1), 
        ...)
    if (!is.null(scl <- attr(points, "internalscaling"))) {
        out$dist <- out$dist/scl
        out$dhat <- out$dhat/scl
    }
    if (is.null(rownames(points))) 
        rownames(points) <- rownames(comm)
    wa <- if (wascores) {
        comm <- attr(dis, "comm")
        wascores(points, comm, expand = expand)
    }
    else {
        NA
    }
    out$points <- points
    out$species <- wa
    out$call <- match.call()
    if (is.null(out$data)) 
        out$data <- commname
    class(out) <- c("metaMDS", class(out))
    out
}

##############
metaMDSdist2 <- function(comm, distance = "bray", tree = NULL, autotransform = TRUE, noshare = TRUE, 
    trace = 1, commname, zerodist = "ignore", distfun = vegdist2, 
    ...) 
{
    if (inherits(comm, "dist") || ((is.matrix(comm) || is.data.frame(comm)) && 
        isSymmetric(unname(as.matrix(comm))))) 
        return(comm)
    distname <- deparse(substitute(distfun))
    distfun <- match.fun(distfun)
    zerodist <- match.arg(zerodist, c("fail", "add", "ignore"))
    formals(distfun) <- c(formals(distfun), alist(... = ))
    formals(stepacross) <- c(formals(stepacross), alist(... = ))
    if (missing(commname)) 
        commname <- deparse(substitute(comm))
    xam <- max(comm)
    if (autotransform && xam > 50) {
        comm <- sqrt(comm)
        commname <- paste("sqrt(", commname, ")", sep = "")
        if (trace) 
            cat("Square root transformation\n")
    }
    if (autotransform && xam > 9) {
        comm <- wisconsin(comm)
        commname <- paste("wisconsin(", commname, ")", sep = "")
        if (trace) 
            cat("Wisconsin double standardization\n")
    }
    dis <- distfun(x = comm, tree = tree, method = distance, ...)
    call <- attr(dis, "call")
    call[[1]] <- as.name(distname)
    attr(dis, "call") <- call
    if (zerodist != "ignore" && any(dis <= 0, na.rm = TRUE)) {
        if (zerodist == "fail") 
            stop("Zero dissimilarities are not allowed")
        else if (zerodist == "add") {
            zero <- min(dis[dis > 0], na.rm = TRUE)/2
            dis[dis <= 0] <- zero
            if (trace) 
                cat("Zero dissimilarities changed into ", zero, 
                  "\n")
        }
    }
    if ((isTRUE(noshare) && any(tmp <- no.shared(comm))) || (!is.logical(noshare) && 
        noshare >= 0 && sum(tmp <- no.shared(comm))/length(dis) > 
        noshare)) {
        if (trace) 
            cat("Using step-across dissimilarities:\n")
        rn <- range(dis[tmp], na.rm = TRUE)
        if (rn[2]/rn[1] > 1.01) 
            warning("non-constant distances between points with nothing shared\n", 
                "  stepacross may be meaningless: consider argument 'noshare=0'")
        is.na(dis) <- tmp
        dis <- stepacross(dis, trace = trace, toolong = 0, ...)
        if (length(unique(distconnected(tmp, trace = trace))) > 
            1) 
            warning("Data are disconnected, results may be meaningless")
    }
    attr(dis, "maxdis") <- TRUE
    attr(dis, "commname") <- commname
    attr(dis, "comm") <- comm
    attr(dis, "function") <- distname
    dis
}

##############
vegdist2 <- function (x, method = "bray", tree = NULL, binary = FALSE, diag = FALSE, upper = FALSE, 
    na.rm = FALSE, ...) 
{
    if (method != 17 & method != 18) x1 = x
	ZAP <- 1e-15
    if (!is.na(pmatch(method, "euclidian"))) 
        method <- "euclidean"
    METHODS <- c("manhattan", "euclidean", "canberra", "bray", 
        "kulczynski", "gower", "morisita", "horn", "mountford", 
        "jaccard", "raup", "binomial", "chao", "altGower", "cao", 
        "mahalanobis", "dw", "du")
    method <- pmatch(method, METHODS)
    inm <- METHODS[method]
    if (is.na(method)) 
        stop("invalid distance method")
    if (method == -1) 
        stop("ambiguous distance method")
    if (!method %in% c(1, 2, 6, 16) && any(rowSums(x, na.rm = TRUE) == 
        0)) 
        warning("you have empty rows: their dissimilarities may be meaningless in method ", 
            dQuote(inm))
    if (!method %in% c(1, 2, 3, 6, 16) && any(x < 0, na.rm = TRUE)) 
        warning("results may be meaningless because data have negative entries in method ", 
            dQuote(inm))
    if (method == 11 && any(colSums(x) == 0)) 
        warning("data have empty species which influence the results in method ", 
            dQuote(inm))
    if (method == 6) 
        x <- decostand(x, "range", 2, na.rm = TRUE, ...)
    if (method == 16) 
        x <- veganMahatrans(scale(x, scale = FALSE))
    if (binary) 
        x <- decostand(x, "pa")
    N <- nrow(x <- as.matrix(x))
    if (method %in% c(7, 13, 15) && !identical(all.equal(as.integer(x), 
        as.vector(x)), TRUE)) 
        warning("results may be meaningless with non-integer data in method ", 
            dQuote(inm))
	if (method != 17 & method != 18) {
        d <- .C("veg_distance", x = as.double(x), nr = N, nc = ncol(x), 
            d = double(N * (N - 1)/2), diag = as.integer(FALSE), 
            method = as.integer(method), NAOK = na.rm, PACKAGE = "vegan")$d
        if (method == 10) 
            d <- 2 * d/(1 + d)
        d[d < ZAP] <- 0
        if (any(is.na(d))) 
            warning("missing values in results")
        attr(d, "Size") <- N
        attr(d, "Labels") <- dimnames(x)[[1]]
        attr(d, "Diag") <- diag
        attr(d, "Upper") <- upper
        attr(d, "method") <- paste(if (binary) 
            "binary ", METHODS[method], sep = "")
        attr(d, "call") <- match.call()
        class(d) <- "dist"
	}
    if (method == 17) {
        unifrac <- GUniFrac(x1, tree)
        unifrac <- unifrac$unifracs
        d <- as.dist(unifrac[, , 'd_1'])		# Weighted UniFrac
    }
    if (method == 18) {
        unifrac <- GUniFrac(x1, tree)
        unifrac <- unifrac$unifracs
        d <- as.dist(unifrac[, , 'd_UW'])		# Unweighted UniFrac
    }
    d
}
