`.vdense` <-
function (x, filename) 
{
    nobs <- nrow(x)
    ndim <- ncol(x)
    weights <- rep(1, nobs)
    nbins <- round((nobs > 500) * 8 * log(nobs)/ndim)
    if (nbins > 0) {
        binning <- function(x, y, breaks, nbins) {
            binning.3d <- function(x, y, breaks, nbins) {
                f1 <- cut(x[, 1], breaks = breaks[, 1])
                f2 <- cut(x[, 2], breaks = breaks[, 2])
                f3 <- cut(x[, 3], breaks = breaks[, 3])
                freq <- table(f1, f2, f3)
                dimnames(freq) <- NULL
                midpoints <- (breaks[-1, ] + breaks[-(nbins + 
                  1), ])/2
                z1 <- midpoints[, 1]
                z2 <- midpoints[, 2]
                z3 <- midpoints[, 3]
                X <- as.matrix(expand.grid(z1, z2, z3))
                X.f <- as.vector(freq)
                id <- (X.f > 0)
                X <- X[id, ]
                dimnames(X) <- list(NULL, dimnames(x)[[2]])
                X.f <- X.f[id]
                result <- list(x = X, x.freq = X.f, midpoints = midpoints, 
                  breaks = breaks, table.freq = freq)
                if (!all(is.na(y))) {
                  result$means <- as.numeric(tapply(y, list(f1, 
                    f2, f3), mean))[id]
                  result$devs <- as.numeric(tapply(y, list(f1, 
                    f2, f3), function(x) sum((x - mean(x))^2)))[id]
                }
                result
            }
            if (missing(y)) 
                y <- rep(NA, nrow(x))
            if (missing(nbins)) 
                nbins <- round(log(nrow(x))/log(2) + 1)
            if (missing(breaks)) {
                breaks <- cbind(seq(min(x[, 1]), max(x[, 1]), 
                  length = nbins + 1), seq(min(x[, 2]), max(x[, 
                  2]), length = nbins + 1))
                if (ndim == 3) 
                  breaks <- cbind(breaks, seq(min(x[, 3]), max(x[, 
                    3]), length = nbins + 1))
                breaks[1, ] <- breaks[1, ] - rep(10^(-5), ncol(breaks))
            }
            else nbins <- nrow(breaks) - 1
            if (max(abs(breaks)) == Inf | is.na(max(abs(breaks)))) 
                stop("illegal breaks")
            result <- binning.3d(x, y, breaks = breaks, nbins = nbins)
            result
        }
        bins <- binning(x, nbins = nbins)
        x <- bins$x
        weights <- bins$x.freq
        nx <- length(bins$x.freq)
    }
    else nx <- nobs
    h <- .hnorm(x, weights)
    rawdata <- list(nbins = nbins, x = x, nobs = nobs, ndim = ndim)
    est <- .vdensecalc(x, h = h, weights = weights, rawdata = rawdata, 
        filename = filename)
    return(TRUE)
}

`.vdensecalc` <-
function (x, h = .hnorm(x, weights), eval.points = NULL, weights = rep(1, 
    length(x)), rawdata = list(), eval.type = "grid", filename = "test.wrl") 
{
    xlim <- range(x[, 1])
    ylim <- range(x[, 2])
    zlim <- range(x[, 3])
    ngrid <- 20
    if (eval.type != "points") {
        evp <- cbind(seq(xlim[1], xlim[2], length = ngrid), seq(ylim[1], 
            ylim[2], length = ngrid), seq(zlim[1], zlim[2], length = ngrid))
        eval.points <- evp
    }
    h.weights <- rep(1, nrow(x))
    n <- nrow(x)
    nnew <- nrow(eval.points)
    hmult <- 1
    result <- list(eval.points = eval.points, h = h * hmult, 
        h.weights = h.weights, weights = weights)
    Wh <- matrix(rep(h.weights, nnew), ncol = n, byrow = TRUE)
    W1 <- matrix(rep(eval.points[, 1], rep(n, nnew)), ncol = n, 
        byrow = TRUE)
    W1 <- W1 - matrix(rep(x[, 1], nnew), ncol = n, byrow = TRUE)
    W1 <- exp(-0.5 * (W1/(hmult * h[1] * Wh))^2)/Wh
    W2 <- matrix(rep(eval.points[, 2], rep(n, nnew)), ncol = n, 
        byrow = TRUE)
    W2 <- W2 - matrix(rep(x[, 2], nnew), ncol = n, byrow = TRUE)
    W2 <- exp(-0.5 * (W2/(hmult * h[2] * Wh))^2)/Wh
    W3 <- matrix(rep(eval.points[, 3], rep(n, nnew)), ncol = n, 
        byrow = TRUE)
    W3 <- W3 - matrix(rep(x[, 3], nnew), ncol = n, byrow = TRUE)
    W3 <- exp(-0.5 * (W3/(hmult * h[3] * Wh))^2)/Wh
    if (eval.type == "points") {
        est <- as.vector(((W1 * W2 * W3) %*% weights)/(sum(weights) * 
            (2 * pi)^1.5 * h[1] * h[2] * h[3] * hmult^3))
        return(est)
    }
    est <- .vdensecalc(x, h, x, eval.type = "points", weights = weights, 
        filename = filename)
    props <- c(75, 50, 25)
    cols <- topo.colors(length(props))
    alpha <- seq(1, 0.5, length = length(props))
    levels <- quantile(est, props/100)
    est <- apply(W3, 1, function(x) (W1 %*% (weights * x * t(W2)))/(sum(weights) * 
        (2 * pi)^1.5 * h[1] * h[2] * h[3] * hmult^3))
    est <- array(c(est), dim = rep(ngrid, 3))
    struct <- NULL
    if (require(misc3d)) {
        struct <- contour3d(est, levels, eval.points[, 1], eval.points[, 
            2], eval.points[, 3], engine = "none")
    }
    for (i in 1:length(props)) {
        if (length(props) > 1) 
            strct <- struct[[i]]
        else strct <- struct
        trngs.x <- c(t(cbind(strct$v1[, 1], strct$v2[, 1], strct$v3[, 
            1])))
        trngs.y <- c(t(cbind(strct$v1[, 2], strct$v2[, 2], strct$v3[, 
            2])))
        trngs.z <- c(t(cbind(strct$v1[, 3], strct$v2[, 3], strct$v3[, 
            3])))
        a <- list(x = trngs.x, y = trngs.y, z = trngs.z)
        rcol <- (col2rgb(cols[i])/255)[1]
        gcol <- (col2rgb(cols[i])/255)[2]
        bcol <- (col2rgb(cols[i])/255)[3]
        for (j in seq(1, length(trngs.x), 3)) {
            write(paste("\n Shape { \n\tappearance Appearance {\n\t\t material Material {\n\t\tdiffuseColor ", 
                rcol, " ", gcol, " ", bcol, "\n\t\ttransparency ", 
                alpha[i], " \n\t}\n\t}\t\n \n\t geometry IndexedFaceSet {\n\t \t\tsolid TRUE \t \n\t\tcoord Coordinate {\n\t\t\t point [\n\t\t\t ", 
                trngs.x[j], trngs.z[j], trngs.y[j], " \n\t\t\t  ", 
                trngs.x[j + 1], trngs.z[j + 1], trngs.y[j + 1], 
                " \n\t\t\t  ", trngs.x[j + 2], trngs.z[j + 2], 
                trngs.y[j + 2], " \n\t\t ]\n\t }\n\t coordIndex [\t0 1 2\t] \n\t}\t \n }\n ", 
                sep = " "), file = filename, append = TRUE)
        }
    }
    return(est)
}

`.hnorm` <-
function (x, weights = NA) 
{
    if (all(is.na(weights))) 
        weights <- rep(1, nrow(x))
    ndim <- ncol(as.matrix(x))
    if (ndim != 3) 
        stop("only data with 3 dimensions is allowed.")
    n <- sum(weights)
    sd <- sqrt(apply(x, 2, function(x, w) {
        sum(w * (x - sum(x * w)/sum(w))^2)/(sum(w) - 1)
    }, w = weights))
    hh <- sd * (4/(5 * n))^(1/7)
    hh
}

