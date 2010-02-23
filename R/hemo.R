pairs.plot <- function(x, which=2:3, cex=par("cex"))
{
    pairs(x$bp[which])
}

ts.plot <- function(t, x, ylab="", cex=par("cex"), tlim=range(t),
                    show.stats=TRUE, show.lm=TRUE, green, orange, red)
{
    if (!is.null(t) && !is.null(x)) {
        plot(t, x, type='n', xlab="", ylab=ylab, xlim=tlim)
        if (show.lm) {
            t0 <- t - t[1]          # otherwise lm is poor
            m <- lm(x ~ t0)
            tt <- seq(min(t0), max(t0), length.out=100)
            pp.ci <- predict(m, newdata=list(t0=tt), interval="confidence")
            lines(as.POSIXct(tt+t[1]), pp.ci[,1], col='black')
            lines(as.POSIXct(tt+t[1]), pp.ci[,2], col='gray')
            lines(as.POSIXct(tt+t[1]), pp.ci[,3], col='gray')
        }
        if (!missing(red) && !missing(orange) && !missing(green)) {
            col.green <- rgb(0,1,0)
            col.orange <- rgb(1,1,0)
            col.red <- rgb(1,0,0)
            col <- ifelse(x < green[1], "transparent",
                          ifelse(x <= green[2], col.green,
                                 ifelse(x < orange[2], col.orange,
                                        ifelse(x < red[2], col.red,
                                               "black"))))
        } else col <- rep("transparent", length(x))
        points(t, x, bg=col, col="black", pch=21, cex=2.0*cex)
        if (show.stats) {
            m <- sprintf("%.0f", mean(x))
            sd <- sprintf("%.0f", sd(x))
            mtext(substitute(m %+-% sd, list(m=m, sd=sd)), side=4, cex=cex)
        }
    }
}

clock.plot <- function(t, x, label,
                       R, R.col="gray", R.lty="solid", R.lwd=0.5*par("lwd"),
                       lwd.axis=0.5*par("lwd"),
                       col.axis=gray(.2),
                       cex=par("cex"),
                       green, orange, red,
                       show.mean=TRUE)
{
    ring <- function(R=1, col="gray", lty="dotted", lwd=0.5*par("lwd"))
    {
        i <- seq(0, 2*pi, length.out=360)
        for (rval in R) {
            lines(rval * sin(i), rval * cos(i), col=col, lty=lty, lwd=lwd)
        }
    }
    if (length(x) != length(t)) stop("lengths of t and x do not match")
    tl <- as.POSIXlt(t)
    hour <- tl$hour + tl$min / 60
    hour.angle <- pi + hour / 24 * 2 * pi
    s <- sin(hour.angle)
    c <- cos(hour.angle)
    max.x <- max(x)
    lim <- max(x) * c(-1, 1) * 1.04
    plot(x * s, x * c, asp=1, xlab="", ylab="", xlim=lim, ylim=lim, axes=FALSE, type='n')
    w <- -max.x / 25
    if (!missing(green)) {
        polygon(c(-green[1], -green[1], -green[2], -green[2]),
                c(0, w, w, 0), col="limegreen", border="limegreen")
    }
    if (!missing(orange)) {
        polygon(c(-orange[1], -orange[1], -orange[2], -orange[2]),
                c(0, w, w, 0), col="orange", border="orange")
    }
    if (!missing(red)) {
        polygon(c(-red[1], -red[1], -red[2], -red[2]),
                c(0, w, w, 0), col="red", border="red")
    }
    at <- pretty(c(0,x), n=10)
    axis(side=1, at=-at, labels=at, pos=0, cex.axis=0.9*cex, col=col.axis, col.axis=col.axis, mgp=c(1,1/4,0), tcl=-0.3)
    box()
    if (FALSE) {
        for (r in abs(at)) {
            ring(r, col=col.axis, lty="solid", lwd=0.5*par("lwd"))
        }
    }
    if (!missing(R)) {
        lR <- length(R)
        lcol <- length(R.col)
        llty <- length(R.lty)
        llwd <- length(R.lwd)
        if (lcol < lR) R.col <- c(R.col, rep(R.col[1], lR-lcol))
        if (llwd < lR) R.lwd <- c(R.lwd, rep(R.lwd[1], lR-llwd))
        if (llty < lR) R.lty <- c(R.lty, rep(R.lty[1], lR-llty))
        for (i in 1:lR) {
            ring(R[i], col=R.col[i], lwd=R.lwd[i], lty=R.lty[i])
        }
    }
    if (show.mean) {
        xmean <- mean(x)
        ring(xmean, col=col.axis, lty="solid")
    }

    ## axes
    abline(h=0, lty="solid", col=col.axis, lwd=lwd.axis)
    abline(v=0, lty="solid", col=col.axis, lwd=lwd.axis)
    mtext(side=1, "Midnight", cex=4/5*cex, col=col.axis)
    mtext(side=2, "6 AM",     cex=4/5*cex, col=col.axis)
    mtext(side=3, "Noon",     cex=4/5*cex, col=col.axis)
    mtext(side=4, "6 PM",     cex=4/5*cex, col=col.axis)
    if (!missing(label))
        mtext(side=1, label, line=-1, cex=cex, adj=0)
    if (!missing(red) && !missing(orange) && !missing(green)) {
        col.green <- rgb(0,1,0,alpha=0.5)
        col.orange <- rgb(1,1,0,alpha=0.5)
        col.red <- rgb(1,0,0,alpha=0.5)
        col <- ifelse(x < green[1], "transparent",
                      ifelse(x <= green[2], col.green,
                             ifelse(x < orange[2], col.orange,
                                    ifelse(x < red[2], col.red,
                                           "black"))))
    } else col <- rep("transparent", length(x))
    points(x * s, x * c, bg=col, col="black", pch=21, cex=1.5*cex)
    ## histogram
    h <- hist(x, plot=FALSE)
    hx <- -h$mids
    hx <- c(hx[1], hx, hx[length(hx)])
    hy <- h$density * max.x * 2
    hy <- c(0, hy, 0)
    polygon(hx, hy, col=rgb(0,0,1,alpha=0.5))
}

print.summary.hemo <- function(x, digits=max(6, getOption("digits")-1), ...)
{
    cat("Filename: ", x$filename)
    cat("\nBlood pressure:\n")
    print(data.frame(x$bp))
}

summary.hemo <- function(object, ...)
{
    if (!inherits(object, "hemo")) stop("method is only for hemo objects")
    res <- list(filename=object$filename,
                bp=object$bp)
    class(res) <- "summary.hemo"
    res
}

read.hemo <- function(file, debug=FALSE)
{
    if (is.character(file)) {
        filename <- file
        file <- file(file, "rb")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")

    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "rb")
        on.exit(close(file))
    }
    lines <- readLines(file)
    if (debug) {
        cat("The following was read from the file:\n")
        print(lines)
    }
    bp1 <- NULL
    bp2 <- NULL
    pulse <- NULL
    time <- NULL
    is.r <- grep("^R", lines)           # running
    is.c <- grep("^C", lines)           # comment
    is.w <- grep("^W", lines)           # weight
    is.bp <- grep("^BP", lines)         # blood pressure
    if (length(is.r) > 0) {
        run <- Ymd <- HM <- NULL
        for (line in lines[is.c]) {
            d <- strsplit(line, "[ ]+")
            Ymd <- c(Ymd, d[[1]][2])
            HM <- c(HM, d[[1]][3])
            run <- c(run, d[[1]][4])
        }
        t <- strptime(paste(Ymd, HM), format="%Y-%m-%d %H:%M")
        o <- order(t)
        t <- t[o]
        run <- run[o]
        r <- data.frame(t, run)
    } else r <- NULL
    if (length(is.c) > 0) {
        comment <- Ymd <- HM <- NULL
        for (line in lines[is.c]) {
            d <- strsplit(line, "[ ]+")
            Ymd <- c(Ymd, d[[1]][2])
            HM <- c(HM, d[[1]][3])
            comment <- c(comment, d[[1]][4])
        }
        t <- strptime(paste(Ymd, HM), format="%Y-%m-%d %H:%M")
        o <- order(t)
        t <- t[o]
        comment <- comment[o]
        c <- data.frame(t, comment)
    } else c <- NULL
    if (length(is.w) > 0) {
        Ymd <- HM <- weight <- NULL
        for (line in lines[is.w]) {
            d <- strsplit(line, "[ ]+")
            Ymd <- c(Ymd, d[[1]][2])
            HM <- c(HM, d[[1]][3])
            weight <- c(weight, d[[1]][4])
        }
        t <- strptime(paste(Ymd, HM), format="%Y-%m-%d %H:%M")
        weight <- as.numeric(weight)
        o <- order(t)
        t <- t[o]
        weight <- weight[o]
        w <- data.frame(t, weight)
    } else w <- NULL
    if (length(is.bp) > 0) {
        if (debug) {
            cat("The following are to be interpreted as blood-pressure lines:\n")
        }
        Ymd <- HM <- systolic <- diastolic <- pulse <- NULL
        for (line in lines[is.bp]) {
            d <- strsplit(line, "[ ]+")
            Ymd <- c(Ymd, d[[1]][2])
            HM <- c(HM, d[[1]][3])
            systolic <- c(systolic, d[[1]][4])
            diastolic <- c(diastolic, d[[1]][5])
            pulse <- c(pulse, d[[1]][6])
        }
        t <- strptime(paste(Ymd, HM), format="%Y-%m-%d %H:%M")
        systolic <- as.numeric(systolic)
        diastolic <- as.numeric(diastolic)
        pulse <- as.numeric(pulse)
        o <- order(t)
        t <- t[o]
        systolic <- systolic[o]
        diastolic <- diastolic[o]
        bp <- data.frame(t, systolic, diastolic,
                         map=(2*diastolic + systolic)/3,
                         pp=systolic-diastolic,
                         pulse.rate=pulse)
    } else bp <- NULL
    rval <- list(filename=filename, bp=bp, w=w, c=c, r=r)
    class(rval) <- "hemo"
    rval
}

plot.hemo <- function(x, style=c("ts","clock","pairs"), which,
                      cex=par("cex"), debug=FALSE,
                      show.mean=TRUE,
                      show.lm=TRUE, ...)
{
    style <- match.arg(style)
    opar <- par(no.readonly=TRUE)
    on.exit(par(opar))
    if (style == "ts") {
        par(mgp=c(2, 3/4, 0))
        par(mar=c(2.5, 3, 1, 1.5))
        if (missing(which)) which <- c(1,2,6)
        lw <- length(which)
        par(mfrow=c(lw, 1))
        tlim <- range(c(x$bp$t, x$w$t))
        for (w in which) {
            if (1 == w) ts.plot(x$bp$t, x$bp$systolic,   ylab="Systolic [mm Hg]",  cex=cex, tlim=tlim, show.lm=show.lm,
                green=c(90,119), orange=c(120,139.5), red=c(139.5,159))
            if (2 == w) ts.plot(x$bp$t, x$bp$diastolic,  ylab="Diastolic [mm Hg]", cex=cex, tlim=tlim, show.lm=show.lm,
                green=c(60,79), orange=c(80,89.5), red=c(89.5,99))
            if (3 == w) ts.plot(x$bp$t, x$bp$map,        ylab="MAP [mm Hg]",       cex=cex, tlim=tlim, show.lm=show.lm)
            if (4 == w) ts.plot(x$bp$t, x$bp$pp,         ylab="PP [mm Hg]",        cex=cex, tlim=tlim, show.lm=show.lm)
            if (5 == w) ts.plot(x$bp$t, x$bp$pulse.rate, ylab="Pulse [beats/min]", cex=cex, tlim=tlim, show.lm=show.lm)
            if (6 == w) ts.plot(x$w$t,  x$w$weight,      ylab="Weight [lb]",       cex=cex, tlim=tlim, show.lm=show.lm)
            abline(v=x$c$t)
        }
    } else if (style == "clock") {
        if (missing(which)) which <- c(1,2,5,7)
        lw <- length(which)
        if (lw == 2)
            par(mfrow=c(2,2))
        else if (lw > 2)
            par(mfrow=c(2,2))
        par(mgp=c(1.25,1.5/3,0))
        par(mar=c(1.5,1.5,1.5,1.5))
        for (w in which) {
            if (1 == w) clock.plot(x$bp$t, x$bp$systolic,   " Systolic [mm Hg]",
                R=c(90,120), R.col=c("lightgray","lightgray"),
                cex=cex, show.mean=show.mean, green=c(90,119), orange=c(120,139.5), red=c(139.5,159))
            if (2 == w) clock.plot(x$bp$t, x$bp$diastolic,  " Diastolic [mm Hg]",
                R=c(60,80), R.col=c("lightgray","lightgray"),
                cex=cex, show.mean=show.mean, green=c(60,79), orange=c(80,89.5), red=c(89.5,99))
            if (3 == w) clock.plot(x$bp$t, x$bp$map,        " MAP [mm Hg]",
                cex=cex, show.mean=show.mean)
            if (4 == w) clock.plot(x$bp$t, x$bp$pp,         " PP [mm Hg]",
                cex=cex, show.mean=show.mean)
            if (5 == w) clock.plot(x$bp$t, x$bp$pulse.rate, " Pulse [beats/min]",
                cex=cex, show.mean=show.mean)
            if (6 == w) clock.plot(x$w$t,  x$w$weight,      " Weight [lb]",
                cex=cex, show.mean=show.mean)
            ## Stats
            if (7 == w) {
                par(mar=c(0,0,0,0))
                plot(0:1, 0:1, axes=FALSE, type='n', xlab="", ylab="")
                xx <- 0
                dy <- 1/7
                yy <- dy
                text(xx, yy, paste("  Pulse:",
                                   paste(format(fivenum(x$bp$pulse.rate), digits=1), collapse=" ")),
                     cex=cex, pos=4)
                yy <- yy + dy
                text(xx, yy, paste("  Pulse pressure:",
                                   paste(format(fivenum(x$bp$pp), digits=1), collapse=" ")),
                     cex=cex, pos=4)
                yy <- yy + dy
                text(xx, yy, paste("  Arterial pressure:",
                                   paste(format(fivenum(x$bp$map), digits=1), collapse=" ")),
                     cex=cex, pos=4)
                yy <- yy + dy
                text(xx, yy, paste("  Diastolic:",
                                   paste(format(fivenum(x$bp$diastolic), digits=1), collapse=" ")),
                     cex=cex, pos=4)
                yy <- yy + dy
                text(xx, yy, paste("  Systolic:",
                                   paste(format(fivenum(x$bp$systolic), digits=1), collapse=" ")),
                     cex=cex, pos=4)
                yy <- yy + dy
                text(xx, yy, "Min, Q1, Median, Q2, Max:", cex=1.2*cex, pos=4)
            }
        }
    } else if (style == "pairs") {
        if (missing(which)) which <- 2:3
        pairs.plot(x, which=which)
    } else stop("cannot handle plot style ", style)
}
