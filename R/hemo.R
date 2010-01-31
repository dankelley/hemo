pairs.plot <- function(x, which=c(2,3,6), cex=par("cex"))
{
    plot(x$bp[which])
}

ts.plot <- function(t, x, ylab="", type='l', show.stats=TRUE, cex=par("cex"))
{
    plot(t, x, type=type, xlab="", ylab=ylab)
    if (show.stats) {
        m <- sprintf("%.0f", mean(x))
        sd <- sprintf("%.0f", sd(x))
        mtext(substitute(m %+-% sd, list(m=m, sd=sd)), side=4, cex=cex)
    }
}

clock.plot <- function(t, x, label, R, R.col="gray", R.lty="solid", R.lwd=0.5*par("lwd"), col.axis="darkgray", cex=par("cex"))
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
    box()
    at <- pretty(c(0,x), n=10)
    axis(side=1, at=-at, labels=at, pos=0, cex.axis=3/4*cex, col=col.axis, col.axis=col.axis, mgp=c(1,1/2,1/4))
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
    ## axes
    abline(h=0, lty="dotted", col=col.axis)
    abline(v=0, lty="dotted", col=col.axis)
    mtext(side=1, "Midnight", cex=3/4*cex, col=col.axis)
    mtext(side=2, "6 AM", cex=3/4*cex, col=col.axis)
    mtext(side=3, "Noon", cex=3/4*cex, col=col.axis)
    mtext(side=4, "6 PM", cex=3/4*cex, col=col.axis)
    if (!missing(label))
        mtext(side=1, label, line=-1, cex=2/3*cex, adj=0)
    points(x * s, x * c, col=rgb(0, 0, 1, alpha=0.3), pch=20, cex=cex)
    ## histogram
    h <- hist(x, plot=FALSE)
    hx <- -h$mids
    hx <- c(hx[1], hx, hx[length(hx)])
    hy <- h$density * max.x * 2
    hy <- c(0, hy, 0)
    polygon(hx, hy, col=rgb(1,0,0,alpha=0.5))
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
    is.bp <- grep("^BP", lines)
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
        bp <- data.frame(t, systolic, diastolic,
                         map=(2*diastolic + systolic)/3,
                         pp=systolic-diastolic,
                         pulse.rate=pulse)
    }
    rval <- list(filename=filename, bp=bp)
    class(rval) <- "hemo"
    rval
}

plot.hemo <- function(x, item=c("bp"),
                      style=c("ts","clock","pairs"),
                      which,
                      cex=par("cex"),
                      debug=FALSE,
                      ...)
{
    item <- match.arg(item)
    style <- match.arg(style)
    if (item == "bp") {
        if (style == "ts") {
            par(mgp=c(2, 3/4, 0))
            par(mar=c(2.5, 3, 1, 1.5))
            if (missing(which)) which <- c(1,2,3,4,5)
            lw <- length(which)
            par(mfrow=c(lw, 1))
            for (w in which) {
                if (1 == w) ts.plot(x$bp$t, x$bp$systolic,   ylab="Systolic [mm]",  cex=cex)
                if (2 == w) ts.plot(x$bp$t, x$bp$diastolic,  ylab="Diastolic [mm]", cex=cex)
                if (3 == w) ts.plot(x$bp$t, x$bp$map,        ylab="MAP [mm]",       cex=cex)
                if (4 == w) ts.plot(x$bp$t, x$bp$pp,         ylab="PP [mm]",        cex=cex)
                if (5 == w) ts.plot(x$bp$t, x$bp$pulse.rate, ylab="Pulse [1/s]",    cex=cex)
            }
        } else if (style == "clock") {
            if (missing(which)) which <- c(1,2,5,6)
            lw <- length(which)
            if (lw == 2)
                par(mfrow=c(2,2))
            else if (lw > 2)
                par(mfrow=c(2,2))
            par(mgp=c(1.25,1.5/3,0))
            par(mar=c(1.5,1.5,1.5,1.5))
            if (1 %in% which) clock.plot(x$bp$t, x$bp$systolic, " Systolic [mm]", R=c(90, 119), R.col="green", cex=cex)
            if (2 %in% which) clock.plot(x$bp$t, x$bp$diastolic, " Diastolic [mm]", R=c(60,79), R.col="green", cex=cex)
            if (3 %in% which) clock.plot(x$bp$t, x$bp$map, " MAP [mm]", R=2/3*c(90,60) + 1/3*c(119,79), R.col="green", cex=cex)
            if (4 %in% which) clock.plot(x$bp$t, x$bp$pp, " PP [mm]", R=c(119,79) - c(90,60), R.col="green", cex=cex)
            if (5 %in% which) clock.plot(x$bp$t, x$bp$pulse.rate, " Pulse [1/min]", cex=cex)
            ## Stats
            if (6 %in% which) {
                par(mar=c(0,0,0,0))
                plot(0:1, 0:1, axes=FALSE, type='n', xlab="", ylab="")
                xx <- 0
                dy <- 1/7
                yy <- dy
                text(xx, yy, paste("  Pulse:",
                                   paste(format(fivenum(x$bp$pulse.rate), digits=1), collapse=" ")),
                     cex=par("cex"), pos=4)
                yy <- yy + dy
                text(xx, yy, paste("  Pulse pressure:",
                                   paste(format(fivenum(x$bp$pp), digits=1), collapse=" ")),
                     cex=par("cex"), pos=4)
                yy <- yy + dy
                text(xx, yy, paste("  Arterial pressure:",
                                   paste(format(fivenum(x$bp$map), digits=1), collapse=" ")),
                     cex=par("cex"), pos=4)
                yy <- yy + dy
                text(xx, yy, paste("  Diastolic:",
                                   paste(format(fivenum(x$bp$diastolic), digits=1), collapse=" ")),
                     cex=par("cex"), pos=4)
                yy <- yy + dy
                text(xx, yy, paste("  Systolic:",
                                   paste(format(fivenum(x$bp$systolic), digits=1), collapse=" ")),
                     cex=par("cex"), pos=4)
                yy <- yy + dy
                text(xx, yy, "Min, Q1, Median, Q2, Max:", cex=1.2*par("cex"), pos=4)
            }
        } else if (style == "pairs") {
            if (missing(which)) which <- c(2,3,6)
            pairs.plot(x, which=which)
        } else stop("cannot handle plot style ", style)
    } else stop("cannot handle item ", item)
}
#a <- read.hemo("health.dat")
#plot(a, style="clock", which=c(1,2,3,5))
