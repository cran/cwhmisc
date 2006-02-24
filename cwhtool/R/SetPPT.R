setPowerPointStyle <- function() {
    NORMAL <- 1
    BOLD <- 2
    par(font=BOLD);
    par(font.axis=BOLD);
    par(font.lab=BOLD);
    par(font.main=BOLD);
    par(font.sub=NORMAL);
    par(cex=1.2);
    par(cex.axis=1.2);
    par(cex.lab=1.2);
    par(cex.main=1.4);
    par(cex.sub=1.2);
    par(col="black");
    par(col.axis="black");
    par(col.lab="black");
    par(col.main="black");
    par(col.sub="black");
    par(lwd=2);
    par(pch=1);
    par(ps=12);
    par(tmag=1.2);
    par(mar=c(4, 3, 3, 1) + 0.1);
    par(mgp=c(1.5, 0.2, 0));
    par(tcl=0.3);
}

