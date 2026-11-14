#######################################################################################################################
##  Draws the paper's figures and prints the numbers quoted in its text and tables, from the committed data in
##  ../data, with base R and no packages. Run from the repository root:
##
##      Rscript paper/figures/make_figures.R
##
##  The exact energies and correlations come from the functions in data_visualization.R.
#######################################################################################################################

src <- readLines('data_visualization.R')
eval(parse(text=src[grep('^exact_energy', src):(grep('^themes', src) - 1)]))       ## exact_energy, exact_correlations

out <- function(name) file.path('paper', 'figures', name)
last <- 128                                                       ## Epochs averaged for the final estimates
ink <- '#1f2328'; muted <- '#59636e'; grid <- '#e6eaef'; blue <- '#0969da'; orange <- '#bc4c00'; grey <- '#818b98'

blocking <- function(x) {                                 ## Flyvbjerg-Petersen standard error at each blocking level
  se <- c()
  while (length(x) >= 4) {
    se <- c(se, sd(x)/sqrt(length(x)))
    k <- 2*(length(x) %/% 2)
    x <- (x[seq(1, k, 2)] + x[seq(2, k, 2)])/2
  }
  se
}

open_pdf <- function(name, width, height) {
  pdf(out(name), width=width, height=height, family='Helvetica', pointsize=9, useDingbats=FALSE)
  par(fg=ink, col.axis=ink, col.lab=ink, las=1, mgp=c(2.2, 0.5, 0), tcl=-0.25, cex.axis=0.9)
}

log_axis <- function(side, lo, hi) {
  at <- 10^(ceiling(log10(lo)):floor(log10(hi)))
  axis(side, at=at, labels=parse(text=sprintf('10^%d', log10(at))), col=NA, col.ticks=ink)
  abline(h=at, col=grid, lwd=0.6)
}

## Long chains in the ordered phase ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chains <- list(
  A=list(energies='data/energies_A.csv', correlations='data/correlations_A.csv.gz', n=1024, J=-0.5, B=0.1,
         label='Antiferromagnet, n = 1024', col=blue),
  F=list(energies='data/energies_F.csv', correlations='data/correlations_F.csv.gz', n=1000, J=0.5, B=0.1,
         label='Ferromagnet, n = 1000', col=orange)
)
cat('Long chains: m = 64, 4 images x 256 samples, dtau = 0.01, final estimates over the last', last, 'epochs\n')
for (k in names(chains)) {
  ch <- chains[[k]]
  d <- read.csv(ch$energies)
  e0 <- exact_energy(ch$n, abs(ch$J), abs(ch$B))
  tail_e <- tail(d$Energy, last)
  se <- blocking(tail_e)
  nsamp <- 4*256
  var_eloc <- median(tail(d$Error, last))^2*nsamp                         ## Error = sqrt(var(E_loc)/samples per epoch)
  cat(sprintf(paste0('  %s: E0 = %.4f, mean = %.4f, blocked se = %.4f (levels %s), relative error = %.2e,\n',
                     '     scatter of epoch energies = %.4f vs median reported per-epoch error = %.4f (ratio %.2f),\n',
                     '     var(E_loc) = %.4f, var(E_loc)/n = %.2e, var(E_loc)/E0^2 = %.2e, first epoch under 1e-2/1e-3/1e-4: %s\n'),
              ch$label, e0, mean(tail_e), max(se), paste(sprintf('%.4f', se), collapse=' '), (mean(tail_e) - e0)/abs(e0),
              sd(tail_e), median(tail(d$Error, last)), sd(tail_e)/median(tail(d$Error, last)),
              var_eloc, var_eloc/ch$n, var_eloc/e0^2,
              paste(sapply(c(1e-2, 1e-3, 1e-4), function(t) which((d$Energy - e0)/abs(e0) < t)[1]), collapse='/')))
  chains[[k]]$rel <- (d$Energy - e0)/abs(e0)
  m <- as.matrix(read.csv(gzfile(ch$correlations), header=TRUE))
  chains[[k]]$net <- rowMeans(m[, (ncol(m) - last + 1):ncol(m)])
  ref <- ch$n %/% 2 + 1
  chains[[k]]$ref <- ref
  chains[[k]]$exact <- exact_correlations(ch$n, abs(ch$J), abs(ch$B), ref)
  stagger <- if (ch$J < 0) (-1)^abs(seq_len(ch$n) - ref) else rep(1, ch$n)
  chains[[k]]$stagger <- stagger
  far <- abs(seq_len(ch$n) - ref) >= ch$n %/% 4
  cat(sprintf('     correlations: mean |network - exact| = %.1e, max = %.1e; far-field mean network %.5f, exact %.5f\n',
              mean(abs(stagger*chains[[k]]$net - chains[[k]]$exact)), max(abs(stagger*chains[[k]]$net - chains[[k]]$exact)),
              mean(stagger[far]*chains[[k]]$net[far]), mean(chains[[k]]$exact[far])))
}
cat(sprintf('  Bulk order m^2 = (1 - (B/J)^2)^(1/4) = %.5f\n', (1 - (0.1/0.5)^2)^0.25))

running_mean <- function(x, k) {
  h <- k %/% 2
  sapply(seq_along(x), function(i) mean(x[max(1, i - h):min(length(x), i + h)]))
}

open_pdf('training.pdf', width=3.4, height=2.5)
par(mar=c(3.2, 3.6, 0.6, 0.6))
lo <- 1e-6; hi <- 1
plot(NA, xlim=c(0, 1000), ylim=c(lo, hi), log='y', axes=FALSE, xlab='Epoch', ylab='')
mtext(expression((E - E[0])/abs(E[0])), side=2, line=2.4, las=0)
log_axis(2, lo, hi)
axis(1, col=NA, col.ticks=ink)
box(col=grey, lwd=0.6)
for (k in c('F', 'A')) {
  ch <- chains[[k]]
  y <- ifelse(ch$rel > lo, ch$rel, NA)
  lines(seq_along(y), y, col=adjustcolor(ch$col, alpha.f=0.25), lwd=0.5)
  m <- running_mean(ch$rel, 21); m[1:30] <- ch$rel[1:30]
  lines(seq_along(m), ifelse(m > lo, m, NA), col=ch$col, lwd=1.4)
}
legend('topright', legend=sapply(chains, `[[`, 'label'), col=sapply(chains, `[[`, 'col'), lwd=1.4, bty='n',
       cex=0.85, bg='white')
dev.off()

## Sweep of the transverse field ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
n <- 64; J <- 0.5
corr <- read.csv('data/field_sweep/correlations.csv')                    ## Averaged over the last 100 epochs already
fields <- sort(unique(corr$B))
middle <- n %/% 2 + 1
far <- abs(seq_len(n) - middle) >= n %/% 4
sweep <- data.frame(B=fields)
sweep$e0 <- sapply(fields, function(B) exact_energy(n, J, B))
sweep$mean <- sapply(fields, function(B) mean(tail(read.csv(sprintf('data/field_sweep/energies_B%.2f.csv', B))$Energy, last)))
sweep$se <- sapply(fields, function(B) max(blocking(tail(read.csv(sprintf('data/field_sweep/energies_B%.2f.csv', B))$Energy, last))))
sweep$rel <- (sweep$mean - sweep$e0)/abs(sweep$e0)
sweep$order <- sapply(fields, function(B) mean(corr$correlation[corr$B == B][far]))
sweep$exact_order <- sapply(fields, function(B) mean(exact_correlations(n, J, B, middle)[far]))
cat('\nField sweep: n = 64, m = 16, J = 0.5, 4 images x 256 samples, dtau = 0.01, energies over the last', last, 'epochs\n')
cat('  LaTeX rows: B & E0 & E & relative error & far-field order network & exact\n')
for (i in seq_len(nrow(sweep))) with(sweep[i, ], cat(sprintf('  %.2f & $%.4f$ & $%.4f(%d)$ & %.2g & %.3f & %.3f \\\\\n',
  B, e0, mean, round(se*1e4), 100*rel, order, exact_order)))

grid_B <- seq(0.01, 0.99, by=0.01)
exact_curve <- sapply(grid_B, function(B) mean(exact_correlations(n, J, B, middle)[far]))

open_pdf('field_sweep.pdf', width=3.4, height=3.9)                    ## Two panels stacked, one journal column wide
layout(matrix(1:2, ncol=1))
par(mar=c(1.2, 3.6, 1.4, 0.6))
plot(NA, xlim=c(0, 1), ylim=c(0, 1.02), axes=FALSE, xlab='', ylab='')
mtext(expression('Long-range correlation ' * bar(C)), side=2, line=2.4, las=0)
abline(h=seq(0, 1, 0.25), col=grid, lwd=0.6)
axis(1, labels=FALSE, col=NA, col.ticks=ink); axis(2, at=seq(0, 1, 0.25), col=NA, col.ticks=ink)
box(col=grey, lwd=0.6)
abline(v=J, col=grey, lty=3)
lines(grid_B, exact_curve, col=ink, lwd=1)
points(sweep$B, sweep$order, pch=19, cex=0.6, col=blue)
legend('bottomleft', legend=c('Network', 'Exact'), col=c(blue, ink), pch=c(19, NA), lty=c(NA, 1), pt.cex=0.6,
       bty='n', cex=0.85)
mtext('(a)', side=3, line=0.2, adj=0, font=2)
par(mar=c(3.2, 3.6, 1.4, 0.6))
lo <- 1e-6; hi <- 1e-1
plot(NA, xlim=c(0, 1), ylim=c(lo, hi), log='y', axes=FALSE, xlab=expression('Transverse field ' * B), ylab='')
mtext(expression((E - E[0])/abs(E[0])), side=2, line=2.4, las=0)
log_axis(2, lo, hi)
axis(1, col=NA, col.ticks=ink)
box(col=grey, lwd=0.6)
abline(v=J, col=grey, lty=3)
points(sweep$B, sweep$rel, pch=19, cex=0.6, col=blue)
mtext('(b)', side=3, line=0.2, adj=0, font=2)
dev.off()
cat('\nWrote', out(c('training.pdf', 'field_sweep.pdf')), '\n')
