#######################################################################################################################
##  Draws the README's figures from the data in ./data, in a light and a dark version, with base R and no packages.
##  Run from the repository root:  Rscript data_visualization.R
##
##  Every energy is compared with the exact ground-state energy of the open chain. The chain maps to free fermions, so
##  that energy is minus the sum of the singular values of the n x n bidiagonal matrix with B on the diagonal and J
##  on the superdiagonal.
#######################################################################################################################

exact_energy <- function(n, J, B) {                                        ## Exact ground-state energy of the open chain
  M <- diag(B, n)
  M[cbind(1:(n-1), 2:n)] <- J
  -sum(svd(M, nu=0, nv=0)$d)
}

exact_correlations <- function(n, J, B, ref) {          ## Exact <sz_ref sz_j> of the open chain for every site j
  ## With Majorana operators (a_1, b_1, ..., a_n, b_n), H = (i/4) g^T A g and the ground state has
  ## <g_p g_q> = delta_pq + sign(iA)_pq. The correlation <sz_i sz_k> (i < k) is the determinant of <-i b_p a_(q+1)>
  ## for p, q = i, ..., k-1. This agrees with exact diagonalization of 8 spins to 1e-15.
  ai <- 2*(1:n) - 1; bi <- 2*(1:n)
  A <- matrix(0, 2*n, 2*n)
  A[cbind(ai, bi)] <- 2*B; A[cbind(bi, ai)] <- -2*B
  A[cbind(bi[-n], ai[-1])] <- 2*J; A[cbind(ai[-1], bi[-n])] <- -2*J
  ev <- eigen(1i*A, symmetric=TRUE)
  C <- Re(-1i*(ev$vectors %*% diag(sign(ev$values)) %*% Conj(t(ev$vectors))))
  sapply(1:n, function(j) {
    if (j == ref) return(1)
    p <- min(ref, j):(max(ref, j) - 1)
    det(matrix(C[cbind(rep(bi[p], times=length(p)), rep(ai[p+1], each=length(p)))], length(p)))
  })
}

themes <- list(                                                                       ## Colours of GitHub's two themes
  light = list(bg='#ffffff', fg='#1f2328', muted='#59636e', grid='#e6eaef', axis='#d1d9e0', accent='#0969da',
               grey='#818b98', band='#0969da33'),
  dark  = list(bg='#0d1117', fg='#f0f6fc', muted='#9198a1', grid='#21262d', axis='#3d444d', accent='#4493f8',
               grey='#656c76', band='#4493f833')
)
font <- 'Adwaita Sans'

percent <- function(x) {                                                    ## 1e-4 -> "0.01%", 0.1 -> "10%", and so on
  sapply(x, function(v) paste0(format(100*v, scientific=FALSE, drop0trailing=TRUE, trim=TRUE), '%'))
}

open_figure <- function(name, th, width, height) {
  svg(sprintf('./data/%s_%s.svg', name, names(themes)[sapply(themes, identical, th)]), width=width, height=height,
      family=font, bg=th$bg)
  par(fg=th$fg, col.axis=th$muted, col.lab=th$fg, col.main=th$fg, bg=th$bg, las=1, mgp=c(2.6, 0.6, 0), tcl=-0.25,
      cex.axis=0.85, cex.lab=0.95)
}

log_axis <- function(side, lo, hi, th) {                                       ## Decades, labelled in percent, with grid
  at <- 10^(ceiling(log10(lo)):floor(log10(hi)))
  axis(side, at=at, labels=percent(at), col=NA, col.ticks=th$axis)
  if (side == 2) abline(h=at, col=th$grid, lwd=0.8) else abline(v=at, col=th$grid, lwd=0.8)
}

title_block <- function(title, subtitle, th) {
  mtext(title, side=3, line=2.1, adj=0, cex=1.15, font=2, col=th$fg)
  mtext(subtitle, side=3, line=0.9, adj=0, cex=0.85, col=th$muted)
}

## Training curves of the two long chains ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chains <- list(
  list(file='./data/energies_A.csv', n=1024, J=-0.5, B=0.1, label='Antiferromagnet, 1024 spins'),
  list(file='./data/energies_F.csv', n=1000, J=0.5,  B=0.1, label='Ferromagnet, 1000 spins')
)
for (k in seq_along(chains)) {
  ch <- chains[[k]]
  d <- read.csv(ch$file, header=TRUE)
  ch$e0 <- exact_energy(ch$n, abs(ch$J), abs(ch$B))
  ch$rel <- (d$Energy - ch$e0)/abs(ch$e0)
  ch$err <- d$Error/abs(ch$e0)
  chains[[k]] <- ch
}

running_mean <- function(x, k) {                                  ## Centred mean over k values, fewer at the ends
  h <- k %/% 2
  sapply(seq_along(x), function(i) mean(x[max(1, i-h):min(length(x), i+h)]))
}

spread_labels <- function(y, gap) {                       ## Moves label positions apart until they are gap apart
  o <- order(y); z <- y[o]
  for (i in seq_along(z)[-1]) z[i] <- max(z[i], z[i-1] + gap)
  z <- z - (mean(z) - mean(y[o]))                                          ## Keep the group centred where it was
  y[o] <- z; y
}

for (th in themes) {
  open_figure('training', th, width=8, height=4.6)
  par(mar=c(4, 5.8, 4.4, 10.5))
  lo <- 1e-6; hi <- 1
  xmax <- max(sapply(chains, function(ch) length(ch$rel)))
  plot(NA, xlim=c(1, xmax), ylim=c(lo, hi), log='y', axes=FALSE, xlab='Training epoch', ylab='')
  mtext('Energy above the exact ground state', side=2, line=4.4, cex=0.95, las=0)
  log_axis(2, lo, hi, th)
  axis(1, col=NA, col.ticks=th$axis)
  finals <- sapply(chains, function(ch) mean(tail(ch$rel, 100)))
  at <- 10^spread_labels(log10(pmax(finals, lo)), 0.8)
  for (k in rev(seq_along(chains))) {                                       ## The headline chain is drawn last, on top
    ch <- chains[[k]]
    col <- if (k == 1) th$accent else th$grey
    y <- ifelse(ch$rel > lo, ch$rel, NA)                                     ## Values at or below 0 are off a log axis
    lines(seq_along(y), y, col=adjustcolor(col, alpha.f=0.3), lwd=0.8)                       ## Each epoch, faintly
    m <- running_mean(ch$rel, 21); m[1:30] <- ch$rel[1:30]           ## Keep the sharp early drop unsmoothed
    lines(seq_along(m), ifelse(m > lo, m, NA), col=col, lwd=if (k == 1) 2 else 1.6)    ## 21-epoch running mean
    text(xmax*1.03, at[k], labels=sprintf('%s\n%s above exact', ch$label, percent(signif(finals[k], 2))), adj=0,
         xpd=NA, cex=0.8, col=col, font=if (k == 1) 2 else 1)
  }
  title_block('A neural network learns the ground state of a quantum spin chain',
              'Error of the variational energy against the exact energy at B = 0.1; labels average the last 100 epochs', th)
  dev.off()
}

## Sweep of the transverse field across the critical point ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
n <- 64; J <- 0.5
corr <- read.csv('./data/field_sweep/correlations.csv')
fields <- sort(unique(corr$B))
middle <- n %/% 2 + 1
far <- abs(seq_len(n) - middle) >= n %/% 4                                ## Sites at least a quarter of the chain away
sweep <- data.frame(B=fields)
sweep$order <- sapply(fields, function(B) mean(corr$correlation[corr$B == B][far]))
sweep$e0 <- sapply(fields, function(B) exact_energy(n, J, B))
final <- t(sapply(fields, function(B) tail(read.csv(sprintf('./data/field_sweep/energies_B%.2f.csv', B)), 1)))
sweep$energy <- unlist(final[, 1]); sweep$error <- unlist(final[, 2])
sweep$rel <- (sweep$energy - sweep$e0)/abs(sweep$e0)

grid <- seq(0.01, 0.99, by=0.01)                                         ## Exact long-range order of the same chain
exact_order <- sapply(grid, function(B) mean(exact_correlations(n, J, B, middle)[far]))

for (th in themes) {
  open_figure('field_sweep', th, width=8, height=4.6)
  layout(matrix(1:2, nrow=1))
  par(oma=c(0, 0, 3.4, 0))

  ## Long-range order
  par(mar=c(4, 4.2, 2.2, 1))
  plot(NA, xlim=c(0, 1), ylim=c(0, 1.05), axes=FALSE, xlab='Transverse field B', ylab='Correlation across the chain')
  abline(h=seq(0, 1, 0.25), col=th$grid, lwd=0.8)
  axis(1, col=NA, col.ticks=th$axis); axis(2, at=seq(0, 1, 0.25), col=NA, col.ticks=th$axis)
  abline(v=J, col=th$axis, lty=2)
  text(J, 1.03, 'critical point B = J', pos=4, cex=0.75, col=th$muted)
  lines(grid, exact_order, col=th$grey, lwd=1.4)
  points(sweep$B, sweep$order, pch=19, cex=0.9, col=th$accent)
  legend('bottomleft', legend=c('Network', 'Exact'), col=c(th$accent, th$grey),
         pch=c(19, NA), lty=c(NA, 1), lwd=c(NA, 1.4), bty='n', cex=0.78, text.col=th$fg)
  mtext('Order', side=3, line=0.6, adj=0, font=2, cex=0.9)

  ## Accuracy
  lo <- 1e-6; hi <- 1e-1
  par(mar=c(4, 5.8, 2.2, 1))
  plot(NA, xlim=c(0, 1), ylim=c(lo, hi), log='y', axes=FALSE, xlab='Transverse field B', ylab='')
  mtext('Energy above the exact ground state', side=2, line=4.4, cex=0.95, las=0)
  log_axis(2, lo, hi, th)
  axis(1, col=NA, col.ticks=th$axis)
  abline(v=J, col=th$axis, lty=2)
  ok <- sweep$rel > lo
  segments(sweep$B[ok], pmax(sweep$rel[ok] - sweep$error[ok]/abs(sweep$e0[ok]), lo), sweep$B[ok],
           sweep$rel[ok] + sweep$error[ok]/abs(sweep$e0[ok]), col=th$accent)
  points(sweep$B[ok], sweep$rel[ok], pch=19, cex=0.9, col=th$accent)
  mtext('Accuracy', side=3, line=0.6, adj=0, font=2, cex=0.9)

  mtext('The network learns the ordered phase, but not yet the disordered one', side=3, line=1.9, adj=0.02,
        outer=TRUE, cex=1.15, font=2, col=th$fg)
  mtext('Ferromagnetic chain of 64 spins, J = 0.5, 16 hidden units; energies against the exact energy of each chain', side=3, line=0.7, adj=0.02,
        outer=TRUE, cex=0.85, col=th$muted)
  dev.off()
}
