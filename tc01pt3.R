require(igraph)
graphics.off()
pdf('ej3.pdf')
g = read_graph('tc01_data/as-22july06.gml',format='gml')
#plot(g)
grados = degree(g)
breaks=0:(max(grados)+1)-0.5
pkhist=hist(grados, breaks=breaks, plot=F)
plot(pkhist$mids, pkhist$density, xlab='grados', ylab='p(k)')
#x11()
plot(pkhist$mids, pkhist$density, xlab='grados', ylab='p(k)', log='xy')

#x11()
breaks_log = 2**(0:(log(max(grados))/log(2) + 1))
log_hist = hist(grados, breaks=breaks_log, plot=F)
plot(log_hist$mids, log_hist$density, xlab='grados', ylab='p(k)')
plot(log_hist$mids, log_hist$density, xlab='grados', ylab='p(k)', log='xy')

fit_pow = fit_power_law(grados)
abline(a=log((fit_pow$alpha-1)*(fit_pow$xmin**(fit_pow$alpha - 1))), b=-fit_pow$alpha, col='red')
lines(c(1,3000),(fit_pow$alpha-1)*(fit_pow$xmin**(fit_pow$alpha - 1))*(c(1,3000)**(-fit_pow$alpha)), col='blue')

dev.off()
