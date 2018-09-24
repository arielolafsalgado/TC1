require(igraph)
graphics.off()
#pdf('ej3.pdf')
g = read_graph('tc01_data/as-22july06.gml',format='gml')
#plot(g)
grados = degree(g)
breaks=1:(max(grados)+1)-0.5
pkhist=hist(grados, breaks=breaks, plot=F)
png('P3_binlin_lin.png')
plot(pkhist$mids, pkhist$density, xlab='k', ylab='p(k)', main='Bineado lineal - Escala lineal')
dev.off()

png('P3_binlin_loglin.png')
plot(pkhist$mids, pkhist$density, xlab='k', ylab='p(k)', log='x', main='Bineado lineal - Escala log-lin')
dev.off()

png('P3_binlin_linlog.png')
plot(pkhist$mids, pkhist$density, xlab='k', ylab='p(k)', log='y', main='Bineado lineal - Escala lin-log')
dev.off()

png('P3_binlin_loglog.png')
plot(pkhist$mids, pkhist$density, xlab='k', ylab='p(k)', log='xy', main='Bineado lineal - Escala log-log')
dev.off()



breaks_log = 2**(0:(log(max(grados))/log(2) + 1))
log_hist = hist(grados, breaks=breaks_log, plot=F)

png('P3_binlog_lin.png')
plot(log_hist$mids, log_hist$density, xlab='k', ylab='p(k)', main='Bineado logarítmico - Escala lineal')
dev.off()

png('P3_binlog_log.png')
plot(log_hist$mids, log_hist$density, xlab='k', ylab='p(k)', log='xy', main='Bineado logarítmico - Escala logarítmica')

fit_pow = fit_power_law(grados)
#abline(a=log((fit_pow$alpha-1)*(fit_pow$xmin**(fit_pow$alpha - 1))), b=-fit_pow$alpha, col='red')
lines(fit_pow$xmin:3000,(fit_pow$alpha-1)*(fit_pow$xmin**(fit_pow$alpha - 1))*((fit_pow$xmin:3000)**(-fit_pow$alpha)), col='blue')

dev.off()
