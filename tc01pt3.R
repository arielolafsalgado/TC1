require(igraph)
graphics.off()
g = read_graph('tc01_data/as-22july06.gml',format='gml')
#plot(g)
grados = degree(g)
breaks=0:(max(grados)+1)-0.5
pkhist=hist(grados, breaks=breaks, xlab='grados', ylab='p(k)',freq=F)

