require(igraph)
graphics.off()
g = read_graph('tc01_data/dolphins.gml',format='gml')
dolgen = read.table('tc01_data/dolphinsGender.txt')

V(g)$gender =   as.character(dolgen$V2[match(V(g)$label,dolgen$V1)])

##Punto a.
pdf('layouts_dolphins.pdf')
lN = layout_nicely(g)
plot(g, layout=lN,main='Nicely')
lFR = layout_with_fr(g)
#x11()
plot(g, layout=lFR,main='FR')
lT = layout_as_tree(g)
#x11()
plot(g, layout=lT,main='Tree')
lC = layout_in_circle(g)
#x11()
plot(g, layout=lC,main='Circle')
lSPH = layout_on_sphere(g)
#x11()
plot(g, layout=lSPH,main='Sphere')
lKK = layout_with_kk(g)
#x11()
plot(g, layout=lKK,main='KK')
lG = layout_on_grid(g)
#x11()
plot(g, layout=lG,main='Grid')
lSUG = layout_with_sugiyama(g)$layout
#x11()
plot(g, layout=lSUG,main='Sugiyama')
dev.off()
##Punto b.
if(T){
males = as.numeric(V(g)$gender=='m')
males[is.na(males)] = 0
females = as.numeric(V(g)$gender=='f')
females[is.na(females)] = 0
X = as_adjacency_matrix(g)
cruces = as.numeric(males%*%X%*%females)
mym = as.numeric(males%*%X%*%males/2)/length(E(g))
fyf = as.numeric(females%*%X%*%females/2)/length(E(g))
delta_cruces = cruces/length(E(g))

N=1000
dolgen_fake = dolgen
cruces_fake = rep(NA,N)
mym_fake = rep(NA,N)
fyf_fake = rep(NA,N)
for(i in 1:N){
	orden = sample(nrow(dolgen))
	dolgen_fake$V1 = dolgen_fake$V1[orden]
	V(g)$gender =   as.character(dolgen_fake$V2[match(V(g)$label,dolgen_fake$V1)])

	males = as.numeric(V(g)$gender=='m')
	males[is.na(males)] = 0
	females = as.numeric(V(g)$gender=='f')
	females[is.na(females)] = 0
	X = as_adjacency_matrix(g)
	cruces_fake[i] = as.numeric(males%*%X%*%females)
	mym_fake[i] = as.numeric(males%*%X%*%males/2)
	fyf_fake[i] = as.numeric(females%*%X%*%females/2)

}
cruces_fake = cruces_fake/length(E(g))
mym_fake = mym_fake/length(E(g))
fyf_fake = fyf_fake/length(E(g))
pval_cruces=sum(delta_cruces>=cruces_fake)/N
pval_fyf=sum(fyf<=fyf_fake)/N
pval_mym=sum(mym<=mym_fake)/N

pdf('histo_homofilia.pdf')
hist(cruces_fake,xlab='Fracción de cruces',main='Histograma de fracción de ejes pakis',xlim=c(0,0.6),ylim=c(0,250))
abline(v=delta_cruces,col='red')
legend(x=0,y=200,legend=c(paste('Valor observado =',round(delta_cruces,2)),paste('p-valor =',round(pval_cruces,3))),lty=c('solid',NA),col=c('red',NA),cex=0.75)
hist(mym_fake,xlab='Fracción de M-M',main='Histograma de fracción de ejes gays',xlim=c(0,0.6),ylim=c(0,250))
abline(v=mym,col='red')
legend(x=0,y=200,legend=c(paste('Valor observado =',round(mym,2)),paste('p-valor =',round(pval_mym,3))),lty=c('solid',NA),col=c('red',NA),cex=0.75)
hist(fyf_fake,xlab='Fracción de F-F',main='Histograma de fracción de ejes lésbicos',xlim=c(0,0.6),ylim=c(0,250))
abline(v=fyf,col='red')
legend(x=0.3,y=200,legend=c(paste('Valor observado =',round(fyf,2)),paste('p-valor =',round(pval_fyf,3))),lty=c('solid',NA),col=c('red',NA),cex=0.75)
dev.off()
}

