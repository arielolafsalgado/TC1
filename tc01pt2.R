require(igraph)
graphics.off()
g = read_graph('tc01_data/dolphins.gml',format='gml')
dolgen = read.table('tc01_data/dolphinsGender.txt')

V(g)$gender = as.character(dolgen$V2[match(V(g)$label,dolgen$V1)])
V(g)$color  = c('red','blue')[match(V(g)$gender,c('m','f'))]
V(g)$color[is.na(V(g)$gender)] = 'black'
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

#Ejercicio 4
gg = g
n = length(V(g))
ggk = gg
ggc = gg
ggb = gg
gga = gg
sizek = NULL
sizec = NULL
sizeb = NULL
sizea = NULL
for(i in 1:n){
	#Primero con grado
	grados = degree(ggk)
	indice = which.max(grados)
	ggk = delete_vertices(ggk, indice)

	#Ahora por clustering
	clust = transitivity(ggc, type='local')
	indice = which.max(clust)
	ggc = delete_vertices(ggc, indice)

	#Ahora por betweenness
	bets = betweenness(ggb)
	indice = which.max(bets)
	ggb = delete_vertices(ggb, indice)


	#Ahora por azar
	indice = sample(length(V(gga)),size=1)
	gga = delete_vertices(gga, indice)


	sizek = rbind(sizek, sort(clusters(ggk)$csize, decreasing=T)[1:2])
	sizec = rbind(sizec, sort(clusters(ggc)$csize, decreasing=T)[1:2])
	sizeb = rbind(sizeb, sort(clusters(ggb)$csize, decreasing=T)[1:2])
	sizea = rbind(sizea, sort(clusters(gga)$csize, decreasing=T)[1:2])
}

pdf('ej2c.pdf')
plot(sizek[,2]/sizek[,1], pch=18, col='red',xlab='paso',ylab='relacion 2/1')
points(sizec[,2]/sizec[,1], pch=17, col='green')
points(sizeb[,2]/sizeb[,1], pch=16, col='blue')
points(sizea[,2]/sizea[,1], pch=15, col='black')
legend(x=40, y=0.2, legend=c('Grado', 'Clustering', 'Betweenness','Azar'), col=c('red', 'green', 'blue','black'), pch=18:15)
plot(sizek[,2], pch=18, col='red',xlab='paso',ylab='size 2',ylim=c(0,n))
points(sizec[,2], pch=17, col='green')
points(sizeb[,2], pch=16, col='blue')
points(sizea[,2], pch=15, col='black')
legend(x=40, y=40, legend=c('Grado', 'Clustering', 'Betweenness','Azar'), col=c('red', 'green', 'blue','black'), pch=18:15)
plot(sizek[,1], pch=18, col='red',xlab='paso',ylab='size 1')
points(sizec[,1], pch=17, col='green',ylim=c(0,n))
points(sizeb[,1], pch=16, col='blue')
points(sizea[,1], pch=15, col='black')
legend(x=40, y=40, legend=c('Grado', 'Clustering', 'Betweenness','Azar'), col=c('red', 'green', 'blue','black'), pch=18:15)

plot(sizek[,2], pch=18, col='red',xlab='paso',ylab='Tamaño',ylim=c(0,n))
points(sizec[,2], pch=17, col='green')
points(sizeb[,2], pch=16, col='blue')
points(sizea[,2], pch=15, col='black')
legend(x=40, y=30, legend=c('Grado', 'Clustering', 'Betweenness','Azar'), col=c('red', 'green', 'blue','black'), pch=18:15,title='Segundo componente')
points(sizek[,1], pch=5, col='red',xlab='paso',ylab='size 1')
points(sizec[,1], pch=2, col='green',ylim=c(0,n))
points(sizeb[,1], pch=1, col='blue')
points(sizea[,1], pch=0, col='black')
legend(x=40, y=50, legend=c('Grado', 'Clustering', 'Betweenness','Azar'), col=c('red', 'green', 'blue','black'), pch=c(5,2,1,0),title='Primer componente')
dev.off()
