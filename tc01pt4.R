require(igraph)
require(Matrix)
graphics.off()
pdf('ej4.pdf')
g = read_graph('tc01_data/as-22july06.gml',format='gml')
h = read_graph('tc01_data/netscience.gml',format='gml')
Ag=as_adjacency_matrix(g)
Ah=as_adjacency_matrix(h)

#a)i)
knng=as.numeric(Ag%*%Ag%*%rep(1,length(V(g))))/as.numeric(degree(g))
knngAgg=aggregate(knng,list('k'=degree(g)),mean)
knnh=as.numeric(Ah%*%Ah%*%rep(1,length(V(h))))/as.numeric(degree(h))
knnhAgg=aggregate(knnh,list('k'=degree(h)),mean)
xg=log(knngAgg$k[knngAgg$k>0 & knngAgg$x>0])
yg=log(knngAgg$x[knngAgg$k>0 & knngAgg$x>0])
modg=lm(yg~xg)
xh=log(knnhAgg$k[knnhAgg$k>0 & knnhAgg$x>0])
yh=log(knnhAgg$x[knnhAgg$k>0 & knnhAgg$x>0])
modh=lm(yh~xh)
ug = modg$coefficients[2]
uh = modh$coefficients[2]
#a)ii)iii)
plot(knngAgg$k,knngAgg$x,xlab='grado',ylab='media de grados de vecinos',main='July')
lines(exp(xg),exp(predict(modg)),col='blue')
plot(knngAgg$k,knngAgg$x,xlab='grado',ylab='media de grados de vecinos',log='xy',main='July')
lines(exp(xg),exp(predict(modg)),col='blue')
plot(knnhAgg$k,knnhAgg$x,xlab='grado',ylab='media de grados de vecinos',pch=0,main='Netscience')
lines(exp(xh),exp(predict(modh)),col='blue')
plot(knnhAgg$k,knnhAgg$x,xlab='grado',ylab='media de grados de vecinos',log='xy',pch=0,main='Netsience')
lines(exp(xh),exp(predict(modh)),col='blue')
#dev.off()

#iv)
rcoeff <- function(g){
	dg=degree(g)
	S1=sum(dg)
	S2=sum(dg^2)
	S3=sum(dg^3)
	Se=0
	for (e in E(g)){
		i = tail_of(g,e)
		j = head_of(g,e)
		Se = Se + prod(dg[c(i,j)])
	}
	Se = 2*Se
	r = S1*Se-S2**2
	r = r/(S1*S3-S2**2)
	return(list('r'=r,'S1'=S1,'S2'=S2,'S3'=S3, 'Se'=Se))
}

rg = rcoeff(g)
rh = rcoeff(h)

#b)
data_y2h = as.matrix(read.table('tc01_data/yeast_Y2H.txt'))
data_APMS = as.matrix(read.table('tc01_data/yeast_AP-MS.txt'))
y2h = graph_from_edgelist(data_y2h)

apms = graph_from_edgelist(data_APMS)
Ay2h=as_adjacency_matrix(y2h)
Aapms=as_adjacency_matrix(apms)

#b)i)
knny2h=as.numeric(Ay2h%*%Ay2h%*%rep(1,length(V(y2h))))/as.numeric(degree(y2h))
knny2hAgg=aggregate(knny2h,list('k'=degree(y2h)),mean)
knnapms=as.numeric(Aapms%*%Aapms%*%rep(1,length(V(apms))))/as.numeric(degree(apms))
knnapmsAgg=aggregate(knnapms,list('k'=degree(apms)),mean)
xy2h=log(knny2hAgg$k[knny2hAgg$k>0 & knny2hAgg$x>0])
yy2h=log(knny2hAgg$x[knny2hAgg$k>0 & knny2hAgg$x>0])
mody2h=lm(yy2h~xy2h)
xapms=log(knnapmsAgg$k[knnapmsAgg$k>0 & knnapmsAgg$x>0])
yapms=log(knnapmsAgg$x[knnapmsAgg$k>0 & knnapmsAgg$x>0])
modapms=lm(yapms~xapms)
uy2h = mody2h$coefficients[2]
uapms = modapms$coefficients[2]
#b)ii)iii)
plot(knny2hAgg$k,knny2hAgg$x,xlab='grado',ylab='media de grados de vecinos',main='y2h')
lines(exp(xy2h),exp(predict(mody2h)),col='blue')
plot(knny2hAgg$k,knny2hAgg$x,xlab='grado',ylab='media de grados de vecinos',log='xy',main='y2h')
lines(exp(xy2h),exp(predict(mody2h)),col='blue')
plot(knnapmsAgg$k,knnapmsAgg$x,xlab='grado',ylab='media de grados de vecinos',pch=0,main='ap-ms')
lines(exp(xapms),exp(predict(modapms)),col='blue')
plot(knnapmsAgg$k,knnapmsAgg$x,xlab='grado',ylab='media de grados de vecinos',log='xy',pch=0,main='ap-ms')
lines(exp(xapms),exp(predict(modapms)),col='blue')
dev.off()

#iv)
rcoeff <- function(g){
	dg=degree(g)
	S1=sum(dg)
	S2=sum(dg^2)
	S3=sum(dg^3)
	Se=0
	for (e in E(g)){
		i = tail_of(g,e)
		j = head_of(g,e)
		Se = Se + prod(dg[c(i,j)])
	}
	Se = 2*Se
	r = S1*Se-S2**2
	r = r/(S1*S3-S2**2)
	return(list('r'=r,'S1'=S1,'S2'=S2,'S3'=S3, 'Se'=Se))
}

ry2h = rcoeff(y2h)
rapms = rcoeff(apms)