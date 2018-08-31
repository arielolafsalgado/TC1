require(igraph)
g = read_graph('tc01_data/dolphins.gml',format='gml')
dolgen = read.table('tc01_data/dolphinsGender.txt')

V(g)$gender =   as.character(dolgen$V2[match(V(g)$label,dolgen$V1)])

males = as.numeric(V(g)$gender=='m')
males[is.na(males)] = 0
females = as.numeric(V(g)$gender=='f')
females[is.na(females)] = 0
X = as_adjacency_matrix(g)
cruces = as.numeric(males%*%X%*%females)
mym = as.numeric(males%*%X%*%males/2)
fyf = as.numeric(females%*%X%*%females/2)
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

