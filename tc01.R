require(igraph)
graphics.off()
data_y2h = as.matrix(read.table('tc01_data/yeast_Y2H.txt'))
data_APMS = as.matrix(read.table('tc01_data/yeast_AP-MS.txt'))
data_LIT = as.matrix(read.table('tc01_data/yeast_LIT.txt'))

#y2h
g_y2h = graph_from_edgelist(data_y2h)
pdf('pdfy2h.pdf')
plot(g_y2h,vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
#x11()
plot(simplify(g_y2h),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
clst_y2h = clusters(g_y2h)
CG_y2h = induced_subgraph(g_y2h,clst_y2h$membership==which.max(clst_y2h$csize))
plot(simplify(CG_y2h),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
sueltas_y2h = induced_subgraph(g_y2h,clst_y2h$membership!=which.max(clst_y2h$csize))
plot(simplify(sueltas_y2h),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')

## b.i 
N = length(V(g_y2h)) #length calcula el # de elementos
L = length(E(g_y2h)) #V son los vertices, E los ejes
# El grado medio resulta ser igual a

k_med = 2*L/N # si consideramos la red sin dirigir
# otra forma de calcularlo sería

k_med2 = mean(degree(g_y2h))

# Esta red tendría más sentido (conceptualmente) si fuese sin dirigir. Para obtener la red equivalente hacemos
gD_y2h = g_y2h
g_y2h = as.undirected(g_y2h) # ahora g es sin dirigir

## Repito los gráficos
plot(g_y2h,vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
#x11()
plot(simplify(g_y2h),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
clst_y2h = clusters(g_y2h)
CG_y2h = induced_subgraph(g_y2h,clst_y2h$membership==which.max(clst_y2h$csize))
plot(simplify(CG_y2h),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
sueltas_y2h = induced_subgraph(g_y2h,clst_y2h$membership!=which.max(clst_y2h$csize))
plot(simplify(sueltas_y2h),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')


# La densidad de links es el número de links que hay respecto a cuantos podría haber

densidad = L/(N*(N-1)/2)

# Los coeficientes de clustering se pueden calcular fácil en igraph

C_global = transitivity(g_y2h,type='global')
C_med = transitivity(g_y2h,type='localaverage')

# El diámetro también es muy fácil

diametro = diameter(g_y2h)


dev.off()
print(paste('Red Y2H'))
print(paste('El número de nodos es',N))
print(paste('El número de ejes es',L))
print(paste('El grado medio es',k_med,'=',k_med2))
print(paste('La densidad es',densidad))
print(paste('El clustering global es',C_global))
print(paste('El clustering local medio es',C_med))
print(paste('El diametro es',diametro))



#APMS
g_APMS = graph_from_edgelist(data_APMS)
pdf('pdfAPMS.pdf')
plot(g_APMS,vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
#x11()
plot(simplify(g_APMS),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
clst_APMS = clusters(g_APMS)
CG_APMS = induced_subgraph(g_APMS,clst_APMS$membership==which.max(clst_APMS$csize))
plot(simplify(CG_APMS),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
sueltas_APMS = induced_subgraph(g_APMS,clst_APMS$membership!=which.max(clst_APMS$csize))
plot(simplify(sueltas_APMS),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')

## b.i 
N = length(V(g_APMS)) #length calcula el # de elementos
L = length(E(g_APMS)) #V son los vertices, E los ejes
# El grado medio resulta ser igual a

k_med = 2*L/N # si consideramos la red sin dirigir
# otra forma de calcularlo sería

k_med2 = mean(degree(g_APMS))

# Esta red tendría más sentido (conceptualmente) si fuese sin dirigir. Para obtener la red equivalente hacemos
gD_APMS = g_APMS
g_APMS = as.undirected(g_APMS) # ahora g es sin dirigir

## Repito los gráficos
plot(g_APMS,vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
#x11()
plot(simplify(g_APMS),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
clst_APMS = clusters(g_APMS)
CG_APMS = induced_subgraph(g_APMS,clst_APMS$membership==which.max(clst_APMS$csize))
plot(simplify(CG_APMS),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
sueltas_APMS = induced_subgraph(g_APMS,clst_APMS$membership!=which.max(clst_APMS$csize))
plot(simplify(sueltas_APMS),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')


# La densidad de links es el número de links que hay respecto a cuantos podría haber

densidad = L/(N*(N-1)/2)

# Los coeficientes de clustering se pueden calcular fácil en igraph

C_global = transitivity(g_APMS,type='global')
C_med = transitivity(g_APMS,type='localaverage')

# El diámetro también es muy fácil

diametro = diameter(g_APMS)


dev.off()
print(paste('Red APMS'))
print(paste('El número de nodos es',N))
print(paste('El número de ejes es',L))
print(paste('El grado medio es',k_med,'=',k_med2))
print(paste('La densidad es',densidad))
print(paste('El clustering global es',C_global))
print(paste('El clustering local medio es',C_med))
print(paste('El diametro es',diametro))


#LIT
g_LIT = graph_from_edgelist(data_LIT)
pdf('pdfLIT.pdf')
plot(g_LIT,vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
#x11()
plot(simplify(g_LIT),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
clst_LIT = clusters(g_LIT)
CG_LIT = induced_subgraph(g_LIT,clst_LIT$membership==which.max(clst_LIT$csize))
plot(simplify(CG_LIT),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
sueltas_LIT = induced_subgraph(g_LIT,clst_LIT$membership!=which.max(clst_LIT$csize))
plot(simplify(sueltas_LIT),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')

## b.i 
N = length(V(g_LIT)) #length calcula el # de elementos
L = length(E(g_LIT)) #V son los vertices, E los ejes
# El grado medio resulta ser igual a

k_med = 2*L/N # si consideramos la red sin dirigir
# otra forma de calcularlo sería

k_med2 = mean(degree(g_LIT))

# Esta red tendría más sentido (conceptualmente) si fuese sin dirigir. Para obtener la red equivalente hacemos
gD_LIT = g_LIT
g_LIT = as.undirected(g_LIT) # ahora g es sin dirigir

## Repito los gráficos
plot(g_LIT,vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
#x11()
plot(simplify(g_LIT),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
clst_LIT = clusters(g_LIT)
CG_LIT = induced_subgraph(g_LIT,clst_LIT$membership==which.max(clst_LIT$csize))
plot(simplify(CG_LIT),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')
sueltas_LIT = induced_subgraph(g_LIT,clst_LIT$membership!=which.max(clst_LIT$csize))
plot(simplify(sueltas_LIT),vertex.label=NA,vertex.size=1,edge.arrow.size=0.3,edge.color='black')


# La densidad de links es el número de links que hay respecto a cuantos podría haber

densidad = L/(N*(N-1)/2)

# Los coeficientes de clustering se pueden calcular fácil en igraph

C_global = transitivity(g_LIT,type='global')
C_med = transitivity(g_LIT,type='localaverage')

# El diámetro también es muy fácil

diametro = diameter(g_LIT)


dev.off()
print(paste('Red LIT'))
print(paste('El número de nodos es',N))
print(paste('El número de ejes es',L))
print(paste('El grado medio es',k_med,'=',k_med2))
print(paste('La densidad es',densidad))
print(paste('El clustering global es',C_global))
print(paste('El clustering local medio es',C_med))
print(paste('El diametro es',diametro))
