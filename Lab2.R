library("dplyr", lib.loc="~/R/win-library/3.4")
library("reshape2", lib.loc="~/R/win-library/3.4")
 library("igraph", lib.loc="~/R/win-library/3.4")
 library("stringi", lib.loc="~/R/win-library/3.4")
 options(stringsAsFactors = F)
 zachbinary <- read.csv(file="zache.csv" , header = TRUE)
 zachweighted <- read.csv(file="zachc.csv" , header = TRUE)
 mat1 <- filter(zachbinary)%>%select(-matches('Node'))
 mat2 <- filter(zachweighted)%>%select(-matches('Node'))
 head(mat1)
head(mat2)
 matr1 <-as.matrix(mat1)
 matr2 <- as.matrix(mat2)
 graphbinary <- graph.adjacency(matr1 , weighted =NULL , mode = 'undirected')
 graphweighted <- graph.adjacency(matr2 , weighted = TRUE , mode = 'undirected')
 zachatt <- read.csv(file = "zachattr.csv" , header = TRUE)
 key1 <- match(V(graphweighted)$name , zachatt$Node)
 key2 <- match(V(graphweighted)$name , zachatt$Node)
 key1 <- match(V(graphbinary)$name , zachatt$Node)
 key1
 V(graphbinary)$support <- zachatt$Support[key1]
 V(graphbinary)$strrength <- zachatt$Srength[key1]
 V(graphbinary)$strength <- zachatt$Strength[key1]
 V(graphbinary)$club <- zachatt$Club[key1]
 V(graphweighted)$club <- zachatt$Club[key2]
 V(graphweighted)$strength <- zachatt$Strength[key2]
 V(graphweighted)$support <- zachatt$Support[key2]
 summary(graphweighted)
 summary(graphbinary)
 plot(graphweighted, layout = layout_with_gem,  edge.color="black", vertex.label=V(graphweighted)$club)
 plot(graphweighted, layout = layout_with_gem,  edge.color="black", vertex.label=V(graphweighted)$support)
 plot(graphweighted, layout = layout_with_gem,  edge.color="black", vertex.label=V(graphweighted)$strength)
 plot(graphweighted, layout = layout.circle,  edge.color="black", vertex.label=V(graphweighted)$strength)
 plot(graphweighted, layout = layout.circle,  edge.color="black", vertex.label=V(graphweighted)$club)
 plot(graphweighted, layout = layout.circle,  edge.color="black", vertex.label=V(graphweighted)$strength)
 plot(graphweighted, layout = layout.circle,  edge.color="black", vertex.label=V(graphweighted)$weight)
 plot(graphweighted, layout = layout.circle,  edge.color="black", vertex.label=V(graphweighted)$name)
 plot(graphweighted, layout = layout.fruchterman.reingold, dist = 2,  edge.color="black", vertex.label=V(graphweighted)$club, ylim=c(-.85,.85),xlim=c(-.85,.85), asp = 0)
 ?degree
 degall <- degree(graphbinary, mode = c("all"))
 degall2 <- degree(graphweighted, mode = c("all"))
 degin <- degree(graphbinary, mode = c("in"))
 degout <- degree(graphbinary, mode = c("out"))
 degtotal <- degree(graphbinary, mode = c("total"))
 bdepend <- power_centrality(graphbinary , loops = FALSE, exponent = -0.5 , rescale = FALSE , sparse = FALSE)
 bpower <- power_centrality(graphbinary, loops = FALSE, exponent = 0.5,
                             +                            rescale = FALSE, sparse=FALSE)
 bdepend <- power_centrality(graphbinary, loops = FALSE, exponent = -0.5,
                              +                             rescale = FALSE, sparse=FALSE)
 bdepend <- power_centrality(graphbinary, loops = FALSE, exponent = -0.5,
                              +                             rescale = FALSE, sparse=FALSE)
 cl1 <- closeness(graphbinary)
 cl1
 cl1norm <- closeness(graphbinary, normalized = TRUE)
 reach2=function(x){
  +     r=vector(length=vcount(x))
  +     for (i in 1:vcount(x)){
    +         n=neighborhood(x,2,nodes=i)
    +         ni=unlist(n)
    +         l=length(ni)
    +         r[i]=(l)/vcount(x)}
  +     r}
 reach3=function(x){
  +     r=vector(length=vcount(x))
  +     for (i in 1:vcount(x)){
    +         n=neighborhood(x,3,nodes=i)
    +         ni=unlist(n)
    +         l=length(ni)
    +         r[i]=(l)/vcount(x)}
  +     r}
 rch2_1 <- reach2(graphbinary)
 rch3_1 <- reach3(graphbinary)
 rch2_1 <- reach2(graphweighted)
 rch22_1 <- reach2(graphweighted)
 rch2_1 <- reach2(graphbinary)
 eigen_centrality(graphbinary , directed = FALSE)
 eigen1 <- eigen_centrality(graphbinary)$vector
 btwn1 <- betweenness(graphbinary , directed = FALSE)
 library("sna", lib.loc="~/R/win-library/3.4")
 comp <- data.frame(degall, degin, degout, degtotal, bdepend, 
                     +                    bpower,cl1,cl1norm,rch2_1,rch3_1,eigen1,btwn1,flwb1)
 write.csv(comp , file = "compdata.csv")
 plot(graphweighted, layout = layout.circle,  edge.color="black", vertex.label=V(graphweighted)$name)
 plot(graphweighted, layout = layout.fruchterman.reingold(),  edge.color="black", vertex.label=V(graphweighted)$name)
 plot(graphweighted, layout = layout.fruchterman.reingold,  edge.color="black", vertex.label=V(graphweighted)$name)
 plot(graphweighted, layout = layout.fruchterman.reingold, dist = 2,  edge.color="black", vertex.label=V(graphweighted)$name, ylim=c(-.85,.85),xlim=c(-.85,.85), asp = 0)
 median(degall)
