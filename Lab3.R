Freeman1 <- read.csv(file="Freeman's_EIES_1.csv", header = TRUE) 
 Freeman2 <- read.csv(file="Freeman's_EIES_2.csv", header = TRUE) 
mat1 <- filter(Freeman1) %>% select(-matches('Node'))
 mat2 <- filter(Freeman2) %>% select(-matches('Node'))
 matr1f <- as.matrix(mat1)
 matr2f <- as.matrix(mat2)
 graph1 <- graph.adjacency(matr1f, weighted = TRUE, mode = 'undirected')
 graph2 <- graph.adjacency(matr2f, weighted = TRUE, mode = 'undirected')
 View(mat1)
 View(mat1)
 graph2 <- graph.adjacency(matr2f, weighted = TRUE, mode = 'directed')
 graph1 <- graph.adjacency(matr1f, weighted = TRUE, mode = 'directed')
summary(graph1)
 summary(graph2)
 freemanatt <- read.csv(file="Freeman's_EIES_Attribute.csv", header = TRUE)
 freemandatamix1 <- match(V(graph1)$name, freemanatt$Node)
 freemandatamix2 <- match(V(graph2)$name, freemanatt$Node)
 datamnix1
 datamix1
 freemandatamix1
 freemandatamix2
 V(graph1)$Citations <- freemanatt$Citations[freemandatamix1]
 V(graph1)$Citations <- freemanatt$Discipline[freemandatamix1]
 V(graph1)$Discipline <- freemanatt$Discipline[freemandatamix1]
 V(graph1)$Citations <- freemanatt$Citations[freemandatamix1]
 V(graph2)$Citations <- freemanatt$Citations[freemandatamix2]
 V(graph2)$Discipline <- freemanatt$Discipline[freemandatamix2]
 summary(graphobj1)
 summary(graph1)
 summary(graph2)
 plot(graph1,vertex.label=V(graphobj)$name,
       +      layout=layout.fruchterman.reingold, 
       +      edge.color="lightblue",
       +      edge.width=E(graph1)$weight*1.5)
 transitivity(graph1, type = c ("local"))
 transitivity(graph2, type = c ("local"))
 transitivity(graph1, type = c ("global"))
 transitivity(graph2, type = c ("global"))
 common1 <-cocitation(graph1)
 common2 <-cocitation(graph2)
 commons1 <- as.matrix(common1)
 commons2 <- as.matrix(common2)
 summary(graph1)
 summary(graph2)
 edge_betweenness(graph1, directed = TRUE)
 edge_betweenness(graph2, directed = TRUE)
 edges1 <- as.data.frame(get.edgelist(graph1))
 edges2 <- as.data.frame(get.edgelist(graph2))
 head(edges1)
 head(edges2)
 ebtwn1 <- edge_betweenness(graph1, directed = TRUE)
 ebtwn2 <- edge_betweenness(graph2, directed = TRUE)
 edges1$ebtwn1 <- ebtwn1
 edges2$ebtwn2 <- ebtwn2
 head(edges2)
 constraint(graph1)
 const1 <- constraint(graph1)
 V(graph1)$const <- const1
 const2 <- constraint(graph2)
 V(graph2)$const <- const2
 summary(graph1)
 summary(graph2)
 btwns1 <- betweenness(graph1, directed=TRUE)
 btwns2 <- betweenness(graph2, directed=TRUE)
 degall1 <- degree(graph1, mode = c("all"))
 degall2 <- degree(graph2, mode = c("all"))
 cl1f <- closeness(graph1)
 cl2f <- closeness(graph2)
 clnorm1 <- closeness(graph1, normalized = TRUE)
 clnorm2 <- closeness(graph2, normalized = TRUE)
 poss <- 32 * 31
graph.density(graph1)
 graph.density(graph2)
library(sna)
reachability(matr1f)
 detach("package:sna", unload=TRUE)
 shortest.paths(graph1, mode = c("all"))
shortest.paths(graph2, mode = c("all"))
 diameter(graph1, directed = TRUE)
 diameter(graph2, directed = TRUE)
 mean_distance(graph1, directed=T)
 mean_distance(graph2, directed=T)
 ebtwn1mat <- as.matrix(ebtwn1)
 write.csv(ebtwn1mat , file="edge1.csv")
 ebtwn2mat <- as.matrix(ebtwn2)
 write.csv(ebtwn2mat , file="edge2.csv")
 const1mat <- as.matrix(const1)
 const2mat <- as.matrix(const2)
 write.csv(const1mat , file="constraint1.csv")
 write.csv(const2mat , file="constraint2.csv")
 centralization.degree(graphobj, mode = c("all"))
 centralization.degree(graph1, mode = c("all"))
centralization.degree(graph2, mode = c("all"))
centralization.betweenness(graph1, directed=TRUE)
centralization.betweenness(graph2, directed=TRUE)
plot(graph1,vertex.label=V(graphobj)$name,
            layout=layout.circle, 
            edge.color="lightblue",
            edge.width=E(graphobj)$ebtwn*.5,
            vertex.size=V(graphobj)$const*10)
plot(graph1)
plot(graph1 , edge.color = "lightblue" , edge.width = E(graph1)$weight*1.5)
plot(graph2 , edge.color = "lightgray" , edge.width = E(graph2)$weight*1.5)

