library("dplyr", lib.loc="~/R/win-library/3.4")
library("igraph", lib.loc="~/R/win-library/3.4")
library("stringi", lib.loc="~/R/win-library/3.4")
library("reshape2", lib.loc="~/R/win-library/3.4")
options(stringsAsFactors = F)
Advice <- read.csv(file="ADVICE.csv" . header = TRUE)
 Advice <- read.csv(file="ADVICE.csv", header = TRUE)
 AdviceMat1 <- filter(Advice) %>% select(-matches('Node'))
 Advicemat <- as.matrix(Advicemat1)
 Advicemat <- as.matrix(AdviceMat1)
 Friends<- read.csv(file="ADVICE.csv", header = TRUE)
 FriendsMat1 <- filter(Friends) %>% select(-matches('Node'))
 Friendsmat <- as.matrix(FriendsMat1)
 Friends<- read.csv(file="FRIENDSHIP.csv", header = TRUE)
 FriendsMat1 <- filter(Friends) %>% select(-matches('Node'))
 Friendsmat <- as.matrix(FriendsMat1)
 Reports <- read.csv(file="REPORTS_TO.csv", header = TRUE)
 ReportsMat1 <- filter(Reports) %>% select(-matches('Node'))
 Reportsmat <- as.matrix(ReportsMat1)
 gfriends <- graph.adjacency(Friendsmat, weighted = NULL, mode = 'directed')
 gadvice <- graph.adjacency(Advicemat, weighted = NULL, mode = 'directed')
 greports <- graph.adjacency(Reportsmat, weighted = NULL, mode = 'directed')
 summary(greports)
 summary(gadvice)
 summary(gfriends)
 techatt <- read.csv(file="High_Tec_Attributes.csv", header = TRUE)
 Friendsmix <- match(V(gfriends)$name, techatt$Node)
Advicemix <- match(V(gadvice)$name, techatt$Node)
 Reportmix <- match(V(greports)$name, techatt$Node)
 Friendsmix
 Reportmix
 Advicemix
 V(gadvice)$club <- techatt$AGE[datamix]
 V(gadvice)$AGE <- techatt$AGE[Advicemix]
 V(gadvice)$TENURE <- techatt$TENURE[Advicemix]
 V(gadvice)$LEVEL <- techatt$LEVEL[Advicemix]
V(gadvice)$DEPT <- techatt$DEPT[Advicemix]
 V(gfriends)$AGE <- techatt$AGE[Friendsmix]
 V(gfriends)$TENURE <- techatt$TENURE[Friendsmix]
 V(gfriends)$LEVEL <- techatt$LEVEL[Friendsmix]
 V(gfriends)$DEPT <- techatt$DEPT[Friendsmix]
 V(greports)$AGE <- techatt$AGE[Reportsmix]
 V(greports)$AGE <- techatt$AGE[Reportmix]
 V(greports)$TENURE <- techatt$TENURE[Reportmix]
 V(greports)$LEVEL <- techatt$LEVEL[Reportmix]
 V(greports)$DEPT <- techatt$DEPT[Reportmix]
 summary(gfriends)
 summary(greports)
 summary(gadvice)
 plot(gadvice, vertex.color=V(gadvice)$DEPT)
 plot(gfriends, vertex.color=V(gfriends)$DEPT)
 plot(greports, vertex.color=V(greports)$DEPT)
 plot(gadvice, vertex.color=V(gadvice)$DEPT)
 graph.density(gadvice)
 graph.density(gfriends)
 sub1gadvice <- induced_subgraph(gadvice, V(gadvice)$DEPT==1, impl = c("copy_and_delete"))
 sub2gadvice <- induced_subgraph(gadvice, V(gadvice)$DEPT==2, impl = c("copy_and_delete"))
 sub3gadvice <- induced_subgraph(gadvice, V(gadvice)$DEPT==3, impl = c("copy_and_delete"))
 View(zachatt)
 View(techatt)
 View(techatt)
 sub4gadvice <- induced_subgraph(gadvice, V(gadvice)$DEPT==4, impl = c("copy_and_delete"))
 sub1friends <- induced_subgraph(gadvice, V(gfriends)$DEPT==1, impl = c("copy_and_delete"))
 sub2friends <- induced_subgraph(gadvice, V(gfriends)$DEPT==2, impl = c("copy_and_delete"))
 sub3friends <- induced_subgraph(gadvice, V(gfriends)$DEPT==3, impl = c("copy_and_delete"))
sub4friends <- induced_subgraph(gadvice, V(gfriends)$DEPT==4, impl = c("copy_and_delete"))
 graph.density(sub1gfriends)
 graph.density(sub1friends)
 graph.density(sub2friends)
 graph.density(sub3friends)
 graph.density(sub4friends)
graph.density(sub1advice)
 graph.density(sub1gadvice)
 graph.density(sub2gadvice)
 sub1friends <- induced_subgraph(gfriends, V(gfriends)$DEPT==1, impl = c("copy_and_delete"))
 sub2friends <- induced_subgraph(gfriends, V(gfriends)$DEPT==2, impl = c("copy_and_delete"))
 sub3friends <- induced_subgraph(gfriends, V(gfriends)$DEPT==3, impl = c("copy_and_delete"))
 sub4friends <- induced_subgraph(gfriends, V(gfriends)$DEPT==4, impl = c("copy_and_delete"))
 graph.density(sub1friends)
 graph.density(sub2friends)
 graph.density(sub3friends)
 graph.density(sub4friends)
 largest_cliques(gadvice)
