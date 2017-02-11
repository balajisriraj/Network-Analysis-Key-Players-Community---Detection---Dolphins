rm(list=ls())
library(igraph)

graph_data = read.graph("dolphins.gml",format = "gml")

plot(graph_data)
tkplot(graph_data)

#Degree of each Node
plot(degree(graph_data),xlab = "Nodes", ylab = "Degree")

#Degree Distribution
plot(degree_distribution(graph_data),xlab = "Nodes", ylab = "Degree Distribution")

#Density
(2*ecount(graph_data))/(gorder(graph_data)*(gorder(graph_data)-1))*100

#Diameter
diameter(graph_data)

#Closeness
closeness(graph_data)

#Betweenness
betweenness(graph_data)

#Clique
clique.number(graph_data)

#Vertex with maximum betweenness value
ver = V(graph_data)
ver[which.max(betweenness(graph_data))]

#vertex with maximum closeness centrality
ver[which.max(closeness(graph_data))]

#Finding key players using betweenness value
sort(betweenness(graph_data),decreasing = T)
data.frame(degree(graph_data)) %>% group_by(degree.graph_data.) %>% count(degree.graph_data.)

data.frame("node" = as.vector(get.vertex.attribute(graph_data)$label),"betweenness" = betweenness(graph_data)) %>% arrange(desc(betweenness))

#Forming Communities using Edge Betweenness
eb = edge.betweenness.community(graph_data)
plot(eb,graph_data,)
mean(eb$modularity)

#Removing top key players and observing newly formed clusters
g = delete_vertices(graph_data,c(37,2,41,38,8,18,21,55,52,58))
plot(g)
tkplot(g)
eb2 = edge.betweenness.community(g)
plot(eb2,g,)
mean(eb2$modularity)
eb2$membership

#storing node names
names1 = data.frame("node" = as.vector(V(graph_data)),"name" = get.vertex.attribute(graph_data)$label)
names2 = data.frame("node" = as.vector(V(g)),"name" = get.vertex.attribute(g)$label)

#Forming subgraphs
g_Cluster1 = subgraph(graph_data,as.vector(unlist(eb[1])))
g_Cluster2 = subgraph(graph_data,as.vector(unlist(eb[2])))
g_Cluster3 = subgraph(graph_data,as.vector(unlist(eb[3])))
g_Cluster4 = subgraph(graph_data,as.vector(unlist(eb[4])))
g_Cluster5 = subgraph(graph_data,as.vector(unlist(eb[5])))
plot(g_Cluster1)
plot(g_Cluster2)
plot(g_Cluster3)
plot(g_Cluster4)
plot(g_Cluster5)

#Key player in each cluster
key1 = get.vertex.attribute(g_Cluster1)$label[which.max(closeness(g_Cluster1))]
key2 = get.vertex.attribute(g_Cluster2)$label[which.max(closeness(g_Cluster2))]
key3 = get.vertex.attribute(g_Cluster3)$label[which.max(closeness(g_Cluster3))]
key4 = get.vertex.attribute(g_Cluster4)$label[which.max(closeness(g_Cluster4))]
key5 = get.vertex.attribute(g_Cluster5)$label[which.max(closeness(g_Cluster5))]

#Remove key players and build clusters
names1[names1$name == key1,]$node
names1[names1$name == key2,]$node
names1[names1$name == key3,]$node
names1[names1$name == key4,]$node
names1[names1$name == key5,]$node
new_graph = delete_vertices(graph_data,c(43,18,15,52,54))
ebnew = edge.betweenness.community(new_graph)
plot(ebnew,new_graph,)
order(ebnew$membership)
