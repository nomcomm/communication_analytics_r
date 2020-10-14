# 1. set up

# install packages
# install.packages("igraph")

# load the installed package
library(igraph)

# 2. Read data and conver it to network 
# load data
attr <- read.csv("Example-Media-attributes.csv", header=T, as.is=T)
am <- read.csv("Example-Media-matrix.csv", header=T, row.names=1)

# inspecting data
head(attr)
head(am)

# inspecting class of am
class(am)

# converting dataframe to matrix
am<-as.matrix(am)
class(am)

# generate network data from adjacency matrix
net <- graph_from_adjacency_matrix(am, weighted=T)
list.edge.attributes(net)
V(net)$name

#assign attributes to nodes in a network
V(net)$media<-attr$media
V(net)$media.type<-attr$media.type
V(net)$type.label<-attr$type.label
V(net)$audience.size<-attr$audience.size
list.vertex.attributes(net)

# 3. visual inspection

# plot the generated network
plot(net, edge.arrow.size=.2, edge.curved=0,
     vertex.color="orange", 
     vertex.frame.color= "#555555",
     vertex.label=V(net)$media,
     vertex.label.color="black",
     vertex.label.cex=.7)

# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]
plot(net, edge.arrow.size=.2,
     vertex.label=V(net)$media, 
     vertex.label.cex=.7)

# Set node size based on audience size:
V(net)$size <- V(net)$audience.size*0.7
plot(net, edge.arrow.size=.2,
     vertex.label=V(net)$media, 
     vertex.label.cex=.7)

# Set edge width based on weight:
E(net)$width <- E(net)$weight/2
plot(net, edge.arrow.size=.2,
     vertex.label=V(net)$media, 
     vertex.label.cex=.7)

# adding legend
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"),
       pch=21, col="#777777", pt.bg=colrs, pt.cex=2, 
       cex=.8, bty="n", ncol=1)

# 4. Simple network analysis

# Number of nodes
vcount(net)

# Number of edges
ecount(net)

# Density (portion of the potential connections in a network that are actual connections)
graph.density(net)

# Diameter (length of network path)
diam <- get_diameter(net, directed = T)
diam

# Color nodes along the diameter:
vcol <- rep("gray40", vcount(net))
vcol[diam] <- "gold"
ecol <- rep("gray80", ecount(net))
ecol[E(net, path=diam)] <- "red"

# E(net, path=diam) finds edges along a path, here 'diam'
plot(net,
     vertex.color=vcol, 
     edge.color=ecol, 
     edge.arrow.mode=0, 
     vertex.label.cex=.7)

# Which media sent out most health information? (degree-centrality)
deg <- degree(net, mode = "out") # mode='in' for in-degree; mode='out for out-degree; mode='all' for total degree
deg
sort(deg, decreasing = T)

V(net)$size<- deg*3
plot(net, edge.arrow.size=.2,
     vertex.label=V(net)$media, 
     vertex.label.cex=.7)

# Which media has the most significant influence on dissemenating information? (betweenness centrality)
bet <- betweenness(net, directed=TRUE, weights=NULL)
sort(bet, decreasing = T)

V(net)$size<- bet*0.2
plot(net, edge.arrow.size=.2,
     vertex.label=V(net)$media, 
     vertex.label.cex=.7)

# We can also find the length of all shortest paths in the graph
dist.from.NYT <- distances(net, v=V(net)[media=="NY Times"], to=V(net), weights=NA)

# Set colors to plot the distances:
V(net)$size <- 15
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.NYT)+1)
col <- col[dist.from.NYT+1]
plot(net, vertex.color=col, vertex.label=dist.from.NYT, edge.arrow.size=.6, vertex.label.color="white")

# what would be the implication of the graph in the context of epidemic?