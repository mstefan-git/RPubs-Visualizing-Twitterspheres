# Replication file for: "Visualizing Twitterspheres"
# RPubs-link: https://rpubs.com/mstefan-rpubs/twittersphere
# (c) Martin Stefan, June 2020

# clear workspace
rm(list = ls())

# load packages
library(rtweet)
library(tidyverse)
library(igraph)

# import tweets
tweets <- readRDS("tweets.RDS")

# get hashtags
htags <- tweets$hashtags
utags <- unique(unlist(htags))
utags <- utags[-which(utags == "BernieSanders")]

# adjacency matrix
mat <- matrix(0, length(utags), length(utags))
rownames(mat) <- utags
colnames(mat) <- utags
for(t in 1:length(htags)){
  tags <- htags[[t]]
  if(length(tags) == 1) next()
  tags <- tags[-which(tags == "BernieSanders")]
  mat[tags,tags] <- mat[tags,tags] + 1
}
rm(t)
diag(mat) <- 0

# network
net <- graph_from_adjacency_matrix(mat, mode = "directed", weighted = T)
net <- net %>% delete.vertices(strength(net,mode="all") < 2)
length(V(net))
clusters <- cluster_walktrap(net, steps = 5)
colors <- sample(rainbow(length(unique(clusters$member))))
colors <- adjustcolor(colors[clusters$member], alpha.f = 0.7) # add transparency

# pdf image
pdf("net.pdf", width = 20, height = 20)
set.seed(1)
plot(simplify(net),
     layout = layout_with_fr(net),
     edge.arrow.width = .001,
     edge.arrow.size = 0.001,
     edge.color = adjustcolor(1, alpha.f = 0.05),
     vertex.size = strength(net)^(1/3),
     vertex.color = adjustcolor(colors[clusters$member], alpha.f = 0.7),
     vertex.frame.color = adjustcolor("black", alpha.f = 0.9),
     vertex.label.color = "black",
     vertex.label.cex = strength(net)^(1/2) / 9,
)
graphics.off()

# png image in html file
png("net.png", width = 4000, height = 3000)
set.seed(1)
plot(simplify(net),
     layout = layout_with_fr(net),
     edge.arrow.width = .001,
     edge.arrow.size = 0.001,
     edge.color = adjustcolor(1, alpha.f = 0.05),
     vertex.size = strength(net)^(1/3),
     vertex.color = adjustcolor(colors[clusters$member], alpha.f = 0.7),
     vertex.frame.color = adjustcolor("black", alpha.f = 0.9),
     vertex.label.color = "black",
     vertex.label.cex = strength(net)^(1/2)/3,
)
graphics.off()
