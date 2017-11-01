library("rjson")
library("igraph")

### Read parameters ###
A = commandArgs(trailingOnly = TRUE)
input_file = A[1]
output_file = A[2]
cat("R print:")
cat(input_file)


### Import data and make graph ###
g = read.csv(file = input_file, header = FALSE, sep = " ")
g = as.matrix(g)
g = graph_from_edgelist(g, directed = FALSE)
g = simplify(g)
g = delete_vertices(g, which(degree(g)==0))


############## Components #################

### Assign a component ID to each node ###
components_g = components(g)
members_components = components_g$membership

### Save the Twitter IDs within each component ###
components.list.IDs = vector("list", max(members_components))
names(components.list.IDs) = as.character(1:max(members_components))
for(i in 1:max(members_components)) components.list.IDs[[i]] = names(members_components[which(members_components==i)])

### Export components to JSON ###
exportJSON_components = toJSON(components.list.IDs)
write(exportJSON_components, file = gsub("communities", "components", output_file))



############## Communities #################

### Assign a community ID to each remaining node ###
communities_g = cluster_infomap(g)
members = membership(communities_g)

### Save the Twitter IDs within each community ###
communities.list.IDs = vector("list", max(members))
names(communities.list.IDs) = as.character(1:max(members))
for(i in 1:max(members)) communities.list.IDs[[i]] = names(members[which(communities_g$membership==i)])

### Export communities to JSON ###
exportJSON = toJSON(communities.list.IDs)
write(exportJSON, file = output_file)




#############  key-players MEB centrality #################

graph.components.size = components_g$csize

giant_component = induced.subgraph(g, names(which(members_components==which.max(graph.components.size))))


MEB = rep(0,length(V(giant_component)))
Betweenness_centrality = betweenness(giant_component, normalized = TRUE)
Betweenness_centrality = Betweenness_centrality + 10^(-9)
for(i in 1:length(V(giant_component))) MEB[i] = -Betweenness_centrality[i]*sum(log(10^(-9) + betweenness(giant_component, v = neighbors(giant_component, V(giant_component)[i]), normalized = TRUE)))

MEB_results = sort.int(MEB, decreasing = TRUE, index.return = TRUE)
top_MEB_users = names(V(giant_component)[MEB_results$ix[1:20]])

write(top_MEB_users, file = gsub("communities", "top_MEB_users", output_file))
