#Load the statnet packages 
library(statnet)
library(network)
library(sna)

#Import first 2 columns of 13B Huddling Edgelist
EL13 = read.csv("13B_HuddlingEdgelist.csv", header=T)
#Convert the edgelist from numeric to string because otherwise R reads in the numerical animal IDs as the length of a list; work with the string version from here forward
EL13S <- data.frame(lapply(EL13, as.character), stringsAsFactors = FALSE)
#Convert the strong edgelist into a network
net13S = network(EL13S[,1:2], directed=F) #This means, read the 1st two columns of EL as a network, with undirected edges (change F to T for directed edges, i.e.grooming).

#Degree centrality (Individual level)
degree_13S<-cbind(network.vertex.names(net13S), degree(net13S, gmode="digraph", rescale = T, cmode = "freeman")) 
head(degree_13S)
write.csv(degree_13S,"13B_Huddling_DegreeCentrality.csv")

#Degree centralization (Group Level)
degreeCentralization_13S<-centralization(net13S, degree, mode="digraph")
degreeCentralization_13S
write.csv(degreeCentralization_13S, "13B_Huddling_DegreeCentralization.csv")

#Eigenvalue centrality (individual level)
eigenvalue_13S<-cbind(network.vertex.names(net13S),evcent(net13S, gmode="digraph", rescale=T))
head(eigenvalue_13S)
write.csv(eigenvalue_13S,"13B_EigenvalueCentrality.csv")

# Closeness centrality (inidividual level)
closeness_13S<-cbind(network.vertex.names(net13S),closeness(net13S, gmode="digraph", rescale=T))
head(closeness_13S)
write.csv(closeness_13S,"13B_ClosenessCentrality.csv")

#Closeness centralization (group level)
closenessCentralization_13S<-centralization(net13S, closeness, mode="digraph")
closenessCentralization_13S
write.csv(closenessCentralization_13S, "13B_Huddling_ClosenessCentralization.csv")

#Import first 2 columns of 13B Huddling Edgelist
betweenness_13S<-cbind(network.vertex.names(net13S),(betweenness(net13S, gmode="digraph", rescale=T)))
head(betweenness_13S)
write.csv(betweenness_13S,"13B_BetweennessCentrality.csv")

#Betweenness centralization (Group Level)
betweennessCentralization_13S_huddle<-centralization(net13S, betweenness, mode="digraph")
betweennessCentralization_13S_huddle
write.csv(betweennessCentralization_13S_huddle, "13B_Huddling_BetweennessCentralization_undirected_GroupLevel.csv")

####################Now do the same for grooming, inclduing undirected and directed networks####################
#Import first 2 columns of 13B GroomExHP Edgelist
EL13G = read.csv("13B_GroomExHPEdgelist.csv", header=T)
#Convert the edgelist from numeric to string because otherwise R reads in the numerical animal IDs as the length of a list; work with the string version from here forward
EL13SG <- data.frame(lapply(EL13G, as.character), stringsAsFactors = FALSE)
#Convert the strong edgelist into a network
net13SG = network(EL13SG[,1:2], directed=F) #We leaved directed=F here to calculate total grooming centrality measures regardless of the direction (i.e. "Groom Degree", "Groom Closeness", and "Groom Eigenvector")
net13SG_givenBehaviors = network(EL13SG[,1:2], directed=T) #This means, read the 1st two columns of EL as a network, with undirected edges (here we changed from F to T because grooming has directed edges) for "Give-Groom" measures.
net13SG_receivedBehaviors = network(EL13SG[,2:1], directed=T) # Switching columns 2 and 1 for calculating in-degree centrality for "Receive Groom" measures EXCLUDING degree, because  degree's argument "cmode=indegree switches direction for you

#Degree centrality (Individual level) undirected grooming network
degree_13SG<-cbind(network.vertex.names(net13SG), degree(net13SG, gmode="digraph", rescale = T, cmode = "freeman")) 
head(degree_13SG)
write.csv(degree_13SG,"13B_GroomExHP_DegreeCentrality_undirected.csv")

#Degree Centrality INDEGREE
degree13SG_groomingReceived_Indegree<-cbind(network.vertex.names(net13SG_givenBehaviors), degree(net13SG_givenBehaviors, gmode="digraph", rescale = T, cmode = "indegree")) 
head(degree13SG_groomingReceived_Indegree)
write.csv(degree13SG_groomingReceived_Indegree,"13B_GroomExHP_groomingReceived_Degree_Indegree.csv")

#Degree Centrality OUTDEGREE
degree13SG_groomingGiven_Outdegree<-cbind(network.vertex.names(net13SG_givenBehaviors), degree(net13SG_givenBehaviors, gmode="digraph", rescale = T, cmode = "outdegree")) 
head(degree13SG_groomingGiven_Outdegree)
write.csv(degree13SG_groomingGiven_Outdegree,"13B_GroomExHP_groomingGiven_Degree_Outdegree.csv")

#Degree centralization (Group Level)
degreeCentralization_13SG<-centralization(net13SG, degree, mode="digraph")
degreeCentralization_13SG
write.csv(degreeCentralization_13SG, "13B_GroomExHP_DegreeCentralization_undirected_GroupLevel.csv")


#Eigenvalue centrality (individual level) undirect grooming network
eigenvalue_13SG<-cbind(network.vertex.names(net13SG),evcent(net13SG, gmode="digraph", rescale=T))
head(eigenvalue_13SG)
write.csv(eigenvalue_13SG,"13B_EigenvalueCentrality_undirected.csv")

#Eigenvalue centrality (individual level) give-groom network
eigenvalue_13SG_groomingGiven<-cbind(network.vertex.names(net13SG_givenBehaviors),evcent(net13SG_givenBehaviors, gmode="digraph", rescale=T))
head(eigenvalue_13SG_groomingGiven)
write.csv(eigenvalue_13SG_groomingGiven,"13B_EigenvalueCentrality_GroomingGiven.csv")

#Eigenvalue centrality (individual level) receive-groom network
eigenvalue_13SG_groomingReceived<-cbind(network.vertex.names(net13SG_receivedBehaviors),evcent(net13SG_receivedBehaviors, gmode="digraph", rescale=T))
head(eigenvalue_13SG_groomingReceived)
write.csv(eigenvalue_13SG_groomingReceived,"13B_EigenvalueCentrality_GroomingReceived.csv")


#Closeness centrality (inidividual level) undirected network: Only exists in undirected because direction in closeness measurements is meaningless
closeness_13SG<-cbind(network.vertex.names(net13SG),closeness(net13SG, gmode="digraph", rescale=T))
head(closeness_13SG)
write.csv(closeness_13SG,"13B_ClosenessCentrality_undirected.csv")


#Closeness centralization (group level)
closenessCentralization_13SG<-centralization(net13SG, closeness, mode="digraph")
closenessCentralization_13SG
write.csv(closenessCentralization_13SG, "13B_GroomExHP_ClosenessCentralization_undirected.csv")

betweenness_13S_groom<-cbind(network.vertex.names(net13S_groom),(betweenness(net13S_groom, gmode="digraph", rescale=T)))
head(betweenness_13S_groom)
write.csv(betweenness_13S_groom,"13B_BetweennessCentrality_Groom.csv")

#Betweenness centralization (Group Level)
betweennessCentralization_13S_groom<-centralization(net13S_groom, betweenness, mode="digraph")
betweennessCentralization_13S_groom
write.csv(betweennessCentralization_13S_groom, "13B_GroomExHP_BetweennessCentralization_undirected_GroupLevel.csv")
