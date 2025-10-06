## Network examples
rm(list = ls()) # clear memory
setwd("/Users/ful083/Work/Lenfest_EBFM/Network_&_Criticality_&_Diets/Hub_index/")

library(data.table)
library(magrittr)
library(tidyverse)
library(tidygraph)
library(qgraph)
library(visNetwork)
library(intergraph)
library(network)
library(ggraph)
library("qgraph")
library(igraph)
library(stringr)
library(enaR)

# Load the NEUS data

inDir <- "/Users/ful083/Work/Lenfest_EBFM/Network_&_Criticality_&_Diets/Hub_index/consumption_csv_files/"
inDir2 <- "/Users/ful083/Work/Lenfest_EBFM/Network_&_Criticality_&_Diets/Hub_index/diet_csv/"

inName <- "Aleutians"
inName <- "Baltic"
inName <- "Benguela"
inName <- "Black_Sea"
inName <- "California_Current"
inName <- "Canada_Nth_Gulf_St_Lawerence"
inName <- "Central_Atlantic_1950s"
inName <- "Central_Atlantic_1990s"
inName <- "Central_Nth_Pacific"
inName <- "Chesapeake"
inName <- "East_Bering_Sea"
inName <- "East_Bering_Sea_Full"
inName <- "East_Bering_Sea_1step"
inName <- "East_Bering_Sea_2step"
inName <- "East_Tropical_Pacific"
inName <- "GBR"
inName <- "Georges_Bank"
inName <- "Georgia_Strait"
inName <- "Grand_Banks"
inName <- "Gulf_of_Maine"
inName <- "Gulf_of_Mexico"
inName <- "Gulf_of_Thailand"
inName <- "Humboldt"
inName <- "Irish_Sea"
inName <- "Kerala"
inName <- "Mid_Atlantic_Bight"
inName <- "New_Zealand"
inName <- "North_Central_Chile"
inName <- "North_Sea"
inName <- "Nth_Atlantic"
inName <- "NWS"
inName <- "Port_Phillip_Bay"
inName <- "SE_Australia"
inName <- "Tampa_Bay"
inName <- "USA_Prince_William_Sound"
inName <- "West_English_Channel"
inName <- "West_Florida_Shelf"

infilecsv <- paste(inDir,inName,".csv",sep="")
indietfilecsv <- paste(inDir2,inName,"_diet.csv",sep="")

use_weights <- 1
consData <- read.table(infilecsv,header=TRUE, sep=",", row.names = 1)
dietData <- read.table(indietfilecsv,header=TRUE, sep=",", row.names = 1)

IDname <- paste(inDir,inName,"_IDs.csv",sep="")
localIDs <- read.table(IDname,header=TRUE, sep=",")


#nodes <- read.csv("-diet-nodes.csv")
#View(consData)
#dim(consData)   # Dimensions of Data
#gg <- network(consData, directed=TRUE, loops=TRUE, vertex.names=nodes)  # Create network from consData
#summary(gg)

#gn <- asIgraph(gg, amap=rules)
#l <- asDF(gn)
#str(l)
#l$edges
#l$vertexes


##################### CREATE NETWORK STRUCTURE ###########################
# Create edges df
# Calculate Degree in and Degree out
step1 <- reshape2::melt(as.matrix(consData))
names(step1)[names(step1) == "Var1"] <- "preyName"
names(step1)[names(step1) == "Var2"] <- "predName"
names(step1)[names(step1) == "value"] <- "propDiet"
step1$predName <- gsub(".", " ", step1$predName, fixed=TRUE)  #Sort the issue with spaces in names

# Replace names with IDs
step2a <- step1 %>% dplyr::inner_join(localIDs,by=c("preyName" = "Group"))
names(step2a)[names(step2a) == "ID"] <- "prey_ID"
step2 <- step2a %>% dplyr::inner_join(localIDs,by=c("predName" = "Group"))
names(step2)[names(step2) == "ID"] <- "pred_ID"
nDiet <- step2[step2$propDiet != 0, ]

if (use_weights > 0) {
  nl <- subset(nDiet, select = -c(preyName, predName))
  names(nl)[names(nl) == "prey_ID"] <- "V1"
  names(nl)[names(nl) == "pred_ID"] <- "V2"
  nl$na <- "FALSE"
  nl$V1 <- as.numeric(nl$V1)
  nl$V2 <- as.numeric(nl$V2)
  nl$na <- as.logical.factor(nl$na)
  nl$na <- FALSE
  nl$weight <- nl$propDiet
  nl <- subset(nl, select = -c(propDiet))
} else {
  nl <- subset(nDiet, select = -c(preyName, predName, propDiet))
  names(nl)[names(nl) == "prey_ID"] <- "V1"
  names(nl)[names(nl) == "pred_ID"] <- "V2"
  nl$na <- "FALSE"
  nl$V1 <- as.numeric(nl$V1)
  nl$V2 <- as.numeric(nl$V2)
  nl$na <- as.logical.factor(nl$na)
  nl$na <- FALSE
}

nlv <- localIDs
nlv$na <- "FALSE"
nlv$na <- as.logical.factor(nlv$na)
nlv$na <- FALSE
nlv$vertex.names <- nlv$Group
names(nlv)[names(nlv) == "species_ID"] <- "intergraph_id"
nlv <- subset(nlv, select = -c(Group))

g <- asIgraph(nl, vertices=nlv, directed=TRUE)
set_vertex_attr(g, "label", value=nlv[,2])


##################### IGRAPH WORK UP ###########################
#g <- asIgraph(l$edges, vertices=l$vertexes, directed=TRUE)
#set_vertex_attr(g, "label", value=l$vertexes[,2])

##g <- graph.incidence(consData, directed=TRUE, mode="in")  ## Don't use this as treating single spp when pred and prey as spearate entities
edges <- get.edgelist(g)   # Can also be extracted using E(g)
nodes <- V(g)$vertex.names    # Name of vertices
E(g)$weight  # Use this to see weight per edge (NULL here as incidence only)

# Adjacency matrix
g[]
# To see only the first line use g[1,]

## Plot the network
#plot(g)

## Use tidygraph instead - create the network (graph) - can also use as_tbl_graph() to convert an igraph or netwprk 
# library network to a tidygraph graph
food_tidy <- as_tbl_graph(g, directed = TRUE)

## Now do real network analysis
centRes <- centrality(food_tidy)
dnonorm <- degree(g, mode = "all", normalized = FALSE)

# Node strength (degree):
centRes$OutDegree # Or InDegree, it's the same in unweighted networks

# Closeness:
centRes$Closeness

# Betweenness:
centRes$Betweenness

# Plotting centrality measures
centralityPlot(food_tidy)
# Compare networks  with this centralityPlot by creating a list of graphs to compare - see Network Analysis in R Cookbook.pdf

#### Other statistical approaches to calculate the indicators
# Number of nodes
length(V(g))

# Find the standalone components - subwebs
clusters(g)   # Output shows its one interconnected cluster (community)
# If it wasn't all one community but you wanted to focus on one community you can suck out just that sub-web using
#subweb<-induced.subgraph(food_tidy, which(clusters(newnet)$membership == x)  where x is the mmerbship number of the subweb you want

# Average path length - can also be done using mean_distance(g)
average.path.length(g)

# Clustering coefficient
transitivity(g)

# Centralization degree score
centralization.degree(g)$centralization

#iGraph degree routine Degree centrality is simplest of the methods, it measures the number of connections between a node and all other nodes. 
d <- degree(g, mode = "in", normalized = TRUE)
write(d, file = "centralityIN.txt", ncolumns = 1, sep = " ")
d <- degree(g, mode = "out", normalized = TRUE)
write(d, file = "centralityOUT.txt", ncolumns = 1, sep = " ")
d <- degree(g, mode = "all", normalized = TRUE)
write(d, file = "centralityALL.txt", ncolumns = 1, sep = " ")
dnonorm <- degree(g, mode = "all", normalized = FALSE)
write(dnonorm, file = "degree.txt", ncolumns = 1, sep = " ")
dnonorm <- degree(g, mode = "in", normalized = FALSE)
write(dnonorm, file = "degreeIN.txt", ncolumns = 1, sep = " ")
dnonorm <- degree(g, mode = "out", normalized = FALSE)
write(dnonorm, file = "degreeOUT.txt", ncolumns = 1, sep = " ")

# Closeness centrality is an evaluation of the proximity of a node to all other
# nodes in a network, not only the nodes to which it is directly connected
c <- closeness(g, mode="in", weights=NA, normalized=T)
write(c, file = "closenessIN.txt", ncolumns = 1, sep = " ")
c <- closeness(g, mode="out", weights=NA, normalized=T)
write(c, file = "closenessOUT.txt", ncolumns = 1, sep = " ")
c <- closeness(g, mode="all", weights=NA, normalized=T)
write(c, file = "closenessALL.txt", ncolumns = 1, sep = " ")

# Betweenness centrality looks for chokepoints in the network
b <- betweenness(g, directed=F, weights=NA, normalized = T)
write(b, file = "betweeness.txt", ncolumns = 1, sep = " ")

# Page rank score
pg <- page.rank(g, damping = 0.85)
write(pg, file = "pagerank.txt", ncolumns = 1, sep = " ")


##################### SURF ###########################

dims <- dim(dietData)
preydim <- dims[[1]]
preddim <- dims[[2]]
dD <- as.matrix(dietData)
pstep <- matrix(0, preydim, preddim)

# Assumes each row is a prey and columns is predators
# The columns are diet vector (proportion of diet for predator j from prey i)
L <- 0
for(i in 1:preydim) {
  for(j in 1:preddim) {
    pstep[i,j] <- dD[i,j] * dD[i,j] 
    if (dD[i,j] > 0) {
      L <- L + 1
    }
  }
}

df <- as.data.frame(pstep)
dfrowsum <- rowSums(df)
SURF <- dfrowsum / L
SURFname <- paste(inName,"SURF.txt",sep="")
write(SURF, file = SURFname, ncolumns = 1, sep = " ")

##################### SUPPLY CHAIN CRITICALITY ###########################
dims <- dim(consData)
cpreydim <- dims[[1]]
cpreddim <- dims[[2]]
cD <- as.matrix(consData)
dfcolsum <- colSums(consData)
step1 <- matrix(0, cpreydim, cpreddim)
step2 <- rep(0, cpreddim)
step3 <- matrix(0, cpreydim, cpreddim)

# Assumes each row is a prey and columns is predators
# The columns are consumption vector (proportion of consumption for predator j from prey i)
for(i in 1:cpreydim) {
  for(j in 1:cpreddim) {
    if(dfcolsum[j] > 0) {
      step1[i,j] <- cD[i,j] / dfcolsum[j]
    }
  }
}

totcons <- sum(dfcolsum, na.rm = TRUE)
step2 <- dfcolsum / totcons

for(i in 1:cpreydim) {
  for(j in 1:cpreddim) {
    step3[i,j] <- step1[i,j] * step2[j] * step2[j]
  }
}

step3df <- as.data.frame(step3)
CRITpred <- colSums(step3df)
CRITprey <- rowSums(step3df)

CRITnameA <- paste(inName,"CRITpred.txt",sep="")
write(CRITpred, file = CRITnameA, ncolumns = 1, sep = " ")
CRITnameB <- paste(inName,"CRITprey.txt",sep="")
write(CRITprey, file = CRITnameB, ncolumns = 1, sep = " ")

#############################################################
### Calculating Keystone index
#############################################################
options(max.print = 99999)
inDir3 <- "/Users/ful083/Work/Lenfest_EBFM/Network_&_Criticality_&_Diets/Hub_index/mti_files/"

inName <- "EBS_Full_"
inName <- "EBS_simplif1_"
inName <- "EBS_simplif2_"
inName <- "Nth_Sea_"
inName <- "USA_PWS_"
inName <- "USA_Florida_"
inName <- "West_Channel_"

Flowcsv <- paste(inDir3,inName,"Flow.csv",sep="")  # Consumption matrix
Respcsv <- paste(inDir3,inName,"Resp.csv",sep="")  # Assumes respiration = 0.15* production for primary producers
Biocsv <- paste(inDir3,inName,"Bio.csv",sep="") # Biomasses
Impcsv <- paste(inDir3,inName,"Import.csv",sep="") # Gross production (and any import from diets)
Expcsv <- paste(inDir3,inName,"Exports.csv",sep="") # Catch, discards and flow to detritus
Livingcsv <- paste(inDir3,inName,"Living.csv",sep="") # Whether living or dead

FlowData <- read.table(Flowcsv,header=TRUE, sep=",", row.names = 1)
RespData <- read.table(Respcsv,header=TRUE, sep=",", row.names = 1)
BioData <- read.table(Biocsv,header=TRUE, sep=",", row.names = 1)
ImpData <- read.table(Impcsv,header=TRUE, sep=",", row.names = 1)
ExpData <- read.table(Expcsv,header=TRUE, sep=",", row.names = 1)
LivingData <- read.table(Livingcsv,header=TRUE, sep=",", row.names = 1)

#FlowDataMelt <- reshape2::melt(as.matrix(FlowData))
#names(FlowDataMelt)[names(FlowDataMelt) == "Var1"] <- "preyRowName"
#names(FlowDataMelt)[names(FlowDataMelt) == "Var2"] <- "predColName"
#names(FlowDataMelt)[names(FlowDataMelt) == "value"] <- "Consumption"
#FlowDataMelt$predColName <- gsub(".", " ", FlowDataMelt$predColName, fixed=TRUE)  #Sort the issue with spaces in names
#FlowDataMelt <- FlowDataMelt %>% dplyr::inner_join(localIDs,by=c("preyRowName" = "Group"))
#names(FlowDataMelt)[names(FlowDataMelt) == "ID"] <- "preyRowID"
#FlowDataMelt <- FlowDataMelt %>% dplyr::inner_join(localIDs,by=c("predColName" = "Group"))
#names(FlowDataMelt)[names(FlowDataMelt) == "ID"] <- "predColID"
#FlowDataMelt$flows <- FlowDataMelt$Consumption
#FlowDataMelt <- FlowDataMelt[, -c(1:3)] # delete columns 5 through 7  

# Build network model of assumed form
netdata <- pack(flow = FlowData,
                input = ImpData$Import,
                export = ExpData$Export,
                respiration = RespData$Respiration,
                storage = BioData$Biomass,
                living = LivingData$Living)

# Check data
unpack(netdata)

# Rebalance in this software
ssCheck(netdata)
netdataBal <- balance(netdata,method="AVG2")

# Calculate mixed trophic inputs 
# and then keystone index following equations from 
# Libralato et al (2006) Ecological Modelling 195: 153–171
nG <- length(BioData$Biomass)
sumBio <- sum(BioData$Biomass)
BioData$prop_bio <- BioData$Biomass / sumBio
mti <- enaMTI(netdataBal, balance.override = TRUE)
MTIname <- paste(inName,"MTI.txt",sep="")
write(mti$M, file = MTIname, ncolumns = 1, sep = " ")

mti$Relations.Table

sumMTI <- 0
for (i in 1:nG) {
  for (j in 1:nG) {
    if (i == j) {
      # Do nothing
    } else {
      sumMTI <- sumMTI + (mti$M[i,j] * mti$M[i,j])
    }
  }
}
epsi <- sqrt (sumMTI)
keystone <- log(epsi * (1 - BioData$prop_bio))

KEYname <- paste(inName,"Keystone.txt",sep="")
write(keystone, file = KEYname, ncolumns = 1, sep = " ")

