library(readr)
#install.packages("igraph")
library(igraph)

# load main edge list of person to crime
out_moreno_crime_crime <- read_table2("out.moreno_crime_crime.tsv", 
                                      col_names = FALSE, comment = "%")
unique(out_moreno_crime_crime$X3)
  # delete unnecessary NA column
out_moreno <- subset(out_moreno_crime_crime, select = -c(X3) )
names(out_moreno) <- c("person_id", "crime_id")
#moreno_network <- graph_from_data_frame(out_moreno, directed = TRUE, vertices = NULL)
#plot(moreno_network)

# load sex attributes
crime_person_sex <- read_table2("ent.moreno_crime_crime.person.sex.tsv", col_names = FALSE)

  # Note these are the same: 
print(length(unique(out_moreno$X1)))
print(dim(crime_person_sex))

# create an id column to join by & join
len = dim(crime_person_sex)[1]
crime_person_sex$person_id <- seq(1,len)
print(tail(crime_person_sex))
df <- merge(x=out_moreno,y=crime_person_sex,by="person_id",all.x=TRUE)

# load name attributes
crime_person_name <- read_table2("ent.moreno_crime_crime.person.name.tsv", col_names = FALSE)

  # Note these are the same: 
print(length(unique(df$person_id)))
print(dim(crime_person_name))

  # create an id column to join by & join
#len = dim(crime_person_sex)[1]
crime_person_name$person_id <- seq(1,len)
print(tail(crime_person_sex))
df <- merge(x=df,y=crime_person_name,by="person_id",all.x=TRUE)

# get edge list (person roles)
crime_person_role <- read_delim("rel.moreno_crime_crime.person.role.tsv", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

# bind involvements
library(dplyr)
df <- bind_cols(df,crime_person_role)
names(df) <- c("person_id", "crime_id","person_sex","person_name", "person_role")
# person_sex: 1 = male; 2 = female; no other options
View(df)


# Should be using person's name rather than their id; switch person_id with person_name
df2 <- df[,c("person_name","crime_id","person_sex","person_role")]
View(df2)

# rpubs.com/pjmurphy/317505
g2 <- graph.data.frame(df2,directed=FALSE)
bipartite.mapping(g2) # works automatically this time 
#g_el <- as_edgelist(g)
#colnames(g_el) <- c('person','crime')
#head(g_el)
#V(g)$type <- ifelse(V(g)$name %in% g_el[,"person"],TRUE,FALSE)
#g # can tell its bipartite because of "B" in "UN-B"
V(g2)$type <- bipartite_mapping(g2)$type
plot(g2,layout=layout.bipartite)
  # Makes sense, many more people involved than crimes

# https://rpubs.com/pjmurphy/317838
# Calculating centrality 
types2 <- V(g2)$type
deg2 <- degree(g2)
bet2 <- betweenness(g2)
clos2 <- closeness(g2)
eig2 <- eigen_centrality(g2)$vector
cent_df2 <- data.frame(types2,deg2,bet2,clos2,eig2)
cent_df2[order(cent_df2$types2,cent_df2$deg2,decreasing=TRUE),] 
  # crimes then degree; crime #110 was th emost involved
cent_df2[order(cent_df2$deg2,decreasing=TRUE),] 
  # Jenny Willis has 25 involvements! 

V(g2)$size <- degree(g2)
V(g2)$label.cex <- degree(g2)*0.09
plot(g2, layout=layout_with_graphopt)
# Saved plot here 

# Stopped at option 2: # https://rpubs.com/pjmurphy/317838


# Research Questions
# Centrality: can we link a bunch of crimes to one person (they're a suspect, a victim, or both?)
# How many clusters are there? 
# Sex and victimization? 
# Race from name and correlation with crime? 

#Interlocking directorates of irish corporate boards, Nyle Freil Adrian Raffrey 
#- Temporal
#- Other bipartite papers


