### Homework 1 

## Some visualizations of the Dining data

# ---- C1 ----
# Load packages

library("ggplot2")
# Must load other packages first
library("sand")

# ---- C2 ----
# Load data

path <- ('/Users/KevQuant/Desktop/Depaul/csc495/wk1/hwk1')
setwd(path)
dining <- read.graph("dining.net", format="pajek")
summary(dining)


# ---- C3 ----
# Student names
#student_name<-V(dining)$id
V(dining)$id

# ---- C4 ----
# Plot network
# Adjust the edge arrow size for less ugliness
plot(dining,main="dining network",edge.arrow.size=0.4,vertex.label=V(dining)$id)


# ---- C5 ----
# In-degree
deg<-degree(dining,mode='in')
deg
summary(deg)

# ---- C6 ----
# Degree distribution histogram
par(mar=c(4,4,4,4),cex.axis=0.8,cex.main=1,cex.lab=1)
barplot(table(deg),
        xlab="Degree",
        ylab = "Count",
        main="Degree Distribution Histogram",
        ylim = c(0,10),
        xlim = c(0,6))

# ---- C7 ----
# GGPlot version (optional)
g<-ggplot(data.frame(deg),aes(x=deg))
g<-g+geom_histogram(binwidth = 1,col="black",fill="blue")
g<-g+scale_y_continuous(breaks = seq(0,8,2))
g<-g+scale_x_continuous(breaks = seq(0,8))
g<-g+xlab("Degree")
g<-g+ylab("Frequency Count")
g<-g+ggtitle("Degree Distribution Histogram")
print(g)

# ---- C8 ----
# Calculate wt attribute
w<-E(dining)$weight
w[w==2]<-0.5
w[w==1]<-1
E(dining)$wt<-rep(0,vcount(dining))
E(dining)$wt<-w
E(dining)$wt

# ---- C9 ----
# Compute weighted degree
weighted.degree<-graph.strength(dining,mode = "in",weight=E(dining)$wt)
weighted.degree
summary(weighted.degree)
weighted.degree2<-weighted.degree
#Reset the Zero weighted.degree to 0.5 for better visualization plot
weighted.degree2[weighted.degree2==0.5]<-0.8
weighted.degree2[weighted.degree2==0.0]<-0.5


# ---- C10 ----
# Visualization
g<-ggplot(data.frame(weighted.degree),aes(x=weighted.degree))
g<-g+geom_histogram(binwidth = 0.5,color="grey",fill="blue")
g<-g+xlab("Vertex Strength")
g<-g+ylab("Frequency")
g<-g+ggtitle("Vertex Strength Histogram")
g<-g+scale_x_continuous(breaks=seq(0,5,1))
print(g)

# ---- Extra ----
# Visualization with weighted degree and weighted edges
plot(dining,
     rescale=TRUE,
     ylim = c(-0.6,0.6),
     xlim = c(-1.1,1.3),
     edge.arrow.size=weighted.degree2*0.5,
     #vertex size with zero values were adjusted at the section #8.
     vertex.size=weighted.degree2*6,
     edge.width=E(dining)$wt*2.5,
     vertex.label.cex=weighted.degree*0.6, 
     vertex.label=V(dining)$id)

