library(qcc)

# Enter our sample data set.
i.data <- c(5045, 4350, 3975, 4290, 4430, 4485, 4285, 
            3980, 3925, 3645, 3760, 3300, 3685, 3463, 5200)

# Create a QCC object of type xbar.one (qcc's term for 
# individuals charts) and plot the individuals graph.
i.X <- qcc(data=i.data, type="xbar.one", 
           title="Individuals Chart", 
           ylab="Resistance", 
           xlab="Observation")

i.adj <- matrix(nrow=length(i.data)-1, ncol=2)

i.adj[,1] <- i.data[1:(length(i.data)-1)]
i.adj[,2] <- i.data[2:length(i.data)]

i.mR <- qcc(data=i.adj, type="R", 
            title="Moving Range Chart", 
            ylab="Range", xlab="Observation")