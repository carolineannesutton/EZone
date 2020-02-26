library(plyr)
data(iris)

## Ecdf over all species
iris.all <- summarize(iris, Sepal.Length = unique(Sepal.Length), 
                      ecdf = ecdf(Sepal.Length)(unique(Sepal.Length)))

ggplot(iris.all, aes(Sepal.Length, ecdf)) + geom_step()

#Ecdf within species
iris.species <- ddply(iris, .(Species), summarize,
                      Sepal.Length = unique(Sepal.Length),
                      ecdf = ecdf(Sepal.Length)(unique(Sepal.Length)))

ggplot(iris.species, aes(Sepal.Length, ecdf, color = Species)) + geom_step()