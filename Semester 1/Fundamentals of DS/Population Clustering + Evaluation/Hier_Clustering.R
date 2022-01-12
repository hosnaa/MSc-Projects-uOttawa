Data <- c(10, 20, 40, 80, 85, 121, 160, 168, 195)
clusters <- hclust(dist(Data), 'single')
#Plot the dendrogram
plot(clusters)
clusters$height
print(clusters)

clusters <- hclust(dist(Data), 'complete')
#Plot the dendrogram
plot(clusters)
clusters$height
print(clusters)
