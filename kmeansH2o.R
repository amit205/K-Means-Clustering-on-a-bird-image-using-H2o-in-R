rm(list=ls())
library(h2o)
h2o.init()
h2o.clusterInfo()

library(jpeg)
library(ggplot2)

#Read Image
img <- readJPEG("BirdImageforCA2.jpg")
head(img)

#Obtain the Dimensions of our Image
imgDM <- dim(img)
imgDM
#Assign RGB Channels to Data Frame
imgRGB <- data.frame(
  x=rep(1:imgDM[2], each=imgDM[1]),
  y=rep(imgDM[1]:1, imgDM[2]),
  R= as.vector(img[,,1]),
  G= as.vector(img[,,2]),
  B= as.vector(img[,,3])
)
head(imgRGB)

# ggplot theme to be used
plotTheme <- function(){
  theme(
    panel.background=element_rect(size=3,
                                  colour = "black",
                                  fill = "white"),
    axis.ticks=element_line(size = 2),
    panel.grid.major = element_line(colour = "gray80",
                                    linetype = "dotted"),
    panel.grid.minor = element_line(colour = "gray90",
                                    linetype = "dashed"),
    
    axis.title.x = element_text(size = rel(1.2),
                                face = "bold"),
    axis.title.y = element_text(size = rel(1.2),
                                face = "bold"),
    plot.title = element_text(size = 20, face = "bold",
                              vjust = 1.5)
  )
}

#plot the image
ggplot(data = imgRGB, aes(x=x, y=y))+
  geom_point(colour=rgb(imgRGB[c("R", "G", "B")]))+
  labs(title = "Original Image: Bird")+
  xlab("x")+
  ylab("y")+
  plotTheme()

bird.hex <- as.h2o(imgRGB)
head(bird.hex)

kmeans.2 <- h2o.kmeans(training_frame = bird.hex, k = 2, x = c('R','G','B'))
kmeans.2@model$centers
kmeans2cluster <- h2o.predict(kmeans.2, bird.hex[,c('R','G','B')])
head(as.data.frame(kmeans.2@model$centers[as.vector(kmeans2cluster)+1,]))
rgbcluster2 <- as.data.frame(kmeans.2@model$centers[as.vector(kmeans2cluster)+1,])[,c('r','g','b')]
kcolors.2 <- rgb(rgbcluster2)
ggplot(data = imgRGB, aes(x=x, y=y))+
  geom_point(colour=kcolors.2)+
  labs(title = paste(2, "Colours"))+
  xlab("x")+
  ylab("y")+
  plotTheme()

kmeans.3 <- h2o.kmeans(training_frame = bird.hex, k = 3, x = c('R','G','B'))
kmeans.3@model$centers
kmeans3cluster <- h2o.predict(kmeans.3, bird.hex[,c('R','G','B')])
head(as.data.frame(kmeans.3@model$centers[as.vector(kmeans3cluster)+1,]))
rgbcluster3 <- as.data.frame(kmeans.3@model$centers[as.vector(kmeans3cluster)+1,])[,c('r','g','b')]
kcolors.3 <- rgb(rgbcluster3)
ggplot(data = imgRGB, aes(x=x, y=y))+
  geom_point(colour=kcolors.3)+
  labs(title = paste(3, "Colours"))+
  xlab("x")+
  ylab("y")+
  plotTheme()

kmeans.4 <- h2o.kmeans(training_frame = bird.hex, k = 4, x = c('R','G','B'))
kmeans.4@model$centers
kmeans4cluster <- h2o.predict(kmeans.4, bird.hex[,c('R','G','B')])
head(as.data.frame(kmeans.4@model$centers[as.vector(kmeans4cluster)+1,]))
rgbcluster4 <- as.data.frame(kmeans.4@model$centers[as.vector(kmeans4cluster)+1,])[,c('r','g','b')]
kcolors.4 <- rgb(rgbcluster4)
ggplot(data = imgRGB, aes(x=x, y=y))+
  geom_point(colour=kcolors.4)+
  labs(title = paste(4, "Colours"))+
  xlab("x")+
  ylab("y")+
  plotTheme()

kmeans.5 <- h2o.kmeans(training_frame = bird.hex, k = 5, x = c('R','G','B'))
kmeans.5@model$centers
kmeans5cluster <- h2o.predict(kmeans.5, bird.hex[,c('R','G','B')])
head(as.data.frame(kmeans.5@model$centers[as.vector(kmeans5cluster)+1,]))
rgbcluster5 <- as.data.frame(kmeans.5@model$centers[as.vector(kmeans5cluster)+1,])[,c('r','g','b')]
kcolors.5 <- rgb(rgbcluster5)
ggplot(data = imgRGB, aes(x=x, y=y))+
  geom_point(colour=kcolors.5)+
  labs(title = paste(5, "Colours"))+
  xlab("x")+
  ylab("y")+
  plotTheme()

# sum of squared errors
sse <- function(predicted, actuals) {
  sum ( (predicted-actuals)^2, na.rm=TRUE ) 
}

elbow <- c(sse(rgbcluster2, imgRGB[,c('R','G','B')]),
           sse(rgbcluster3, imgRGB[,c('R','G','B')]),
           sse(rgbcluster4, imgRGB[,c('R','G','B')]),
           sse(rgbcluster5, imgRGB[,c('R','G','B')]))
plot(elbow, type = 'b')
k <- 1:15
elbow <- vector()
for (i in k){
kmeans <- h2o.kmeans(training_frame = bird.hex, k = i, x = c('R','G','B'))
kmeanscluster <- h2o.predict(kmeans, bird.hex[,c('R','G','B')])
rgbcluster <- as.data.frame(kmeans@model$centers[as.vector(kmeanscluster)+1,])[,c('r','g','b')]
elbow <- append(elbow, sse(rgbcluster, imgRGB[,c('R','G','B')]))
}
plot(elbow, type='b', xlab = 'Number of Clusters', ylab = 'Sum of Squared Errors')
# 5 seems to be optimal number of clusters