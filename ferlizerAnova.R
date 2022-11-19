# Fertilizer
# A field was divided into 12 plots. Each of three different fertilizers were applied to four plots.
# The yield was measured in [dt/ha].


# The fisrt step is to eneter the data
amount <- c(18, 14, 16,  19, 12, 14, 
            16, 13, 15,  17, 15, 15)

# here I did labels for fertilizer
fertlizerType <- gl(n=3, k=1, length = 4*3, labels = c("Mineral", "Straw", "CoverCrop"))


# I created a data frame for my data
production <- data.frame(fertlizer = fertlizerType, # combine to a data frame
                         yield = amount)
production

amount2 <- c(18, 19, 16, 17, # Mineral
             14, 12, 13, 15, #Straw
             16, 14, 15, 15) # CoverCrop

fertlizerType2<- factor(c(rep ("Mineral", times=4), rep ("Straw", times=4), 
                          rep ("CoverCrop", times=4)))

production2 <- data.frame(fertlizer = fertlizerType2, # combine to a data frame
                          yield = amount2)
production2

str(production)

# box plot 
boxplot ( yield ~ fertlizer, # Box and Whisker plot
          range = 0, # Min, max, quartiles, median
          data=production2) # Name of the data set

par ( mar=c(4,4,1,1) ) # No. of lines around the plot for
# axis descriptions: bltr
boxplot (yield ~ fertlizer, # Model
         ylab="Yield [kg]", # Label for y axis
         xlab="Type of fertilizer", # Label for x axis
         col ="lightblue", # Color of boxes
         data=production2)


# the summary overwiev by factor levels. However, we need to install dplyr
# package from R.

install.packages("dplyr")
library("dplyr")

production %>% group_by(fertlizer) %>%
  summarise ( n=n(), mean=mean(yield), var=var(yield),
              min=min(yield), max=max(yield))

# the production data become an independent data for this reason we have to use
# attach command to give an infromaation for R.

attach(production2)
# stripchart command needs to enter the initial name of factor levels. 
# otherways it will not work
stripchart(yield ~ fertlizer,
           method="jitter", # make overlapping points visible
           vertical = T) # rotate the plot

# to estimate of the variances we use barlett test.

bartlett.test(yield ~ fertlizer)

# Estimate the effects of the factor levels

summary ( aov(yield ~ fertlizer) )

# estimation of effects shows us that how the mean of variances are bit 
# vary from sample mean.

model.tables(aov(yield ~ fertlizer))

# the next is to check on outliers from our data. if the data is smaller than 1
# we can conclude that there is no outlier.
plot(cooks.distance(aov(yield ~ fertlizer)))

par(mfrow= c(2,2))
plot(aov(yield ~ fertlizer))

mean(yield)
