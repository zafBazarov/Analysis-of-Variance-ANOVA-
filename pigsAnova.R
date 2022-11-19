#We have a data set that we must manually enter. There are three ways to design the data set. 
# I will provide all three that I know.

# 1 option 

amount<- c(60.8, 57.0, 65.0, 58.6, 61.7, 68.7, 67.7, 74.0, 66.3, 69.8,
           102.6, 102.1, 100.2, 96.5, 87.9, 84.2, 83.1, 85.7, 90.3)

# 2 option
amount2<- c(60.8, 57.0, 65.0, 58.6, 61.7, # 1type
           68.7, 67.7, 74.0, 66.3, 69.8,  # 2type
           102.6, 102.1, 100.2, 96.5,     #3 type
           87.9, 84.2, 83.1, 85.7, 90.3)  # 4type

# We use the gl function to design a data frame.

type <- gl(n=4, k=1, length = 4*5, labels = c("t1", "t2", "t3", "t4"))

# or, and this command "factor ()" is very useful for unbalancing a data set.
type2 <- factor ( c( rep("t1", times=5), rep("t2", times=5), 
                        rep("t3", times=4), rep("t4", times=5)))

# I'll make a data frame for the amount and type 1.
# and we need to mark x (independent variable) and y (dependent variable) in our frame. 

production <- data.frame (pig = type,
                          yield = amount)

# In our case, this command does not work because the data is unbalanced.
production2 <- data.frame (pig = type2,
                           yield = amount2)
 
# A third option is that it also does not work because of unbalanced data.
production3 <- data.frame ( t1 = c(60.8, 57.0, 65.0, 58.6, 61.7), # 1type
                            t2 = c(68.7, 67.7, 74.0, 66.3, 69.8),  # 2type
                            t3 = c(102.6, 102.1, 100.2, 96.5),     #3 type
                            t4 = c(87.9, 84.2, 83.1, 85.7, 90.3)  )

# I will use production2 data.
  
# To visualize the data, I will use a box plot.

boxplot ( yield ~ pig, # Box and Whisker plot
          range = 0, # Min, max, quartiles, median
          data=production2) # Name of the data set

# To change the parameters of bxoplot, we can use this command.

par ( mar=c(4,4,1,1) ) # No. of lines around the plot for
                      # axis descriptions: bltr

boxplot (yield ~ pig, # Model
         ylab="The weight of pigs [kg]", # Label for y axis
         xlab="Type of pig", # Label for x axis
         col ="lightblue", # Color of boxes
         range=0, # Whisker to min and max
         ylim=c(50,110), # Range of y axis
         data=production2)

# we can also use a stripchart command because a boxplot is not feasible to 
# estimate five values from four observations, a stripchart is a more appropriate visualization here

install.packages("dplyr")
attach(production2)

stripchart( yield ~ pig  ,
           method="jitter", # make overlapping points visible
           vertical = T) # rotate the plot

# to manage  parameters of our stripchart we use this codes.
stripchart(yield ~ pig,
           data=production2,
           main="The stripchart for each type of pigs weight of pigs",
           xlab="The weight of pigs",
           ylab="Type of pigs",
           col="brown3",
           group.names=c("T1","T2","T3","T4"),
           vertical=TRUE,
           pch=19 )

# estimate means, variance, max and min
production2 %>% group_by(pig) %>%
  summarise ( n=n(), mean=mean(yield), var=var(yield),
              min=min(yield), max=max(yield))

# estimate means and variance
production2 %>% group_by(pig) %>%
  summarise ( n=n(), mean=mean(yield), var=var(yield) )
getwd()
detach()

# The next step is to use a Bartlett test to ensure 
# that the variances of the factor levels are equal.

bartlett.test( yield ~ pig)

# To test our H0 and H1 hypotheses, we use this command.
summary (aov (yield ~ pig))

# The next command shows us how to estimate the effects of the factor levels.
model.tables( aov (yield ~ pig))

# This command will check for outliers.

plot (cooks.distance( aov (yield ~ pig)))


par (mfrow=c(2,2))

#This command shows diagnostic plots 

plots.plot (aov (yield ~ pig))

mean(yield)


