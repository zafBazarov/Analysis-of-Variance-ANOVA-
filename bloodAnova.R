
# I'll enter the information by hand.

amount <- c(63, 67, 71, 64, 65, 66, # A
            62, 60, 63, 59,         # B
            56, 62, 60, 61, 63, 64, 63, 59, # C
            68, 66, 71, 67, 68, 68) # D

# this command "factor ()" is very useful to design for unbalancing a data set.

diet <- factor ( c( rep("A", times=6), rep("B", times=4), 
                    rep("C", times=8), rep("D", times=6)))

# I'll make a data frame for the amount and type 1.
# and we need to mark x (independent variable) and y (dependent variable) in our frame. 

labResult <- data.frame (additives = diet,
                         coagulation = amount)

# To visualize the data, I will use a box plot.
attach(labResult)

boxplot ( coagulation ~ additives, # Box and Whisker plot
          range = 0, # Min, max, quartiles, median
) # Name of the data set

# To change the parameters of bxoplot, we can use this command.

par ( mar=c(4,4,1,1) ) # No. of lines around the plot for
# axis descriptions: bltr

boxplot (coagulation ~ additives, # Model
         ylab="Coagulation time [sec]", # Label for y axis
         xlab="Feeding additives", # Label for x axis
         col ="lightblue", # Color of boxes
         range=0, # Whisker to min and max
         ylim=c(50,75), # Range of y axis
)
# we can also use a stripchart command because a boxplot is not feasible to 
# estimate five values from four observations, a stripchart is a more appropriate visualization here

install.packages("dplyr")

stripchart( coagulation ~ additives  ,
            method="jitter", # make overlapping points visible
            vertical = T) # rotate the plot

# to manage  parameters of our stripchart we use this codes.
stripchart(coagulation ~ additives,
           data=labResult,
           main="The stripchart for each type of feeding additives of animals",
           xlab="Feeding additives",
           ylab="Coagulation time",
           col="brown3",
           group.names=c("A","B","C","D"),
           vertical=TRUE,
           pch=24 )

## estimate means, variance, max and min
labResult %>% group_by(additives) %>%
  summarise ( n=n(), mean=mean(coagulation), var=var(coagulation),
              min=min(coagulation), max=max(coagulation))

# estimate means and variance
labResult %>% group_by(additives) %>%
  summarise ( n=n(), mean=mean(coagulation), var=var(coagulation) )

# The next step is to use a Bartlett test to ensure 
# that the variances of the factor levels are equal.

bartlett.test( coagulation ~ additives)

# To test our H0 and H1 hypotheses, we use this command.
summary (aov (coagulation ~ additives))

# The next command shows us how to estimate the effects of the factor levels.
model.tables( aov (coagulation ~ additives))

# This command will check for outliers.

plot (cooks.distance( aov (coagulation ~ additives)))


#This command shows diagnostic plots 
par (mfrow=c(2,2))
plot (aov (coagulation ~ additives))

mean(coagulation)

# the predicted coagulation time for feeding additive B (i D 2) is that.
64-3
