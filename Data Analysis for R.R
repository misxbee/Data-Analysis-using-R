#Portfolio Component 3: Solving Data Science Problem	
#By Misba Gajra
#For-University of Bolton

library(ggplot2)
library(tidyverse)
library(scales)
library(dplyr)
library(tidyr)



#Part 1- Exploring the Dataset	

#reading the dataset and storing it in the variable
movieratings <- read.csv("movie_ratings.csv")

#Viewing all the columns of the dataset
colnames(movieratings)

#Changing the columns names with the help of the vector
colnames(movieratings) <- c("Film", "Genre", "Critic.Rating", "Audience.Rating", 
                            "Budget", "Year")
#Retrieve the column names
names(movieratings)

#The first 6 rows of the dataset
head(movieratings)

#The first 10 rows 
head(movieratings, n=10)
movieratings[1:10,]

#First 10 rows of the first 3 columns
movieratings[1:10, 1:3]

# rows of the first 3 columns
movieratings[5:10, 1:3]

#The last 6 rows
tail(movieratings)

#Opens the data editor
edit(movieratings)

#Number of Rows
nrow(movieratings)

#Number of Columns
ncol(movieratings)

#Structure of the dataset
str(movieratings)

#Summary of the dataset 
summary(movieratings)



#2. How Genre impacts the Budget of the Movie ?
#data-aesthetic layer
b <- ggplot(data=movieratings, aes(x=Genre, y=Budget, colour= Genre))
#geometry layer
g <- b + geom_boxplot(size=1.2) + geom_jitter(alpha=0.5, width = 0.2) 
#formatting layer
g <- g +  xlab("Genre") + 
  ylab("Budget (in millions$)") + 
  ggtitle("Distribution of Movie Budget by Genre") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(colour="darkblue", size=12, face= "bold"),
    axis.title.y = element_text(colour="darkblue", size=12, face= "bold"),
    axis.text.x = element_text(colour="darkgreen", size=10),
    axis.text.y = element_text(colour="darkgreen", size=10),
    plot.title = element_text(colour= "Black", size=15, hjust=0.5, face= "bold"),
    panel.background = element_rect(fill = "white", colour = "white"),
    legend.position = c(1,1),
    legend.justification = c(1,1),
    legend.box.background = element_rect(color = "black"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")) 



#3. Is there any relation between the critic rating and the budget?

#Relation between critics and budget
#data-aes layer
c <- ggplot(data=movieratings, aes(x= Budget, y=Critic.Rating,
                                   colour= Genre,
                                   size= Budget))
#geometry and formatting
c + 
  geom_point(alpha=0.7) + 
  xlab("Budget (in millions)") + 
  ylab("Critic Rating") + 
  ggtitle("Critic Ratings VS Budget") +
  theme(
    axis.title.x = element_text(colour="Black", size=12, face= "bold"),
    axis.title.y = element_text(colour="Black", size=12, face= "bold"),
    axis.text.x = element_text(colour="Red", size=10),
    axis.text.y = element_text(colour="Red", size=10),
    plot.title = element_text(colour= "Black", size=15, hjust=0.5, face= "bold"))

#Relation between critics and budget with linear regression
c + 
  geom_point(alpha=0.7) + 
  geom_smooth(method=lm, se=FALSE, colour="Red") +
  xlab("Budget (in millions)") + 
  ylab("Critic Rating") + 
  ggtitle("Critic Ratings VS Budget") +
  theme(
    axis.title.x = element_text(colour="Black", size=12, face= "bold"),
    axis.title.y = element_text(colour="Black", size=12, face= "bold"),
    axis.text.x = element_text(colour="Red", size=10),
    axis.text.y = element_text(colour="Red", size=10),
    plot.title = element_text(colour= "Black", size=15, hjust=0.5, face= "bold"))



#4. Is there relationship between Audience Rating and Budget?
#Audience Rating VS Budget
q <- ggplot(data=movieratings, aes(x= Budget, y=Audience.Rating,
                                   colour= Genre,
                                   size= Budget))
q + 
  geom_point(alpha=0.7) + 
  xlab("Budget (in millions)") + 
  ylab("Audience Rating") + 
  ggtitle("Audience Ratings VS Budget") +
  theme(
    axis.title.x = element_text(colour="Black", size=12, face= "bold"),
    axis.title.y = element_text(colour="Black", size=12, face= "bold"),
    axis.text.x = element_text(colour="Red", size=10),
    axis.text.y = element_text(colour="Red", size=10),
    plot.title = element_text(colour= "Black", size=15, hjust=0.5, face= "bold"))

#Audience rating vs budget with linear regression
q + 
  geom_point(alpha=0.7) + 
  geom_smooth(method=lm, se=FALSE, colour="Red") +
  xlab("Budget (in millions)") + 
  ylab("Audience Rating") + 
  ggtitle("Audience Ratings VS Budget") +
  theme(
    axis.title.x = element_text(colour="Black", size=12, face= "bold"),
    axis.title.y = element_text(colour="Black", size=12, face= "bold"),
    axis.text.x = element_text(colour="Red", size=10),
    axis.text.y = element_text(colour="Red", size=10),
    plot.title = element_text(colour= "Black", size=15, hjust=0.5, face= "bold"))




#5. Show corelation between critic ratings vs audience ratings by genre through out the years
#multilayered plot with individual scatter plot
p <- ggplot(data = movieratings, aes(x = Year, colour = Genre)) + 
  geom_point(aes(y = Audience.Rating), size = 2, shape = 1, alpha = 0.7) + 
  geom_point(aes(y = Critic.Rating), size = 2, shape = 5, alpha = 0.7) +
  geom_line(aes(y = Audience.Rating, linetype = "Audience"), size = 1) +
  geom_line(aes(y = Critic.Rating, linetype = "Critic"), size = 1) +
  facet_grid(Genre ~ ., scales = "free_y") +  # Separate box for each year #y axis scaled independent
  xlab("Year") + 
  ylab("Audience Ratings VS Critic Ratings") + 
  ggtitle("Co-relation between Audience and Critic Ratings throughout years by Genre") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(colour = "DarkGreen", size = 9, face = "bold"),
    axis.title.y = element_text(colour = "DarkGreen", size = 9, face = "bold"),
    axis.text.x = element_text(colour = "black", size = 7),
    axis.text.y = element_text(colour = "black", size = 7),
    plot.title = element_text(colour = "Black", size = 10, hjust = 0.5, face = "bold"),
    legend.position = "bottom", 
    legend.box = "vertical",
    legend.box.background = element_rect(color = "black"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  ) +
  scale_linetype_manual(
    values = c("solid", "dotted"),
    labels = c("Audience", "Critic"),
    name = "Line Type",
    guide = "legend"
  )


#6. Create a graph to show the number of films from the dataset categorised by Genre.
#data-aes layer
s <- ggplot(movieratings, aes(x=Genre, fill= Genre))
#geometry & formatting layer
t <- s +
  geom_bar(color = "Black", position = "stack") +
  geom_text(
    stat = "count", #targ- ext label will represen the count
    #setting label text and y position manually, count statistics
    aes(label = after_stat(count), y = after_stat(count)),
    vjust = -0.5,
    color = "Blue",
    size = 3.5
  ) +
  labs(
    x = "Genre",
    y = "Number of Films",
    title = "Number of Movies categorized by Genre"
  ) +
  theme_light() +
  theme(
    axis.title = element_text(colour = "Black", size = 12, face = "bold"),
    axis.text = element_text(colour = "DarkGreen", size = 10),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.box.background = element_rect(color = "Black"),
    axis.ticks = element_line(color = "DarkGreen")
  )

#Part-2 Advanced Analytics	47
#Importing the Movie Extended Dataset
moviextended <- read_csv("movie_extended.csv")

#Converting the Column Names for simplicity through vectors
colnames(moviextended) <- c("Day.of.Week", "Director", "Genre", 
"Movie.Title", "Release.Date", "Studio", 
"Adjusted.Gross", "Budget", "Gross",
"IMDb.Rating", "MovieLens.Rating", "Overseas",
"Overseas%", "Profit", "Profit..", 
"Runtime", "US", "Gross..US")

#Q-1 They give you the following graph image as the R code is not found and they would like to remove the spelling mistake in the Graph Title and rename the Graph Title “Gross Percentage By Genre”. You need to recreate the graph by writing R code. You must use the Grammar of Graphics to recreate the following graph. You must also explain your code and display the output at each step

#Filtering the Genre
filter1 <- 
  moviextended$Genre == "action" | 
  moviextended$Genre == "adventure" | 
  moviextended$Genre == "animation" | 
  moviextended$Genre == "comedy" |
  moviextended$Genre == "drama"

#Allowing specific studios
filter2 <- moviextended$Studio %in% 
  c("Buena Vista Studios", "WB", "Fox", "Universal", "Paramount Pictures", "Sony")

#Combining filter1 and filter2
newmoviextended <- moviextended[filter1 & filter2,] 

#printing the filter
print(newmoviextended)

#adding data-layer
p <- ggplot(data=newmoviextended, aes(x=Genre, y=Gross..US))

#adding geometries
g <- p + geom_jitter(aes(size=Budget, colour=Studio)) +
  geom_boxplot(alpha=0.7, outlier.color = NA)

#adding formatting
g <- g + xlab("Genre") +
  ylab("Gross % US") +
  ggtitle("Domestic Gross % by Genre") +
  theme(
    axis.title.x = element_text(colour="Blue", size=15),
    axis.title.y = element_text(colour="Blue", size=15),
    axis.text.x = element_text(colour="Black", size=10),
    axis.text.y = element_text(colour="Black", size=10),
    plot.title = element_text(colour="Black", size=20, hjust = 0.5, face = "bold"),
    legend.title=element_text(size=10),
    legend.text = element_text(size=10),
    text = element_text(family="Comic Sans MS"))

#adding label size
g$labels$size <- "Budget $M"


#2.Write R code to find the trend of the Day of the week that most/least movies were released.

#Graph 1 with Bar Plot
#Counting the number of movies released in each day of the week
day_of_week_counts <- table(moviextended$Day.of.Week)
print(day_of_week_counts)

# Find the day with the most and least movie releases
most_released_day <- names(which.max(day_of_week_counts))
print(most_released_day)

least_released_day <- names(which.min(day_of_week_counts))
print(least_released_day)

#data/aesthetic layer
p <- ggplot(data = moviextended, aes(x = as.factor(Day.of.Week),fill = Day.of.Week ))

#geometry layer
k <- p +
  geom_bar( color = "black")

#formatting layer
k <- k +
  labs(title = "Movie Releases by Day of the Week",
       subtitle = paste("Most Released Day:", most_released_day, " | Least Released Day:", least_released_day),
       x = "Day of the Week",
       y = "Number of Movies Released") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(colour="Blue", size=12),
    axis.title.y = element_text(colour="Blue", size=12),
    axis.text.x = element_text(colour="DarkGreen", size=10),
    axis.text.y = element_text(colour="DarkGreen", size=10),
    plot.title = element_text(colour="Black", size=15, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(colour="Blue", size=12, hjust = 0.5),
    legend.title=element_text(size=10),
    legend.text = element_text(size=10),
    legend.justification = c(1, 1),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.box.background = element_rect(color = "Black"))



#Graph-2 with Geom Line
#Counting the number of movies released in each day of the week
day_of_week_counts <- table(moviextended$Day.of.Week)
print(day_of_week_counts)

#creating a dataframe
# count the occurrences of each unique value in the "Day.of.Week" 
#convert the values in day_of_week_counts to a numeric format.
movie_df <- data.frame(Day.of.Week = names(day_of_week_counts), 
                       Count = as.numeric(day_of_week_counts))

#removing any values with NA column
#movie_df <- na.omit(movie_df) 

#Converting the 'Day of Week' column to a factor for proper ordering
movie_df$Day.of.Week <- factor(movie_df$Day.of.Week, 
                               levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#data-layer
#This is used to ensure that all points are connected when lines are drawn. 
#It creates a single group, so the line connecting the points is not split by different groups.
r <- ggplot(data = movie_df, aes(x = as.factor(Day.of.Week), y= Count, group = 1))

#adding geometry
m <- r +
  geom_line(color = "Red", size=1) +
  geom_point(color = "Blue", size=5, alpha=0.7) 

#adding formatting
m <- m +
  labs(title = "Movie Releases by Day of the Week",
       subtitle = paste("Most Released Day:", most_released_day, " | Least Released Day:", least_released_day),
       x = "Day of the Week",
       y = "Number of Movies Released") +
  theme_light() +
  theme(
    axis.title.x = element_text(colour="Blue", size=12),
    axis.title.y = element_text(colour="Blue", size=12),
    axis.text.x = element_text(colour="Black", size=10),
    axis.text.y = element_text(colour="Black", size=10),
    plot.title = element_text(colour="Black", size=15, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(colour="Blue", size=12, hjust = 0.5),
    legend.title=element_text(size=10),
    legend.text = element_text(size=10))



#3. Identify  Identify if the profit of a movie depends on any of the features in this data set i.e. genre, director, profit etc

#Graph-1 Profit VS Directors
#Threshold value 5 means including only those directors who directed more than 5 movies
threshold <- 5 

# Filter directors with movies above the threshold
new_d <- moviextended %>%
  group_by(Director) %>%
  filter(n() >= threshold) %>%
  # reorder directors based on the average profitability of their movies.
  #The reorder function takes three arguments: the variable to be reordered ("Director"), the variable whose means are used for ordering ("Profit"), and the function to calculate the means (FUN = mean).
  #mutate=adds a new column
  mutate(Director_reorder = reorder(Director, Profit, FUN = mean))

# Data-layer
pd <- ggplot(new_d, aes(x = Director_reorder, y = Profit))

# Geometry layer
pd <- pd +
  stat_summary(fun = mean, geom = "bar", colour = "Black", aes(fill= Director))

# Formatting layer
pd <- pd +
  labs(
    title = "Profit of the Movie based upon Directors",
    x = "Directors",
    y = "Profit of the Movie"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(colour = "Black", size = 12, face = "bold"),
    axis.title.y = element_text(colour = "Black", size = 12, face = "bold"),
    axis.text.x = element_text(colour = "DarkGreen", size = 10, hjust = 1),
    axis.text.y = element_text(colour = "DarkGreen", size = 10),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.box.background = element_rect(color = "Black"),
    legend.position = "bottom",
    legend.justification = "center") +
  coord_flip()


#Graph-2 Profit VS IMBD
#data-aesthetic layer
a <- ggplot(data = moviextended, aes(x = IMDb.Rating, y = Profit, group = 1))
#geometry
i <- a +
  geom_point(alpha=0.3, aes(group=1, colour=Genre, size=Budget)) +
  geom_smooth(method = "lm", se = FALSE, aes(group=1)) 
#adding formatting
i <- i +  
  labs(
  title = "Profit vs IMDb Rating",
  x = "IMDb Rating",
  y = "Profit") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(colour = "Black", size = 12),
    axis.title.y = element_text(colour = "Black", size = 12),
    axis.text.x = element_text(colour = "Blue", size = 10),
    axis.text.y = element_text(colour = "Blue", size = 10),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.justification = c(1, 1),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.box.background = element_rect(color = "Black")
  )


#3. Profit VS Overseas
#If profit depends upon Overseas
a <- ggplot(data = moviextended, aes(x = Overseas, y = Profit, group = 1))
#geometry
o <- a +
  geom_point(alpha=0.3, aes(group=1, colour=Genre, size=Budget)) +
  geom_smooth(method = "lm", se = FALSE, aes(group=1)) 
#adding formatting
o <- o +  
  labs(
    title = "Profit vs Overseas",
    x = "Overseas",
    y = "Profit") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(colour = "Black", size = 12),
    axis.title.y = element_text(colour = "Black", size = 12),
    axis.text.x = element_text(colour = "Blue", size = 10),
    axis.text.y = element_text(colour = "Blue", size = 10),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.justification = c(1, 1),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.box.background = element_rect(color = "Black")
  )


#Graph4- Profit VS MovieLens Rating
a <- ggplot(data = moviextended, aes(x = MovieLens.Rating , y = Profit, group = 1))
#geometry
mr <- a +
  geom_point(alpha=0.3, aes(group=1, colour=Genre, size=Budget)) +
  geom_smooth(method = "lm", se = FALSE, aes(group=1)) 
#adding formatting
mr <- mr +  
  labs(
    title = "Profit vs Movie Lens Rating",
    x = "Movie Lens Rating",
    y = "Profit") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(colour = "Black", size = 12),
    axis.title.y = element_text(colour = "Black", size = 12),
    axis.text.x = element_text(colour = "Blue", size = 10),
    axis.text.y = element_text(colour = "Blue", size = 10),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.justification = c(1, 1),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.box.background = element_rect(color = "Black")
  )



#Graph 5- Profit VS Budget
a <- ggplot(data = moviextended, aes(x = Budget , y = Profit, group = 1))
#geometry
bb <- a +
  geom_point(alpha=0.3, aes(group=1, size=Budget)) +
  geom_smooth(method = "lm", se = FALSE, aes(group=1)) 
#adding formatting
bb <- bb +  
  labs(
    title = "Profit depending upon Budget",
    x = "Budget",
    y = "Profit") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(colour = "Black", size = 12),
    axis.title.y = element_text(colour = "Black", size = 12),
    axis.text.x = element_text(colour = "Blue", size = 10),
    axis.text.y = element_text(colour = "Blue", size = 10),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.justification = c(1, 1),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.box.background = element_rect(color = "Black")
  )


#Graph 6- Profit VS Profit%
a <- ggplot(data = moviextended, aes(x = Profit.. , y = Profit, group = 1))
#geometry
pp <- a +
  geom_point(alpha=0.3, aes(group=1, size=Budget, colour=Genre)) +
  geom_smooth(method = "lm", se = FALSE, aes(group=1)) 
#adding formatting
pp <- pp +  
  labs(
    title = "Profit depending upon Profit%",
    x = "Profit%",
    y = "Profit") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(colour = "Black", size = 12),
    axis.title.y = element_text(colour = "Black", size = 12),
    axis.text.x = element_text(colour = "Blue", size = 10),
    axis.text.y = element_text(colour = "Blue", size = 10),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.justification = c(1, 1),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.box.background = element_rect(color = "Black")
  )




#Graph 7- Profit VS Studio
# Filter out studios with profit below 500
new_s <- moviextended %>%
  filter(Profit >= 500)

#data-aes
a <- ggplot(data = new_s , aes(x = Studio, y = Profit, fill = Studio))

#geometry layer
st <- a +
  geom_bar(stat = "identity", color = "black", width=0.4) 

#formatting
st <- st +
  labs(
    title = "Total Profits by Studio",
    x = "Studio",
    y = "Total Profit"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(colour = "Black", size = 12, face="bold"),
    axis.title.y = element_text(colour = "Black", size = 12, face="bold"),
    axis.text.x = element_text(colour = "DarkGreen", size = 10, angle= 0, hjust=1),
    axis.text.y = element_text(colour = "DarkGreen", size = 10, angle= 0, hjust=1),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.title = element_text(size = 8),
    legend.position = "bottom", 
    legend.box = "vertical",
    legend.box.background = element_rect(color = "Black")) +
    coord_flip()

#count function to cross check with the graph
studio_counts <- new_s %>%
  count(Studio, wt = Profit, sort = TRUE)
print(studio_counts)

#Graph 8- If Profit depends upon the Genre
new_g <- moviextended %>%
  filter(Profit >= 500)

#data-aes
gg <- ggplot(data = new_g , aes(x = Genre, y = Profit, fill = Genre))

#geometry layer
gg <- gg +
  geom_bar(stat = "identity", color = "black", width=0.4) 

#formatting
gg <- gg +
  labs(
    title = "Total Profits by Genre",
    x = "Genre",
    y = "Total Profit"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(colour = "Black", size = 12, face="bold"),
    axis.title.y = element_text(colour = "Black", size = 12, face="bold"),
    axis.text.x = element_text(colour = "DarkGreen", size = 10, angle= 0, hjust=1),
    axis.text.y = element_text(colour = "DarkGreen", size = 10, angle= 0, hjust=1),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.title = element_text(size = 8),
    legend.position = "bottom", 
    legend.box = "vertical",
    legend.box.background = element_rect(color = "Black")) +
    coord_flip()

#Count function to count Genre
genre_counts <- filtered_data %>%
  count(Genre, wt = Profit, sort = TRUE)


#4. Use ggplot and boxplot to identify if there is an anomaly / any anomalies in the data?


# Select only the numerical columns for the boxplots
new_me <- moviextended %>%
  select(Adjusted.Gross, Budget, Gross, IMDb.Rating, MovieLens.Rating,
         Overseas, `Overseas%`, Profit, `Profit..`, Runtime, US, `Gross..US`) %>%
  gather(key = "Variable", value = "Value")


# Creating the boxplot grid using facet_wrap
p <- ggplot(data = new_me, aes(x = Variable, y = Value)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 1, fill="yellow", size=1) +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  theme(axis.text.y = element_text(colour = "Blue", size = 10 , angle= 90, hjust=0.5),
        axis.text.x = element_text(colour = "DarkGreen", size = 10),
        axis.title.x = element_text(colour = "Red", size = 12, face="bold"),
        axis.title.y = element_text(colour = "Red", size = 12, face="bold"),
        strip.text.x = element_text(size = 10 , face="bold", colour="Blue"),
        strip.background = element_rect(color = "black"),
        plot.title = element_text(colour = "RED", size = 15, hjust = 0.5, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black")
        ) + 
  labs(title = "Boxplot to identify Anomalies") + coord_flip()


#Boxplot for Budget
ggplot(data = moviextended, aes(x="", y = Budget)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 1, outlier.size= 5, size=1) +
  labs(title = "Boxplot for Budget Anomalies", 
       y = "Budget ($millions)", 
       x = "") +
  theme(
  axis.title.x = element_text(colour = "Blue", size = 12, face="bold"),
  axis.text.x = element_text(colour = "DarkGreen", size = 10, angle= 0, hjust=1),
  plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
  axis.line = element_line(color = "black"),
  axis.ticks = element_line(color = "black"),
  legend.title = element_text(size = 8)) +
  coord_flip()

#Boxplot for Adjusted Gross
ggplot(data = moviextended, aes(x="", y = Adjusted.Gross)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 1, outlier.size= 5, size=1) +
  labs(title = "Boxplot for  Adjusted Gross", 
       y = "Adjusted Gross", 
       x = "") +
  theme(
    axis.title.x = element_text(colour = "Blue", size = 12, face="bold"),
    axis.title.y = element_text(colour = "Blue", size = 12, face="bold"),
    axis.text.x = element_text(colour = "DarkGreen", size = 10, angle= 0, hjust=1),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.title = element_text(size = 8))  + coord_flip()

#Boxplot for Gross
ggplot(data = moviextended, aes(x="", y = Gross)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 1, outlier.size= 5, size=1) +
  labs(title = "Boxplot for Gross", 
       y = "Gross", 
       x = "Value") +
  theme(
    axis.title.x = element_text(colour = "Blue", size = 12, face="bold"),
    axis.title.y = element_text(colour = "Blue", size = 12, face="bold"),
    axis.text.x = element_text(colour = "DarkGreen", size = 10, angle= 0, hjust=1),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.title = element_text(size = 8)) + coord_flip()

#Boxplot for Gross US
ggplot(data = moviextended, aes(x="", y = Gross..US)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 1, outlier.size= 5, size=1) +
  labs(title = "Boxplot for Gross US", 
       y = " Gross US", 
       x = "Value") +
  theme(
    axis.title.x = element_text(colour = "Blue", size = 12, face="bold"),
    axis.title.y = element_text(colour = "Blue", size = 12, face="bold"),
    axis.text.x = element_text(colour = "DarkGreen", size = 10, angle= 0, hjust=1),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.title = element_text(size = 8)) 

#Boxplot for Overseas
ggplot(data = moviextended, aes(x="", y = Overseas)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 1, outlier.size= 5, size=1) +
  labs(title = "Boxplot for Overseas", 
       y = "Overseas", 
       x = "Value") +
  theme(
    axis.title.x = element_text(colour = "Blue", size = 12, face="bold"),
    axis.title.y = element_text(colour = "Blue", size = 12, face="bold"),
    axis.text.x = element_text(colour = "DarkGreen", size = 10, angle= 0, hjust=1),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.title = element_text(size = 8)) + coord_flip()

#Boxplot for Profit
ggplot(data = moviextended, aes(x="", y = Profit)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 1, outlier.size= 5, size=1) +
  labs(title = "Boxplot for Profit", 
       y = "Profit", 
       x = "") +
  theme(
    axis.title.x = element_text(colour = "Blue", size = 12, face="bold"),
    axis.title.y = element_text(colour = "Blue", size = 12, face="bold"),
    axis.text.x = element_text(colour = "DarkGreen", size = 10, angle= 0, hjust=1),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.title = element_text(size = 8)) + coord_flip()

#Boxplot for Runtime
ggplot(data = moviextended, aes(x="", y = Runtime)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 1, outlier.size= 5, size=1) +
  labs(title = "Boxplot for Runtime", 
       y = "US", 
       x = "") +
  theme(
    axis.title.x = element_text(colour = "Blue", size = 12, face="bold"),
    axis.title.y = element_text(colour = "Blue", size = 12, face="bold"),
    axis.text.x = element_text(colour = "DarkGreen", size = 10, angle= 0, hjust=1),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.title = element_text(size = 8)) + coord_flip()



#Boxplot for US
ggplot(data = moviextended, aes(x="", y = US)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 1, outlier.size= 5, size=1) +
  labs(title = "Boxplot for US", 
       y = "US", 
       x = "") +
  theme(
    axis.title.x = element_text(colour = "Blue", size = 12, face="bold"),
    axis.title.y = element_text(colour = "Blue", size = 12, face="bold"),
    axis.text.x = element_text(colour = "DarkGreen", size = 10, angle= 0, hjust=1),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.title = element_text(size = 8)) + coord_flip()



#6. Bar graph for average IMDb ratings by genre
genre_ratings <- moviextended %>%
  group_by(Genre) %>%
  summarise(Avg_IMDb_Rating = mean(IMDb.Rating, na.rm = TRUE))

#Data-Aesthetics
p <- bar_plot <- ggplot(genre_ratings, aes(x = Genre, y = Avg_IMDb_Rating, fill = Genre)) 

#Geometry Layer
pg <- p +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black") 

#Formatting
pg <- pg + 
  labs(title = "Average IMDb Ratings by Genre",
       x = "Genre",
       y = "Average IMDb Rating") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(colour = "Black", size = 12),
    axis.title.y = element_text(colour = "Black", size = 12),
    axis.text.x = element_text(colour = "Darkgreen", size = 10, angle= 45, hjust=1),
    axis.text.y = element_text(colour = "DarkGreen", size = 10),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.box.background = element_rect(color = "Black"),
    legend.position = "bottom",
    legend.justification = "center"
  )


# Scatter plot for Budget vs. Gross earnings
sp <- ggplot(moviextended, aes(x = Budget, y = Gross)) +
  geom_point(aes(color = Genre), alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, aes(colour=Genre)) +
  labs(title = "Budget vs. Gross Earnings",
       x = "Budget",
       y = "Gross Earnings") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(colour = "Black", size = 12),
    axis.title.y = element_text(colour = "Black", size = 12),
    axis.text.x = element_text(colour = "DarkGreen", size = 8),
    axis.text.y = element_text(colour = "DarkGreen", size = 8),
    plot.title = element_text(colour = "Black", size = 15, hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.justification = c(1, 1),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.box.background = element_rect(color = "Black"),
    strip.text.x = element_text(size = 9 , face="bold", colour="Brown"),
    strip.background = element_rect(color = "black")
  ) +  facet_wrap(~toupper(Genre)) 



