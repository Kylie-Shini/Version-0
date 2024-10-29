# Version-0
Version 0
# Install and load required packages 
#this allows for an more function and systems to be used in R
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(gganimate)) install.packages("gganimate")
if (!require(gifski)) install.packages("gifski")
# Load the required packages
library(tidyverse)
library(gganimate)
library(gifski)
# Define the Generalized Context Model function
#this would would categorize the data to a pre-existing group based on similarity
gcm <- function(stimulus, exemplars, weights, c = 1) {
similarities <- exp(-c * colSums((t(exemplars) - stimulus)^2))
numerator <- sum(similarities * weights[, 1])
denominator <- sum(similarities * rowSums(weights))
prob_A <- numerator / denominator
return(prob_A)
}
# Generate exemplars
#this would generate to the validation functions, using what is currently being considered the ideal output
set.seed(123)
n_exemplars <- 20
n_dimensions <- 2
exemplars <- matrix(runif(n_exemplars * n_dimensions), ncol = n_dimensions)
weights <- matrix(runif(n_exemplars * 2), ncol = 2)
weights <- weights / rowSums(weights) # Normalize weights
# Generate grid of stimuli
grid_size <- 50
x <- seq(0, 1, length.out = grid_size)
y <- seq(0, 1, length.out = grid_size)
grid <- expand.grid(x = x, y = y)
# Calculate GCM probabilities for each point in the grid
#
grid$prob_A <- apply(grid, 1, function(point) gcm(point, exemplars, weights))
# Create a data frame for exemplars with a frame column
exemplar_df <- as.data.frame(exemplars) %>%
mutate(weight = weights[,1],
frame = row_number())
# Create the base plot
p <- ggplot() +
geom_raster(data = grid, aes(x = x, y = y, fill = prob_A)) +
scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5) +
geom_point(data = exemplar_df, aes(x = V1, y = V2, size = weight, group = seq_along(frame)), color
= "black", alpha = 0.7) +
theme_minimal() +
labs(title = "Generalized Context Model",
subtitle = "Exemplars shown: {closest_state}",
x = "Dimension 1",
y = "Dimension 2",
fill = "P(A|X)")
# Animate the plot
#animating the plot will make a visual over how that data changes with the time and frequency
anim <- p +
transition_states(frame, transition_length = 2, state_length = 1) +
enter_fade() +
exit_fade()
# Render and save the animation
#This turns an animation into a 2D image or video that can then be saved
animate(anim,
nframes = n_exemplars * 3, # Increased number of frames for smoother animation
fps = 5, # Adjusted fps for better viewing
width = 800,
height = 600,
renderer = gifski_renderer(),
start_pause = 10,
end_pause = 10)
anim_save("gcm_animated.gif")
cat("Animation completed. Check your working directory for gcm_animated.gif\n")

