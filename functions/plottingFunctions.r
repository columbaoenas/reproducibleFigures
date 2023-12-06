## ---------------------------
##
## Script name: plottingFunctions.r
##
## Purpose of script: 
##      Plotting exploratory and results graphs used in the analysis of the penguin data, 
##      as well as the initial graph showing bad design features.
##
## Date Created: 2023-05-12
##
##
## ---------------------------
##
## Notes:
##  
##
##
## ---------------------------


# function to create the exploratory graph from a cleaned penguins dataset, showing flipper v culmen length colour coded by species
basic_graph <- function(penguins_data) {
  ggplot(data = penguins_data, aes(x = flipper_length_mm, y = culmen_length_mm, colour = species)) + 
  geom_point(alpha = 0.5) + 
  xlab("Flipper Length (mm)") + ylab("Culmen Length (mm)") + 
  labs(title = "Flipper length and culmen length in 3 penguin species", colour = "Species") + 
  theme_light() +
  scale_colour_manual(values = c("#648FFF", "#DC267F", "#FE6100")) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "whitesmoke"),
        panel.grid.minor = element_line(colour = "whitesmoke"))
}

# function to create a poorly designed graph from the cleaned data
bad_graph <- function(penguins_data) {
  ggplot(data = penguins_data, aes(x = culmen_length_mm, y = flipper_length_mm)) +
  geom_point(size = 0.5, shape = 15, colour = "white") + #adds points, and sets their aes properties to match the grid elements
  theme_light() + #sets base theme to be modified
  xlim(60,30) + ylim(150,300) + #sets axis limits
  theme(panel.grid.major = element_line(colour = "white", linewidth = 1, linetype = "dotted"), #sets colour and aes properties of background elements
        panel.grid.minor = element_line(colour = "white", linewidth = 1, linetype = "dotted"),
        panel.border = element_rect(colour = "white", linewidth = 1, linetype = "dotted"),
        panel.background = element_rect(fill = "yellow")) 
}

# function to create the final results graph, showing the correlation for each species individually
results_graph <- function(penguins_data) {
  ggplot(data = penguins_clean, aes(x = flipper_length_mm, y = culmen_length_mm, colour = species)) + 
    geom_point(alpha = 0.5) +
    geom_smooth(method = lm) +
    xlab("Flipper Length (mm)") + ylab("Culmen Length (mm)") + 
    labs(title = "Flipper length and culmen length in 3 penguin species", colour = "Species") + 
    scale_colour_manual(values = c("#648FFF", "#DC267F", "#FE6100")) +
    theme_light() +
    facet_wrap(vars(species), labeller = as_labeller(c(Adelie = 'Adelie, r = 0.326', Chinstrap = 'Chinstrap, r = 0.472', Gentoo = 'Gentoo, r = 0.661'))) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_line(colour = "whitesmoke"),
          panel.grid.minor = element_line(colour = "whitesmoke"))
}

# save basic graph as a vecor for report
save_basic_graph_pvg <- function(penguins_data, filename, w, h, scaling){
  svglite(filename, width   =  w, 
          height  =  h, 
          scaling =  scaling)
  graph <- basic_graph(penguins_data)
  print(graph)
  dev.off()
}

# save results graph as vector for report
save_results_graph_pvg <- function(penguins_data, filename, w, h, scaling){
  svglite(filename, width   =  w, 
          height  =  h, 
          scaling =  scaling)
  graph <- results_graph(penguins_data)
  print(graph)
  dev.off()
}
