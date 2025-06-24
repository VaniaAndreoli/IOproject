library(readxl)

Countries_cwmc <- read_excel("R-data-variables.xlsx", sheet = "Countries-cwmc")

library(ggcorrplot)
install.packages("ggcorrplot")

Countries<-Countries_cwmc[,-1]

ggcorrplot(cor(Countries))

comm_g<- read_excel("R-data-variables.xlsx", sheet = "Comm group taxa-cwmc")
comm_g<-comm_g[,-1]
ggcorrplot(cor(comm_g))


taxa <- read_excel("R-data-variables.xlsx", sheet = "All taxa")
taxa <-taxa[,-1]
ggcorrplot(cor(taxa))
x<-cor(taxa)


SP <- read_excel("R-data-variables.xlsx", sheet = "Species")
SP<-SP[,-1]
ggcorrplot(cor(SP))


#### Correlation matrix prettier###

# Rename the columns with new variable names
colnames(comm_g) <- c("Calcium", "DHA+EPA", "Iron", "Selenium", "Vitamin A", "Zinc", "Vitamin B12", "Price", "Fishing vuln", "Climate vuln")  # Replace with your actual new names

desired_order <- c("Calcium", "DHA+EPA", "Iron", "Selenium", "Vitamin A", 
                   "Vitamin B12", "Zinc", "Price", "Fishing vuln", "Climate vuln")

# Rearrange the data frame based on the desired order
comm_g <- comm_g[, desired_order]

# Compute the correlation matrix (only numeric data)
M <- cor(comm_g, use = "pairwise.complete.obs")  # Use pairwise to handle missing values

# Create a clustering order for the correlation matrix
dendrogram <- as.dendrogram(hclust(dist(M)))

# Create the correlation plot for the lower triangle with a red-to-blue gradient
ggcorrplot(M[desired_order, desired_order],  # Use the desired order directly
           type = "lower",                     # Show lower triangle
           lab = TRUE,                         # Add correlation coefficients
           lab_size = 4,                       # Size of labels
           tl.col = "black",                   # Color of the text labels
           tl.srt = 45,                        # Rotate text labels
           title = "") +  # Title 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1, 1), 
                       name = "Correlation")  # Red to blue gradient

