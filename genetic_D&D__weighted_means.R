#make plot weighted means


# Create matrices for correlation results with Ne
# Create a correlation matrix
correlation_matrix_with_Ne <- data.frame(
  AMOVA = c(1, -0.385, -0.291, -0.349, -0.421),
  λ = c(-0.385, 1, 0.215, 0.392, 0.34),
  EcoSim = c(-0.291, 0.215, 1, 0.034, 0.192), 
  NeEstimator = c(-0.349, 0.392, 0.034, 1, 0.586), 
  SpeedNe = c(-0.421, 0.34, 0.192, 0.586, 1)
)
colnames(correlation_matrix_with_Ne) <- c("AMOVA", ":λ[cor]", "Allelic Overlap", "Ne Estimator", "Speed Ne")
rownames(correlation_matrix_with_Ne) <- c("AMOVA", ":λ[cor]", "Allelic Overlap", "Ne Estimator", "Speed Ne")
correlation_matrix_with_Ne <- as.matrix(correlation_matrix_with_Ne)

p_matrix_with_Ne <- data.frame(
  AMOVA = c(1, 0.001, 0.001, 0.001, 0.001),
  rare = c(0.001, 1, 0.001, 0.001, 1), 
  Overlaps = c(0.001, 0.001, 1, 0.001, 0.001),
  Ne = c(0.001, 0.001, 0.001, 1, 0.001),
  NeS = c(0.001, 1, 0.001, 0.001, 1)
)
colnames(p_matrix_with_Ne) <- c("AMOVA", ":λ[cor]", "Allelic Overlap", "Ne Estimator", "Speed Ne")
rownames(p_matrix_with_Ne) <- c("AMOVA", ":λ[cor]", "Allelic Overlap", "Ne Estimator", "Speed Ne")
p_matrix_with_Ne <- as.matrix(p_matrix_with_Ne)



# Create a plot for correlation results excluding the effective population size:
# prepare a new plot
plot.new()
# Display results
corrplot(correlation_matrix_with_Ne, 
         method= "square", 
         type= "upper", 
         order = 'hclust', 
         addCoef.col = "black", 
         tl.col= "black", 
         p.mat = p_matrix_with_Ne, 
         sig.level = 0.10, 
         addrect = 2, 
         col= COL2('RdBu', 10),
         diag=FALSE )
text(p1$x, p1$y, round(p1$corr, 2))
cat( " *** = p-value is below 0.001, ** = p-value is below 0.01, * = p-value is below 0.05, the numbers show the correlation coefficient. " )

dev.copy(png, file.path(paste("Results", describe_results_variable, "/Compare_all_methods", sep = ""), "correlations_without_Ne.png"))
dev.off()
