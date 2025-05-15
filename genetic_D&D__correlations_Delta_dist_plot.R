correlation_results <- data.frame(
  tested_methods = c("AMOVA", "lambda", "Ne", "Overlaps"),
  p_value = c(1, 0.0001, 0.0001, 0.0001,
              0.0001, 1, 1, 1,
              0.0001, 0.0001, 0.0001, 0.0001, 
              0.0001, 0.0001, 0.0001, 0.03),  
  correlation_coefficient = c(0.05, -0.19, -0.21, -0.25,
                              0.5, 0.14, -0.11, 0.04,
                              0.37, -0.35, -0.17, -0.2,
                              -0.32, 0.39, 0.38, 0.09)
)

# add E, N, P, T like in the original table
suffixes <- rep(c("E", "N", "P", "T"), each = 4)
correlation_results$tested_methods <- paste0(correlation_results$tested_methods, suffixes)

# rename
corr_all_dist <- correlation_results

par(mfrow = c(4, 4), # print several plots in one
    mar = c(0, 0, 0, 0), # the correlation squares should not have margin
    oma = c(6, 9, # left
            6, 6  # right
            )) # there should be a margin around all correlations

counter <- 0 # for y labels
for(i in 1:nrow(corr_all_dist))  {
  
  corr_val <- corr_all_dist[i,3]
  p_val <- corr_all_dist[i,2]
  
  # column names
  DnD_methods_without_Delta <- c(expression(bold("AMOVA")), 
                                 expression(bold("λ"["cor"])),
                                 expression(bold("NᴇEsᴛɪᴍᴀᴛᴏʀ")), 
                                 expression(bold("Allelic Overlap")))
  plot(1,
       type="n",
       xlim=c(0, 1),
       ylim=c(0, 1),
       xlab="", 
       ylab= "",
       axes=FALSE,
       main = title(ifelse(i < 6, DnD_methods_without_Delta[i], ""), # only print the methods for the upper row of plots
                    xpd =NA, # allows main to show up outside of the plot margins
                    line = 1, # moves main outside the plot
                    cex.main = 1.2), # font size
       asp = 1) # the correlations will be shown as a square
  
  plot_frame <- par("usr")  # aktuelle Grenzen des Plotrahmens erhalten
  rect(plot_frame[1], plot_frame[3], plot_frame[2], plot_frame[4], border = "gray", lwd = 0.5)
  
  # Koordinaten für das innere Quadrat berechnen
  inner_size <- sqrt(abs(corr_val))  # Größe des inneren Quadrats, um 21% der Fläche des äußeren Quadrats einzunehmen
  inner_x <- (plot_frame[2] + plot_frame[1]) / 2 - inner_size / 2  # x-Koordinate des inneren Quadrats
  inner_y <- (plot_frame[4] + plot_frame[3]) / 2 - inner_size / 2  # y-Koordinate des inneren Quadrats
  
  #find colors
  # if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  #   install.packages("RColorBrewer")
  # }
  # library(RColorBrewer)
  # palette <- colorRampPalette(brewer.pal(11, "RdBu"))
  # num_colors <- 10
  # farben <- palette(num_colors)
  
  # Color for the inner square
  if (corr_val < 0) {
    if (corr_val > -0.2) {
      corr_color <- "#FAE7DC"
    } else if (corr_val > -0.4) {
      corr_color <- "#F7B698"
    } else if (corr_val > -0.6) {
      corr_color <- "#DC6F58"
    } else if (corr_val > -0.8) {
      corr_color <- "#B51F2E"
    } else {
      corr_color <- "#67001F"
    }
  } else {
    if (corr_val < 0.2) {
      corr_color <- "#E1EDF3"
    } else if (corr_val < 0.4) {
      corr_color <- "#A7CFE4"
    } else if (corr_val < 0.6) {
      corr_color <- "#549EC9"
    } else if (corr_val < 0.8) {
      corr_color <- "#246BAE" 
    } else if (corr_val < 1) {
      corr_color <- "#053061" 
    }
  }
  # symbol for p-value
  if (p_val < 0.001) {
    p_val <- "***"
  } else if (p_val < 0.01) {
    p_val <- "**"
  } else if (p_val < 0.05) {
    p_val <- "*"
  } else if (p_val > 0.05) {
    p_val <- ""
  }
  
  # Inneres Quadrat zeichnen
  rect(inner_x, inner_y, inner_x + inner_size, inner_y + inner_size, border="transparent", col= corr_color)
  # Text in die Mitte des Plots schreiben
  text(x = 0.5, y = 0.5, labels = round(corr_val, 2), cex = 1.5, font = 2)
  # Text für p-Wert hochgestellt hinter dem korrigierten Wert schreiben
  text(x = 0.5, y = 0.5, labels = p_val, cex = 1.5, font = 2, adj = c(-1, -0.3))
  
  distance_methods <- c(expression(bold("Edwards")), 
                        expression(bold("Nei")),
                        expression(bold("pairwise")), 
                        expression(bold("taxa2dist()")))
  
  # Add y axis labels
  if (i %% 4 == 1) {
    counter <- counter +1
    text(x = -0.7, y = 0.5,
         distance_methods[counter],
         xpd = NA, 
         cex = 1.2)
  }  
}

# Create the legend explaining the color code: 
colors <- c("#053061", "#246BAE", "#549EC9", "#A7CFE4", "#E1EDF3", "#FAE7DC",  "#F7B698", "#DC6F58", "#B51F2E", "#67001F")
num_rects <- 10 # number of colors/rectangles that should be created

# coordinates of the legend
x1 <- -4.34
y1 <- -0.5
x2 <- 1.18
y2 <- -0.3

rect_width <- (x2 - x1) / num_rects  # width of every rectangle

# draw one rectangle for every color
for (i in 1:num_rects) {
  rect(x1 + (i - 1) * rect_width, y1, x1 + i * rect_width, y2,
       border = "transparent", col = colors[i], xpd = NA)
}

# Create a frame for the legend
rect(x1 , y1, x2, y2, 
     border = "black",
     xpd = NA)

# Add an axis
axis(side = 1, at = seq(x1, x2, length.out = num_rects + 1),
     labels = FALSE,
     pos = -0.5, 
     xpd = NA)

#Add axis labels
text(x = seq(x1, x2, length.out = num_rects + 1),
     y = -0.7,
     labels = c("1", "0.8", "0.6", "0.4", "0.2", "0", "-0.2", "-0.4", "-0.6", "-0.8", "-1"),
     xpd = NA)