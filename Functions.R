## Functions##

# Function to plot the kBET and Silhouette scores
plotMetrics <- function(scoresKbet = NULL, scoresSilhouette = NULL, methods = NULL, ...){
  df <- data.frame("Silhouette" = t(scoresSilhouette), "kBET" = t(scoresKbet), Method = methods)
  p <- ggplot(df, aes(Silhouette, kBET, color = Method, label = Method)) + 
    geom_point(size = 8, alpha = 0.5) +
    geom_text_repel(size = 9, alpha = 1.0, direction = "both", ...) + 
    ylab("kBET(acceptance rate)") + theme(legend.position = "none")
  
  return(p)
}

# Plot a correction by different groups (e.g. batch, celltype, etc.)
plotCorrection <- function(object, groups = NULL, reduction = "pca", ...){
  plotls <- list()
  for(g in groups){
    plotls[[g]] <-  DimPlot(object, reduction = reduction, group.by = g, ...) 
  }
  return(plotls)
}

# Scales the plot color gradient to a given scale (limits) and colors.
scaleGradient <- function(plotls, low = "gray", high = "purple", limits = NULL){
  if(is.null(limits)){
    stop("No defined limits. Select the limits and try again.", call. = TRUE)
  }
  
  for(i in seq_len(length(plotls))){
    plotls[[i]] <- plotls[[i]] + 
      scale_color_gradient(low = low, high = high, limits = limits)  
  }
  
  return(plotls)
}

# Plot features from a Seurat objects with a given low-high color scale
plotFeatures <- function(object, features = NULL, reduction = "pca",colorLow = "gray", colorHigh = "purple", colorLimits = c(0,1), ...){
  pFeatures <- FeaturePlot(object, features = features, reduction = reduction,
                           order = TRUE, combine = FALSE)
  pFeatures <- scaleGradient(pFeatures, low = colorLow, high = colorHigh, limits = colorLimits)
  names(pFeatures) <- features
  
  return(pFeatures)
}