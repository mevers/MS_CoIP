ggpairwise <- function(df, xlab = "", ylab = "", title = "") {
  # Pairwise plots
  my_fn <- function(data, mapping, method = "lm", ...) {
    #print(str(data));
    xVal <- data[, as.character(mapping$x)];
    yVal <- data[, as.character(mapping$y)];
    lbl <- data[, as.character(mapping$label)];
    proteins <- c("HEATR3", "RPL5", "RPL11");
    df.highlight <- cbind.data.frame(x = xVal, y = yVal, id = lbl);
    df.highlight <- do.call(rbind, lapply(proteins, function(x) df.highlight[grep(x, df.highlight$id), ]));
    df.highlight$id <- proteins;
    label.Ntot <- sprintf("N=%i", nrow(data[complete.cases(xVal, yVal), ]));
    p <- ggplot(data = data, mapping = mapping);
    p <- p + geom_point();
    p <- p + geom_point(data = df.highlight, aes(x = x, y = y), size = 4, fill = "red", colour = "red", alpha = 0.5);
    p <- p + geom_text_repel(
      data = df.highlight, 
      aes(x = x, y = y, label = id), 
      colour = "red", 
      size = 3,
      box.padding = unit(0.5, 'lines'),
      point.padding = unit(1.5, 'lines'),
      min.segment.length = unit(0, "lines"));
    p <- p + geom_abline(slope = 1, intercept = 0, colour = "red", linetype = 3);
    p <- p + geom_smooth(method = method, fullrange = TRUE, ...);
    p <- p + geom_text(aes(x = -Inf, y = Inf, hjust = 0, vjust = 1, label = label.Ntot));
    return(p);
  }
  gg <- ggpairs(df, mapping = aes(label = id), columns = 1:(ncol(df)-1), lower = list(continuous = my_fn));
  gg <- gg + theme_bw();
  gg <- gg + theme(
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black"),
    legend.position = "bottom",
    legend.key = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  );
  gg <- gg + labs(
    x = xlab,
    y = ylab,
    title = title
  );
  gg;
}
