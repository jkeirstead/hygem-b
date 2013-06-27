## Make a waterfall plot
## @param df a dataframe with columns labelled category (an ordered factor, such that 1 + 2 + ... + n-1 = n), value
waterfall <- function(df) {

  df <- transform(df, order=as.numeric(category))
  df <- arrange(df, order)
                  
  ## Calculate the min and max for each category
  cs <- cumsum(df$value)
  min.value <- c(0, cs[-c(1, length(cs))], 0)
  max.value <- c(cs[1], cs[1:(length(cs)-1)])
  df <- cbind(df, min=min.value, max=max.value)
  df <- ddply(df, .(category), transform, min=min(min, max), max=max(min, max))
  
  ## Make the plot
  offset <- 0.3
  df <- mutate(df, offset=offset)
  lines <- data.frame(x=rep(df$order, each=2),
                      y=c(0, rep(head(cs, -1), each=2), tail(cs, 1)))
  lines <- lines[-c(1, nrow(lines)), ]

  ##  print(df)
  ##  print(class(df$order))
  ##  print(lines)
  
  require(scales)
  gg <- ggplot() +
    geom_line(data=lines, aes(x=x, y=y), linetype="dashed")  +
    geom_rect(data=df, aes(xmin=order - offset,
                  xmax=order + offset, 
                  ymin=min,
                  ymax=max)) +
    scale_x_continuous(breaks=df$order, labels=df$category)

  return(gg)
}

debug <- FALSE

if (debug) {
  raw <- data.frame(category=c("A", "B", "C", "D"),
                    value=c(100, -20, 10, 90))

  df1 <- transform(raw, category=factor(category))                
  gg1 <- waterfall(df1) + theme_bw() + labs(x="", y="Value")
  
  df2 <- transform(raw, category=factor(category, levels=c("A", "C", "B", "D")))
  gg2 <- waterfall(df2) + theme_bw() + labs(x="", y="Value")

}


