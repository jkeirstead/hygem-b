## Make a waterfall plot
## @param df a dataframe with columns labelled category (an ordered factor, such that 1 + 2 + ... + n-1 = n), value, and an additional sector column for a further split
waterfall <- function(df) {

  df <- transform(df, order=as.numeric(category))
  df <- arrange(df, order)
  ids <- which(df$order==max(df$order))
  df$value[ids] <- -df$value[ids]
  
  ## Calculate the cumulative sums
  df <- ddply(df, .(order, category, sector, value), summarize, cs1=cumsum(value))
  df <- mutate(df, cs2=cumsum(cs1))

  ## Calculate the max and mins for each category and sector
  df <- transform(df, min.val=c(0, head(cs2, -1)),
                   max.val=c(head(cs2, -1), 0))
  df <- ddply(df, .(order, category, sector, value), summarize, min=min(min.val, max.val), max=max(min.val, max.val))
    
  ## Make the plot
  offset <- 0.3
  df <- mutate(df, offset=offset)

  ## Create the lines data frame
  cs <- cumsum(ddply(df, .(order), summarize, value=sum(value))$value)
  lines <- data.frame(x=df$order,
                      y=c(0, head(rep(cs, each=2), -2), 0))

  
  require(scales)
  gg <- ggplot() +
    geom_line(data=lines, aes(x=x, y=y), linetype="dashed")  +
    geom_rect(data=df, aes(xmin=order - offset,
                  xmax=order + offset, 
                  ymin=min,
                  ymax=max, fill=sector)) +
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


