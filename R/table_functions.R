IndexPerformance <- function(df, from){
  symbols <- df$symbol %>%
    unique()
  df <- df %>%
    filter(date >= as.Date(from))
  table <- data.frame(matrix(ncol = 5, nrow=length(symbols)))
  for(k in 1:length(symbols)){
    table[k,1] <- df %>%
      filter(symbol == symbols[k]) %>%
      pull(ret) %>%
      cumRet() %>%
      tail(.,1) %>%
      round(., 3)
    table[k,1] <- (table[k,1] - 1) %>% round(., 3)

    table[k,2] <- df %>%
      filter(symbol == symbols[k]) %>%
      pull(ret) %>%
      mean(., na.rm=T) %>%
      round(., 3)

    table[k,3] <- df %>%
      filter(symbol == symbols[k]) %>%
      pull(ret) %>%
      sd(., na.rm=T) %>%
      round(., 3)

    table[k,4] <- (((1+table[k,2])^(252)-1) / (table[k,3] * sqrt(252))) %>%
      round(., 3)

    table[k,5] <- df %>%
      filter(symbol == symbols[k]) %>%
      pull(range) %>%
      mean(., na.rm=T) %>%
      round(., 3)
  }
  rownames(table) <- symbols
  colnames(table) <- c("Return (cum)", "Return (mean)", "Volatility", "Sharpe Ratio", "Daily Range (mean)")
  kable(table)
}
