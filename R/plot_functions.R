eqLineLong <- function(df, from, dec = NULL){
  if(!(from %in% df$date)){
    print("Startdate not in df.")
  } else{
    df <- df %>%
      filter(date >= as.Date(from))
    symbols <- df$symbol %>%
      unique()
    df$eq_l <- NA
    for(k in 1:length(symbols)){
      df$eq_l[df$symbol == symbols[k]] <- df$adjusted[df$symbol == symbols[k]] %>%
        Delt() %>%
        as.numeric() %>%
        cumRet()
    }

    ## plot pre processeing
    df <- df %>%
      mutate("Date" = as.Date(date),
             "Equity" = eq_l -1)

    p <- ggplot(df) +
      geom_line(aes(x = Date, y = Equity, color = symbol))

    # scale
    if(!is.null(dec)){
      MIN <- df$Equity %>% min()
      MIN <- MIN * 0.9
      MAX <- df$Equity %>% max()
      MAX <- MAX * 1.1
      BREAK <- seq(MIN, MAX, 10^(-dec)) %>%
        round(., dec) %>%
        unique()
      p <- p + scale_y_continuous(breaks=BREAK)
    }

    return(p)

  }
}

## Scaled Lineplot für Indikatoren
scaledLP <- function(l1,l2,X_axis, y1,y2,x,title){

  scale <- sd(l2, na.rm=T) / sd(l1, na.rm = T) %>% abs()
  a <-  l1[1] -(l2[6] / scale)
  data <- data.frame(
    "X_axis"=X_axis,
    "l1"=l1,
    "l2"=(l2 / scale + a)
  )
  ggplot(data) +
    geom_line(aes(x = X_axis, y = l1)) +
    geom_line(aes(x = X_axis, y = l2), color= "red")+
    ggtitle(title) +
    ylab(y1)+ xlab(x)+
    scale_y_continuous(sec.axis = sec_axis(~ (.-a)*scale, name=y2))

}

## Scaled Barplot für label als bar vs indikator
scaledBLP <- function(b,l,X_axis, y1,y2,x,title){

  scale <- sd(l, na.rm=T) / sd(b, na.rm = T) %>% abs()
  a <-  b[1] -(l[1] / scale)
  data <- data.frame(
    "X_axis"=X_axis,
    "b"=b,
    "l"=(l / scale + a)
  )
  #month_brks <- c(1,endpoints(data$X_axis, "weeks")[-1])
  ggplot(data) +
    geom_col(aes(x = X_axis, y = b)) +
    geom_line(aes(x = X_axis, y = l), color= "red")+
    ggtitle(title) +
    ylab(y1)+ xlab(x)+
    scale_y_continuous(sec.axis = sec_axis(~ (.-a)*scale, name=y2))
  #+
  # scale_x_bd(business.dates=X_axis, max.major.breaks=5)
  #+
  # scale_x_continuous(breaks=month_brks, labels=format(X_axis, "%d %b %Y"))

}
