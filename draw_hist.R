draw_hist<-function( df, gfit,b=5) {
  stat <- stat_function(aes(x = x, y = ..y..), fun = dghyp, 
                        colour="black", geom="area",
                        fill="#FF6666", alpha=0.2,
                        args = list(object = gfit))
  #stat_function(fun=dnorm_limit, geom="area", fill="blue", alpha=0.2)
  pic<-ggplot(df, aes(x=x)) + 
    geom_histogram(aes(y=..density..), 
                   binwidth=b,colour="black", fill="white")+
    stat 
  pic
}



#fig<-plot_ly(data=df.mod.hc, x=~Country, y=~happy -3.9, type='bar', name="Happy")
#fig <- fig %>% add_trace(y=~phappy-3.9, name="predHappy")
#fig <- fig %>% layout(yaxis = list(title = 'log(x+1)'), barmode = 'group')
#fig