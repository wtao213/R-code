library(plotly)

plot_ly(x=dff$q13s,
        type="histogram")


output$heat <- renderPlotly({
  plot_ly(x = nms, y = nms, z = correlation, 
          key = correlation, type = "heatmap", source = "heatplot") %>%
    layout(xaxis = list(title = ""), 
           yaxis = list(title = ""))
  
# heatmap in plotly
  plot_ly(x = g_dff$year, y = g_dff$collcode, z=g_dff$pct_satisfied,
        colors = colorRamp(c("white", "coral")),
        type="heatmap", source="heatplot")

plot_ly(dff,x=~year, y=~q13s, type="bar")%>%layout(barmode="stack")

ggplot(dff,aes(year,fill=q13s)) +
  geom_bar(position="fill") + # plot for year, parents' education)
  labs(title= "Figure 2", subtitle="response distribution by year", caption="no obvious changes during 3 years", y="response percentage") +
  theme(legend.position = "bottom")
