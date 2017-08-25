
library(ggplot2)
library(plotly)


## stacked histogram in plotly
p <- ggplot(dff,aes(year,fill=q13s)) +
  geom_bar(position="fill") + # plot for year, parents' education)
  labs(title= "Figure 2", subtitle="response distribution by year", caption="no obvious changes during 3 years", y="response percentage")

p<- ggplotly(p)

p

# data preparing

g_dff <-
  dff %>%
  group_by(collcode, year) %>%
  mutate(is_satisfied = (q13s %in% c("Satisfied", "Very Satisfied"))) %>%
  summarise(n = n(),
            num_satisfied = sum(is_satisfied),
            pct_satisfied = sum(is_satisfied) / n())%>%
  mutate(p_satisfied= paste(round(100*pct_satisfied,2),"%",sep=""))


## heatmap in plotly
plot_ly(x=g_dff$year, y = g_dff$collcode, z=g_dff$pct_satisfied,
        type = "heatmap",
        colors = colorRamp(c("white", "coral")))