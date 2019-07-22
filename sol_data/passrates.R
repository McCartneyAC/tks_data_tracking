dat  %>% 
mutate(passed = if_else(scaled_score > 400, 1, 0))  %>% 
group_by(test)  %>% 
mutate(num = n())  %>% 
mutate(passes = sum(passed))  %>% 
ungroup()  %>% 
mutate(pass_rate = passes / num)  %>% 
select(test, pass_rate)  %>% 
distinct()  %>% 
ggplot(aes(y = pass_rate, x = reorder(test, pass_rate), fill = pass_rate)) + 
geom_col() + 
coord_flip() + 
#inova<-c("#004b8d", "#d52b1e", "#6caddf", "#4d4f53", "#a5a5a9")
scale_fill_gradient(low = "#d52b1e", high = "#6caddf") +
theme_light() + 
labs(title = "SOL Pass Rates by Test",
    x = "Test", 
    y = "Pass Rate", 
    subtitle = "Bars indicate TKS rates. 
Solid lines are statewide pass rates. 
Dotted lines are statewide for students with disabilities.", 
    caption = "TKS Rates are past four years. 
State pass rates are previous three years (averaged)") +
annotate("text", x = 5, y = .25, label = "These assessments", color = "#004b8d") +
annotate("text", x = 4, y = .25, label = "not yet passed", color = "#004b8d") +
annotate("text", x = 3, y = .25, label = "at TKS / only one attempt", color = "#004b8d") +
# BEGIN MAIN ANNOTATIONS 
# label = "82% (54%)", color = "#004b8d") +
annotate("segment", x = 6.5, xend = 7.5,  y = .82, yend = .82,  color = "#004b8d") +
annotate("segment", x = 6.5, xend = 7.5,  y = .54, yend = .54,  color = "#004b8d", linetype = "dotted") +
annotate("segment", x = 7.5, xend = 8.5,  y = .89, yend = .89,  color = "#004b8d") +
annotate("segment", x = 7.5, xend = 8.5,  y = .69, yend = .69,  color = "#004b8d", linetype = "dotted") +
annotate("segment", x = 8.5, xend = 9.5,  y = .78, yend = .78,  color = "#004b8d") +
annotate("segment", x = 8.5, xend = 9.5,  y = .45, yend = .45,  color = "#004b8d", linetype = "dotted") +
annotate("segment", x = 9.5, xend = 10.5,  y = .76, yend = .76,  color = "#004b8d") +
annotate("segment", x = 9.5, xend = 10.5,  y = .38, yend = .38,  color = "#004b8d", linetype = "dotted") +
annotate("segment", x = 10.5, xend = 11.5,  y = .72, yend = .72,  color = "#004b8d") +
annotate("segment", x = 10.5, xend = 11.5,  y = .33, yend = .33,  color = "#004b8d", linetype = "dotted") +
annotate("segment", x = 11.5, xend = 12.5,  y = .83, yend = .83,  color = "#004b8d") +
annotate("segment", x = 11.5, xend = 12.5,  y = .5, yend = .5,  color = "#004b8d", linetype = "dotted") +
# annotate("segment", x = 12.5, xend = 13.5,  y = .71, yend = .71,  color = "#004b8d") +
# annotate("segment", x = 12.5, xend = 13.5,  y = .39, yend = .39,  color = "#004b8d", linetype = "dotted") +
annotate("text", x = 14, y = .25, label = "CAT scores", color = "#004b8d") +
annotate("text", x = 13, y = .25, label = "unreported", color = "#004b8d") +
# annotate("segment", x = 13.5, xend = 14.5,  y = .76, yend = .76,  color = "#004b8d") +
# annotate("segment", x = 13.5, xend = 14.5,  y = .38, yend = .38,  color = "#004b8d", linetype = "dotted") +
annotate("segment", x = 14.5, xend = 15.5,  y = .86, yend = .86,  color = "#004b8d") +
annotate("segment", x = 14.5, xend = 15.5,  y = .59, yend = .59,  color = "#004b8d", linetype = "dotted") +
annotate("segment", x = 15.5, xend = 16.5,  y = .84, yend = .84,  color = "#004b8d") +
annotate("segment", x = 15.5, xend = 16.5,  y = .56, yend = .56,  color = "#004b8d", linetype = "dotted") +
annotate("segment", x = 16.5, xend = 17.5,  y = .82, yend = .82,  color = "#004b8d") +
annotate("segment", x = 16.5, xend = 17.5,  y = .55, yend = .55,  color = "#004b8d", linetype = "dotted") +
# annotate("segment", x = 17.5, xend = 18.5,  y = .82, yend = .82,  color = "#004b8d") +
# annotate("segment", x = 17.5, xend = 18.5,  y = .47, yend = .47,  color = "#004b8d", linetype = "dotted") +
annotate("segment", x = 18.5, xend = 19.5,  y = .85, yend = .85,  color = "#004b8d") +
annotate("segment", x = 18.5, xend = 19.5,  y = .58, yend = .58,  color = "#004b8d", linetype = "dotted") +
annotate("segment", x = 19.5, xend = 20.5,  y = .79, yend = .79,  color = "#004b8d") +
annotate("segment", x = 19.5, xend = 20.5,  y = .46, yend = .46,  color = "#004b8d", linetype = "dotted") +
annotate("segment", x = 20.5, xend = 21.5,  y = .88, yend = .88,  color = "#004b8d") +
annotate("segment", x = 20.5, xend = 21.5,  y = .57, yend = .57,  color = "#004b8d", linetype = "dotted") +
annotate("segment", x = 21.5, xend = 22.5,  y = .84, yend = .84,  color = "#004b8d") +
annotate("segment", x = 21.5, xend = 22.5,  y = .51, yend = .51,  color = "#004b8d", linetype = "dotted") +
annotate("segment", x = 22.5, xend = 23.5,  y = .89, yend = .89,  color = "#004b8d") +
annotate("segment", x = 22.5, xend = 23.5,  y = .64, yend = .64,  color = "#004b8d", linetype = "dotted") +
annotate("segment", x = 23.5, xend = 24.5,  y = .87, yend = .87,  color = "#004b8d") +
annotate("segment", x = 23.5, xend = 24.5,  y = .53, yend = .53,  color = "#004b8d", linetype = "dotted") +
scale_y_continuous(labels = scales::percent) +
guides(fill = FALSE) +
NULL



# and

dat  %>% 
mutate(level = if_else(
    grepl("Gr", test, fixed=TRUE), 0, 1
  ))   %>% 
group_by(test)  %>% 
mutate(mu = mean(scaled_score))  %>% 
ungroup()  %>% 
ggplot(aes(y = scaled_score, x = fct_reorder(test, mu), color = fct_reorder(test, mu)))  + 
geom_hline(yintercept = 375, alpha = 0.5) +
geom_hline(yintercept = 400, alpha = 0.5) +
geom_boxplot() + 
guides(color = F) + 
labs(title = "Average SOL score by Test",
    x = "Test", 
    y = "Scaled Score", 
    subtitle = "Last Four Years") +
coord_flip() + 
theme_light() +
geom_jitter(width = 0.2, alpha = 0.4, stroke = 0) + 
# facet_grid(rows = level) +
NULL
