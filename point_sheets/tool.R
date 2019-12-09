tool  %>% 
    ggplot(aes(x = fct_reorder(name, mu_minutes), y = mu_minutes, fill = mu_minutes))  +
    geom_col() + 
    coord_flip() +
    scale_fill_gradient(high = "#d52b1e", low = "#6caddf") + 
    theme_light() + 
    geom_hline(yintercept = mean(tool$mu_minutes), color = "#004b8d", linetype = "dotted") + 
    geom_hline(yintercept = 6, color = "#004b8d", linetype = "dotted") + 
    labs(
        title = "Time out of Learning by Student",
        subtitle = "Averages for first interim period of quarter 2",
        x = "Student",
        y = "Average Minutes per Day out of Class",
        fill = "Minutes Missed", 
        caption = "'Theoretical Minimum' is how much a student should miss, 
on average, given 30 minutes of weekly counselor time. "
    ) + 
    annotate("text", x =10, y = 17, label = "TKS Average", color = "#004b8d") + 
    annotate("text", x =2, y = 9, label = "Theoretical 
Minimum", color = "#004b8d") + 
guides(fill = FALSE)
