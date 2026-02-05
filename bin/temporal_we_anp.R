ggplot(df %>% filter(metric == "Weighted Endemism"),
       aes(x = CAT, y = value)) +
  geom_violin(fill = "salmon", alpha = 0.4) +
  geom_boxplot(width = 0.15, fill = "grey80") +
  theme_bw(base_size = 14) +
  xlab("") +
  ylab("Weighted Endemism") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )
