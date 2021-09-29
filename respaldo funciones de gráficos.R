# Respaldo gg_item (1 caso) ----
item <- "item13"



p_item_aux1<- df %>% 
  group_by(empresa, item13) %>% 
  summarize(n = n()) %>% 
  mutate(pct = n/sum(n),
         lbl = percent(pct), 
         var_lbl = paste0(lbl, "(", n, ")"))

p_item_aux2 <- df %>% 
  count(item13) %>%
  mutate(empresa = paste0("Total muestra"),
         pct = n/sum(n),
         lbl = percent(pct), 
         var_lbl = paste0(lbl, "(", n, ")"))

p_item_aux <- rbind(p_item_aux1, p_item_aux2)

p_item <- ggplot(p_item_aux, aes(x = empresa, 
                                 y = pct, 
                                 fill = factor(item13, levels = c('Casi siempre', 'Frecuentemente', 'Algunas veces', 'Casi nunca')))) + 
  geom_bar(stat = "identity",
           position = "fill", 
           color = "black") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  geom_text(aes(label = var_lbl), 
            size = 3,
            position = position_stack(vjust = 0.5)) + 
  labs(y = "Porcentaje", 
       x = "Cuando ocurre un accidente las jefaturas buscan las causas, \nno a los culpables.", 
       caption = "Fuente: Elaboración propia \n Nota: N de cada categoría entre paréntesis") +
  scale_fill_manual(values = c("#2171B5","#6BAED6","#BDD7E7", "#EFF3FF"))

p_item + theme(legend.title = element_blank(), legend.position = "top")

