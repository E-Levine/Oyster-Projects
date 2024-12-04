##Working code - extra/notes/etc.

#Within SPV1
(TB_WQ_Months <- TB_WQ_df %>% ungroup() %>% 
  dplyr::select(Month, Temperature, Salinity, pH, DO_mgl) %>% 
  group_by(Month) %>% 
  mutate(Temperature = mean(Temperature, na.rm = T),
         Salinity = mean(Salinity, na.rm = T),
         pH = mean(pH, na.rm = T),
         DO_mgl = mean(DO_mgl, na.rm = T)) %>% distinct() %>%
  column_to_rownames(var = "Month"))
#
#Variance-covariance of parameters - little correlation among variables, but Temperature trend in Months
cor(TB_WQ_Months, method = "s"); corrplot(cor(TB_WQ_Months), method = "ellipse")
#PCA
Month_WQ_PCA <- princomp(TB_WQ_Months, cor = TRUE, scores = TRUE)
summary(Month_WQ_PCA);plot(Month_WQ_PCA)
#Number of components to include:
plot(cumsum(Month_WQ_PCA$sdev/sum(Month_WQ_PCA$sdev^2)), type = "b")
abline(h = 0.90)
factoextra::fviz_pca_biplot(Month_WQ_PCA)
#
ggplot(data.frame(cmdscale(dist(TB_WQ_Months), k = 2)), 
       aes(X1, X2, label = row.names(cmdscale(dist(TB_WQ_Months), k = 2))))+
  geom_text()

       