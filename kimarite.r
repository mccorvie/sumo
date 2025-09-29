library( tidyverse)
library(class)


library(FactoMineR)
library(factoextra)


faceoff_table <- bind_rows( faceoff_list)

faceoff_table |> filter( )
kimarite <- faceoff_table |> 
  filter( kimarite != "", division =="Juryo" | division =="Makuuchi" ) |> 
  select( bashoId, day, rikishiId, win, kimarite) |> 
  group_by( bashoId, day) |> 
  mutate( win = 2*win-1) |> 
  pivot_wider(names_from=kimarite, values_from = win, values_fill=0) |> 
  group_by( rikishiId ) |> 
  summarize(
    across(yorikiri:okurigake, \(x) sum( pmax(x,0)), .names = "{.col}_win"),
    across(yorikiri:okurigake, \(x) sum( pmin(x,0)), .names = "{.col}_loss")
  ) |> 
  rowwise() |> 
  mutate(
    total_win  = sum(abs(c_across(yorikiri_win:okurigake_win))),
    total_loss = sum(abs(c_across(yorikiri_loss:okurigake_loss))),
  ) |> 
  filter( total_win >=10,  total_loss >= 10) |> 
  ungroup() |> 
  mutate( 
    across(yorikiri_win:okurigake_win, \(x) if_else( total_win==0, 0, x / total_win)),
    across(yorikiri_loss:okurigake_loss, \(x) if_else( total_loss==0, 0, x / total_loss))
  )

kimarite |> select( rikishiId, total_win, total_loss)


## kmeans analysis

kmeans_res <- kmeans( kimarite[,c(-1,-ncol(kimarite)+1, -ncol(kimarite))], centers=3, nstart=25)

kmeans_res
kimarite |> bind_cols( tibble( cluster = kmeans_res$cluster )) 

makuuchi_ids <- basho_sumo_rank() |> filter( rankValue < 600) |> pull( rikishiId)

style <- tibble( rikishiId = kimarite$rikishiId, cluster = kmeans_res$cluster) |> 
  left_join( sumo_name_t, by="rikishiId") |> 
  filter( rikishiId %in% makuuchi_ids)

style |> print( n=50)
style |> pull( cluster) |> table()


cc <- kmeans_res$centers |> as_tibble() |> mutate( cluster = 1:n()) |> 
  pivot_longer(yorikiri_win:okurigake_loss, names_to="kimarite", values_to="freq") |> 
  mutate( kimarite = factor( kimarite, levels = kimarite_levels), cluster = factor( cluster)) 


cc |> pivot_wider( names_from=cluster, values_from=freq) |> arrange( kimarite) |> print( n=60)

filter( cc, str_starts( kimarite, "kotenage" ))


ggplot(filter( cc, abs(freq)>0.02), aes(x=kimarite, y=freq, color=cluster)) + geom_point() 


filter( cc, cluster==1) |> arrange( -abs(freq)) |> print(n=60)

kimarite_levels


# make kimarite_levels (frequency ordered factor of kimarite names)

kimarite_all <- faceoff_table |> 
  select( bashoId, day, rikishiId, win, kimarite) |> 
  filter( kimarite != "") |> 
  group_by( bashoId, day) |> 
  mutate( win = 2*win-1) |> 
  pivot_wider(names_from=kimarite, values_from = win, values_fill=0) |> 
  ungroup() |> 
  summarize(across(yorikiri:okurigake, \(x) sum( abs(x))))

kimarite_summary <- unlist( kimarite_all[1,])
kimarite_summary / sum( kimarite_summary)

kk_names <- tibble( kimarite = names( kimarite_summary), freq = kimarite_summary/sum( kimarite_summary)) |> 
  arrange( -freq) |> 
  pull( kimarite)


factor( c(paste0(kk_names, "_win" ),paste0(kk_names, "_loss" )))
kimarite_levels <- factor( paste0( rep( kk_names, each=2), c( "_win", "_loss")), ordered=T)
kimarite_factor = factor( )

  
## pca analysis



res <- PCA( kimarite[,c(-1,-ncol(kimarite)+1, -ncol(kimarite))], scale.unit = F, graph = F)

fviz_pca_ind(res, habillage = kmeans_res$cluster, addEllipses = TRUE)
fviz_pca_var(res)

#fviz_pca_ind() → individuals (points, samples)
#fviz_pca_var() → variables (loadings)
