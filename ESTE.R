library(ggplot2)
library(knitr)
library(kableExtra)
library(dplyr)
library(caret)
library(reshape2)


data <- read.csv("data.csv", head=T, stringsAsFactors = F, encoding = 'UTF-8')
data <- data[order(data$date), ]
data <- data[, c(3:4, 6:7, 11:12, 16:25)]
data$conf_H <- as.factor(data$conf_H)
data$conf_A <- as.factor(data$conf_A)
data$id <- seq(1, nrow(data))
data$real_result <- ifelse(data$home_score==data$away_score, 'E', 
                           ifelse(data$home_score>data$away_score, 'L', 'V')) #Ref away
home <- data[, c(2:3, 5:8, 11, 13, 15, 17:18)]
away <- data[, c(1, 4:6, 9:10, 12, 14, 16:18)]
home_away <- cbind(home[, c(1:4, 7, 10:11)], (home[, c(5:6, 8:9)] - away[, c(5:6, 8:9)]) )
away_home <- cbind(away[, c(1:4, 7, 10:11)], (away[, c(5:6, 8:9)] - home[, c(5:6, 8:9)]) )
names(home_away) <- names(away_home) <- c('team', 'score', 'neutral','year', 'conf', 'id_match', 
                                          'real_result', 'age_mean', 'age_sd', 'ext_big5', 'rank')
all <- rbind(home_away, away_home)
duplicated(all$id_match)
all2 <- all[which(all$year<2018), ]
all2 <- all2[order(all2$id_match), ]
all3 <- all[which(all$year==2018), ]

data$sum <- data$home_score + data$away_score
datat <- data[which(data$Year<2018), ]


results <- as.data.frame(table(datat$real_result, useNA='ifany'))
names(results)[1:2] <- c('Resultado', 'Cantidad')
results$Porcentaje = round(results$Cantidad/sum(results$Cantidad) *100, 1)
results[, 1] <- c('Empate', 'Gano Equipo 1', 'Gano Equipo 2')
results <- results[order(results$Cantidad, decreasing=T), ]
row.names(results) <- NULL

View(results)



ggplot(data=all2, aes(score)) +
  geom_histogram(stat='count', col='black', breaks=seq(0, 8, by = 1)) +
  theme_bw() +
  labs(x="Goles de 1 equipo por partido", y="Frecuencia") +
  coord_cartesian(ylim=c(0, 300)) +
  scale_x_discrete(limits=seq(0,8,1)) +
  scale_y_discrete(limits=seq(0,300,50)) +
  ggtitle('Histograma de goles de un equipo por partido', 
          subtitle='Partidos de mundiales entre USA 1994 y Brasil 2014')



set.seed(2018)
flag <- all2[!duplicated(all2$id_match), ]
split <- createDataPartition(y=flag$score, p=0.75, list=FALSE)
train <- all2[all2$id_match %in% split, ] 
test <- all2[!all2$id_match %in% split, ] 
train$conf2 <- ifelse(train$conf=='CONMEBOL', 'T', 'F')
test$conf2 <- ifelse(test$conf=='CONMEBOL', 'T', 'F')
fit3 <- glm(score ~ ., train[ , c(2, 12, 8, 9:11)], family=poisson(link=log))
summary(fit3)

all3$conf2 <- ifelse(all3$conf=='CONMEBOL', 'T', 'F')
t <- predict(fit3, newdata=all3[ , c(2, 12, 8, 9:11)], type="response")
xt <- cbind(all3, t)
xt <- xt[order(xt$id_match), ]
row.names(xt) <- NULL
mean(xt$t[11])
a <- dpois(seq(0, 7), xt$t[22])
b <- dpois(seq(0, 7), xt$t[20])
c <- matrix(ncol=8, nrow=8)
for (j in 1:8) {
  c[j,] <- a[j]*b[1:8]
}
melted_c <- melt(c)
melted_c$Var1 <- melted_c$Var1 -1
melted_c$Var2 <- melted_c$Var2 -1
melted_c$value <- round(melted_c$value*100, 1)
melted_c$result <- ifelse(melted_c$Var1==melted_c$Var2, 'E', 
                          ifelse(melted_c$Var1>melted_c$Var2, 'L', 'V'))
r <- as.data.frame(aggregate(value ~ result, melted_c, sum))
#out <- c(out, r[which.max(r$value), 'result'])

team1 <- xt$team[22]
team2 <- xt$team[20]
ggplot(data=melted_c, aes(x=Var1, y=Var2, fill=as.numeric(value))) + 
  geom_tile() + labs(x=team1, y=team2) +
  scale_fill_gradient(low="#fff7ec", high="#2171b5", name='Prob. [%]') +
  theme_minimal() +
  scale_x_continuous(labels=c(0:7), breaks=c(0:7)) +
  scale_y_continuous(labels=c(0:7), breaks=c(0:7)) +
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 4) + 
  ggtitle('Matriz de probabilidades de resultados exactos', 
          subtitle='Se obtiene a partir de las probabilidades de convertir X goles para cada equipo según modelo')



r <- r[c(2,1,3), ]
r[1,1] <- paste('Gana', team1)
r[3,1] <- paste('Gana', team2)
r[2,1] <- 'Empate'

p <- ggplot(r, aes(x=result, y=value)) +
  geom_bar(stat="identity", position = position_identity(), fill='#3690c0', alpha=0.6) +
  coord_flip()
p1 = p + theme_bw() +
  labs(x="Resultado", y="Probabilidad [%]") +
  theme(axis.text.x = element_text(vjust=0.5, size=10),
        axis.text.y = element_text(vjust=0.5, size=10),
        axis.title.x = element_text(face="bold", size=12, margin=margin(8,8,0,0)),
        axis.title.y = element_text(face="bold", size=12, margin=margin(8,8,0,0))) +
  
  ggtitle(label="Probabilidad de resultados globales", subtitle = paste(team1, "v/s", team2)) +
  geom_text(aes(label=round(value,0)), hjust=1.2, colour="black",
            position = position_dodge(1), size=4)
p1





data_final <- read.csv('table_final.csv', encoding='utf8')

table(data_final)

 row_spec(row=c(gr), background= '#f0f0f0') %>%
 row_spec(row=c(gr2), background= 'white') %>%
 column_spec(c(2,7), bold = T, color= 'darkblue') %>%
 row_spec(row=c(no), bold = F, color= 'darkred') 
# column_spec(c(1,3:6), bold = F, color= 'black') %>%
   
 simple <- read_html(
     "https://www.lanacion.com.ar/politica/el-enojo-sergio-berni-reunion-definir-nueva-nid2372560")
   simple %>%
     html_nodes(".capital") %>% 
     html_text()
   
   
   
  
   