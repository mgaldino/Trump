## Trump

library(ggplot2)
library(dplyr)
library(lubridate) 
library(tidyr)
library(stringr)

## como erraram
# http://www.economist.com/blogs/graphicdetail/2016/11/daily-chart-6?fsrc=scn%2Ffb%2Fte%2Fbl%2Fed%2Fwherethepollswentwrong


predict <- read.table(file = "clipboard", sep = "\t", header=TRUE, dec=",")
save(file="previsoes.RData", predict)
predict$x538_vote <- qnorm(predict$X538, .5, .029)

ggplot(predict, aes(x=x538_vote , y=two_party_share, label = State)) + geom_text() +
  geom_abline(intercept = 0, 1)
ggplot(predict, aes(x=x538_vote , y=two_party_share, label = State)) + geom_point() +
  geom_abline(intercept = 0, 1)

mean(predict$x538_vote - predict$two_party_share, na.rm=T)

median(predict$x538_vote - predict$two_party_share, na.rm=T)
## diferen?a entre previsto e observado


# http://www.nytimes.com/interactive/2016/upshot/presidential-polls-forecast.html#state-by-state

turnout <- read.table(file = "clipboard", sep = "\t", header=TRUE, dec=",")
head(turnout)

save(file="turnout.RData", turnout)

voto <- read.table(file = "clipboard", sep = "\t", header=TRUE, dec=",")

save(file="voto.RData", voto)
head(voto)
voto <- voto %>%
  mutate(candidatos = as.character(candidatos))

# sapply(strsplit(voto$candidatos, " ") , function(x){
#   toupper(paste(substring(x, 1, 1), collapse = ""))
# })
  

voto1 <- voto %>%
  select(-2) %>%
  spread(party, voto)

voto2 <- voto %>%
  select(-4) %>%
  spread(party, candidatos)
  

setwd("C:\\Users\\mgaldino\\2016\\pessoal\\Trump")

renda <- read.table("DPIC96.csv", sep=",", header=T)

save(file="renda.RData", renda)

renda$DATE1 = as.POSIXct(renda$DATE)

# renda <- renda %>% 
#   group_by(month=month(DATE1)) %>%
#   arrange(DATE1) %>%
#   mutate(yearOverYear = DPIC96/lag(DPIC96,1) -1)

renda <- renda %>% 
 mutate(annualized_quarter_growth = (DPIC96/lag(DPIC96,1))^4 -1)

head(renda)

rendapc <- read.table("pic_pc.csv", sep=",", header=T) 

save(file="renda_pc.RData", rendapc)


names(rendapc)[2] <- "renda_percapita"
head(rendapc)

renday <- renda %>%
  mutate(ano = year(as.Date(DATE))) %>%
  group_by(ano) %>%
  filter(rank(ano, ties.method="first")==3) ## second quarter growth


df_vote <- renday %>%
  inner_join(voto1, by="ano") %>%
  mutate(Demo_vote_two_party_share = D/(D + R))

R <- "R"
D <- "D"
df_vote$party_incumbent = c(D, D, R, R, D, D, R, R, D, R, R, R, D, D, R, R, D, D )

df_vote <- df_vote %>%
  mutate(incubent_two_party_vote = ifelse(party_incumbent == "D", D/(D + R), R/(D + R) ))

# plot

df_vote %>%
  ggplot(aes(x=ano, y=Demo_vote_two_party_share, label="D")) + geom_point() +
  ylim(.3, .7)

df_vote %>%
  ggplot(aes(x=quarter_growth, y=incubent_two_party_vote)) + geom_point() + 
  geom_smooth(method = "lm")

# rank(x, ties.method="first")==1

## previs?es correlacionadas
library(mvtnorm)


# correlated
library(mvtnorm)
library(mnormt)
require(MBESS)

# n estados
n <- 5

# estimativa m?dia nos estados. Usando 538
# http://projects.fivethirtyeight.com/2016-election-forecast/
fl <- .481/(.481+.475)
pa <- .489/(.489+.452)
mi <- .484/(.484+.442)
nc <- .482/(.482+.475)
nh <- .475/(.475+.439)
mean <- c(fl, pa, mi, nc, nh)

## estimativas independentes
percentual_vote_to_elect <- .5
sd_margin_error <- .03

result <- numeric()
for ( i in 1:5) {
  result[i] <- 1 - pnorm(percentual_vote_to_elect, mean[i], sd_margin_error)
}

# chance de perder 5 estados
cumprod(result)[n]


# correla??o entre os estados
state_cor <- .8

x <- c(1, rep(state_cor, n-1))
sd = rep(.03, n)
cor_matrix <- matrix(c(x, x[c(2,1,3:n)], x[c(3:2,1,(n-1):n)], x[c(2:(n-1),1,n)], x[c(2:n, 1)]),n)

# cria matrix de vari?ncia e covari?ncia
vcov <- cor2cov(cor_matrix , sd)

round(pmnorm(rep(.5, 5), mean = mean, varcov = vcov)[1], 2)

pnorm(.5, mean(mean), .03)

hist(rnorm(10000, .52, .03))
sum(rnorm(100000, .52, .03) < .5)/100000

x <- rt(100000, 300)+.52/.03
y <- rnorm(1000)*.03 + .52
summary(y)

x <- rnorm(10001, .52, .03)
x_pad <- (x - .52)/.03
t_r <- x_pad/sqrt(sum((x_pad)^2)/10000)


hist(t_r)
sum( < 0)/100000

myncp <- (.5 - .52)
hist(rt(1000, 7, ncp=1))
pnorm(.5, .52, .03)
pt(.5, 7, myncp)

rho <- 0.8
mu1 <- .51; s1 <- .03
mu2 <- .52; s2 <- .03
matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2)

### blog post
prev_538 <- c(.85, .79, .55, .7)
1 - mean(prev_538)
round(100*cumprod(1- prev_538)[4],1)

pnorm(percentual_vote_to_elect, mean[i], 3)

49.6/(44.3+49.6)
qnorm(.835, .5, .029)
