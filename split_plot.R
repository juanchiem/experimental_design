# https://www.nature.com/articles/nmeth.3293
# https://www.youtube.com/watch?v=PReZJHaETnQ
# https://stat.ethz.ch/~meier/teaching/anova/index.html

pacman::p_load(doebioresearch, tidyverse, lme4)
oats <- rio::import("data/SP.xlsx") %>% 
  janitor::clean_names() %>% 
  rename(bk=replication,
         geno=variety) %>% 
  mutate_if(is.character, as.factor)

oats %>% str

oats %>% 
ggplot(aes(x = nitrogen, y = yield, group = geno, colour = geno)) + 
  geom_line() + 
  facet_wrap(~ bk) + 
  theme_bw()

# lme4

fit.oats <- lmer(yield ~ geno * nitrogen + 
                        (1|bk/geno), 
                        data = oats)
car::Anova(fit.oats)
emmeans::emmeans(fit.oats, "geno")

# doebioresearch
#Store split plot analysis in variable named output along with LSD test
output <- splitplot(oats[4],           # variable resp
                    oats$bk,  # bloque
                    oats$geno,      # FA
                    oats$nitrogen,     # FB
                    1)               # Post-hoc test
output

# nlme
library(nlme)
m1 <- lme(yield ~ nitrogen + geno, 
                  random = ~1 | bk/geno,
           data = oats)
m1

emmeans::emmeans(m1, "geno")
