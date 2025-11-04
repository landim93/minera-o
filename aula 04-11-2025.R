# dados de consumodo de água

set.seed(10)
r=rnorm(1000,12,3)
attach(cae)
summary(cae)
plot(density(r))
summary(r)
rc=numeric(length(r))
rc <- ifelse(r < 10, "[Até 10)",
             ifelse(r < 14, "[10 a 14)",
                    ifelse(r < 20, "[14 a 20)", "[20 a +)")))
table(rc)
hist(table(rc))
valor<-r*3+runif(1000,-0.3,0.4)
valorc<-numeric(length(r))
valorc<-ifelse(valor<40,"Até 40 reais","Acima 40 reais")
table(valorc)
table(valor,r)
chisq.test(valorc,rc)
plot(valor~r)
qchisq(0.95,3)# gl=(l-1)-(c-1)
m1=lm(valor~r)
plot(m1)
summary(m1)
cor(valor,r)# correlação forte e direta

