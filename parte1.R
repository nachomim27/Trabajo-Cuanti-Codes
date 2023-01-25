#datos
data=progra_explained
sexob=factor(data$Sexo, labels=c("MUJ","VAR"))
#cargar librerias


library(caret)
library(glmnet)
library(pacman)
library(olsrr)
library(xtable)

#estadisticas descriptivas

datos=data.frame(data$notas_progra, data$notas_estad,data$notas_mateIII,data$notas_conta)
summary(datos)

plot(density(data$notas_conta),col=2, main="Distribución de notas",lwd=3)
lines(density(data$notas_mateIII),col=3, lwd=3)
lines(density(data$notas_progra),col=4, lwd=3)
lines(density(data$notas_estad),col=5, lwd=3)
legend("topleft", legend=c("Programación","Estadística","Matemática","Contabilidad"),col=c(4,5,3,2), lty=1:1, cex=0.8)

#covarianzas

estad=data$notas_estad
conta=data$notas_conta
mate=data$notas_mateIII
cov(estad,conta)
cov(estad,mate)
cov(mate,conta)

#RL normal modelo base

RL=lm(notas_progra ~ notas_estad + notas_mateIII + notas_conta, data=data)
RL2=lm(notas_progra ~ notas_estad + notas_mateIII, data=data)
RL3=lm(notas_progra ~ notas_conta, data=data)

#olsrr package

ols_step_all_possible(RL)
ols_step_best_subset(RL)
ols_step_forward_p(RL)
ols_step_backward_p(RL)

#dividir datos

n=nrow(progra_explained)
ntrain=50
train.obs=sample(1:n,ntrain)
datatrain=progra_explained[train.obs,]
datatest=progra_explained[-train.obs,]

#ridge/lasso con validación cruzada

x=model.matrix(notas_progra ~ notas_estad+ notas_mateIII + notas_conta, datatrain)[,-1]
y=datatrain$notas_progra

ridge=glmnet(x,y,alpha=0)
lasso=glmnet(x,y,alpha=1)

testeo=model.matrix(notas_progra ~ notas_estad+ notas_mateIII + notas_conta, datatest)[,-1]

ml1=cv.glmnet(x,y,alpha=0)
ml2=cv.glmnet(x,y,alpha=1)
plot(ml2)
mlr=ml1$lambda.min
mll=ml2$lambda.min

predr=predict(ridge, s=mlr, newx=testeo)
predl=predict(lasso, s=mll, newx=testeo)

data.frame(RMSE=RMSE(predr,datatest$notas_progra),Rsquare = R2(predr,datatest$notas_progra))
data.frame(RMSE=RMSE(predl,datatest$notas_progra),Rsquare = R2(predl,datatest$notas_progra))

coef(ridge, s=mlr)
coef(lasso, s=mll)

#modelo extendido

notasmate2 = data$notas_mateIII^2
notasestad2 = data$notas_estad^2

notasmate3 = data$notas_mateIII^3
notasestad3 = data$notas_estad^3

notasmate4 = data$notas_mateIII^4
notasestad4 = data$notas_estad^4

RL=lm(notas_progra ~ notas_estad + notas_mateIII + notas_conta + sexo_var + r_santiago, data=data)
RL2=lm(notas_progra ~ notas_estad + notas_mateIII, data=data)
RL3=lm(notas_progra ~ sexo_var + r_santiago, data=data)
RL4=lm(notas_progra ~notas_estad + notas_mateIII + notas_conta + sexo_var*r_santiago, data=data)

#olsrr package

ols_step_all_possible(RL)
ols_step_best_subset(RL)
ols_step_forward_p(RL)
ols_step_backward_p(RL)
ols_step_both_p(RL)

ols_step_all_possible(RL4)
ols_step_best_subset(RL4)
ols_step_forward_p(RL4)
ols_step_backward_p(RL4)
ols_step_both_p(RL4)


#regresion exponencial

RLC2=lm(notas_progra ~ notas_estad + notasestad2 + notas_mateIII + notasmate2, data=data)
RLC3=lm(notas_progra ~ notas_estad + notasestad2 + notasestad3 + notas_mateIII + notasmate2 + notasmate3, data=data)
RLC4=lm(notas_progra ~ notas_estad + notasestad2 + notasestad3 + notasestad4+ notas_mateIII + notasmate2 + notasmate3 + notasmate4, data=data)

ols_step_forward_p(RLC4)
ols_step_backward_p(RLC4)
ols_step_both_p(RLC4)
ols_step_best_subset(RLC4)

#dividir datos

n=nrow(progra_explained)
ntrain=50
train.obs=sample(1:n,ntrain)
datatrain=progra_explained[train.obs,]
datatest=progra_explained[-train.obs,]

#ridge/lasso con validación cruzada

x=model.matrix(notas_progra ~ notas_estad+ notas_mateIII + notas_conta + sexo_var + r_santiago, datatrain)[,-1]
y=datatrain$notas_progra

ridge=glmnet(x,y,alpha=0)
lasso=glmnet(x,y,alpha=1)

testeo=model.matrix(notas_progra ~ notas_estad+ notas_mateIII + notas_conta + sexo_var + r_santiago, datatest)[,-1]

ml1=cv.glmnet(x,y,alpha=0)
ml2=cv.glmnet(x,y,alpha=1)
plot(ml2)
mlr=ml1$lambda.min
mll=ml2$lambda.min

predr=predict(ridge, s=mlr, newx=testeo)
predl=predict(lasso, s=mll, newx=testeo)

data.frame(RMSE=RMSE(predr,datatest$notas_progra),Rsquare = R2(predr,datatest$notas_progra))
data.frame(RMSE=RMSE(predl,datatest$notas_progra),Rsquare = R2(predl,datatest$notas_progra))

coef(ridge, s=mlr)
coef(lasso, s=mll)