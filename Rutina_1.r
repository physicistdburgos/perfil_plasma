library("pdftools")
library("tidyverse")
library("latex2exp")
txt = pdf_text(pdf = "articulo.pdf")
txt_1 = txt[2]

#txt_1
tab <-txt_1 %>% # Remueve Todos los caracteres Que están después del número
  str_split("\n")
#tab
matriz = c(tab[[1]][5:17])
matriz11=as.matrix(matriz)
Ajk_n10=matrix(0,10,1)
for(i in 1:10){
  Ajk_n10[i,]=matriz11[3+i,]  
}

ajk =function(r){
  as.numeric(unlist(str_extract_all(string =Ajk_n10[r,1],pattern ="[\\-]*\\d\\.\\d{6,7}",simplify = T)))
}

Ajk_n102=matrix(0,10,10)
for (i in 1:10) {
  for (j in 1:10) {
    Ajk_n102[i,j]=ajk(i)[j]
  }
}
# Se corrigen algunos datos que no se pudieron leer 
Ajk_n102[8,7]= -1.164009
Ajk_n102[8,8]= 1.464693
Ajk_n102[8,9]= 0.159977

print(Ajk_n102)

Ajk_bocka = as.matrix(read_csv(file = "Ajk_n20.csv",col_names = T))  
print(Ajk_bocka)

library(tidyverse)
library(ggplot2)
library("latex2exp")

error = function(Anillos){


    aA_c = matrix(0,Anillos,Anillos)
    p =seq(0,Anillos-1)
    q =seq(0,Anillos-1)

    Ajk_h =function(j,k){
      (sqrt(abs((j+1)^2-k^2))-sqrt(j^2-k^2))/(2*j+1)
    }

    Ajk_men1h =function(j,k){
      (sqrt(abs(j^2-k^2))-sqrt((j-1)^2-k^2))/(2*j-1)
    }





    suppressWarnings(for (m in p) {
      for (l in q) {
        ifelse(m==l, {aA_c[l+1,m+1]=-Ajk_h(l,m)},{ aA_c[l+1,m+1]=Ajk_men1h(l,m)-Ajk_h(l,m)})
      }
    })
    aA_c


    ################################ Calculo grafica ################################



    r= rep(0,Anillos)
    r_0 = 1
    for (i in 1:{Anillos-1}) {
      r[i+1]=r[i]+r_0/Anillos
    }
    #r[8]=r[8]+0.001 #errores relacionados cona la función epsilon_3
    #r[10]=r[10]+.00001  



    e_num = matrix(0,1,Anillos)
    #e_num_b = matrix(0,1,Anillos)

    #G1 = (1-r^4)
    G1 = 1600*(1-r^2)
    #e_teo = (4/(3*pi*r_0))*(1+2*r^2)*sqrt(1-r^2)
    e_teo =(3200/pi)*sqrt(1-r^2)
    e_teo

    for (i in 1:Anillos-1) {
      #e_num[i]=(-1/(pi*r_0))*sum(aA_c[,i]*G1,na.rm = TRUE) #na.rm = TRUE Ignor los valores los NA's
      e_num[i]=(-2/((r_0/Anillos)*pi))*sum(aA_c[,i]*G1,na.rm = TRUE)
      #e_num_b[i]=sum(Ajk_n20[,i]*G1,na.rm = TRUE)
    }
   return(e_num)
    #as.numeric(abs(e_num-e_teo))
}

 radioo=function(x){
    rr =rep(0,x)
    r_0 = 1
    for (i in 1:{x-1}) {
      rr[i+1]=rr[i]+r_0/x
   }
as.vector(rr) 
 }

radioo(10)

err_10 = error(10)
error_10 = list(anillos=c(rep("10",10)),
                error = as.vector(err_10),
                radio =as.vector(radioo(10)) )
err_20 = error(20)
error_20 = list(anillos=c(rep("20",20)),
                error = as.vector(err_20),
                radio =as.vector(radioo(20)) )

err_30 = error(30)
error_30 = list(anillos=c(rep("30",30)),
                error = as.vector(err_30),
                radio =as.vector(radioo(30)) )
err_40 = error(40)
error_40 = list(anillos=c(rep("40",40)),
                error = as.vector(err_40),
                radio =as.vector(radioo(40)) )


errores = bind_rows(error_10,error_20,error_30,error_40)
#errores=errores%>%filter(radio<0.87)

a=0.0000000000001
b = 0.0000000000001
errores%>%
  ggplot(aes(radio,error,color = anillos))+
  geom_line()+
  geom_point()+
  ggtitle("Errores con diferentes anillos")+
  #scale_y_continuous(trans = "log10",breaks =c(0,1,10,100,1000))
  scale_y_continuous(trans = "log10",breaks =c(a,0.5*a,3*a,10*a,20*a,444))

max(errores$error)

errores%>%group_by(anillos)%>%
          #filter(radio<0.95)%>%
          summarize(error_medio=mean(error))%>%
          ggplot(aes(anillos,error_medio, group = 1))+geom_point()+geom_line()+
          labs(title ="Desviación estandar sin ruido en función del número de anillos", 
          y=quote('Error medio'),
          x=quote(Anillos))

#rrores%>%group_by(anillos)
errores%>%filter(anillos==10)%>%summarize(n())#summarise(error_medio=sum(error)/n())
#          summarize(error_medio=mean(error))

error_deci = function(Anillos,deci){


    aA_c = matrix(0,Anillos,Anillos)
    p =seq(0,Anillos-1)
    q =seq(0,Anillos-1)

    Ajk_h =function(j,k){
      (sqrt(abs((j+1)^2-k^2))-sqrt(j^2-k^2))/(2*j+1)
    }

    Ajk_men1h =function(j,k){
      (sqrt(abs(j^2-k^2))-sqrt((j-1)^2-k^2))/(2*j-1)
    }





    suppressWarnings(for (m in p) {
      for (l in q) {
        ifelse(m==l, {aA_c[l+1,m+1]=round(-Ajk_h(l,m),deci)},{ aA_c[l+1,m+1]=round(Ajk_men1h(l,m)-Ajk_h(l,m),deci)})
      }
    })
    aA_c

    r= rep(0,Anillos)
    r_0 = 1
    for (i in 1:{Anillos-1}) {
      r[i+1]=r[i]+r_0/Anillos
    }

    e_num = matrix(0,1,Anillos)

    G1 = 1600*(1-r^2)
    e_teo =(3200/pi)*sqrt(1-r^2)
    e_teo

    for (i in 1:Anillos-1) {
      e_num[i]=(-2/((r_0/Anillos)*pi))*sum(aA_c[,i]*G1,na.rm = TRUE)
    }
e_num
    as.numeric(abs(e_num-e_teo))
    
}

err_20_3_deci = error_deci(20,3)
error_20_3_deci = list(decimales=c(rep("3",20)),
                error = as.vector(err_20_3_deci),
                radio =as.vector(radioo(20)) )
err_20_4_deci = error_deci(20,4)
error_20_4_deci = list(decimales=c(rep("4",20)),
                error = as.vector(err_20_4_deci),
                radio =as.vector(radioo(20)) )
err_20_5_deci = error_deci(20,5)
error_20_5_deci = list(decimales=c(rep("5",20)),
                error = as.vector(err_20_5_deci),
                radio =as.vector(radioo(20)) )
err_20_6_deci = error_deci(20,6)
error_20_6_deci = list(decimales=c(rep("6",20)),
                error = as.vector(err_20_6_deci),
                radio =as.vector(radioo(20)) )
err_20_7_deci = error_deci(20,7)
error_20_7_deci = list(decimales=c(rep("7",20)),
                error = as.vector(err_20_7_deci),
                radio =as.vector(radioo(20)) )

errores_deci345 = bind_rows(error_20_3_deci,error_20_4_deci,error_20_5_deci,error_20_6_deci,error_20_7_deci)


errores_deci345%>%
  ggplot(aes(radio,error,color = decimales))+
  geom_line()+
  geom_point()+
  ggtitle("Errores con diferentes decimales")+
  scale_y_continuous(trans = "log2",breaks = c(0.0001,0.001,0.01,0.02,0.05,0.5,1,10,25,50,300))

errores_deci345%>%group_by(decimales)%>%
          #filter(radio<0.95)%>%
          summarize(e_medio=mean(error))%>%
          ggplot(aes(decimales,e_medio, group = 1))+geom_point()+geom_line(linetype="dotted")+
          labs(title ="                 Error medio en función de las cifras significativas", 
          y=quote("Error medio"),
          x=quote('Decimales'))+ 
          theme(#axis.text=element_text(size=13))#,
              axis.title.y=element_text(size=20),
              axis.title.x=element_text(size=20))+
              annotate(geom="text", x=3, y=21.5, label=TeX("Error para $\\epsilon_{1}=\\frac{3200}{pi}\\,\\sqrt{1-r^2}$"),color="red",size=5)



e_num_b = matrix(0,1,20)

G1b = 1600*(1-radioo(20)^2)
e_teob =(3200/pi)*sqrt(1-radioo(20)^2)


for (i in 1:20) {
      e_num_b[i]=sum(Ajk_bocka[,i+1]*G1b,na.rm = TRUE)
    }
error_b =as.numeric(abs(e_num_b-e_teob))

e_num_b

err_20_6_deci = error_deci(20,6)
error_20_nest = list(Método=c(rep("Nestor",20)),
                error = as.vector(err_20_6_deci),
                radio =as.vector(radioo(20)) )


error_20_bock = list(Método=c(rep("Bockasten",20)),
                error = as.vector(error_b),
                radio =as.vector(radioo(20)))


errores_BN = bind_rows(error_20_nest,error_20_bock)


errores_BN%>%
  ggplot(aes(radio,error,color = Método))+
  geom_line()+
  geom_point()+
  #scale_y_continuous("log()")
  scale_y_continuous(trans = "log10",breaks =c(10^3,10^2,10^1,10^-1,10^-2,10^-3,10^-4,10^-5))+
  labs(   x="radio normalizado",
          y="error")+
          theme(#axis.text=element_text(size=13))#,
          axis.title.y=element_text(size=20),
          axis.title.x=element_text(size=20),)+
      theme(legend.position=c(.25, 0.8))+
      annotate(geom="text", x=.55, y=1.5, label=TeX("$\\epsilon_{1\\,teo}(r)=\\sqrt{1-r^{2}}$"),color="black",size=5)+
      annotate(geom="text", x=.55, y=0.4, label=TeX("$\\sigma_{1\\,Nestor}= 7.11E+01$"),color="black",size=5)+
      annotate(geom="text", x=.55, y=0.1, label=TeX("$\\sigma_{1\\,Bocka}= 7.11E-04$"),color="black",size=5)

############

    r= rep(0,20)
    r_0 = 1
    for (i in 1:{20-1}) {
      r[i+1]=r[i]+r_0/20
    }

    e_teo =(3200/pi)*sqrt(1-r^2)





###########



deci=6
Anillos=20
g_anill = function(nn){
    integrales=integrales[,nn]%>%filter(.!=0 )%>%pull(.)         ########  g_anill   ########
    }

    aA_c = matrix(0,Anillos,Anillos)
    p =seq(0,Anillos-1)
    q =seq(0,Anillos-1)

    Ajk_h =function(j,k){
      (sqrt(abs((j+1)^2-k^2))-sqrt(j^2-k^2))/(2*j+1)
    }

    Ajk_men1h =function(j,k){
      (sqrt(abs(j^2-k^2))-sqrt((j-1)^2-k^2))/(2*j-1)
    }





    suppressWarnings(for (m in p) {
      for (l in q) {
        ifelse(m==l, {aA_c[l+1,m+1]=round(-Ajk_h(l,m),deci)},{ aA_c[l+1,m+1]=round(Ajk_men1h(l,m)-Ajk_h(l,m),deci)})
      }
    })
   # aA_c

    r= rep(0,Anillos)
    r_0 = 1
    for (i in 1:{Anillos-1}) {
      r[i+1]=r[i]+r_0/Anillos
    }

    e_num = matrix(0,1,Anillos)

    G1 = 1600*(1-r^2)
    #e_teo = (4/(3*pi*r_0))*(1+2*r^2)*sqrt(1-r^2)
    e_teo =(3200/pi)*sqrt(1-r^2)

    for (i in 1:Anillos-1) {
      e_num[i]=(-2/((r_0/Anillos)*pi))*sum(aA_c[,i]*G1,na.rm = TRUE)
    }
e_teo
e_num
    #as.numeric(abs(e_num-e_teo))
e_num_b

u = e_teo
e_k_b = e_num_b
e_k_n = e_num
s.d.nestor = as.numeric(sqrt((1/20)*sum((e_k_n-u)^2)))
s.d.nestor
s.d.bocka = as.numeric(sqrt((1/20)*sum((e_k_b-u)^2)))
s.d.bocka 
