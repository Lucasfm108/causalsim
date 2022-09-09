##Pacotes necessários
packages <- c("dplyr","faux")

#Instala pacotes se não instalados
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

#Carregando os pacotes
invisible(lapply(packages, library, character.only = TRUE))


##Função causalsim
causalsim <- function(n, treat_effect, prob_treat, 
                      covar_dist,covar_prob, covar_mean,covar_sd,
                      covar_effect,
                      treat_covar=rep(0,length(covar_dist)),
                      intercept) {
  
  ##Construir a tabela com o id, distribuição do tratamento e erro
  #com dist normal, 0 de média e 1 de sd;
  temp <- data.frame(id=1:n,
                     treat=rbinom(n,1,prob_treat)) %>% 
    mutate(error=rnorm(n,0,1))
  
  temp2 <- temp #Tabela temp para armanezar as covar*efeito sobre Y
  
  ##Identificando as indexacoes dos vetores das covariaveis
  cov_num <- which(covar_dist=="norm")
  cov_bin <- which(covar_dist=="binom")
  
  ##Simulando as variaveis numericas
  if(length(cov_num)>0){
    for (i in 1:length(covar_mean)){
      
      
      temp <- temp %>% mutate(x=rnorm_pre(treat, covar_mean[i],
                                          covar_sd[i],
                                          r=treat_covar[cov_num[i]])) 
      
      temp2 <- temp2 %>% mutate(y=temp$x*covar_effect[cov_num[i]])
      
      names(temp)[3+i] <- paste0("x",cov_num[i])
      names(temp2)[3+i] <- paste0("y",cov_num[i])
    }
  } else{}
  
  ##Simulando as variaveis binarias
  if(length(cov_bin)>0){
    
    for (j in 1:length(covar_prob)){
      
      ##Código quando há variável numérica
      if(length(cov_num)>0){
        
        temp <- temp %>% mutate(x=rnorm_pre(treat,
                                            covar_prob[j], 1, r=treat_covar[cov_bin[j]])) %>% 
          mutate(x=norm2binom(x, 1,prob=covar_prob[j])) #Converter para binomial
        
        temp2 <- temp2 %>% mutate(y=temp$x*covar_effect[cov_bin[j]])
        
        names(temp)[3+i+j] <- paste0("x",cov_bin[j])
        names(temp2)[3+i+j] <- paste0("y",cov_bin[j])
        
        ##Código quando não há variável numérica  
      } else{
        
        temp <- temp %>% mutate(x=rnorm_pre(treat,
                                            covar_prob[j], 1, r=treat_covar[cov_bin[j]])) %>% 
          mutate(x=norm2binom(x, 1,prob=covar_prob[j]))
        
        temp2 <- temp2 %>% mutate(y=temp$x*covar_effect[cov_bin[j]])
        
        names(temp)[3+j] <- paste0("x",cov_bin[j])
        names(temp2)[3+j] <- paste0("y",cov_bin[j])  
        
      }
      
    }} else{} 
  
  temp$outcome <- intercept +
    temp$treat*treat_effect+apply(temp2[4:length(temp2)],1,sum)+
    temp$error #Criar a variável resposta
  temp %>% select(-error)
}
