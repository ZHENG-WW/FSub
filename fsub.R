#y= "oz1143"
#x = train_set
#r_thres = 0.8
#method="spearman"


mci = function(y, x, r_thres=0.8, method="spearman"){
  mean_res = rank_res = summary_res = vector("list", length =4)
  names(mean_res) = names(rank_res)= names(summary_res) =c("be_valid_alone", "be_valid_in_subset", "noise", "redundant")
  com = c(list(mean_res),list(rank_res),list(summary_res))
  names(com) = c("mean","rank","summary")
  #library(dplyr)
  #x = x %>% select_if(lapply(.,anyNA) %>% unlist %>% !.)
  x.data = x[,colnames(x)!=y]
  formu = as.formula(paste(y,"~.",sep = ""))
  score.mean = score.rank =  matrix(0, ncol(x.data),)
  n_noise = n_samples = membership_prob = c()
  
  for (npts in 1:3) {
    if(npts == 1){
      temp.data = x
      n_samples = c(n_samples, nrow(temp.data))
      n_noise=c(n_noise,0)
    }else{
      cluster.res <-dbscan::hdbscan(x.data, minPts = npts) #minPts: Minimum size of clusters
      temp.data = x[!cluster.res[[1]]==0,]
      n_samples = c(n_samples, nrow(temp.data))
      n_noise=c(n_noise,sum(cluster.res[[1]]==0))
      if(n_samples[length(n_samples)]==0){
        break
      }else{
        membership_prob = c(membership_prob,mean(cluster.res$membership_prob[cluster.res[[1]]!=0]))
      }
    }
    if(nrow(temp.data)>0){
      # ======================
      # relief
      # ======================
      imp.relief = as.matrix(abs(CORElearn::attrEval(formu,data.frame(temp.data), estimator="ReliefFexpRank")))
      
      # ======================
      # xgboost: boosting
      # ======================
      temp.x.data = temp.data[,colnames(temp.data)!=y]
      model.xgboost = xgboost::xgboost(data = as.matrix(temp.x.data), label = temp.data[,y], 
                                       nrounds = 100, objective = "binary:hinge") #binary:hinge;reg:squarederror
      result.xgboost = xgboost::xgb.importance(model = model.xgboost)[,c("Feature","Gain")]
      xgboost.res = data.frame(dplyr::filter(result.xgboost, Feature %in% colnames(temp.data)))
      rownames(xgboost.res) = xgboost.res$Feature
      if(nrow(xgboost.res)<nrow(imp.relief)){
        temp.var = setdiff(rownames(imp.relief),rownames(xgboost.res))
        temp.mat = cbind.data.frame(Feature = temp.var, Gain = rep(0,length(temp.var)))
        rownames(temp.mat) = temp.mat$Feature
        xgboost.res = rbind.data.frame(xgboost.res, temp.mat)
      }
      imp.xgboost = xgboost.res[rownames(imp.relief),]
      
      # ======================
      # Boruta?F?ʗL??,randomforest?̕ρE?d?v?x?ɁE?ÁE?ρE?I?????@
      # ======================
      boruta.res<-Boruta::Boruta(formu, data.frame(temp.data), doTrace=1)
      imp.boruta = data.frame(Boruta::attStats(Boruta::TentativeRoughFix(boruta.res)))
      imp.boruta[,1] = abs(imp.boruta[,1])
      
      # ======================
      # Adaptive lasso
      # ======================
      ## Ridge Regression to create the Adaptive Weights Vector
      cv.ridge <- glmnet::cv.glmnet(x = as.matrix(temp.x.data), y = temp.data[,y], family='gaussian', 
                                    alpha=0, parallel=TRUE, standardize=TRUE)
      w3 <- 1/abs(matrix(coef(cv.ridge, s=cv.ridge$lambda.min)[, 1][2:(ncol(temp.x.data)+1)] ))^1 
      w3[w3[,1] == Inf] <- 999999999 ## Replacing values estimated as Infinite for 999999999
      
      lasso.model.cv <- glmnet::cv.glmnet(x = as.matrix(temp.x.data) , y = temp.data[,y], family = "gaussian",
                                          alpha=1, parallel=TRUE, standardize=TRUE, type.measure='mse', penalty.factor=w3)
      coef <- coef(lasso.model.cv, s='lambda.1se')
      imp.lasso = abs(as.matrix(coef)[-1,])
      
      imp.mat =  cbind(relief = imp.relief, 
                       xgboost = imp.xgboost$Gain,
                       boruta = imp.boruta$meanImp,
                       lasso = imp.lasso)
      
      imp.mat = prop.table(imp.mat,2)
      #apply(imp.mat,2,sum)
      score.mean = cbind(score.mean,apply(imp.mat,1,mean))
      imp.rank= apply(imp.mat, 2, function(x){
        rank(x,ties.method = "min")
      })
      imp.rank = prop.table(imp.rank, 2)
      score.rank =cbind(score.rank, apply(imp.rank,1,mean))
    }else{
      break
    }
  }
  membership_prob = membership_prob[!is.na(membership_prob)]
  if(length(membership_prob)>0){
    weight = membership_prob/sum(membership_prob)
    if(length(weight)>1){
      Mean = (score.mean[,-c(1,2)] %*% weight)+score.mean[,2]
      Rank = (score.rank[,-c(1,2)] %*% weight)+score.rank[,2]
    }else{
      Mean = (score.mean[,-c(1,2)] * weight)+score.mean[,2]
      Rank = (score.rank[,-c(1,2)] * weight)+score.rank[,2]
    }
  }else{
    Mean = score.mean[,2]
    Rank = score.rank[,2]
  }
  Summary = Mean + Rank
  score = list(Mean, Rank, Summary)
  
  for(score.num in 1:3){
    vari.imp = cbind.data.frame(feature = rownames(imp.relief),
                                individual = score[[score.num]])
    com[[score.num]]$noise = vari.imp[vari.imp$individual==0,]
    vari.imp = vari.imp[vari.imp$individual>0,]
    
    #compute the correction
    #Are variables and classes correlated?
    varis.cor1 = rownames(dplyr::filter(imp.boruta, decision %in% "Confirmed"))
    varis.cor2 = rownames(imp.xgboost[round(imp.xgboost$Gain,2)>0,])
    tab = table(c(varis.cor1, varis.cor2))
    varis.cor = names(tab[tab==2])
    varis.noncor=rownames(imp.relief)[!(rownames(imp.relief) %in% varis.cor)]
    
    if(length(varis.noncor)>0){
      noncors_set = dplyr::filter(vari.imp, feature %in% varis.noncor) 
      com[[score.num]]$be_valid_in_subset = dplyr::filter(noncors_set, feature %in% names(imp.lasso[imp.lasso>0]))
      com[[score.num]]$noise = rbind(com[[score.num]]$noise,
                                     dplyr::filter(noncors_set, feature %in% names(imp.lasso[imp.lasso==0])))
    }
    #compute the correction
    cor_matrix = as.matrix(abs(cor(x.data, method= method)))
    if(length(varis.cor)>1){
      cors = cor_matrix[varis.cor,varis.cor] #correlated features
      diag(cors) = NA
      cor.filter = function(x, cor){
        apply(x, 1, function(i, r = cor){
          xx = na.omit(i)
          return(as.list(names(xx[xx > r])))
        })
      }
      redundant.varis = cor.filter(cors, r_thres)
      vars = rownames(cors)
      non.varis = unlist(redundant.varis)
      if(length(non.varis) > 0){
        redundant.varis = redundant.varis[sapply(redundant.varis, length) > 0]
        tab = table(unlist(redundant.varis))
        
        #data.frame(dplyr::filter(vari.imp, feature %in% names(tab)))
        nams = names(tab[order(tab,decreasing = T)])
        nams.mat = dplyr::filter(vari.imp, feature %in% nams)
        nams = nams.mat$feature[order(nams.mat$individual,decreasing = T)]
        
        for(i in nams){
          if(i %in% names(redundant.varis))
            redundant.varis = redundant.varis[names(redundant.varis) %in% redundant.varis[i][[1]] == FALSE] 
        }
        com[[score.num]]$redundant = dplyr::filter(vari.imp, feature %in% nams[nams %in% names(redundant.varis) == FALSE])
        
        #non-redundant variables
        noncors.withvaris = vars[-which(vars %in% com[[score.num]]$redundant$feature)] 
      }else{
        noncors.withvaris = vars
      }
      
      if(length(noncors.withvaris)>0){
        com[[score.num]]$be_valid_alone = rbind(com[[score.num]]$be_valid_alone, 
                                                dplyr::filter(vari.imp, feature %in% noncors.withvaris))
      }
    }else{
      com[[score.num]]$be_valid_alone = rbind(com[[score.num]]$be_valid_alone, 
                                              dplyr::filter(vari.imp, feature %in% varis.cor))
    }
  }
 
  call = match.call()
  call[[1]] = as.name("Subdivisions of variables")
  ret = list(summary = com,
             noise = n_noise,
             samples = n_samples,
             score.res = score,
             call = call)
  class(ret) = "filter"
  return(ret)
}
