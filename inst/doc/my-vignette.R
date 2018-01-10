## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = F,
  comment = "#>"
)

## ---- message=FALSE, warning=FALSE---------------------------------------
library(multiROC)
data(test_data)
head(test_data)

## ------------------------------------------------------------------------
res <- multi_roc(test_data, force_diag=T)

## ------------------------------------------------------------------------
unlist(res$AUC)

## ------------------------------------------------------------------------
multi_roc_auc <- function(true_pred_data, idx) {
  results <- multi_roc(true_pred_data[idx, ])$AUC
  results <- unlist(results)
  return(results)
}

res_boot <- boot::boot(data=test_data, statistic=multi_roc_auc, R=1000)
res_boot_ci <- boot::boot.ci(res_boot, type='bca', index=4)
res_boot_ci

## ------------------------------------------------------------------------
n_method <- length(unique(res$Methods))
n_group <- length(unique(res$Groups))
res_df <- data.frame(Specificity= numeric(0), Sensitivity= numeric(0), Group = character(0), AUC = numeric(0), Method = character(0))
for (i in 1:n_method) {
      for (j in 1:n_group) {
        temp_data_1 <- data.frame(Specificity=res$Specificity[[i]][j],
                                  Sensitivity=res$Sensitivity[[i]][j],
                                  Group=unique(res$Groups)[j],
                                  AUC=res$AUC[[i]][j],
                                  Method = unique(res$Methods)[i])
        colnames(temp_data_1) <- c("Specificity", "Sensitivity", "Group", "AUC", "Method")
        res_df <- rbind(res_df, temp_data_1)

      }
      temp_data_2 <- data.frame(Specificity=res$Specificity[[i]][n_group+1],
                                Sensitivity=res$Sensitivity[[i]][n_group+1],
                                Group= "Macro",
                                AUC=res$AUC[[i]][n_group+1],
                                Method = unique(res$Methods)[i])
      temp_data_3 <- data.frame(Specificity=res$Specificity[[i]][n_group+2],
                                Sensitivity=res$Sensitivity[[i]][n_group+2],
                                Group= "Micro",
                                AUC=res$AUC[[i]][n_group+2],
                                Method = unique(res$Methods)[i])
      colnames(temp_data_2) <- c("Specificity", "Sensitivity", "Group", "AUC", "Method")
      colnames(temp_data_3) <- c("Specificity", "Sensitivity", "Group", "AUC", "Method")
      res_df <- rbind(res_df, temp_data_2)
      res_df <- rbind(res_df, temp_data_3)
    }

## ---- fig.width=7, fig.height=5.5----------------------------------------
ggplot2::ggplot(res_df, ggplot2::aes(x = 1-Specificity, y=Sensitivity)) + ggplot2::geom_path(ggplot2::aes(color = Group, linetype=Method)) + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 1), colour='grey', linetype = 'dotdash') + ggplot2::theme_bw() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), legend.justification=c(1, 0), legend.position=c(.95, .05), legend.title=ggplot2::element_blank(), legend.background = ggplot2::element_rect(fill=NULL, size=0.5, linetype="solid", colour ="black"))

