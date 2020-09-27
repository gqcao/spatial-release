#' Model AQI data and make predictions on new data. 
#' 
#' Author:      Guanqun Cao (guanqun.cao@ieee.org) 
#' Last update: Sep 19, 2020
#'

library(dplyr)
library(spBayes)

DATA_PATH = "../data/"
DATE_LIST <- sapply(seq(as.Date("2016/01/01"), by = "day", length.out = 366), function(a) format(a, "%Y-%m-%d")) # One month in Sept 2016

model_aqi <- function(date_list){
    trn_data_all <- read.csv(paste(DATA_PATH, 'trn.csv', sep=''))
    test_data_all <- read.csv(paste(DATA_PATH, 'test.csv', sep=''))
    model_all = list()
    pred_all = list()

    for (i in 1:length(date_list)){
        print(paste0('start processing date ', i))
        trn_data <- trn_data_all[which(trn_data_all$Date.Local==date_list[i]), ]
        trn_data <- trn_data[!duplicated(trn_data[, 1:2]), ]
        test_data <- test_data_all[which(test_data_all$Date.Local==date_list[i]), ]

        d.max <- max(iDist(trn_data[, c("x.coord","y.coord")]))
        r <- 2
        priors <- list("phi.Unif"=list(rep(3/(0.75*d.max), r), rep(3/(0.001*d.max), r)),
            "sigma.sq.IG"=list(rep(2, r), rep(1, r)),
            "tau.sq.IG"=c(2, 1))
        starting <- list("phi"=rep(3/(0.1*d.max), r), "sigma.sq"=rep(1, r), "tau.sq"=1)
        tuning <- list("phi"=rep(0.1, r), "sigma.sq"=rep(0.05, r), "tau.sq"=0.1)
        n.samples <- 10000

        model <- spSVC(Sqrt_AQI ~ Sqrt_Conc, coords=c("x.coord","y.coord"),
            data=trn_data, starting=starting, svc.cols=c(1,2),
            tuning=tuning, priors=priors, cov.model="exponential",
            n.samples=n.samples, n.report=5000, n.omp.threads=20)
        model <- spRecover(model, start=floor(0.75*n.samples), thin=2,
            n.omp.threads=4, verbose=FALSE)
        model.pred <- spPredict(model, pred.covars=cbind(1, test_data$Sqrt_Conc),
            pred.coords=test_data %>% select(x.coord, y.coord), thin=25,
            joint=TRUE, n.omp.threads=20, verbose=FALSE)
        model_all <- append(model_all, model)
        pred_all <- append(pred_all, model.pred)
    }
    #save(model_all, file = "cal_aqi_model.RData")
    save(pred_all, file = "cal_aqi_pred.RData")
}

model_aqi(DATE_LIST)
