# TODO: Add comment
#
# Author: Paco
###############################################################################
library("VSURF")
library("randomForest")
library("plyr")
library("parallel")
library("doParallel")
setwd("E:\\CMS2\\CMS2Models")
load("Data/Modeling_filtered.RData")

Variables_to_model <- c(
    "Eff_CBD", "Ht_Eff_CBD_m",
    "CBH_m", "BIOMASS_MG_HA_FIA", "Aboveground_Total_Live",
    "Forest_Down_Dead_Wood", "Forest_Floor",
    "Forest_Shrub_Herb"
)
Variables_to_model <- c(
    "Eff_CBD", "CBH_m",
    "BIOMASS_MG_HA_FIA", "Aboveground_Total_Live",
    "Forest_Down_Dead_Wood", "Forest_Floor"
)
Lidar <- c("Both")
Regions <- c("ALL", "L3_KEY")


models_to_fit <- expand.grid(
    Response = Variables_to_model,
    Lidar = Lidar, Region = Regions, stringsAsFactors = FALSE
)
# Prepares a dataframe with all combinations, there is a
# column that contains data frames for the model fitting
Fit_ALL <- ddply(models_to_fit, c("Response", "Lidar", "Region"),
    function(x, y) {
        Response <- x$Response[1]
        print(x$Lidar)
        if (x$Lidar == "Both") {
            Predictors <- c(
                y$columns$Lidar_ALL,
                y$columns$Lidar_FIRST,
                y$columns$Climate,
                y$columns$Topo, "L3_KEY"
            )
        } else {
            Predictors <- c(
                y$columns[[x$Lidar[1]]],
                y$columns$Climate,
                y$columns$Topo, "L3_KEY"
            )
        }
        print(x$Response)
        if (x$Region == "ALL") {
            df <- y$filtered_modeling_df
            row.names(df) <- df$FileTitle
            df <- df[, c(Response, Predictors)]
            df <- df[!is.na(df$L3_KEY), ]
            L3_KEY <- df[, "L3_KEY"]
            df <- as.data.frame(sapply(df, as.numeric))
            df$L3_KEY <- L3_KEY
            df$L3_KEY <- as.factor(df$L3_KEY)
            print(dim(df))
            # 				df<-df[complete.cases(df),]
            print(dim(df))
            res <- x
            res$fitting_data[1] <- I(list(df))
        } else {
            res <- x
            res <- res[rep(1, length(y$filtered_by_ECO[[x$Region]])), ]
            cont <- 1
            fitting_data <- list()
            res$n_preds <- c()
            res$type <- x$REgion
            for (i in y$filtered_by_ECO[[x$Region]]) {
                res[cont, "Region"] <- names(y$filtered_by_ECO[[x$Region]])[cont]
                df <- i[, c(Response, Predictors[-length(Predictors)])]
                row.names(df) <- i$FileTitle
                df <- as.data.frame(sapply(df, as.numeric))
                df <- df[complete.cases(df), ]
                print(dim(df))
                fitting_data <- c(fitting_data, I(list(df)))
                cont <- cont + 1
            }
            res$fitting_data <- fitting_data
        }
        res$type <- x$Region
        res$n_preds <- length(Predictors)
        res
    },
    y = Modeling
)
Fit_ALL <- Fit_ALL[!Fit_ALL$Region == "NA", ]
Fit_ALL <- Fit_ALL[Fit_ALL$Response %in% c("Aboveground_Total_Live", "Forest_Floor"), ]
Fit_ALL <- Fit_ALL[Fit_ALL$type %in% c("ALL"), ]

# Runs VSURF for each combination of type of Region (ALL vs specific Ecoregions)
# adds a list column to the data frame with the modeled object
loaded <- .packages()
ncores <- 2
cl <- makePSOCKcluster(ncores, outfile = "Output/Models_VSURF.txt")
doParallel::registerDoParallel(cl)
Sys.time()
VSURF_ALL <- ddply(Fit_ALL, c("Response", "Lidar", "Region"), function(x) {
    output_VSURF <- paste("Output/Models_VSURF/", x$Response[1],
        "_", x$Lidar[1], "_", x$Region[1], ".Rdata",
        sep = ""
    )
    output_VSURF <- gsub("Arizona/New Mexico Plateau", "Arizona_New Mexico Plateau", output_VSURF, fixed = TRUE)
    data <- x$fitting_data[[1]]
    data <- data[complete.cases(data), ]
    print(dim(data))

    x$n_plots <- dim(data)[1]
    set.seed(54321)
    print(output_VSURF)
    if (file.exists(output_VSURF)) {
        print("One")
        load(output_VSURF)
    } else {
        print("Two")
        print(form)
        VSURF_OBJ <- try(VSURF(x = data[, -1], data = data))
        save(VSURF_OBJ, file = output_VSURF)
    }

    x$VSURF_OBJ <- I(list(VSURF_OBJ))
    x
})
Sys.time()
stopCluster(cl)
save(VSURF_ALL, file = "Output/Models_VSURF/All_models_VSURF.Rdata")
#
# gets the RF models with the selected variables using interp and pred
# cross validate the objects and store results in list columns
# results from cross validation are data frames with obs pred columns
load("Output/Models_VSURF/All_models_VSURF.Rdata")
loaded <- .packages()
ncores <- 8
cl <- makePSOCKcluster(ncores, outfile = "Output/Models_VSURF.txt")
doParallel::registerDoParallel(cl)
Sys.time()
VSURF_ALL <- ddply(VSURF_ALL, c("Response", "Lidar", "Region"), function(x) {
    if (!(x$Region == "ALL" & x$Response == "Aboveground_Total_Live")) {
        return(x)
    }
    a <- try({
        data <- x$fitting_data[[1]]
        if (!x$Region == "ALL") {
            data <- data[, -dim(data)[2]]
        }
        data <- data[complete.cases(data), ]
        data_interp <- data[, c(1, x$VSURF_OBJ[[1]][["varselect.interp"]])]
        RF_interp <- randomForest(data_interp[, -1], data_interp[, 1])
        data_pred <- data[, c(1, x$VSURF_OBJ[[1]][["varselect.pred"]])]
        RF_pred <- randomForest(x = data_pred[, -1], y = data_pred[, 1])

        CV_intrep <- CV_pred <- data.frame(
            obs = data[, 1],
            pred = NA, fold = NA
        )

        folds <- 20
        fold_id <- sample(1:folds, dim(data)[1], replace = TRUE)


        for (i in 1:folds) {
            print(i)
            in_sample <- !fold_id == i
            out_of_sample <- fold_id == i
            RF_interp_i <- randomForest(data_interp[in_sample, -1], data_interp[in_sample, 1])
            RF_pred_i <- randomForest(x = data_pred[in_sample, -1], y = data_pred[in_sample, 1])
            CV_intrep[out_of_sample, "pred"] <- predict(RF_interp_i, data_interp[out_of_sample, -1])
            CV_pred[out_of_sample, "pred"] <- predict(RF_pred_i, data_pred[out_of_sample, -1], type = "response")
            CV_intrep[out_of_sample, "fold"] <- i
            CV_pred[out_of_sample, "fold"] <- i
        }
    })

    if (!inherits(a, "try-error")) {
        x$RF_interp <- I(list(RF_interp))
        x$RF_pred <- I(list(RF_pred))
        x$CV_interp <- I(list(CV_intrep))
        x$CV_pred <- I(list(CV_pred))
    } else {
        x$RF_interp <- I(list(1))
        x$RF_pred <- I(list(1))
        x$CV_interp <- I(list(1))
        x$CV_pred <- I(list(1))
    }
    x
}, .parallel = TRUE, .paropts = list(.packages = "VSURF", "randomForest"))
Sys.time()
stopCluster(cl)
save(VSURF_ALL, file = "Output/Models_VSURF/VSURF_RF_obj_ALL_CV.Rdata")


CV1 <- VSURF_ALL$CV_pred[[1]]
CV1$L3_KEY <- VSURF_ALL$Region[1]

CV2 <- VSURF_ALL$CV_pred[[2]]
CV2$L3_KEY <- VSURF_ALL$Region[2]

CV4 <- VSURF_ALL$CV_pred[[4]]
CV4$L3_KEY <- VSURF_ALL$Region[4]

CV5 <- VSURF_ALL$CV_pred[[5]]
CV5$L3_KEY <- VSURF_ALL$Region[5]


CV_Eco <- rbind(CV1, CV2, CV4, CV5)
