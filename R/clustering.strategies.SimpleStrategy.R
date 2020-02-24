#' @title <<tittle>>
#'
#' @description SimpleStrategy
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{ClusteringStrategy}}
#'
#' @keywords NULL
#'
#' @import R6
#'
#' @export SimpleStrategy

SimpleStrategy <- R6::R6Class(
  classname = "SimpleStrategy",
  inherit = ClusteringStrategy,
  portable = TRUE,
  public = list(
    #'
    #' @description <<description>>
    #'
    #' @param subset <<description>>
    #' @param heuristic <<description>>
    #' @param configuration <<description>>
    #'
    initialize = function(subset, heuristic, configuration = StrategyConfiguration$new()) {
      description <- "<<Pending>>"
      super$initialize(subset = subset, heuristic = heuristic,
                        description = description, configuration = configuration)
    },
    #'
    #' @description <<description>>
    #'
    #' @param verbose <<description>>
    #' @param ... <<description>>
    #'
    #' @return <<return>>
    #'
    #' @importFrom varhandle to.dummy
    #'
    execute = function(verbose = FALSE, ...) {
      private$all.distribution <- data.frame(k = integer(), deltha = numeric(), dist = I(list()))

      colIndex <- which(levels(as.factor(private$subset$getClassValues())) == private$subset$getPositiveClass())
      class <- varhandle::to.dummy(as.character(private$subset$getClassValues()),
                                    as.character(private$subset$getPositiveClass()))[, colIndex]

      minClusters <- private$configuration$minNumClusters()
      maxClusters <- private$configuration$maxNumClusters()

      ## COMPUTING HEURISTIC (BETWEEN EACH FEATURE AND THE CLASS)
      corpus <- private$subset$getFeatures()
      heuristic.values <- sapply(names(corpus), function(colName, class) {
        abs(private$heuristic[[1]]$heuristic(col1 = corpus[, colName], col2 = class,
                                              column.names = c(colName, private$subset$getClassName())))
      }, class)

      heuristic.valid <- heuristic.values[complete.cases(heuristic.values)]
      notHeuristic <- setdiff(names(heuristic.values), names(heuristic.valid))
      sorted.values <- heuristic.valid[order(heuristic.valid, decreasing = TRUE)]

      ## DISTRIBUTE FEATURES IN CLUSTERS (2 <= k <= maxClusters)
      if (isTRUE(verbose)) {
        message("[", class(self)[1], "][INFO] Performing feature clustering using '",
                 class(private$heuristic[[1]])[1], "' heuristic")
        pb <- txtProgressBar(min = 0, max = (maxClusters - 1), style = 3)
      }

      if (length(heuristic.valid) > 0) {
        for (k in minClusters:maxClusters) {
          clustering <- rep(c(1:k, (k:1)), length(sorted.values) / (2 * k) + 1)[1:length(sorted.values)]
          cluster <- vector(mode = "list", length = length(sorted.values))
          names(cluster) <- names(sorted.values)
          sumGroup <- vector(mode = "list", length = k)
          for (i in 1:k) {
            sumGroup[[i]] <- sorted.values[clustering == i]
            for (j in names(sorted.values[clustering == i])) { cluster[[j]] <- c(i) }
          }
          groupMeasure <- sapply(sumGroup, sum)
          deltha <- (max(groupMeasure) - min(groupMeasure))
          df <- data.frame(k = k, deltha = deltha, dist = I(list(cluster)))
          private$all.distribution <- rbind(private$all.distribution, df)
          if (isTRUE(verbose)) { setTxtProgressBar(pb, (k - 1)) }
        }

        if (isTRUE(verbose)) { close(pb) }

        for (i in 1:nrow(private$all.distribution)) {
          aux.dist <- unlist(private$all.distribution[i, ]$dist, recursive = FALSE)
          aux.list <- list()
          for (j in 1:private$all.distribution[i, ]$k) {
            aux.list <- append(aux.list, list(names(aux.dist[ aux.dist == j ])))
            private$all.distribution[i, ]$dist <- I(list(aux.list))
          }
        }

        bestK <- which.min(private$all.distribution$deltha)
        aux.dist <- unlist(private$all.distribution[bestK, ]$dist,
                            recursive = FALSE)
        private$best.distribution <- data.frame(cluster = integer(),
                                                 dist = I(list()))
        for (i in 1:length(aux.dist)) {
          df <- data.frame(cluster = i, dist = I(list(aux.dist[[i]])))
          private$best.distribution <- rbind(private$best.distribution, df)
        }
      }
      if (length(notHeuristic) > 0) {
        message("[", class(self)[1], "][WARNING] ",
                 length(notHeuristic), " features were incompatible with '",
                 class(private$heuristic[[1]])[1], "' heuristic")
        private$not.distribution <- data.frame(cluster = 1,
                                                dist = I(list(notHeuristic)))
      } else private$not.distribution <- data.frame()
    },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getBestClusterDistribution = function() {
      list(private$best.distribution)
    },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getUnclustered = function() {
      list(private$not.distribution)
    },
    #'
    #' @description <<description>>
    #'
    #' @param num.clusters <<description>>
    #' @param num.groups <<description>>
    #' @param include.unclustered <<description>>
    #'
    #' @return <<return>>
    #'
    getDistribution = function(num.clusters = NULL, num.groups = NULL,
                                include.unclustered = FALSE) {
      if (is.null(private$best.distribution) || is.null(private$all.distribution)) {
        stop("[", class(self)[1], "][FATAL] Clustering not done or errorneous. ",
             "Aborting...")
      }

      if (is.null(num.clusters)) {
        distribution <- lapply(private$best.distribution$dist, function(x) {x})
      } else {
        if (is.numeric(num.clusters) && (num.clusters %in% c(2:tail(private$all.distribution$k, n = 1)))) {
          distribution <- unlist(private$all.distribution[which(num.clusters == private$all.distribution$k), ]$dist, recursive = FALSE)
        } else {
          message("[", class(self)[1], "][WARNING] Number of clusters not found. ",
                  "Assuming best cluster distribution")
          distribution <- unlist(private$all.distribution[which.min(private$all.distribution$deltha), ]$dist, recursive = FALSE)
        }
      }

      if (!is.null(num.groups) && is.numeric(num.groups) &&
           num.groups %in% c(1:length(distribution))) {
        distribution <- distribution[num.groups]
      }

      if (isTRUE(include.unclustered) && nrow(private$not.distribution) > 0) {
        distribution <- append(distribution, lapply(private$not.distribution$dist,
                                                   function(x) {x}))
      }
      return(distribution)
    },
    #'
    #' @description <<description>>
    #'
    #' @param subset <<description>>
    #' @param num.clusters <<description>>
    #' @param num.groups <<description>>
    #' @param include.unclustered <<description>>
    #'
    #' @return <<return>>
    #'
    createTrain = function(subset, num.clusters = NULL, num.groups = NULL,
                            include.unclustered = FALSE) {
      if (!inherits(subset, "Subset")) {
        stop("[", class(self)[1], "][FATAL] Subset parameter must be defined as ",
             "'Subset' type. Aborting...")
      }

      if (is.null(private$best.distribution) || is.null(private$all.distribution)) {
        stop("[", class(self)[1], "][FATAL] Clustering not done or errorneous. ",
             "Aborting...")
      }
      distribution <- self$getDistribution(num.clusters = num.clusters,
                                            num.groups = num.groups,
                                            include.unclustered = include.unclustered)

      train.dist <- lapply(distribution, function(group) {
        subset$getFeatures(feature.names = group)
      })

      Trainset$new(cluster.dist = train.dist, class.name = subset$getClassName(),
                    class.values = subset$getClassValues(),
                    positive.class = subset$getPositiveClass())
    },
    #'
    #' @description <<description>>
    #'
    #' @param dir.path <<description>>
    #' @param file.name <<description>>
    #' @param ... <<description>>
    #'
    #' @return <<return>>
    #'
    #' @import ggplot2
    #'
    plot = function(dir.path = NULL, file.name = NULL, ...) {

      summary <- data.frame(k = private$all.distribution$k,
                            dispersion = private$all.distribution$deltha,
                            row.names = NULL)
      plot <- BinaryPlot$new()$plot(summary) +
        ggplot2::labs(title = "Data") + ggplot2::theme_light() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0.5))
      if (!is.null(dir.path)) {
        if (!dir.exists(dir.path)) {
          dir.create(dir.path, recursive = TRUE)
          if (dir.exists(dir.path)) {
            message("[", class(self)[1], "][INFO] Directory '", dir.path,
                    "'has been succesfully created")
          } else {
            stop("[", class(self)[1], "][FATAL] Cannot create directory '", dir.path,
                 "'. Aborting...")
          }
        }
        ggplot2::ggsave(paste0(file.path(dir.path, file.name), ".pdf"), device = "pdf",
                         plot = plot, limitsize = FALSE)
        message("[", class(self)[1], "][INFO] Plot has been succesfully saved ",
                "at: ", file.path(dir.path, file.name, ".pdf"))
      } else {  show(plot) }
      plot
    },
    #'
    #' @description <<description>>
    #'
    #' @param name <<description>>
    #' @param dir.path <<description>>
    #' @param num.clusters <<description>>
    #'
    #'
    #' @return <<return>>
    #'
    saveCSV = function(dir.path, name = NULL, num.clusters = NULL) {
      if (is.null(dir.path))
        stop("[", class(self)[1], "][FATAL] Path not defined. Aborting...")

      if (is.null(name)) {
        name <- class(private$heuristic[[1]])[1]
        message("[", class(self)[1], "][WARNING] File name not defined. Using '",
                name, ".csv'")
      }

      if (is.null(private$all.distribution) || nrow(private$all.distribution) == 0) {
        stop("[", class(self)[1], "][FATAL] Clustering not done or errorneous. ",
             "Aborting...")
      }

      if (!dir.exists(dir.path)) {
        dir.create(dir.path, recursive = TRUE)
        if (dir.exists(dir.path)) {
          message("[", class(self)[1], "][INFO] Directory '", dir.path,
                  "'has been succesfully created")
        } else {
          stop("[", class(self)[1], "][FATAL] Cannot create directory '", dir.path,
               "'. Aborting...")
        }
      }

      if (is.null(num.clusters)) {
        message("[", class(self)[1], "][WARNING] Number of clusters not defined. ",
                 "Saving all cluster configurations")
        num.clusters <- list(2:max(private$all.distribution$k))
      } else {
        if (!is.list(num.clusters)) {
          message("[", class(self)[1], "][WARNING] Type of num.clusters not valid ",
                   "(must be NULL or list type). Saving all cluster configurations")
          num.clusters <- list(2:max(private$all.distribution$k))
        } else {
          if (length(num.clusters[[1]]) > max(private$all.distribution$k)) {
            message("[", class(self)[1], "][WARNING] Number of clusters exceeds ",
                     "maximum number of clusters. Saving all cluster configurations")
            num.clusters <- list(2:max(private$all.distribution$k))
          } else {
            if (!all(unlist(num.clusters) <= max(private$all.distribution$k) &&
                      unlist(num.clusters) >= min(private$all.distribution$k))) {
              message("[", class(self)[1], "][WARNING] Number of clusters ",
                       "exceeds the range of minimum and maximum number of ",
                       "clusters. Saving all cluster configurations")
              num.clusters <- list(2:max(private$all.distribution$k))
            }
          }
        }
      }
      write.table(data.frame(k = private$all.distribution[private$all.distribution$k %in% unlist(num.clusters), "k"],
                               dispersion = private$all.distribution[private$all.distribution$k %in% unlist(num.clusters), "deltha"],
                               row.names = NULL),
                   file = file.path(dir.path, paste0(name, ".csv")),
                   row.names = FALSE,
                   col.names = TRUE,
                   sep = ";")
    }
  )
)
