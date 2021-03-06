% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/common_functions_mmm.R
\name{addFootprintData}
\alias{addFootprintData}
\title{Add Footprint level data}
\usage{
addFootprintData(input_data, metric_cols = media_names_vec,
  agg_func = "sum")
}
\arguments{
\item{input_data}{The data containing period and DMA to which the footprint data needs to be added}

\item{metric_cols}{The columns for which the footprint data need to be added. Default value is media_names_vec}

\item{agg_func}{The function that needs to be used to generate the Footprint level aggregate data. Default is 'sum'}
}
\value{
Input data subset to the columns being aggregated and DMA column along with new records for the footprint level data
}
\description{
Function that adds aggregate data by Footprint
}
\details{
This function is used to add Footprint level aggregate data (Altice, Suddenlink and Optimum) as new DMAs
to the input_data, not aggregated at period level. \cr
Used in visualization functions to display metrics at the footprint level.
The default aggregation function used is sum. The aggregation function can be changed by passing the name of the aggregator as a string.
The function produces 3 new records by period with the data aggregated using aggregate function. (Drops period level)
}
\note{
This function uses optimum_dma from the .GlobalEnv or the calling environment to compute the aggregate value for Optimum.
Does not remove the already existing Optimum data even if its a single Optimum DMA. If no metric_cols are provided, uses media_names_vec parameter from
.GlobalEnv or calling environment.
}
