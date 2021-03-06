% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/common_functions_mmm.R
\name{normalize_x_dma}
\alias{normalize_x_dma}
\title{Normalize DMA Media Data}
\usage{
normalize_x_dma(x, min_media_mat, max_media_mat, prospect_mat_media, media,
  dma)
}
\arguments{
\item{x}{Value of media for a DMA.}

\item{min_media_mat}{The minimum matrix of each media by DMA. Dimensions - n_geo*n_media}

\item{max_media_mat}{The maximum matrix of each media by DMA. Dimensions - n_geo*n_media}

\item{prospect_mat_media}{The matrix of subs/non_subs by DMA used to normalize media during preprocessing. Dimensions - n_geo*n_media}

\item{media}{The name of the media which needs to be normalized.}

\item{dma}{The DMA whose media is being normalized.}
}
\value{
Returns the normalized value of media for the given DMA
}
\description{
Function to Normalize specific DMA Media Data by Prospect and MinMax
}
\details{
This function is used to normalize a media of a DMA by prospect (subs/non_subs) and
minmax by media. \cr
This function follows the normalization method specified under in the Google paper used to build the MMM model. \cr
The minmax can vary based on the normalization technique. It is obtained from the preprocessing module. \cr
This is a variant of the normalize_x function which normalizes media across DMA to output a matrix. \cr
This function returns a single normalized value of the media for a DMA.
}
