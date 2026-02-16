FROM rocker/verse:4.5.1 AS base
WORKDIR /usr/src/study
COPY . .

FROM base AS setup
RUN apt update && apt upgrade -y

FROM setup AS renv
ENV RENV_PATHS_LIBRARY=renv/library
RUN R -e "renv::restore()"

FROM renv AS build
RUN make