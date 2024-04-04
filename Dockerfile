FROM rocker/verse:4.4.1 AS base
WORKDIR /usr/src/void
COPY . .
FROM base as setup
RUN apt update && apt upgrade -y

FROM setup AS renv
ENV RENV_PATHS_LIBRARY renv/library
RUN R -e "renv::restore()"

FROM renv as build
RUN make