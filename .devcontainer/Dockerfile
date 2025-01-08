FROM FROM docker.io/rhub/r-minimal:devel

RUN apk update && \
  apk add --no-cache git \
  && echo 'options(repos="https://cloud.r-project.org")' >> /usr/local/lib/R/etc/Rprofile.site

COPY DESCRIPTION .

RUN Rscript -e '                                  \
  install.packages("remotes");                    \
  remotes::install_deps(dependencies = "Imports") \
'
