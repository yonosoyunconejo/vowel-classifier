library("shinylive")
library("httpuv")

shinylive::export(appdir = "myapp", destdir = "docs")

httpuv::runStaticServer("docs/", port=8008)
