
# DeepsmirUD_web

<!-- badges: start -->
<!-- badges: end -->

Source code of the web interface of DeepsmirUD.

## How to run

Due to the computation resource limitation of the cloud service, it is
highly recommened to run DeepsmirUD_web on your local computer. You
could use one of following approaches,

### Using Docker image (recommended)

``` sh
docker run --rm jinlongru/deepsmirud_web:latest
```

### Run from GitHub

``` sh
# You need to install dependent packages manually
R -e "shiny::runGitHub('DeepsmirUD_web/app', 'rujinlong')"
```

### Run a cloned version

``` sh
git clone https://github.com/rujinlong/DeepsmirUD_web.git
R -e "shiny::runApp('DeepsmirUD_web/app')"
```

### Online version

[https://rujinlong.shinyapps.io/DeepsmirUD/](https://rujinlong.shinyapps.io/DeepsmirUD/)
