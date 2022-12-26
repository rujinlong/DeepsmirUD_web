# DeepsmirUD_web

<!-- badges: start -->
<!-- badges: end -->

Source code of the web interface of [DeepsmirUD](https://github.com/2003100127/deepsmirud).

## How to run

Due to the computation resource limitation of the cloud service, it is
highly recommened to run DeepsmirUD_web on your local computer. You
could use one of following approaches,

### Using Docker (recommended)

``` sh
docker run --rm jinlongru/deepsmirud_web

# if the default port 3838 is occupied, you could set a new port by adding `-p` option
docker run --rm -p 3838:3838 jinlongru/deepsmirud_web
```

Then open your browser and visit `http://localhost:3838/` to use the DeepsmirUD database.

### Run from GitHub

You need to manually install dependent packages in `renv.lock` file before running the following command.

``` sh
R -e "shiny::runGitHub('DeepsmirUD_web/app', 'rujinlong', subdir = 'app')"
```

### Run a cloned version

You need to manually install dependent packages in `renv.lock` file before running the following command.

``` sh
git clone https://github.com/rujinlong/DeepsmirUD_web.git
R -e "shiny::runApp('DeepsmirUD_web/app')"
```

### Online version

[https://rujinlong.shinyapps.io/DeepsmirUD/](https://rujinlong.shinyapps.io/DeepsmirUD/)

Please note that the online version is not stable and may not work properly.

## Citation

If you use DeepsmirUD in your research, please cite the following paper:

> Jianfeng Sun, Jinlong Ru, Zihao Chen, Fei Qi, Lorenzo Ramos-Mucci, Suyuan Chen, Adam P. Cribbs, Li Deng, and Xia Wang. "DeepsmirUD: Precise prediction of regulatory effects on miRNA expression mediated by small molecular compounds using competing deep learning frameworks." bioRxiv (2022).

