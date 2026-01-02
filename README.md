# qlive
Repository to prototype apps using shinylive and quarto-live

I took example code from [Posit's website](https://shinylive.io/r/examples/#r-file-upload) and made `app.R`. 

By going through the [{shinylive} documentation](https://github.com/posit-dev/r-shinylive), I installed `{shinylive}` and followed the instructions below:

```r
install.packages("shinylive")
shinylive::export(".", "site") # Took about 20 seconds
httpuv::runStaticServer("site")
```

Oh wow, the `site/` directory for this small app is 62MB!

As outlined in [an example](https://github.com/wch/shinylive-example) by Winston at Posit, I'm going to use github actions to deploy this to Github Pages. 