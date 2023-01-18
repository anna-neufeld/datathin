What is datathin?
-----

The ``datathin`` R package splits a random variable (or a vector or matrix of random variables) into an independent training set and a test set using the methodology introduced in Neufeld et al., 2023 [(link to preprint coming soon)](...). 

How can I get datathin?
-----

Make sure that ``remotes`` is installed by running ``install.packages("remotes")``, then type

```{r}
remotes::install_github("anna-neufeld/datathin")
```

Where can I learn more? 
-----

For now, you can check our our [introductory tutorial](articles/introduction_tutorial.html), which gives basic examples of how to apply data thinning under a variety of distributional assumptions.  You can also check out our [unsupervised learning tutorial](articles/unsupervised_tutorial.html), which shows how data thinning can be applied to estimate the number of clusters in normally distributed data. 

More tutorials for this package are coming soon! We will provide examples of how to use ``datathin`` for tasks such as model evaluation and inference after model selection under a variety of distributional assumptions. 

To learn more, check out our preprint [(link to preprint coming soon)](...).

To reproduce the figures from our preprint, please see the following repository: [https://github.com/anna-neufeld/datathin_paper](https://github.com/anna-neufeld/datathin_paper). 

The scRNA-seq dataset analyzed in our paper is available for free from [10X genomics](https://cf.10xgenomics.com/samples/cell/pbmc3k/pbmc3k_filtered_gene_bc_matrices.tar.gz), and is also included in the R package [countsplit](https://anna-neufeld.github.io/countsplit/).


References 
----

Neufeld, A.,Dharamshi, A., Gao, L.L. & Witten, D. (2023), ‘Data thinning for convolution-closed distributions’, 




