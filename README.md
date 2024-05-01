What is datathin?
-----

The ``datathin`` R package splits a random variable (or a vector or matrix of random variables) into an independent training set and a test set using the methodology developed in Neufeld et al., 2024 [(link to paper)](https://www.jmlr.org/papers/volume25/23-0446/23-0446.pdf) and Dharamshi et al., 2024 [(link to preprint)](https://arxiv.org/abs/2303.12931). 

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

To learn more, check out our papers linked above!

To reproduce the figures from Neufeld et al., please see the following repository: [https://github.com/anna-neufeld/datathin_paper](https://github.com/anna-neufeld/datathin_paper). 

The scRNA-seq dataset analyzed in Neufeld et al. is available for free from [10X genomics](https://cf.10xgenomics.com/samples/cell/pbmc3k/pbmc3k_filtered_gene_bc_matrices.tar.gz), and is also included in the R package [countsplit](https://anna-neufeld.github.io/countsplit/).

To reproduce the figures from Dharamshi et al., please see the following repository: [https://github.com/AmeerD/gdt-experiments](https://github.com/AmeerD/gdt-experiments). 

References 
----

Neufeld, A., Dharamshi, A., Gao, L. L., & Witten, D. (2024). Data thinning for convolution-closed distributions. Journal of Machine Learning Research, 25(57), 1-35.

Dharamshi, A., Neufeld, A., Motwani, K., Gao, L. L., Witten, D., & Bien, J. (2024). Generalized data thinning using sufficient statistics. To appear in: Journal of the American Statistical Association.





