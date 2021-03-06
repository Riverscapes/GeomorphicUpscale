---
title: Geomorphic Upscale
weight: 1
---

## About

Welcome to Geomorphic Upscale website. This site provides documentation for how to upscale reach-level fish capacity estimates to the network scale using geomorphic metrics.  The code leverages a base dataset of 121 wadeable salmon-bearing streams in the interior Columbia River Basin (USA) and represent the diversity of geomorphic character in the region.  The methods detailed here were developed to upscale salmonid (Chinook and steelhead) juvenile and spawner redd capacity estimates from net rate energy intake [(NREI)](https://doi.org/10.1139/cjfas-2015-0290) and [fuzzy habitat suitability models](https://github.com/bangen/FuzzyHabModel) using geomorphic units derived from the Geomorphic Unit Tool [(pyGUT)](https://github.com/Riverscapes/pyGUT).  

The basic geomorphic upscale workflow is:

1. [Familiarize]({{ site.baseurl }}/familiarize) yourself with the training dataset to make sure that the stream characteristics in your network are similar.
2. [Select and Review]({{ site.baseurl }}/select) similar sites based on River Style geoindicators using [RSselection.R](https://github.com/Riverscapes/GeomorphicUpscale/blob/master/RSselection.R)
3. [Upscale]({{ site.baseurl }}/upscale) geomorphic assemblages and fish response using [UpscaleWrapper.R](https://github.com/Riverscapes/GeomorphicUpscale/blob/master/UpscaleWrapper.R)

We document an [example]({{ site.baseurl }}/example) for the Asotin Basin for your reference.

## Source Code

To run these scripts you need R [(*download R Studio*)](https://www.rstudio.com/products/rstudio/download/).   The source code is available in the [Geomorphic Upscale repo](https://github.com/Riverscapes/GeomorphicUpscale) on GitHub.   
