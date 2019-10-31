---
title: Geomorphic Upscale
weight: 1
---

## About

Welcome to Geomorphic Upscale website. This site provides documentation for how to upscale reach-level fish capacity estimates to the network scale using geomorphic metrics.  The code leverages a base dataset of 123 coarse bed, wadable, salmon-bearing streams in the interior Columbia River Basin (USA) and represent the diversity of geomorphic character in the region.  The methods detailed here were developed to upscale salmonid (Chinook and steelhead) juvenile and spawner capacity estimates from net rate energy intake (NREI) and fuzzy habitat suitability models using geomorphic units derived from the Geomorphic Unit Tool [pyGUT](https://github.com/Riverscapes/pyGUT).  

The basic geomorphic upscale workflow is:

1. [Familiarize]({{ site.baseurl }}/familiarize) yourself with the base dataset to make sure that the stream characteristics in your network are similar to those in the base dataset
2. [Select and Review]({{ site.baseurl }}/select) similar sites based on River Style geoindicators using [RSselection.R]()
3. [Upscale]({{ site.baseurl }}/upscale)  geomorphic assemblages and responses by geomorphic unit type using [UpscaleWrapper.R]()

We document an [example]({{ site.baseurl }}/example) for the Asotin Basin for your reference.
