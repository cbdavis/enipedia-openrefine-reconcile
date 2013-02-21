enipedia-openrefine-reconcile
==================================
This contains R code used to set up a OpenRefine reconciliation service for the data on enipedia.tudelft.nl.  It also contains numerous functions which have been developed to help with the process of entity matching between data sets.  The code is being refactored to make the functions more modular so that it is easy to be able to test out different entity matching strategies and evaluate how well they work compared to other methods.  

The code is organized as a package to make it easier to work with.

Check that everything is ok:
<pre>
cd Directory/Of/enipedia-openrefine-reconcile
R CMD check .
</pre>

Build:
<pre>
cd .. 
R CMD build enipedia-openrefine-reconcile
</pre>

Install it so that it is accessible within the R environment:
<pre>
R CMD INSTALL EnipediaOpenrefineReconcile_0.1.tar.gz
</pre>

The functions can then be accessed from within R code by first declaring:
<pre>
library(EnipediaOpenrefineReconcile)
</pre>

