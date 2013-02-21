enipedia-openrefine-reconcile
==================================
R code used to set up a reconciliation service for the data on enipedia.tudelft.nl

The code is organized as a package

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
