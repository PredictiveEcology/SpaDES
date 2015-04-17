---
layout: frontpage
title: PredictiveEcology - SpaDES
description: This is the main web page for the SpaDES R package for spatial discrete event simulation.
keywords: DES, Spatial, R, spatially explicit models, ABM, landscape ecology, forecasting, ecological forecasting
---

<head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="chrome=1">
    <link rel="stylesheet" href="stylesheets/styles.css">
    <link rel="stylesheet" href="stylesheets/pygment_trac.css">
    <meta name="viewport" content="width=device-width, initial-scale=1, user-scalable=no">
</head>

<div class="wrapper">
  <header>
    <h1>SpaDES</h1>
    <p>R package for developing Spatial Discrete Event Simulation models.</p>

    <p class="view"><a href="https://github.com/PredictiveEcology/SpaDES">View the Project on GitHub <small>PredictiveEcology/SpaDES</small></a></p>


    <ul>
      <li><a href="https://github.com/PredictiveEcology/SpaDES/zipball/master">Download <strong>ZIP File</strong></a></li>
      <li><a href="https://github.com/PredictiveEcology/SpaDES/tarball/master">Download <strong>TAR Ball</strong></a></li>
      <li><a href="https://github.com/PredictiveEcology/SpaDES">View On <strong>GitHub</strong></a></li>
    </ul>
  </header>
  
  <section>
    <p>Master Branch: <a href="https://travis-ci.org/PredictiveEcology/SpaDES"><img src="https://travis-ci.org/PredictiveEcology/SpaDES.svg?branch=master" alt="Build Status"></a></p>
    
    <p>Development Branch: <a href="https://travis-ci.org/PredictiveEcology/SpaDES"><img src="https://travis-ci.org/PredictiveEcology/SpaDES.svg?branch=development" alt="Build Status"></a></p>
    
    <hr>
    
    <h1>
    <a id="spatial-discrete-event-simulation-spades" class="anchor" href="#spatial-discrete-event-simulation-spades" aria-hidden="true"><span class="octicon octicon-link"></span></a>Spatial Discrete Event Simulation (SpaDES)</h1>
    
    <p><em>Develop and run spatially explicit discrete event simulation models.</em></p>
    
    <p>Easily implement a variety of simulation models, with a focus on spatially explicit agent based models. The core simulation components are built upon a discrete event simulation framework that facilitates modularity, and easily enables the user to include additional functionality by running user-built simulation modules. Included are numerous tools to visualize raster and other maps.</p>
    
    <p>This project began at the Canadian Forest Service in 2014.</p>
    
    <p>Come back soon as we set up more content, including manuals, examples and a new SpaDES-Modules page for eventual hosting of modules.</p>
    
    
    <p><strong>Website:</strong> <a href="http://SpaDES.PredictiveEcology.org">http://SpaDES.PredictiveEcology.org</a></p>
    <p><strong>A live proof of concept version:</strong> <a href="http://spades.shinyapps.io/ForestChange_ProofOfConcept">http://spades.shinyapps.io/ForestChange_ProofOfConcept</a></p>
    
    <h2>
    <a id="installation" class="anchor" href="#installation" aria-hidden="true"><span class="octicon octicon-link"></span></a>Installation</h2>
    
    <p>To install the development version of <code>SpaDES</code> directly from GitHub, you will need the <code>devtools</code> package, as well as the appropriate development libraries for your operating system (e.g., Windows users should install <a href="http://cran.r-project.org/bin/windows/Rtools/">Rtools</a>). In order to build the vignettes from source (which is done when installing a package from GitHub) you need to have a LaTeX distribution installed. We recommend <a href="https://www.tug.org/texlive/">TexLive</a>.</p>
    
    <pre><code>install.packages("devtools")
    library("devtools")
    
    install_github("PredictiveEcology/SpaDES") # stable
    install_github("PredictiveEcology/SpaDES", ref="development") # active development
    </code></pre>
    
    <p>The suggested package <code>fastshp</code> can be installed with:</p>
    
    <pre><code>install_github("s-u/fastshp")
    </code></pre>
    
    <p>If the install from GitHub fails during vignette building, you can skip this step (and avoid having to install LaTeX) by using <code>install_github("PredictiveEcology/SpaDES", ref="development", build=FALSE)</code>.</p>
    
    <h2>
    <a id="reporting-bugs" class="anchor" href="#reporting-bugs" aria-hidden="true"><span class="octicon octicon-link"></span></a>Reporting bugs</h2>
    
    <p>Contact us via the package GitHub site: <a href="https://github.com/PredictiveEcology/SpaDES/issues">https://github.com/PredictiveEcology/SpaDES/issues</a>.</p>
</section>
  
  <footer>
    <p>This project is maintained by
    <br>
  	Eliot McIntire (emcintir at nrcan.gc.ca), and
  	<br>
  	Alex Chubaty (achubaty at nrcan.gc.ca)</p>
    <p><small>Hosted on GitHub Pages &mdash; Theme by <a href="https://github.com/orderedlist">orderedlist</a></small></p>
  </footer>
</div>

<script src="javascripts/scale.fix.js"></script>

