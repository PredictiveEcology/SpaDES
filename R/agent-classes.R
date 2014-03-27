### agent class (this is an aspatial agent)
setClass("agent", slots=list(ID="character", other="list"), prototype=list(ID=NA_character_))

### rasterAgent class extends agent by making it spatial
setClass("rasterAgent", slots=list(ID="character", other = "list"), contains="agent")

### vectorAgent class extends agent by making it spatial
setClass("vectorAgent", slots=list(ID="character", other = "list"), contains="agent")

### polygonAgent class extends agent by making it spatial
setClass("polygonAgent", slots=list(spatial="SpatialPolygons"), contains="vectorAgent")

### pointAgent class extends vectorAgent
setClass("pointAgent", slots=list(spatial="SpatialPoints"), contains="vectorAgent")

### spreadAgent class extends pointAgent by not only storing single position but also area
setClass("spreadAgent", slots=list(NumPixels="numeric"),
         prototype=list(NumPixels=NA_integer_), contains="rasterAgent")

### mobileAgent class extends pointAgent by allowing movement
setClass("mobileAgent", slots=list(heading="numeric", distance="numeric"),
         prototype=list(heading=NA_real_, distance=NA_real_), contains="pointAgent")
