###################################################################
###
###     ABM simulation: "observer" module:
###
###     1) all plotting (via event calls);
###         - plots to file or to screen (toggle)
###         - see `plotting.R`
###     
###     2) statistics gathering & reporting (via event calls);
###         - stats to file or to screen (toggle)
###         - see `statistics.R`
###     
###     3) experiment builder.
###         - see `experiments.R`
###
###################################################################

### event functions:
#   - follow the naming convention `moduleName.eventType()`;
#   - `moduleName.init()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling
#       subroutines from section below.
#
#   # schedule the next event:
#    schedule.event(EVENT.TIME, "MODULE.NAME", "EVENT.TYPE", list(OPTIONAL.ITEMS))
#
#
do.event.observer = function(event.time, event.type, other) {
    if (event.type=="init") {
        observer.init() # initialize observer module
    } else if (event.type=="plot") {
        observer.plot()
    } else {
        # do stuff for this event
        print("polar bears. grr!")
    }
}

observer.init = function() {
    ### check for module dependencies
    # if a required module isn't loaded yet,
    # reschedule this module init for later
    depends = c("NONE") # list package names here
        
    if (reload.module.later(depends)) {
        schedule.event(sim.time(sim), "observer", "init")
    } else {
        # last thing to do is add module name to the loaded list
        len = length(globals$.loaded)
        globals$.loaded <<- append(globals$.loaded, "observer")
    }
}

observer.plot = function(other) {
    ## following is temporary until plotting.R is implemented
    obj = other$obj
    
    if (is(object)=="raster") {
        
    } else if (is(object)=="mobileAgent") {
        points(caribou, pch=19, cex=0.1)
    }
}