################################################
###
### MODULE TEMPLATE
### - MODULE.NAME: character
### - EVENT.TYPE: character
###
###############################################



### event functions:
#   - follow the naming convention `moduleName.eventType()`;
#   - `module.NAME.init()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling
#       subroutines from section below.
react.event.template = function(event.time, event.type) {
    if (event.type=="init") {
        # do stuff for this event
        module.template.init()
        
        # schedule the next event
        schedule.event(EVENT.TIME, "MODULE.NAME", "EVENT.TYPE", list(OPTIONAL.ITEMS))
    } else {
        # do stuff for this event
        print("polar bears. grr!")
        
        # schedule the next event
        schedule.event(EVENT.TIME, "MODULE.NAME", "EVENT.TYPE", list(OPTIONAL.ITEMS))
    }


#     if (head$evnttype == "arrv") {  # arrival
#       # if server free, start service, else add to queue (added to queue
#       # even if empty, for convenience)
#       if (length(abm.globals$srvq) == 0) {
#          abm.globals$srvq <<- head$arrvtime
#          srvdonetime <- sim$currtime + rexp(1,abm.globals$srvrate)
#          schedule.event(srvdonetime,"srvdone",list(arrvtime=head$arrvtime))
#       } else abm.globals$srvq <<- c(abm.globals$srvq,head$arrvtime)
#       # generate next arrival
#       arrvtime <- sim$currtime + rexp(1,abm.globals$arrvrate)
#       schedule.event(arrvtime,"arrv",list(arrvtime=arrvtime))
#    } else {  # service done
#       # process job that just finished
#       # do accounting
#       abm.globals$njobsdone <<- abm.globals$njobsdone + 1
#       abm.globals$totwait <<- 
#          abm.globals$totwait + sim$currtime - head$arrvtime
#       # remove from queue
#       abm.globals$srvq <<- abm.globals$srvq[-1]
#       # more still in the queue?
#       if (length(abm.globals$srvq) > 0) {
#          # schedule new service
#          srvdonetime <- sim$currtime + rexp(1,abm.globals$srvrate)
#          schedule.event(srvdonetime,"srvdone",list(arrvtime=abm.globals$srvq[1]))
#       }
#    }
}

module.template.init = function() {
    ### load any required packages
    pkgs = list("raster") # list required packages here
    load.required.pkgs(pkgs)

    ### module parameters
    #   - export module params to global list
    globals$params[[MODULE.NAME]] <<- list()
    
    #   -  export data structure for module stats
    globals$modulestats[[MODULE.NAME]] <<- list()
}


### user-defined subroutines
