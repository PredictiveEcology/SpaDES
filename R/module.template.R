# autogenerate a skeleton for a new module
module.skeleton = function(name, path) {
    path <- check.path(path) # defined in simulation.R

###    
    ################################################
    ###
    ### - moduleName: character
    ### - EVENT.TYPE: character
    ### - description: enter your description here
    ###
    ################################################
    
    
    
    ### event functions:
    #   - follow the naming convention `moduleName.eventType()`;
    #   - `moduleName.init()` function is required for initiliazation;
    #   - keep event functions short and clean, modularize by calling
    #       subroutines from section below.
#     do.event.template = function(event.time, event.type) {
#         if (event.type=="init") {
#             # do stuff for this event
#             module.template.init()
#             
#             # schedule the next event
#             schedule.event(EVENT.TIME, "moduleName", "EVENT.TYPE", list(OPTIONAL.ITEMS))
#         } else {
#             # do stuff for this event
#             print("polar bears. grr!")
#             
#             # schedule the next event
#             schedule.event(EVENT.TIME, "moduleName", "EVENT.TYPE", list(OPTIONAL.ITEMS))
#         }
#     }
#     
#     module.template.init = function() {
#         ### check for module dependencies
#         # if a required module isn't loaded yet,
#         # reschedule this module init for later
#         depends = c("NONE") # list package names here
#             
#         if (reload.module.later(depends)) {
#             schedule.event(sim.time(sim), "moduleName", "init")
#         } else {
#             ### load any required packages
#             pkgs = list("raster") # list required packages here
#             load.required.pkgs(pkgs, install=FALSE)
#             
#             ### module parameters
#             #   - export module params to global list
#             globals$params[["moduleName"]] <<- list()
#             
#             #   -  export data structure for module stats
#             globals$modulestats[["moduleName"]] <<- list()
#             
#             # last thing to do is add module name to the loaded list
#             len = length(globals$.loaded)
#             globals$.loaded <<- append(globals$.loaded, "moduleName")
#         }
#     }
#     
#     
#     ### user-defined subroutines
#     
# ###

    # print each module component into a file "path/name"
    filename <- paste(path, name, sep="")
#    sprintf()
}