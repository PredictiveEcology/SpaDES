## see: ?.Random.seed
##
## RNG SAVE CODE MODIFIED FROM:
## http://www.cookbook-r.com/Numbers/Saving_the_state_of_the_random_number_generator/
## https://stackoverflow.com/questions/13997444/

do.event.checkpoint = function(sim, event.time, event.type, debug=FALSE) {
    if ( (event.type=="init") || (event.type=="load") ) {
        checkpoint.load(sim.params(sim)$checkpoint$file)        
        
        # schedule the next save
        time.next.save <- currentTime(sim) + sim.params(sim)$checkpoint$interval
        sim <- schedule.event(sim, time.next.save, "checkpoint", "save")
    } else if (event.type=="save") {
        checkpoint.save(sim.params(sim)$checkpoint$file)
        
        # schedule the next save
        time.next.save <- currentTime(sim) + sim.params(sim)$checkpoint$interval
        sim <- schedule.event(sim, time.next.save, "checkpoint", "save")
    } else {
        # do stuff for this event
        print("polar bears. grr!")
    }
    return(sim)
}

### these next two functions are copied from:
### https://raw.githubusercontent.com/achubaty/r-tools/master/checkpoint.R

checkpoint.load = function(file="checkpoint.RData") {
    # check for previous checkpoint file
    if (file.exists(file)) {
        load(file)
        if (exists(".Random.seed")) {
            do.call("RNGkind", as.list(rng.kind))
            assign(".Random.seed", rng.state, .GlobalEnv)
        }
        return(invisible(TRUE))
    } else {
        return(invisible(FALSE))
    }
}

checkpoint.save = function(file="checkpoint.RData") {
    if (exists(".Random.seed"))  {
        assign("rng.state", get(".Random.seed", .GlobalEnv), .GlobalEnv)
        assign("rng.kind", RNGkind(), .GlobalEnv)
    }
    save.image(file) # saves entire workspace
    invisible(TRUE) # return "success" invisibly
}
