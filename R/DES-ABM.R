# DES-ABM.R:  R routines for discrete-event simulation (DES)
# modified from Matloff 2009 to use data.table instead of data.frame

require(data.table)

# each event will be represented by a data table row consisting of the
# following components:
#   evnttime, the time the event is to occur;
#   evnttype, a character string for the programmer-defined event type;
#   optional application-specific components,
#       e.g., the job's arrival time in a queuing app

# a global list named "sim" holds the events data frame, evnts, and
# current simulated time, currtime; there is also a component dbg, which
# indicates debugging mode

# insert event with time evnttm and type evntty into event list;
# appin is an optional set of application-specific traits of this event,
# specified in the form a list with named components
schedevnt <- function(evnttm, evntty, appin=NULL) {
    # forms a row for an event of type evntty that will occur at time evnttm;
    # see comments in schedevnt() regarding appin
    newevnt <- as.data.table(c(list(evnttime=evnttm,evnttype=evntty),appin))
    setkey(newevnt, evnttime)
   # if the event list is empty, set it to consist of evnt and return
   if (length(sim$evnts)==0) {
      sim$evnts <<- newevnt
      return()
   }
   # otherwise, "insert," by reconstructing the data frame; we find what
   # portion of the current matrix should come before the new event and
   # what portion should come after it, then string everything together
   before <- sim$evnts[evnttime<newevnt$evnttime[1]]
   after <- sim$evnts[evnttime>newevnt$evnttime[1]]
   sim$evnts <<- setkey(rbind(before,newevnt,after), evnttime)
}

# start to process next event (second half done by application
# programmer via call to reactevnt()) 
getnextevnt <- function() {
   head <- sim$evnts[1,]
   # delete head
   if (nrow(sim$evnts) == 1) {
      sim$evnts <<- NULL
   } else sim$evnts <<- sim$evnts[-1,]
   return(head)
}

# simulation body
# arguments:
#    initglbls:  application-specific initialization function; inits
#      globals to statistical totals for the app, etc.; records apppars
#      in globals; schedules the first event
#    reactevnt: application-specific event handling function, coding the
#       proper action for each type of event
#    prntrslts:  prints application-specific results, e.g. mean queue
#       wait
#    apppars:  list of application-specific parameters, e.g.
#      number of servers in a queuing app
#    maxsimtime:  simulation will be run until this simulated time 
#    dbg:  debug flag; if TRUE, sim will be printed after each event
dosim <- function(initglbls,reactevnt,prntrslts,maxsimtime,apppars=NULL,dbg=FALSE) {
   sim <<- list()
   sim$currtime <<- 0.0  # current simulated time
   sim$evnts <<- NULL  # events data table (filled in by schedevnt)
   sim$dbg <<- dbg
   
   initglbls(apppars)
   while(sim$currtime < maxsimtime) {  
      head <- getnextevnt()
      sim$currtime <<- head$evnttime  # update current simulated time
      reactevnt(head)  # process this event 
      if (dbg) print(sim)
   }
   prntrslts()
}
