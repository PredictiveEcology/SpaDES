#######################################################################
###     Modified from Matloff (2009):                               ###
###     - uses S4 classes for the sim objects                       ###
###     - uses `data.table` instead of `data.frame`                 ###
###     - implemented in a more modular fashion so it's easier      ###
###       to add submodules to the simulation                       ###
#######################################################################



#' The \code{SimList} class
#'
#' This class contains the minimum components of a simulation.
#'
#' An object of class SimList contains: 
#'      events: the events data.table (see below);
#'      .loaded: list of already loaded modules;
#'      modules: list of required modules to load;
#'      params: list of simulation parameters;
#'      simtime: the current simulated time;
#'      debug: indicates debugging mode.
#'  Each event is represented by a data.table row consisting of:
#'      event.time: the time the event is to occur;
#'      module.name: the module from which the event is taken;
#'      event.type: a character string for the programmer-defined event type;
#'      : optional application-specific components.
#'
#' @slot .loaded    List of character names specifying which modules are currently loaded.
#' 
#' @slot modules    List of character names specifying which modules to load.
#' 
#' @slot params     Named list of potentially other lists specifying simulation parameters.
#' 
#' @slot events     The list of scheduled events, as a data.table class.
#' 
#' @slot simtime    Numerical value describing the current simulation time.
#' 
#' @slot debug      Logical value specifying whether to run simulation in debugging mode.
#'
#' @note add additional notes here.
#' 
#' @name SimList
#' @rdname SimList-class
#' @aliases SimList-class
#' @importClassesFrom data.table data.table
#' @exportClass SimList
#' 
setClass("SimList",
         slots=list(.loaded="list", modules="list", params="list",
                    events="data.table", simtime="numeric", debug="logical"
))
