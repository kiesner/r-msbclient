library(httr)
library(digest)

# MSBClient class ####
MSBClient <- setClass(
  # Set the name for the class
  "MSBClient",

  # Define the instance variables
  slots = c(
    url = "character",
    config = "list",
    debug = "logical",
    events = "list",
    functions = "list"
  ),

  prototype = c(
    config = NULL,
    debug = FALSE,
    events = list(),
    functions = list()
  ),

  # Define constraints for objects of this class
  validity = function(object) {
    if (length(object@debug) != 1) {
      return("'debug' must be a logical vector of length 1")
    }
    return(TRUE)
  }
)


# method do_something ####
setGeneric(name = "do_something",
           def = function(theObject) {
             standardGeneric("do_something")
           })

setMethod(f = "do_something",
          signature = "MSBClient",
          definition = function(theObject) {
            # overwrite this method to execute some code after registering with the MSB
          })

# method get_self_description ####
setGeneric(name = "get_self_description",
           def = function(theObject) {
             standardGeneric("get_self_description")
           })

setMethod(f = "get_self_description",
          signature = "MSBClient",
          definition = function(theObject) {
            events = theObject@events
            functions = theObject@functions

            if (length(events) > 0) {
              for (i in seq(1, length(events))) {
                events[[i]]["implementation"] <- NULL
              }
            }

            if (length(functions) > 0)
            {
              for (i in seq(1, length(functions))) {
                functions[[i]]["implementation"] <- NULL
              }
            }

            self_description = list(
              "uuid" = theObject@config[["uuid"]],
              "name" = theObject@config[["name"]],
              "description" = theObject@config[["description"]],
              "token" = theObject@config[["token"]],
              "@class" = theObject@config[["@class"]],
              "events" = I(unname(events)),
              "functions" = I(unname(functions))
            )

            return(self_description)
          })

# method add_function ####
setGeneric(name = "add_function",
           def = function(theObject, func) {
             standardGeneric("add_function")
           })

setMethod(f = "add_function",
          signature = "MSBClient",
          definition = function(theObject, func) {
            theObject@functions[[length(theObject@functions) + 1]] <- func
            return(theObject)
          })

# method add_event ####
setGeneric(name = "add_event",
           def = function(theObject, ev) {
             standardGeneric("add_event")
           })

setMethod(f = "add_event",
          signature = "MSBClient",
          definition = function(theObject, ev) {
            orig_event_id <- ev[["eventId"]]
            new_event_id <- ifelse(!is.null(orig_event_id), orig_event_id,
                                      ifelse(theObject@debug, as.character(length(theObject@events) + 1),
                                              digest(ev, algo="md5")))

            ev[["eventId"]] = new_event_id
            ev[["@id"]] = length(theObject@events) + 1
            ev[["implementation"]][["uuid"]] = theObject@config[["uuid"]]
            ev[["implementation"]][["eventId"]] = new_event_id

            theObject@events[[orig_event_id]] <- I(ev)
            #theObject@events[[length(theObject@events) + 1]] <- I(ev)
            return(theObject)
          })


##### REST METHODS ####

# method register_application ####
setGeneric(name = "register_application",
           def = function(theObject) {
             standardGeneric("register_application")
           })

setMethod(f = "register_application",
          signature = "MSBClient",
          definition = function(theObject) {
            register_url <- paste0(theObject@url,
                                   ifelse(endsWith(theObject@url, "/"), "", "/"),
                                   "rest/application/register")
            r = send_message(register_url,
                         msg=get_self_description(theObject),
                         verbose = theObject@debug
                         )
            do_something(theObject)
            return(r)
          })
# method register_smartobject ####
setGeneric(name = "register_smartobject",
           def = function(theObject) {
             standardGeneric("register_smartobject")
           })

setMethod(f = "register_smartobject",
          signature = "MSBClient",
          definition = function(theObject) {
            register_url <- paste0(theObject@url,
                                   ifelse(endsWith(theObject@url, "/"), "", "/"),
                                   "rest/smartobject/register")
            r = send_message(register_url,
                             msg=get_self_description(theObject),
                             verbose = theObject@debug
            )
            do_something(theObject)
            return(r)
          })

# method send_event ####
setGeneric(name = "send_event",
           def = function(theObject, ev_id, ...) {
             standardGeneric("send_event")
           })

setMethod(f = "send_event",
          signature = "MSBClient",
          definition = function(theObject, ev_id, ...) {
            send_url = paste0(theObject@url,
                              ifelse(endsWith(theObject@url, "/"), "", "/"),
                              "rest/data")
            print("sending something...")
            ev = theObject@events[[ev_id]][["implementation"]]

            if (theObject@debug) {
              print("EVENT:")
              print(ev)
            }

            ev[["postDate"]] <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%S")

            kwargs <- list(...)
            for (k in names(kwargs)) {
              ev[["dataObject"]][[k]] <- kwargs[[k]]
            }

            r <- send_message(send_url, ev, verbose=theObject@debug)
            return(r)
          })


##### Utils ####
# function send_message ####
send_message <- function(receiver, msg, encoding = "json", verbose = TRUE) {
  if (verbose) {
    r <- POST(receiver, body=msg, encode=encoding, verbose())
  } else {
    r <- POST(receiver, body=msg, encode=encoding)
  }
  if (!(r$status_code %in% c(200,201))) {
    stop(paste("POST resonse with status code", r$status_code, ":\n", r$content))
  }

  return(r)
}
