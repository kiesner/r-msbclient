source("R/MSBClient.R")

cfg = list(uuid="eb3ceb98-25c1-4750-a70c-1660fb1b348d",
           name = "Tuerlich(t)",
           description="Katzenwarnlicht",
           token="licht",
           "@class" = "SmartObject")
broker_url = "http://10.3.10.9:8083"

# Inherit from MSBClient and overwrite do_something method ####
LichtClient <- setClass(
  "LichtClient",

  contains = "MSBClient"
)

setMethod(f = "do_something",
          signature = "LichtClient",
          definition = function(theObject) {
            print("hab ich doch schon gemacht!")
          })

handle_event <- function() {
  print("handling event")
}

client <- LichtClient(url = broker_url, config = cfg, debug=FALSE)
func <- list(
  functionId = "send_light",
  name = "send light",
  description = "Sends light to the display.",
  dataFormat = list(
    message = I(list(
      "$ref" = "#/definitions/LightEvent"
    )),
    LightEvent = list(
      type = "object",
      properties = list(
        color = list(
          type = "string"
        )
      )
    )
  ),
  implementation = handle_event
)

client <- add_function(client, func)
register_smartobject(client)
