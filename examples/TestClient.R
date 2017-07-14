source("R/MSBClient.R")


cfg = list(uuid="55d88-34cf-4836-8cc1-7e0d9c5dca12c4",
           name = "Python sensor client",
           description="Test client description goes here.",
           token="py1",
           "@class" = "SmartObject")
broker_url = "http://10.3.10.9:8083"

# Inherit from MSBClient and overwrite do_something method ####
TestClient <- setClass(
  "TestClient",

  contains = "MSBClient"
)

setMethod(f = "do_something",
          signature = "TestClient",
          definition = function(theObject) {
            start_time <- Sys.time()
            num_messages <- 10
            print('doing something...')
            for (i in seq(num_messages)) {
              send_event(theObject, 'sensor_event',
                         name = sample(c('a', 'b', 'c'), 1),
                         timestamp = strftime(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
                         status = sample(c('on', 'off'), 1)
                         )
            }
            finished_time = Sys.time()
            print(paste(as.character(finished_time - start_time),
                        "seconds for",
                        num_messages,
                        "messages")
                  )
            Sys.sleep(10)
          })

# Instantiate object and initiate connection ####
client <- TestClient(url=broker_url,
                    config = cfg,
                    debug=FALSE)
event <- list(
  name = "Sensor event",
  eventId = "sensor_event",
  description = "desc for ev 1",
  dataFormat = list(
    dataObject = list("$ref" = "#/definitions/SensorEvent"),
    SensorEvent = list(
      type = "object",
      properties = list(
        timestamp = list(type = "string"),
        name = list(type = "string"),
        status = list(type = "string")
      )
    )
  ),
  implementation = list(
    priority = 2,
    dataObject = list(
      timestamp = "2016-07-24 12:00:45",
      name = "sensor1",
      status = "on"
    )
  )
)

client <- add_event(client, event)
register_smartobject(client)
do_something(client)
