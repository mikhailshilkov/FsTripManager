open ETA

[<EntryPoint>]
let main argv = 

    let origin = { Latitude = 55.0<degree>; Longitude = 5.0<degree>}
    let destination = { Latitude = 56.0<degree>; Longitude = 4.0<degree>}
    let current = { Latitude = 55.27<degree>; Longitude = 4.66<degree>}
    let vehicle = { ModemID = "Modem1"; LicensePlate = "01-ABC-2"}
    let movement = { Vehicles = [vehicle]; Origin = origin; Destination = destination; STD = System.DateTime.Now; STA = System.DateTime.Now.AddHours(2.0) }
    let position =  { ModemID = "Modem1"; Location = origin; Timestamp = System.DateTime.Now }
    let path = makeStraightPath origin destination (System.TimeSpan.FromMinutes(100.0))
    let currentCheckpoint = findNearestPoint path current

    printfn "%A" currentCheckpoint
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
