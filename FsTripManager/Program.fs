open Akka.FSharp
open ETA

type MovementCreated = { Movement: Movement }
type PositionReceived = { Position: Position }
type TopLevelMessage =
    | NewMovement of MovementCreated
    | NewPosition of PositionReceived 

[<EntryPoint>]
let main argv = 

    let origin = { Latitude = 55.0<degree>; Longitude = 5.0<degree>}
    let destination = { Latitude = 56.0<degree>; Longitude = 4.0<degree>}
    let current = { Latitude = 55.27<degree>; Longitude = 4.66<degree>}
    let vehicle = { ModemID = "Modem1"; LicensePlate = "01-ABC-2"}
    let movement = { Code = "EBF123"; Vehicles = [vehicle]; Origin = origin; Destination = destination; STD = System.DateTime.Now; STA = System.DateTime.Now.AddHours(2.0) }
    let position =  { ModemID = "Modem1"; Location = origin; Timestamp = System.DateTime.Now }
    let position =  { ModemID = "Modem1"; Location = origin; Timestamp = System.DateTime.Now }
    let path = makeStraightPath origin destination (System.TimeSpan.FromMinutes(100.0))
    let currentCheckpoint = findNearestPoint path current
    //printfn "%A" currentCheckpoint

    let handleMovement (mailbox : Actor<'MovementCreated>) = 
        let rec loop(movement) = actor {
            let! message = mailbox.Receive()
            match message with
                | NewMovement(m) ->
                    printfn "[%A]: Movement created %A" mailbox.Self.Path m.Movement.Code
                    return! loop(Some(m.Movement))
                | NewPosition(p) -> 
                    printfn "[%A]: Position received %A" mailbox.Self.Path p
                | _ -> printf "Unknown message type"
            return! loop(movement)
        }
        loop(None)

    let handleMovements (mailbox : Actor<'TopLevelMessage>) = 
        let rec loop(movements: (Movement * Akka.Actor.IActorRef) list) = actor {
            let! message = mailbox.Receive()
            match message with
                | NewMovement(m) ->
                    printfn "[%A]: Movement created %A, total movements %i" mailbox.Self.Path m.Movement.Code (movements.Length+1)
                    let childActorName = sprintf "movement-control-%s" m.Movement.Code
                    let childActor = spawn mailbox.Context childActorName handleMovement
                    childActor <! message
                    return! loop((m.Movement, childActor) :: movements)
                | NewPosition(p) -> 
                    printfn "[%A]: Position received" mailbox.Self.Path
                    movements 
                        |> List.find (fun m -> isPositionInMovement (fst m) p.Position) 
                        |> snd
                        <! message
                | _ -> printf "Unknown message type"
            return! loop(movements)
        }
        loop([])

    use system = System.create "my-system" (Configuration.load())
    let aref = spawn system "my-root" handleMovements

    aref <! NewMovement({ Movement = movement })
    aref <! NewPosition({ Position = position })

    let movement2 = { Code = "POI556"; Vehicles = [vehicle]; Origin = origin; Destination = destination; STD = System.DateTime.Now; STA = System.DateTime.Now.AddHours(2.0) }
    aref <! NewMovement({ Movement = movement2 })

    aref <! NewPosition({ Position = position })

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
