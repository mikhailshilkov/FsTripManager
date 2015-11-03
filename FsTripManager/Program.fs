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
    let path = makeStraightPath origin destination (System.TimeSpan.FromMinutes(100.0))
    let currentCheckpoint = findNearestPoint path current
    //printfn "%A" currentCheckpoint
    
    let randomColor() =
        let _random = new System.Random();
        let consoleColors = System.Enum.GetValues(typeof<System.ConsoleColor>)
        consoleColors.GetValue(_random.Next(consoleColors.Length)) :?> System.ConsoleColor

    let logConsoleLock = new System.Object()
    let writelog color sender s = 
        lock (logConsoleLock) (fun _ ->
            let currentColor = System.Console.ForegroundColor
            System.Console.ForegroundColor <- color
            printf "[%s]: " sender
            System.Console.ForegroundColor <- currentColor
            printfn "%s" s)
    let writelogf color sender fmt = Printf.kprintf (writelog color sender) fmt
    let writeactorlogf color (mailbox: Actor<'Message>) fmt = writelogf color mailbox.Self.Path.Name fmt

    let handleMovement (mailbox : Actor<'MovementCreated>) = 
        let color = randomColor()
        let logf fmt = writeactorlogf color mailbox fmt
        let rec loop(movement) = actor {
            let! message = mailbox.Receive()
            match message with
                | NewMovement(m) ->
                    logf "Movement created %s" m.Movement.Code
                    return! loop(Some(m.Movement))
                | NewPosition(p) -> 
                    logf "Position received %s" (p.Position.Timestamp.ToString())
                    match movement with
                        | Some(m) ->
                            let path = makeStraightPath m.Origin m.Destination (System.TimeSpan.FromMinutes(100.0))
                            let currentCheckpoint = findNearestPoint path p.Position.Location
                            logf "Current checkpoint %A" currentCheckpoint
                        | None -> printf "Position received for unknown movement"
            return! loop(movement)
        }
        loop(None)

    let handleMovements (mailbox : Actor<'TopLevelMessage>) = 
        let color = randomColor()
        let logf fmt = writeactorlogf color mailbox fmt
        let rec loop(movements: (Movement * Akka.Actor.IActorRef) list) = actor {
            let! message = mailbox.Receive()
            match message with
                | NewMovement(m) ->
                    logf "Movement created %s" m.Movement.Code
                    let childActorName = sprintf "movement-control-%s" m.Movement.Code
                    let childActor = spawn mailbox.Context childActorName handleMovement
                    childActor <! message
                    return! loop((m.Movement, childActor) :: movements)
                | NewPosition(p) -> 
                    logf "Position received %s" (p.Position.Timestamp.ToString())
                    movements 
                        |> List.find (fun m -> isPositionInMovement (fst m) p.Position) 
                        |> snd
                        <! message
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
