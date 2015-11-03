module Actors

open Akka.FSharp
open ETA

type MovementCreated = { Movement: Movement }
type PositionReceived = { Position: Position }
type TopLevelMessage =
    | NewMovement of MovementCreated
    | NewPosition of PositionReceived 

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

type ILogger = abstract Log : System.ConsoleColor -> Actor<'b> -> Printf.StringFormat<'a,unit> -> 'a
let logger = { 
    new ILogger with member __.Log color (mailbox : Actor<'b>) format = 
        Printf.kprintf (writelog color mailbox.Self.Path.Name) format }

let handleMovement (logger:ILogger) (mailbox : Actor<TopLevelMessage>) = 
    let color = randomColor()
    let logf fmt = logger.Log color mailbox fmt
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

let handleMovements (logger:ILogger) (mailbox : Actor<TopLevelMessage>) = 
    let color = randomColor()
    let logf fmt = logger.Log color mailbox fmt
    let rec loop(movements: (Movement * Akka.Actor.IActorRef) list) = actor {
        let! message = mailbox.Receive()
        match message with
            | NewMovement(m) ->
                logf "Movement created %s" m.Movement.Code
                let childActorName = sprintf "movement-control-%s" m.Movement.Code
                let childActor = spawn mailbox.Context childActorName (handleMovement logger)
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