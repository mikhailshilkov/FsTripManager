module Actors

open Akka.FSharp
open ETA
open Contracts

type MovementCreated = { Movement: Movement }
type PositionReceived = { Position: Position }
type TopLevelMessage =
    | NewMovement of MovementCreated
    | NewPosition of PositionReceived 
type ResultMessage =
    | NewMovementCreated of MovementCreated
    | NewMovementPosition of Movement * Position

let purify (logger: ILogger) (actor: Actor<'a>) : IPureLogger =
    { new IPureLogger with member __.Log format = logger.Log actor format }

let handleMovementBody (logger: IPureLogger) (movement: Movement option) (message: TopLevelMessage) : Movement option =
    let logf fmt = logger.Log fmt
    match message with
        | NewMovement(m) ->
            logf "Movement created %s" m.Movement.Code
            Some(m.Movement)
        | NewPosition(p) -> 
            logf "Position received %s" (p.Position.Timestamp.ToString())
            match movement with
                | Some(m) ->
                    let path = makeStraightPath m.Origin m.Destination (System.TimeSpan.FromMinutes(100.0))
                    let currentCheckpoint = findNearestPoint path p.Position.Location
                    logf "Current checkpoint %A" currentCheckpoint
                    movement
                | None -> failwith "Position received for unknown movement"

let handleMovement (logger:unit -> ILogger) (mailbox : Actor<TopLevelMessage>) = 
    let logr = logger()
    let pureLogger = purify logr mailbox
    let rec loop(movement) = actor {
        let! message = mailbox.Receive()
        let result = handleMovementBody pureLogger movement message
        return! loop(result)
    }
    loop(None)

let handleMovementsBody (logger: IPureLogger) (movements: Movement list) (message: TopLevelMessage) : Movement list * ResultMessage =
    let logf fmt = logger.Log fmt
    match message with
        | NewMovement(m) ->
            logf "Movement created %s" m.Movement.Code
            (m.Movement :: movements, NewMovementCreated({ Movement = m.Movement }))
        | NewPosition(p) -> 
            logf "Position received %s" (p.Position.Timestamp.ToString())
            let matching = movements |> List.find (fun m -> isPositionInMovement m p.Position) 
            (movements, NewMovementPosition(matching, p.Position))

let handleMovements (logger:unit -> ILogger) (mailbox : Actor<TopLevelMessage>) = 
    let logr = logger()
    let pureLogger = purify logr mailbox
    let dict = new System.Collections.Generic.Dictionary<string, Akka.Actor.IActorRef>()
    let rec loop (movements: Movement list) = actor {
        let! message = mailbox.Receive()
        let actorFactory n = spawn mailbox.Context n (handleMovement logger)
        let result = handleMovementsBody pureLogger movements message
        match snd result with
            | NewMovementCreated(m) ->
                let childActorName = sprintf "movement-control-%s" m.Movement.Code
                let childActor = actorFactory childActorName
                dict.Add(m.Movement.Code, childActor)
                childActor <! message
            | NewMovementPosition(m, p) -> 
                let childActor = dict.[m.Code]
                childActor <! message
        return! loop(fst result)
    }
    loop([])