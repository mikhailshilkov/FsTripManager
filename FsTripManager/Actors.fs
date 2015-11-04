module Actors

open Akka.FSharp
open ETA
open Contracts

type MovementCreated = { Movement: Movement }
type PositionReceived = { Position: Position }
type TopLevelMessage =
    | NewMovement of MovementCreated
    | NewPosition of PositionReceived 

let handleMovement (logger:unit -> ILogger) (mailbox : Actor<TopLevelMessage>) = 
    let logf fmt = logger().Log mailbox fmt
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

let handleMovements (logger: unit -> ILogger) (mailbox : Actor<TopLevelMessage>) = 
    let logf fmt = logger().Log mailbox fmt
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