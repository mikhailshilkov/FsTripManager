module ETA

[<Measure>] 
type degree

type Geo = float<degree>

type Location =
  {Latitude: Geo;
   Longitude: Geo;}

type Position =
  {ModemID: string;
   Location: Location;
   Timestamp: System.DateTime;}

type Vehicle =
  {ModemID: string;
   LicensePlate: string;}

type Movement =
  {Vehicles: Vehicle list;
   Origin: Location;
   Destination: Location;
   STD: System.DateTime;
   STA: System.DateTime;}

type CheckPoint =
  {Location: Location;
   RemainingTime: System.TimeSpan}

let isPositionInMovement movement position =
  let doesModemBelongToMovement movement modemID =
    movement.Vehicles |> List.exists (fun v -> v.ModemID = modemID)

  movement.STD.AddHours(-1.0) < position.Timestamp &&
    movement.STA.AddHours(24.0) > position.Timestamp &&
    doesModemBelongToMovement movement position.ModemID

let distanceTo location1 location2 =
  sqrt((location1.Latitude - location2.Latitude) * (location1.Latitude - location2.Latitude) + (location1.Longitude - location2.Longitude) * (location1.Longitude - location2.Longitude))

let isAtProximity location1 location2 =
  distanceTo location1 location2 < 0.1<degree>

let isAtDestination movement position =
  isAtProximity position.Location movement.Destination

let isAtOrigin movement position =
  isAtProximity position.Location movement.Origin

let makeStraightPath origin destination (duration: System.TimeSpan) =
  let deltas = [destination.Latitude - origin.Latitude; destination.Longitude - origin.Longitude]
  let legCount = (List.max deltas) / 0.01<degree>
  let steps = deltas |> List.map (fun d -> d / legCount)
  seq [1.0..legCount] 
    |> Seq.map (fun n -> 
                { Location = { Latitude = origin.Latitude + n * steps.[0]; Longitude = origin.Longitude + n * steps.[1] }; 
                  RemainingTime = System.TimeSpan.FromTicks(int64 ((float duration.Ticks) * (1.0 - (n / legCount)))) }
               )

let findNearestPoint path location =
  path |> Seq.minBy (fun p -> distanceTo location p.Location)