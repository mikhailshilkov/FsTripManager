module ETA

type Location =
  {Latitude: double;
   Longitude: double;}

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

let isPositionInMovement movement position =
  let doesModemBelongToMovement movement modemID =
    movement.Vehicles |> List.exists (fun v -> v.ModemID = modemID)

  movement.STD.AddHours(-1.0) < position.Timestamp &&
    movement.STA.AddHours(24.0) > position.Timestamp &&
    doesModemBelongToMovement movement position.ModemID

let distanceTo location1 location2 =
  sqrt((location1.Latitude - location2.Latitude) ** 2.0 + (location1.Longitude - location2.Longitude) ** 2.0)

let isAtProximity location1 location2 =
  distanceTo location1 location2 < 0.1

let isAtDestination movement position =
  isAtProximity position.Location movement.Destination

let isAtOrigin movement position =
  isAtProximity position.Location movement.Destination

