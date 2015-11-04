module Contracts

open Akka.FSharp

type IPureLogger = abstract Log :  Printf.StringFormat<'a,unit> -> 'a
type ILogger = abstract Log :  Actor<'b> -> Printf.StringFormat<'a,unit> -> 'a