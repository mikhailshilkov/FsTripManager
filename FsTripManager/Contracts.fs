module Contracts

open Akka.FSharp

type ILogger = abstract Log :  Actor<'b> -> Printf.StringFormat<'a,unit> -> 'a