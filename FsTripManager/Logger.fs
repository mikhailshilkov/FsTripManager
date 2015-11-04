module Logger

open Akka.FSharp
open Contracts

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

let newLogger() = 
    let color = randomColor()
    { new ILogger with member __.Log (mailbox : Actor<'b>) format = 
        Printf.kprintf (writelog color mailbox.Self.Path.Name) format }
