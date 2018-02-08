// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open Battleship.Game

[<EntryPoint>]
let main argv = 
    let p1 = initPlayer "Joe" |> placeShips
    let p2 = initPlayer "Frank" |> placeShips

    printBoards p1
    printBoards p2

    playGame p1 p2

    printBoards p1
    printBoards p2

    Console.ReadLine() |> ignore
    0 // return an integer exit code