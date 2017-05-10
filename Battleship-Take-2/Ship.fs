namespace Battleship

open Battleship.Board

module Ship =

    type Ship = {
        Type: OccupationType
        Width: int
        Hits: int
    }
    
    let recordHit ship = 
        { Type = ship.Type; Width = ship.Width; Hits = ship.Hits + 1 }


    let isSunk ship =
        ship.Hits >= ship.Width

    let initShips =
        [
            { Type = OccupationType.Carrier; Width = 5; Hits = 0 }
            { Type = OccupationType.Battleship; Width = 4; Hits = 0 }
            { Type = OccupationType.Cruiser; Width = 3; Hits = 0 }
            { Type = OccupationType.Submarine; Width = 3; Hits = 0 }
            { Type = OccupationType.Destroyer; Width = 2; Hits = 0 }
        ]

    let updateShips ships ship =
        let filteredShips = List.filter (fun (x:Ship) -> x.Type <> ship.Type) ships
        let newShips = List.append filteredShips [ship]
        newShips
