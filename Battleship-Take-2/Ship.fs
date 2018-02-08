namespace Battleship

open Battleship.Board

module Ship =

    type Ship = {
        Hull: OccupationType
        Width: int
        Hits: int
    }
    
    let recordHit ship = 
        { Hull = ship.Hull; Width = ship.Width; Hits = ship.Hits + 1 }

    let isSunk ship =
        ship.Hits >= ship.Width

    let initShips =
        [
            { Hull = OccupationType.Carrier; Width = 5; Hits = 0 }
            { Hull = OccupationType.Battleship; Width = 4; Hits = 0 }
            { Hull = OccupationType.Cruiser; Width = 3; Hits = 0 }
            { Hull = OccupationType.Submarine; Width = 3; Hits = 0 }
            { Hull = OccupationType.Destroyer; Width = 2; Hits = 0 }
        ]

    let updateShips ships ship =
        let filteredShips = List.filter (fun (x:Ship) -> x.Hull <> ship.Hull) ships
        let newShips = List.append filteredShips [ship]
        newShips
