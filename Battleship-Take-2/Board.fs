namespace Battleship

open System

module Board =

    type Coordinate = {
        Row: int
        Col: int
    }
    
    type OccupationType = 
        | Empty
        | Carrier
        | Battleship
        | Cruiser
        | Destroyer
        | Submarine
        | Hit
        | Miss

    type Panel = {
        Coordinate: Coordinate
        Status: OccupationType
    }

    let isOccupied panel =
        match panel.Status with
            | Empty -> false
            | Miss -> false
            | _ -> true

    let setStatus panel status = { 
        Coordinate = panel.Coordinate
        Status = status 
    }

    let getStatus panel = 
        match panel.Status with 
            | Empty -> "_" 
            | Carrier -> "A"
            | Battleship -> "B"
            | Cruiser -> "C"
            | Destroyer -> "D"
            | Submarine -> "S"
            | Hit -> "*"
            | Miss -> "v"

    let initBoard = 
        let mutable board = Array.empty

        for x in 1 .. 10 do
            for y in 1 .. 10 do
                let panel = { Coordinate = { Row = x; Col = y }; Status = OccupationType.Empty }
                board <- Array.append board [|panel|]

        board

    let updateBoard board panel =
        let filteredBoard = Array.filter (fun (x:Panel) -> (x.Coordinate.Row <> panel.Coordinate.Row || x.Coordinate.Col <> panel.Coordinate.Col)) board
        let newBoard = Array.append filteredBoard [|panel|]
        newBoard

    let getOpenRandomPanels board = 
        let isRandomAvailable panel =
            (panel.Coordinate.Row % 2 = 0 && panel.Coordinate.Col % 2 = 0) || (panel.Coordinate.Row % 2 = 1 && panel.Coordinate.Col % 2 = 1)
    
        Array.filter (fun (x:Panel) -> x.Status = OccupationType.Empty && isRandomAvailable x) board

    let getHitNeighbors board =
        let mutable neighbors = Array.empty
        let getNeighbors coordinate =
            Array.filter (fun (x:Panel) -> x.Coordinate.Col = coordinate.Col && x.Coordinate.Row = coordinate.Row - 1 || 
                                            x.Coordinate.Col = coordinate.Col && x.Coordinate.Row = coordinate.Row + 1 ||
                                            x.Coordinate.Col = coordinate.Col - 1 && x.Coordinate.Row = coordinate.Row ||
                                            x.Coordinate.Col = coordinate.Col + 1 && x.Coordinate.Row = coordinate.Row) board

        let hits = Array.filter (fun (x:Panel) -> x.Status = OccupationType.Hit) board

        for hit in hits do
            neighbors <- Array.append neighbors (getNeighbors hit.Coordinate)
        
        neighbors <- Array.filter (fun (x:Panel) -> x.Status = OccupationType.Empty) neighbors
        Array.distinct neighbors
