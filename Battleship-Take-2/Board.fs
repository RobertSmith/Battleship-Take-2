namespace Battleship

open System

module Board =

    type Coordinates = {
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
        Coordinates: Coordinates
        Status: OccupationType
    }

    let isOccupied panel =
        match panel.Status with
            | Empty -> false
            | Miss -> false
            | _ -> true

    let setStatus panel status = { 
        Coordinates = panel.Coordinates
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
                let panel = { Coordinates = { Row = x; Col = y }; Status = OccupationType.Empty }
                board <- Array.append board [|panel|]

        board

    let updateBoard board panel =
        let filteredBoard = Array.filter (fun (x:Panel) -> (x.Coordinates.Row <> panel.Coordinates.Row || x.Coordinates.Col <> panel.Coordinates.Col)) board
        let newBoard = Array.append filteredBoard [|panel|]
        newBoard

    let getOpenRandomPanels board = 
        let isRandomAvailable panel =
            (panel.Coordinates.Row % 2 = 0 && panel.Coordinates.Col % 2 = 0) || (panel.Coordinates.Row % 2 = 1 && panel.Coordinates.Col % 2 = 1)
    
        Array.filter (fun (x:Panel) -> x.Status = OccupationType.Empty && isRandomAvailable x) board

    let getHitNeighbors board =
        let mutable neighbors = Array.empty
        let getNeighbors coordinates =
            Array.filter (fun (x:Panel) -> x.Coordinates.Col = coordinates.Col && x.Coordinates.Row = coordinates.Row - 1 || 
                                            x.Coordinates.Col = coordinates.Col && x.Coordinates.Row = coordinates.Row + 1 ||
                                            x.Coordinates.Col = coordinates.Col - 1 && x.Coordinates.Row = coordinates.Row ||
                                            x.Coordinates.Col = coordinates.Col + 1 && x.Coordinates.Row = coordinates.Row) board

        let hits = Array.filter (fun (x:Panel) -> x.Status = OccupationType.Hit) board

        for hit in hits do
            neighbors <- Array.append neighbors (getNeighbors hit.Coordinates)
        
        neighbors <- Array.filter (fun (x:Panel) -> x.Status = OccupationType.Empty) neighbors
        Array.distinct neighbors
