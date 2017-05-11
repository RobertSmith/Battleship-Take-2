namespace Battleship

open System
open Battleship.Board
open Battleship.Ship

module Game =

    type Player = {
        Name: string
        mutable GameBoard: Panel array
        mutable FiringBoard: Panel array
        mutable Ships: Ship list
    }

    let private hasLost player = 
        let floatingShips = List.filter (fun (x:Ship) -> isSunk x = false) player.Ships
        List.length floatingShips = 0

    let initPlayer name = 
        {
            Name = name
            GameBoard = initBoard
            FiringBoard = initBoard
            Ships = initShips
        }

    let placeShips player = 
        let random = new Random(Guid.NewGuid().GetHashCode())

        let getEnds row column width =
            let orientation = random.Next(1, 101) % 2;
            match orientation with
                | 0 -> (row + width - 1, column)
                | _ -> (row, column + width - 1)

        for ship in player.Ships do
            let mutable isFinished = false;

            while isFinished = false do
                let startColumn = random.Next(1, 11);
                let startRow = random.Next(1, 11);
                let endRow, endColumn = getEnds startRow startColumn ship.Width

                if (endRow <= 10 && endColumn <= 10) then
                    let affectedPanels = Array.filter (fun (p:Panel) -> (p.Coordinates.Row >= startRow 
                                                                    && p.Coordinates.Col >= startColumn 
                                                                    && p.Coordinates.Row <= endRow 
                                                                    && p.Coordinates.Col <= endColumn)) player.GameBoard
                    let isNotFree = Array.filter (fun (x:Panel) -> (isOccupied x)) affectedPanels

                    if Array.length isNotFree = 0 then
                        for panel in affectedPanels do
                            let newPanel = { Coordinates = { Row = panel.Coordinates.Row; Col = panel.Coordinates.Col }; Status = ship.Type }
                            player.GameBoard <- updateBoard player.GameBoard newPanel

                        isFinished <- true
        player

    let printBoards (player:Player) =
        printfn "%s" (player.Name)

        for x in 1 .. 10 do
            for y in 1 .. 10 do
                let p = Array.find (fun (z:Panel) -> z.Coordinates.Row = x && z.Coordinates.Col = y) player.GameBoard
                Console.Write((getStatus p) + " ");
        
            Console.Write("                 ");

            for y in 1 .. 10 do
                let p = Array.find (fun (z:Panel) -> z.Coordinates.Row = x && z.Coordinates.Col = y) player.FiringBoard
                Console.Write((getStatus p) + " ");

            Console.WriteLine()
        Console.WriteLine()

    let private randomShot player =
        let random = new Random(Guid.NewGuid().GetHashCode())
        let openPanels = getOpenRandomPanels player.FiringBoard
        let panelId = random.Next(Seq.length openPanels)
        let shot = Seq.item panelId openPanels
        printfn """%s says: "%i, %i" """ player.Name shot.Coordinates.Row shot.Coordinates.Col
        shot

    let private searchingShot player =
        let random = new Random(Guid.NewGuid().GetHashCode())
        let openPanels = getHitNeighbors player.FiringBoard
        let panelId = random.Next(Seq.length openPanels)
        let shot = Seq.item panelId openPanels
        printfn """%s says: "%i, %i" """ player.Name shot.Coordinates.Row shot.Coordinates.Col
        shot

    let private fireShot player =
        let openPanels = getHitNeighbors player.FiringBoard
        match Seq.length openPanels with
            | 0 -> randomShot player
            | _ -> searchingShot player

    let private processHit player panel =
        let ship = List.find (fun (x:Ship) -> x.Type = panel.Status) player.Ships
        let newShip = recordHit ship
        player.Ships <- updateShips player.Ships newShip
        printfn """%s says: "Hit!" """ player.Name
                
        match isSunk newShip with
            | true -> printfn """%s says: "You sunk my %A!" """ player.Name ship.Type
            | _ -> ()

        OccupationType.Hit

    let private processMiss player = 
        printfn """%s says: "Miss!" """ player.Name
        OccupationType.Miss

    let private processShot player (panel:Panel) =
        let ourPanel = Array.find (fun (x:Panel) -> x.Coordinates.Row = panel.Coordinates.Row && x.Coordinates.Col = panel.Coordinates.Col) player.GameBoard
        
        match isOccupied ourPanel with
            | true -> processHit player ourPanel
            | _ -> processMiss player

    let private processShotResult player (panel:Panel) occupationType =
        let mutable ourPanel = Array.find (fun (x:Panel) -> x.Coordinates.Row = panel.Coordinates.Row && x.Coordinates.Col = panel.Coordinates.Col) player.FiringBoard
        match occupationType with
            | Hit -> ourPanel <- setStatus ourPanel OccupationType.Hit
            | Miss -> ourPanel <- setStatus ourPanel OccupationType.Miss
            | _ -> ourPanel <- setStatus ourPanel OccupationType.Empty

        updateBoard player.FiringBoard ourPanel

    let playRound player1 player2 =
        let coordinates = fireShot player1
        let result = processShot player2 coordinates
        player1.FiringBoard <- processShotResult player1 coordinates result

        match hasLost player2 with
            | false ->  let coordinates = fireShot player2
                        let result = processShot player1 coordinates
                        player2.FiringBoard <- processShotResult player2 coordinates result
            | true -> ()
                    
    let playToEnd player1 player2 =
        let message = printfn "%s has won the game!"

        while (hasLost player1 = false && hasLost player2 = false) do
            playRound player1 player2

        match hasLost player1 with
            | true -> message player2.Name
            | false -> message player1.Name
