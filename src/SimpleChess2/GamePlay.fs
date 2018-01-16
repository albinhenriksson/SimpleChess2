namespace SimpleChess2

open System
module GamePlay =
    open System
    open Chess
    open Pieces

    /// Methods:

    /// <summary>Print text with recieved color.</summary>
    let cprintf (color:ConsoleColor) (text:string) :unit =
        Printf.kprintf
            (fun str ->
                let fgDefault = Console.ForegroundColor
                try
                  Console.ForegroundColor <- color;
                  Console.Write str
                finally
                  Console.ForegroundColor <- fgDefault)
            "%s" text

    let letterTonumber (letter:char) :int =
        match letter with
            | 'a' -> 0
            | 'b' -> 1
            | 'c' -> 2
            | 'd' -> 3
            | 'e' -> 4
            | 'f' -> 5
            | 'g' -> 6
            | 'h' -> 7
            | _ -> invalidArg "letter" (sprintf "Value passed in was %c." letter)

    let numberToLetter (number:int) :char =
        match number with
            | 0 -> 'a'
            | 1 -> 'b'
            | 2 -> 'c'
            | 3 -> 'd'
            | 4 -> 'e'
            | 5 -> 'f'
            | 6 -> 'g'
            | 7 -> 'h'
            | _ -> invalidArg "number" (sprintf "Value passed in was %i." number)

    let charToInt (c:char) = (int c) - (int '0')

    let parse (s:string) :Position list =
        [(charToInt s.[1]) - 1, (letterTonumber s.[0]); (charToInt s.[4]) - 1, (letterTonumber s.[3])]

    let nicePos (pos:Position) :string =
        sprintf "%c%i" (numberToLetter (snd pos)) (fst pos + 1)

    /// Classes:

    /// <summary>
    /// Simulation of a card-player. Superclass of human and robot (and dealer).
    /// </summary>
    /// <param name="name">The players name.</param>
    /// <param name="color">The players color.</param>
    [<AbstractClass>]
    type Player(name:string, color:Color) = class
        member this.name with get() = name
        member this.color with get() = color
        
        // Abstract functions:
        abstract member nextMove : Board -> Position list
    end

    /// The human
    type Human(name:string, color:Color) = class
        inherit Player(name, color)
        /// <summary>
        /// The human chooses a chess move
        /// </summary>
        override this.nextMove(board:Board) :Position list =
            do printf "\nEnter a codestring to move, enter quit to quit.\n(E.g. \"a4 a5\"): "
            let input = System.Console.ReadLine()

            let validNumbers = ['1';'2';'3';'4';'5';'6';'7';'8']
            let validLetters = ['a';'b';'c';'d';'e';'f';'g';'h']

            if input = "quit" then System.Environment.Exit(0); []
            elif input.Length <> 5 then
                do printfn "Codestring invalid: wrong length."
                this.nextMove(board)
            elif not (List.contains input.[0] validLetters) ||
                    not (List.contains input.[1] validNumbers) ||
                    not (List.contains input.[3] validLetters) ||
                    not (List.contains input.[4] validNumbers) then
                        do printfn "Codestring invalid: unrecognized letters/numbers."
                        this.nextMove(board)
            else
                let move = parse input
                if board.Item(fst move.[0], snd move.[0]).IsNone then
                    do printfn "Invalid move: No piece there."
                    this.nextMove(board)
                else
                    let movePiece = board.Item(fst move.[0], snd move.[0]).Value
                    if movePiece.color <> this.color then
                        do printfn "Invalid move: Not your piece."
                        this.nextMove(board)
                    elif not (List.contains (move.[1]) (movePiece.availableMoves(board).[1]) ||
                                List.contains (move.[1]) (movePiece.availableMoves(board).[0])) then
                        do printfn "Invalid move: Not legal."
                        this.nextMove(board)
                    else move
    end

    /// <summary>The game.</summary>
    type Game(
                nHumans:int,
                wCol:ConsoleColor,
                bCol:ConsoleColor,
                boardCol:ConsoleColor) = class

        let board = Chess.Board()

        member this.printPiece (p:chessPiece) :unit =
            let aMoves = p.availableMoves board
            let mutable fgCol = Console.ForegroundColor
            if p.color = White then fgCol <- wCol
            else fgCol <- bCol
            cprintf fgCol (sprintf "%A can move from %s to:" p (nicePos p.position.Value))
            aMoves.[1] |>
                List.iter (fun pos -> cprintf fgCol (sprintf " %s," (nicePos pos)))
            if not aMoves.[0].IsEmpty then
                cprintf ConsoleColor.DarkRed "\b and attack:"
                aMoves.[0] |>
                    List.iter (fun pos -> cprintf ConsoleColor.DarkRed (sprintf " %s," (nicePos pos)))
            printf "\b.\n"

        member this.printBoard() :unit =
            let boardLst = board.ToString() |> Seq.toList
            printf "\n"
            boardLst |> List.iter (fun c ->
            match c with
                | 'k' -> cprintf bCol (string c)
                | 'r' -> cprintf bCol (string c)
                | 'K' -> cprintf wCol (string c)
                | 'R' -> cprintf wCol (string c)
                | '|' -> cprintf boardCol (string c)
                | '-' -> cprintf boardCol (string c)
                | _ -> printf "%c" c)

        member this.run() =
            let mutable players :Player list = []
            let mutable colors = [White;Black]
            // Add human(s):
            for i in [0..(nHumans-1)] do
                players <- List.append players
                    [Human((sprintf "Human-%i (%A)" (i+1) colors.[0]), colors.[0])]
                colors <- colors.Tail
            
            // Add ai(s)
            //*
    //        for i in [1..(2-nHumans)] do
    //                players <- List.append players
    //                    [new Robot((sprintf "Robot T-%i" (Random.rInt 800 4000)),
    //                        colors.[0])]
    //                colors <- colors.Tail

            // choose pieces:
            let mutable pieces = [|
                king (White) :> chessPiece;
                rook (White) :> chessPiece;
                rook (Black) :> chessPiece;
                king (Black) :> chessPiece |]
            // Place pieces on the board
            board.[0,0] <- Some pieces.[0]
            board.[1,1] <- Some pieces.[1]
            board.[4,1] <- Some pieces.[2]
            board.[5,1] <- Some pieces.[3]

            // Welcome message:
            printf ">Welcome to SimpleChess!\n"
            printf "%s: " players.[0].name; cprintf wCol "K\n"
            printf "%s: " players.[1].name; cprintf bCol "k\n"
            
            // The game loop:
            let mutable turnCount = 0
            while 1 <> 0 do
                // White player's turn:
                turnCount <- turnCount + 1
                do printf "\n>Turn %i: " turnCount
                cprintf wCol (players.[0].name + ".\n")
                this.printBoard()
                do printfn "\nAnalysis:"
                Array.iter this.printPiece pieces

                let nextM = players.[0].nextMove(board)
                if board.[fst nextM.[1], snd nextM.[1]].IsSome then
                    if board.[fst nextM.[1], snd nextM.[1]].Value.nameOfType = "king" then
                        do printf "Game Over! %s won!" players.[0].name
                    else //must be the tower:
                        cprintf wCol (sprintf "\n%s took %s!\n"
                            (board.[fst nextM.[0], snd nextM.[0]].Value.ToString())
                            (board.[fst nextM.[1], snd nextM.[1]].Value.ToString()))
                        pieces <- pieces |> Array.filter (fun rem ->
                            rem <> board.[fst nextM.[1], snd nextM.[1]].Value)

                board.move nextM.[0] nextM.[1]

                // Black player's turn:
                turnCount <- turnCount + 1
                do printf "\n>Turn %i: " turnCount
                cprintf bCol (players.[1].name + ".\n")
                this.printBoard()
                do printfn "\nAnalysis:"
                Array.iter this.printPiece pieces

                let nextM = players.[1].nextMove(board) //next move
                if board.[fst nextM.[1], snd nextM.[1]].IsSome then
                    if board.[fst nextM.[1], snd nextM.[1]].Value.nameOfType = "king" then
                        do printf "Game Over! %s won!" players.[1].name
                    else //must be the tower:
                        cprintf wCol (sprintf "\n%s took %s!\n"
                            (board.[fst nextM.[0], snd nextM.[0]].Value.ToString())
                            (board.[fst nextM.[1], snd nextM.[1]].Value.ToString()))
                        pieces <- pieces |> Array.filter (fun rem ->
                            rem <> board.[fst nextM.[1], snd nextM.[1]].Value)

                board.move nextM.[0] nextM.[1]
    end