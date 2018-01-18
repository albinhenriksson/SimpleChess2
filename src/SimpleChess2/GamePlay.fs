namespace SimpleChess2

open System

module GamePlay =
    open Chess
    open Pieces

    // For later use:

    //// Methods:

    /// <summary>Print text with recieved color.</summary>
    /// <param name="color">The color.</param>
    /// <param name="text">The text.</param>
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

    /// <summary>Converts a char to an int, {a->0,b->1,..,h->7}.</summary>
    /// <param name="letter">char to be converted.</param>
    /// <returns>An int such that {a->0,b->1,..,h->7}.</returns>
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
            | _ -> invalidArg "letter"
                    (sprintf "Value passed in was %c." letter)
    
    /// <summary>Converts an int to a char, {0->a,1->b,..,7->h}.</summary>
    /// <param name="number">char to be converted.</param>
    /// <returns>A char such that {0->a,1->b,..,7->h}.</returns>
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
            | _ -> invalidArg "number"
                    (sprintf "Value passed in was %i." number)

    /// <summary>Parses a chess codestring to chess moves.</summary>
    /// <param name="s">String to be parsed.</param>
    /// <returns>A Position list as [sourcePos;targetPos].</returns>
    let parseInput (s:string) :Position list =
        let charToInt (c:char) = (int c) - (int '0')
        [(charToInt s.[1]) - 1, (letterTonumber s.[0]);
            (charToInt s.[4]) - 1, (letterTonumber s.[3])]

    /// <summary>Converts a Position to a chess codestring.</summary>
    /// <param name="pos">Position to be converted.</param>
    /// <returns>A codestring representing the position as (0,0)->a1.</returns>
    let nicePos (pos:Position) :string =
        sprintf "%c%i" (numberToLetter (snd pos)) (fst pos + 1)

    //// Classes:

    /// <summary>
    /// Simulation of a chess-player. Superclass of human and computer.
    /// </summary>
    /// <param name="name">The players name.</param>
    /// <param name="color">The players color.</param>
    [<AbstractClass>]
    type Player(name:string, color:Color) = class
        let mutable _pieces :chessPiece[] = [||]
        member this.name with get() = name
        member this.color with get() = color
        member this.pieces
            with get() = _pieces
            and set(p:chessPiece[]) = _pieces <- p
        
        abstract member nextMove : Board -> chessPiece [] -> Position list
    end

    /// <summary>Simulation of a chess game.</summary>
    /// <param name="nHumans">Number of human players.</param>
    /// <param name="wCol">White player color.</param>
    /// <param name="bCol">Black player color.</param>
    /// <param name="boardCol">Color of the chessboard.</param>
    type Game(
                playerList:Player list,
                wPieces:chessPiece [],
                bPieces:chessPiece [],
                pPos:Position Option list,
                wCol:ConsoleColor,
                bCol:ConsoleColor,
                boardCol:ConsoleColor) = class

        let board = Chess.Board()

        /// <summary>Print info about a chess piece.</summary>
        /// <param name="p">The chess piece.</param>
        member this.printPiece (p:chessPiece) :unit =
            let avMoves = p.availableMoves board
            // Print in the correct color:
            let mutable fgCol = Console.ForegroundColor
            if p.color = White then fgCol <- wCol
            else fgCol <- bCol
            cprintf fgCol (sprintf "%A can move from %s to:"
                            p (nicePos p.position.Value))
            // Print available moves:
            avMoves.[1] |>
                List.iter (fun pos ->
                            cprintf fgCol (sprintf " %s," (nicePos pos)))
            // Print attack moves:
            if not avMoves.[0].IsEmpty
            then
                cprintf ConsoleColor.DarkRed "\b and attack:"
                avMoves.[0] |>
                    List.iter (fun pos ->
                                cprintf ConsoleColor.DarkRed
                                    (sprintf " %s," (nicePos pos)))
            printf "\b.\n"

        /// <summary>Print the chess board with correct colors.</summary>
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

        /// <summary>Checks if a p1 is in check.</summary>
        /// <param name="kingPos">The kings position.</param>
        /// <param name="enemy">The enemy player.</param>
        /// <returns>True if in check, else false.</returns>
        member this.isCheck(kingPos:Position,enemy:Player) :bool =
            let mutable check = false
            for piece in enemy.pieces do
                if List.contains kingPos
                    (piece.availableMoves(board)).[0]
                then check <- true
            check

        /// <summary>Start and run the game.</summary>
        member this.run() :(Color * int list) =
        
            let mutable players :Player list = playerList
            let mutable colors = [White;Black]
            let mutable  nextM :Position list = []
            let mutable points = [0;0]

            // Hand out the pieces:
            players.[0].pieces <- wPieces 
            players.[1].pieces <- bPieces

            // Place pieces on the board
            board.[fst pPos.[0].Value,snd pPos.[0].Value] <- Some players.[0].pieces.[0] // White king.
            board.[fst pPos.[1].Value,snd pPos.[1].Value] <- Some players.[0].pieces.[1] // White rook.
            board.[fst pPos.[2].Value,snd pPos.[2].Value] <- Some players.[1].pieces.[0] // Black king.
            board.[fst pPos.[3].Value,snd pPos.[3].Value] <- Some players.[1].pieces.[1] // Black rook.

            // Welcome message:
            printf ">Welcome to SimpleChess!\n"
            printf "%s: " players.[0].name; cprintf wCol "K\n"
            printf "%s: " players.[1].name; cprintf bCol "k\n"
            
            // Game variables:
            let mutable turnCount = 0
            let mutable whoWon :Color option = None
            let mutable checkmate = false
            let mutable pTurn     = 0 // whoose turn it is.

            // Start the game loop:
            while not checkmate do

                turnCount <- turnCount + 1
                let pCol = if pTurn = 0 then wCol else bCol // Correct color.
                let wTurn = if pTurn = 0 then 1 else 0 // Waiting player number.
                    
                // Print turn info:
                do printf "\n>Turn %i: " turnCount
                cprintf pCol (players.[pTurn].name + ".\n")
                this.printBoard()
                do printfn "\nAnalysis:"
                Array.iter this.printPiece players.[0].pieces
                Array.iter this.printPiece players.[1].pieces

                if (this.isCheck(players.[pTurn].pieces.[0].position.Value,
                        players.[wTurn]))
                then
                    printf "\n%s is in check!\n" players.[pTurn].name
                    nextM <- players.[pTurn].nextMove board players.[wTurn].pieces
                else
                    // Ask player for next move:
                    nextM <- players.[pTurn].nextMove board players.[wTurn].pieces

                do printf "\nMove made: "

                // Check if a piece was taken:
                if board.[fst nextM.[1], snd nextM.[1]].IsSome
                then
                    let taker = board.[fst nextM.[0], snd nextM.[0]].Value
                    let taken = board.[fst nextM.[1], snd nextM.[1]].Value

                    if taker.color = White
                    then points <- [points.[0] + 3; points.[1] - 3 ]
                    else points <- [points.[0] - 3; points.[1] + 3 ]

                    // Print what happened:
                    cprintf pCol (sprintf "[%s -> %s], %s took %s!\n"
                        (nicePos nextM.[0]) (nicePos nextM.[1])
                            (taker.ToString()) (taken.ToString()))

                    // Remove the taken piece from the board:
                    players.[if pTurn = 0 then 1 else 0].pieces <-
                        players.[if pTurn = 0 then 1 else 0].pieces |>
                            Array.filter (fun rem -> rem <> taken)
                else
                    // Print what happened:
                    cprintf pCol (sprintf "[%s -> %s].\n"
                        (nicePos nextM.[0]) (nicePos nextM.[1]))
                // Make move:
                board.move nextM.[0] nextM.[1]

                // Change turn:
                if pTurn = 0 then pTurn <- 1 else pTurn <- 0

                // Check if chessmate (if the kings are out of moves):
                if players.[1].pieces.[0].availableMoves(board).[1].IsEmpty ||
                    Array.isEmpty players.[1].pieces
                then
                    cprintf wCol (sprintf "\nCheckmate! %s won the game."
                        (players.[0].name))
                    whoWon <- Some players.[0].color
                    checkmate <- true
                elif players.[0].pieces.[0].availableMoves(board).[1].IsEmpty ||
                        Array.isEmpty players.[0].pieces
                then
                    cprintf bCol (sprintf "\nCheckmate! %s won the game."
                        (players.[1].name))
                    whoWon <- Some players.[1].color
                    checkmate <- true
            (whoWon.Value, points)
    end

    /// <summary>
    /// Simulation of a human chess-player. Subclass of Player.
    /// </summary>
    /// <param name="name">The players name.</param>
    /// <param name="color">The players color.</param>
    type Human(name:string, color:Color) = class
        inherit Player(name, color)

        /// <summary>
        /// The human picks a move by entering a codestring. The codestring is
        /// then checked for validity and if it checks out the source and target
        /// position is returned.
        /// </summary>
        /// <param name="board">Chess board the move is to be played on.</param>
        /// <returns>The chosen source and target positions in a list.</returns>
        override this.nextMove (board:Board) (enemyPieces:chessPiece [])
            :Position list =

            // Ask for input:
            do printfn "%s%s"
                "\nEnter a codestring to move, enter quit to exit. "
                    "(E.g. \"a4 a5\")"
            do printf "Input: "
            let input = System.Console.ReadLine()

            let validNumbers = ['1';'2';'3';'4';'5';'6';'7';'8']
            let validLetters = ['a';'b';'c';'d';'e';'f';'g';'h']

            if input = "quit" then System.Environment.Exit(0); []
            // Check if input is valid:
            elif input.Length <> 5
            then
                do printfn "Codestring invalid: wrong length."
                this.nextMove board enemyPieces
            elif not (List.contains input.[0] validLetters) ||
                    not (List.contains input.[1] validNumbers) ||
                        not (List.contains input.[3] validLetters) ||
                            not (List.contains input.[4] validNumbers)
                then
                    do printfn
                        "Codestring invalid: unrecognized letters/numbers."
                    this.nextMove board enemyPieces
            else
                let move = parseInput input
                if board.Item(fst move.[0], snd move.[0]).IsNone
                then
                    do printfn "Invalid move: No piece there."
                    this.nextMove board enemyPieces
                else
                    let movePiece = board.Item(fst move.[0], snd move.[0]).Value
                    if movePiece.color <> this.color
                    then
                        do printfn "Invalid move: Not your piece."
                        this.nextMove board enemyPieces
                    elif not (List.contains (move.[1]) 
                                (movePiece.availableMoves(board).[1]))
                    then
                        do printfn "Invalid move: Not legal."
                        this.nextMove board enemyPieces
                    // if nothing weird happened, return chosen move:
                    else move
    end

    /// <summary>
    /// Simulation of a computer chess-player. Subclass of Player.
    /// </summary>
    /// <param name="name">The players name.</param>
    /// <param name="color">The players color.</param>
    type Computer(name:string, color:Color) = class
        inherit Player(name, color)

        /// <summary>
        /// The computer chooses a random move.
        /// </summary>
        /// <param name="board">Chess board the move is to be played on.</param>
        /// <returns>The chosen source and target positions in a list.</returns>
        override this.nextMove (board:Board) (enemyPieces:chessPiece [])
            :Position list =

            let r = Random();
            let rPiece = this.pieces |> Seq.item (r.Next this.pieces.Length)
            let aMoves = rPiece.availableMoves(board).[1]
            let computerMoves = [rPiece.position.Value; aMoves |>
                                    Seq.item (r.Next aMoves.Length)]

            // return:
            computerMoves

        /// <summary>
        /// The computer tries to be smart. (11g.4)
        /// </summary>
        /// <param name="board">Chess board the move is to be played on.</param>
        /// <returns>The chosen source and target positions in a list.</returns>
        /// override this.nextMove (board:Board) (enemyPieces:chessPiece []) // Not working very good...
        member this.nextMove2 (board:Board) (enemyPieces:chessPiece [])
            :Position list =

            let simPlayers :Player list = if this.color = White
                                           then [this;(Computer("SimComputer", Black))]
                                           else [(Computer("SimComputer", White));this]

            let avMoves = List.append ((this.pieces.[0].availableMoves board).[1] |>
                            List.map (fun elm -> (elm,this.pieces.[0])))
                                ((this.pieces.[0].availableMoves board).[1] |>
                                    List.map (fun elm -> (elm,this.pieces.[0])))
            
            //for simMove in avMoves do
            let mutable points :int list = []
            let r = Random()

            for elm in avMoves do
                match elm with
                    | (simMove:Position), (simPiece:chessPiece) ->
                        let sim = Game(
                                    simPlayers,
                                    (if this.color = White then this.pieces else enemyPieces),
                                    (if this.color = Black then this.pieces else enemyPieces),
                                    (if this.color = White
                                     then if (simPiece).nameOfType = "king"
                                          then [Some (simMove);
                                                this.pieces.[1].position;
                                                    enemyPieces.[0].position;
                                                        enemyPieces.[1].position]
                                          else [this.pieces.[0].position;
                                                Some (simMove);
                                                    enemyPieces.[0].position;
                                                        enemyPieces.[1].position]
                                     else if (simPiece).nameOfType = "king"
                                          then [enemyPieces.[0].position;
                                                enemyPieces.[1].position;
                                                    Some (simMove);
                                                        this.pieces.[1].position]
                                          else [enemyPieces.[0].position;
                                                    enemyPieces.[1].position;
                                                        this.pieces.[0].position;
                                                            Some (simMove)]),
                                    ConsoleColor.Green,
                                    ConsoleColor.Black,
                                    ConsoleColor.DarkMagenta)                                  

                        let simResult = sim.run()
                        let mutable simPoints = snd simResult
                        if fst simResult = White
                        then simPoints <- [simPoints.[0] + 100; simPoints.[1] - 100]
                        else simPoints <- [simPoints.[0] - 100; simPoints.[1] + 100]
                        if this.color = White
                        then points <- simPoints.[0]::points
                        else points <- simPoints.[1]::points
            points <- List.rev points
            [fst avMoves.[points |> List.findIndex (fun elm -> elm = List.max points)]]
            
    end