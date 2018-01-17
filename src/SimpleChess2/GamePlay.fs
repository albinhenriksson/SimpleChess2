namespace SimpleChess2

open System
module GamePlay =
    open Chess
    open Pieces

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
        
        abstract member nextMove : Board -> Position list
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
        override this.nextMove(board:Board) :Position list =

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
                this.nextMove(board)
            elif not (List.contains input.[0] validLetters) ||
                    not (List.contains input.[1] validNumbers) ||
                        not (List.contains input.[3] validLetters) ||
                            not (List.contains input.[4] validNumbers)
                then
                    do printfn
                        "Codestring invalid: unrecognized letters/numbers."
                    this.nextMove(board)
            else
                let move = parseInput input
                if board.Item(fst move.[0], snd move.[0]).IsNone
                then
                    do printfn "Invalid move: No piece there."
                    this.nextMove(board)
                else
                    let movePiece = board.Item(fst move.[0], snd move.[0]).Value
                    if movePiece.color <> this.color
                    then
                        do printfn "Invalid move: Not your piece."
                        this.nextMove(board)
                    elif not (List.contains (move.[1]) 
                                (movePiece.availableMoves(board).[1]))
                    then
                        do printfn "Invalid move: Not legal."
                        this.nextMove(board)
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
        override this.nextMove(board:Board) :Position list =
            let r = Random();
            let rPiece = this.pieces |> Seq.item (r.Next this.pieces.Length)
            let aMoves = rPiece.availableMoves(board).[1]
            let computerMoves = [rPiece.position.Value; aMoves |>
                                    Seq.item (r.Next aMoves.Length)]

            // return:
            computerMoves
    end

    /// <summary>Simulation of a chess game.</summary>
    /// <param name="nHumans">Number of human players.</param>
    /// <param name="wCol">White player color.</param>
    /// <param name="bCol">Black player color.</param>
    /// <param name="boardCol">Color of the chessboard.</param>
    type Game(
                nHumans:int,
                wCol:ConsoleColor,
                bCol:ConsoleColor,
                boardCol:ConsoleColor) = class

        let board = Chess.Board()

        /// <summary>Print info about a chess piece.</summary>
        /// <param name="p">The chess piece.</param>
        member this.printPiece (p:chessPiece) :unit =
            let aMoves = p.availableMoves board
            // Print in the correct color:
            let mutable fgCol = Console.ForegroundColor
            if p.color = White then fgCol <- wCol
            else fgCol <- bCol
            cprintf fgCol (sprintf "%A can move from %s to:"
                            p (nicePos p.position.Value))
            // Print available moves:
            aMoves.[1] |>
                List.iter (fun pos ->
                            cprintf fgCol (sprintf " %s," (nicePos pos)))
            // Print attack moves:
            if not aMoves.[0].IsEmpty
            then
                cprintf ConsoleColor.DarkRed "\b and attack:"
                aMoves.[0] |>
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
        member this.run() =
        
            let mutable players :Player list = []
            let mutable colors = [White;Black]
            let mutable  nextM :Position list = []

            // Add human(s):
            for i in [0..(nHumans-1)] do
                players <- List.append players
                    [Human((sprintf "Human-%i (%A)" (i+1) colors.[0]), colors.[0])]
                colors <- colors.Tail
            
            //Add computer(s):
            for i in [1..(2-nHumans)] do
                    players <- List.append players
                        [Computer((sprintf "Computer-%i (%A)"
                                    i colors.[0]), colors.[0])]
                    colors <- colors.Tail

            // Hand out the pieces:
            players.[0].pieces <- [|
                king (White) :> chessPiece;
                rook (White) :> chessPiece|]
            players.[1].pieces <- [|
                king (Black) :> chessPiece;
                rook (Black) :> chessPiece|]

            // Place pieces on the board
            board.[0,0] <- Some players.[0].pieces.[0] // White king.
            board.[1,1] <- Some players.[0].pieces.[1] // White rook.
            board.[5,1] <- Some players.[1].pieces.[0] // Black king.
            board.[4,1] <- Some players.[1].pieces.[1] // Black rook.

            // Welcome message:
            printf ">Welcome to SimpleChess!\n"
            printf "%s: " players.[0].name; cprintf wCol "K\n"
            printf "%s: " players.[1].name; cprintf bCol "k\n"
            
            // Game variables:
            let mutable turnCount = 0
            let mutable checkmate = false
            let mutable pTurn     = 0 // whoose turn it is.

            // Start the game loop:
            while not checkmate do

                turnCount <- turnCount + 1
                let pCol = if pTurn = 0 then wCol else bCol // Correct color.
                    
                // Print turn info:
                do printf "\n>Turn %i: " turnCount
                cprintf pCol (players.[pTurn].name + ".\n")
                this.printBoard()
                do printfn "\nAnalysis:"
                Array.iter this.printPiece players.[0].pieces
                Array.iter this.printPiece players.[1].pieces

                if (this.isCheck(players.[pTurn].pieces.[0].position.Value,
                        players.[if pTurn = 0 then 1 else 0]))
                then
                    printf "\n%s is in check!\n" players.[pTurn].name
                    nextM <- players.[pTurn].nextMove(board)
                else
                    // Ask player for next move:
                    nextM <- players.[pTurn].nextMove(board)

                do printf "\nMove made: "

                // Check if a piece was taken:
                if board.[fst nextM.[1], snd nextM.[1]].IsSome
                then
                    let taker = board.[fst nextM.[0], snd nextM.[0]].Value
                    let taken = board.[fst nextM.[1], snd nextM.[1]].Value

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
                if players.[1].pieces.[0].availableMoves(board).[1].IsEmpty
                then
                    cprintf wCol (sprintf "\nCheckmate! %s won the game."
                        (players.[0].name))
                    checkmate <- true
                elif players.[0].pieces.[0].availableMoves(board).[1].IsEmpty
                then
                    cprintf bCol (sprintf "\nCheckmate! %s won the game."
                        (players.[1].name))
                    checkmate <- true
    end