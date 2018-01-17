namespace SimpleChess2

module Pieces =
    open Chess

    /// A king is a chessPiece which moves 1 square in any direction
    type king(col:Color) =
        inherit chessPiece (col)
        override this.nameOfType with get() = "king"
        // king has runs of 1 in 8 directions : (N, NE, E, SE, S, SW, W, NW)
        override this.candiateRelativeMoves =
            [[(-1,0)];[(-1,1)];[(0,1)];[(1,1)];
            [(1,0)];[(1,-1)];[(0,-1)];[(-1,-1)]]

        /// <summary>
        /// Finds available moves for the king.
        /// </summary>
        /// <param name="board">Chess board the move is to be played on.</param>
        /// <returns>
        /// A list with two Position lists in it. The first one with attack
        /// moves and the second one all available moves as
        /// [attackMoves;availableMoves].
        /// </returns>
        override this.availableMoves (board:Board) : Position list list =

            // Find available attack moves by checking adjacent tiles and 
            // filtering out the unoccupied and friedly-occupied tiles.
            let mutable attackMoves :Position list =
                this.candiateRelativeMoves |>
                    List.map (fun elm ->
                        match elm.[0] with
                        | (y,x) -> (y + this.y, x + this.x)) |>
                            List.filter (fun (u,v) -> 
                                not (u<0 || u>7 || v<0 || v>7) &&
                                board.Item(u, v).IsSome &&
                                board.Item(u, v).Value.color <> this.color)

            // Add attackMoves to avMoves:
            let mutable avMoves :Position list =
                List.append attackMoves (fst (board.getVacantNNeighbours this))

            // Check if the moves in avMoves are valid, if not -
            // filter them out:
            for testPos in avMoves do
                match testPos with
                    | (y,x) ->

                        // Checks if piece can be attacked from a certain path:
                        let checkMove (range:int list) (direction:Direction) =

                            // blockingPiece=true if dangerous path is blocked.
                            let mutable blockingPiece = false

                            // Check all tiles in given range:
                            for i in range do
                                let mutable look :chessPiece option = None

                                // If looking east or west change x:
                                if direction = RightLeft
                                then
                                    look <- board.Item(y,i)
                                // If looking east or west change y:
                                elif direction = UpDown
                                then
                                    look <- board.Item(i,x)
                                
                                if look.IsSome
                                then
                                    if look.Value.nameOfType = "king" &&
                                        look.Value.color <> this.color
                                    then
                                        // Check if enemy rook attack path is
                                        // blocked by enemy king:
                                        if ((abs(look.Value.x-x) > 1) ||
                                            (abs(look.Value.y-y) > 1))
                                        then
                                            blockingPiece <- true
                                        elif ((abs(look.Value.x-x) <= 1) &&
                                                (abs(look.Value.y-y) <= 1))
                                        then
                                            let dangerMove :Position = (y,x)
                                            avMoves <- avMoves |>
                                                List.filter (fun pos ->
                                                    pos <> dangerMove)
                                            attackMoves <- attackMoves |>
                                                List.filter (fun pos ->
                                                    pos <> dangerMove)

                                    // Check if enemy rook attack path is
                                    // blocked by friendly rook:
                                    elif look.Value.nameOfType = "rook" &&
                                            look.Value.color = this.color
                                    then
                                        blockingPiece <- true

                                    // If enemy rooks attack path is not blocked
                                    // then check if it can be killed - if not
                                    // remove position from available moves:
                                    elif look.Value.nameOfType = "rook" &&
                                            look.Value.color <> this.color &&
                                            (blockingPiece = false) &&
                                            (not ((abs(look.Value.x-x) = 0) &&
                                                (abs(look.Value.y-y) = 0)))
                                    then
                                        let dangerMove :Position = (y,x)
                                        avMoves <- avMoves |>
                                            List.filter (fun pos ->
                                                            pos <> dangerMove)
                                            
                        // Look east:
                        let eastRange = [for i = x+1 to 7 do yield i]
                        checkMove eastRange RightLeft
                        // Look west:
                        let westRange = [for i = x-1 downto 0 do yield i]
                        checkMove westRange RightLeft
                        // Look north:
                        let northRange = [for i = y+1 to 7 do yield i]
                        checkMove northRange UpDown
                        // Look south:
                        let southRange = [for i = y-1 downto 0 do yield i]
                        checkMove southRange UpDown

            // return:
            [attackMoves; avMoves]

    // / A rook is a chessPiece which moves horisontally and vertically
    type rook(col:Color) =
        inherit chessPiece(col)
        // rook can move horisontally and vertically
        // Make a list of relative coordinate lists . We consider the
        // current position and try all combinations of relative moves
        // (1,0); (2,0) ... (7,0); (-1 ,0); ( -2 ,0) ; ...; (0 , -7) .
        // Some will be out of board , but will be assumed removed as
        // illegal moves .
        // A list of functions for relative moves
        let indToRel = [
            fun elm -> (elm,0); // South by elm
            fun elm -> (-elm,0) ; // North by elm
            fun elm -> (0,elm) ; // West by elm
            fun elm -> (0,-elm) // East by elm
        ]
        // For each function in indToRel, we calculate List.map f [1..7].
        // swap converts ( List.map fct indices) to (List.map indices fct).
        let swap f a b = f b a
        override this.candiateRelativeMoves =
            List.map (swap List.map [1..7]) indToRel
        
        override this.availableMoves (board:Board) : Position list list =

            let mutable avMoves = (fst (board.getVacantNNeighbours this))
            let mutable attackMoves :Position list = []



            // Return:
            [attackMoves;List.append attackMoves avMoves]

        override this.nameOfType with get() = "rook"