namespace SimpleChess2

module Pieces =
    open Chess

    // / A king is a chessPiece which moves 1 square in any direction
    type king(col:Color) =
        inherit chessPiece (col)
        override this.nameOfType with get() = "king"
        // king has runs of 1 in 8 directions : (N , NE , E , SE , S , SW , W , NW )
        override this.candiateRelativeMoves =
            [[(-1,0)];[(-1,1)];[(0,1)];[(1,1)];
            [(1,0)];[(1,-1)];[(0,-1)];[(-1,-1)]]

        // Glöm inte att skriva kommentarer här!
        override this.availableMoves (board:Board) : Position list list =

            let mutable attackMoves :Position list = this.candiateRelativeMoves |> List.map (fun elm ->
                match elm.[0] with
                    | (y,x) -> (y + this.y, x + this.x)) |> List.filter (fun (u,v) -> 
                        not (u<0 || u>7 || v<0 || v>7) &&
                        board.Item(u, v).IsSome &&
                        board.Item(u, v).Value.color <> this.color)

            let mutable aMoves :Position list = List.append attackMoves (fst (board.getVacantNNeighbours this))

            for testPos in aMoves do
                match testPos with
                    | (y,x) ->
                        let checkMove (range:int list) (direction:Direction) :unit =
                            let mutable blockingPiece = false
                            for i in range do
                                let mutable look :chessPiece option = None
                                if direction = RightLeft then
                                    look <- board.Item(y,i)
                                elif direction = UpDown then
                                    look <- board.Item(i,x)
                                if look.IsSome then 
                                    if look.Value.nameOfType = "king" &&
                                        look.Value.color <> this.color &&
                                        ((abs(look.Value.x-x) > 1) || (abs(look.Value.y-y) > 1)) then
                                            blockingPiece <- true
                                    elif look.Value.nameOfType = "rook" &&
                                        look.Value.color = this.color then
                                            blockingPiece <- true
                                    elif look.Value.nameOfType = "rook" &&
                                        look.Value.color <> this.color &&
                                        (blockingPiece = false) then
                                            // do printf "%s found enemy %s attack-path at (%i,%i) and "
                                            //     (this.ToString()) look.Value.nameOfType y x
                                            if (abs(look.Value.x-this.x) <= 1) &&
                                                (abs(look.Value.y-this.y) <= 1) &&
                                                (abs(look.Value.x-x) = 0) &&
                                                (abs(look.Value.y-y) = 0) then
                                                    //do printfn "is feeling brave!"
                                                    let attackMove :Position = (y,x)
                                                    printf "pling"
                                                    attackMoves <- attackMove::attackMoves
                                            else
                                                //do printfn "is feeling scared!"
                                                let dangerMove :Position = (y,x)
                                                aMoves <- aMoves |> List.filter (fun pos -> pos <> dangerMove)
                                            
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
            [attackMoves; (List.append aMoves attackMoves)]

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
        // For each function in indToRel , we calculate List . map f [1..7].
        // swap converts ( List . map fct indices ) to ( List . map indices fct ) .
        let swap f a b = f b a
        override this.candiateRelativeMoves =
            List.map (swap List.map [1..7]) indToRel
        
        override this.availableMoves (board:Board) : Position list list =
            
            // OBS FEL!
            let mutable attackMoves :Position list = this.candiateRelativeMoves |> List.map (fun elm ->
                match elm.[0] with
                    | (y,x) -> (y + this.y, x + this.x)) |> List.filter (fun (u,v) -> 
                        not (u<0 || u>7 || v<0 || v>7) &&
                        board.Item(u, v).IsSome &&
                        board.Item(u, v).Value.color <> this.color)

            let mutable aMoves :Position list = List.append attackMoves (fst (board.getVacantNNeighbours this))
            
            // Return:
            [attackMoves; (List.append aMoves attackMoves)]

        override this.nameOfType with get() = "rook"