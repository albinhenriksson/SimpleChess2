#load "Chess.fs"
#load "Pieces.fs"
#load "GamePlay.fs"

open System
open SimpleChess2
open Chess
open Pieces
open GamePlay

// Number of human players:
let numberOfHumanPlayers = 1

// The color of the white player:
let whitePlayerColor     = ConsoleColor.Green

// The color of the black player:
let blackPlayerColor     = ConsoleColor.Magenta

// The color of the chess board:
let boardColor           = ConsoleColor.DarkBlue

// White pieces (king first):
let wPieces = [|
                king (White) :> chessPiece;
                rook (White) :> chessPiece|]

// Black pieces (king first):
let bPieces = [|
                king (Black) :> chessPiece;
                rook (Black) :> chessPiece|]

// Positions of the pieces:
let piecePositions :Position Option list =
    [Some (0,0); // K
    Some (1,1);  // R
    Some (4,1);  // k
    Some (3,1)]  // r

// The players (white first):
let players :Player list = [
        Human((sprintf "Human-%i (%A)" 1 White), White);
            Computer((sprintf "Computer-%i (%A)" 1 Black), Black)]

// Run game:
let game = GamePlay.Game(
            players,
            wPieces,
            bPieces,
            piecePositions,
            whitePlayerColor,
            blackPlayerColor,
            boardColor)
game.run() |> ignore