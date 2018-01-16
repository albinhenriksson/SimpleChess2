#load "Chess.fs"
#load "Pieces.fs"
#load "GamePlay.fs"

open System
open SimpleChess2

let numberOfHumanPlayers = 2
let whitePlayerColor     = ConsoleColor.Green
let blackPlayerColor     = ConsoleColor.Magenta
let boardColor           = ConsoleColor.DarkBlue

// Run game:
let game = GamePlay.Game(
            numberOfHumanPlayers,
            whitePlayerColor,
            blackPlayerColor,
            boardColor)
game.run()