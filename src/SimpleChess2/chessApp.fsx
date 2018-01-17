#load "Chess.fs"
#load "Pieces.fs"
#load "GamePlay.fs"

open System
open SimpleChess2

// Number of human players:
let numberOfHumanPlayers = 1

// The color of the white player:
let whitePlayerColor     = ConsoleColor.Green

// The color of the black player:
let blackPlayerColor     = ConsoleColor.Magenta

// The color of the chess board:
let boardColor           = ConsoleColor.DarkBlue

// Run game:
let game = GamePlay.Game(
            numberOfHumanPlayers,
            whitePlayerColor,
            blackPlayerColor,
            boardColor)
game.run()