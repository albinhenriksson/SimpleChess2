#!/bin/bash

# Loading animation:
load_animation() {
    local j sp n
    sp='/-\|'
    n=${#sp}
    printf ' '
    while sleep 0.1; do
        printf "%s\b" "${sp:j++%n:1}"
    done
}

# Compile SimpleChess
compile() {
    fsharpc --nologo --out:bin/SimpleChess2.exe Chess.fs Pieces.fs GamePlay.fs chessApp.fsx && compile_done=true
}

# Run SimpleChess
run() {
    clear
    mono bin/SimpleChess2.exe
}

# Commands:
printf 'Compiling SimpleChess...'
load_animation &
compile_done=false
compile
kill "$!"
wait $! 2>/dev/null
if [ $compile_done = true ] ; then
    run
else
    printf '\bCompilation was unsuccessful.\n'
fi
