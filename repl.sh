#!/bin/sh

sbcl --eval "(ql:quickload \"sdl2-tutorial\")" \
     --eval "(sdl2-tutorial:create-swank-server)"
