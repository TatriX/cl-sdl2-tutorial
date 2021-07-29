# Common Lisp SDL2 tutorials
Adapted from http://lazyfoo.net/tutorials/SDL/

## Installing the SDL2 Libraries

  The tutorial requires that SDL2, SDL2 Image, and SDL2 TTF are installed before
  running the examples.

  See instructions for [Linux](README_LINUX.md) and [macOS](README_MACOS.md) respectively.


## Installation

Using [quicklisp](https://www.quicklisp.org/beta/):
```
git clone https://github.com/TatriX/cl-sdl2-tutorial/ ~/quicklisp/local-projects
```

```lisp
(ql:quickload :sdl2-tutorial)
```

or [asdf](https://common-lisp.net/project/asdf/)
```
git clone https://github.com/TatriX/cl-sdl2-tutorial/ ~/common-lisp
```

```lisp
(asdf:load-system "sdl2-tutorial")
```

## Running the Examples

Tutorial packages are named as `sdl2-tutorial-XX-DESC`
where `XX` is a number starting from `01`.

```lisp
(sdl2-tutorial-01-hello-sdl:run)
;;
(sdl2-tutorial-04-key-presses:run)
```

## Notes
As far as I can tell `cl-sdl2` in most cases doesn't free created
objects. This means it's up to a user to do so. I've tried to free
allocated resources, but as with any manual memory management I could
miss something. If you notice anything, please create an issue or even
send a PR!
