# Write you a Haskell roguelike for great good!

This repo contains my vague attempts at resources for writing a simple roguelike game in Haskell. The libraries of choice are [roguefunctor](https://github.com/ppkfs/roguefunctor) for a high level toolkit library and [bearlibterminal-hs](https://github.com/ppkfs/bearlibterminal-hs) for rendering.

There is:

- A set of heavily documented source code projects following the original [python roguelike tutorial](https://rogueliketutorials.com) as part of the ["r/roguelikedev does the roguelike tutorial 2024"](https://www.reddit.com/r/roguelikedev/wiki/python_tutorial_series) event.
  - These are (hopefully) in 2 different paths:
  - one using simple Haskell (minimal monad transformers, no optics, no alternative preludes, no type-wizardry)
  - one using bells and whistles (`effectful`, `optics`, and various other fancy tools).
- Attempts at starting to write an `mdbook` written tutorial in the style of the original tutorial.

Currently the first is the most complete, so I recommend looking at that!
