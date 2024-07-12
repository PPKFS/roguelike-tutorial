# Introduction

## Motivation

The Python+libtcod roguelike tutorial series, and all of its spinoffs (including for Rust with `rltk`, Javascript with `rot.js`, and C# with `RogueSharp` to name just a few) have been super popular in the roguelike gamedev space for over a decade now. "r/roguelikedev does the roguelike tutorial" has become a yearly event. There's a great roguelike fan Discord server with an active dev chat. Making roguelikes is great fun, and it's easier than people think. There's a kind of mental jump going between "I can solve all these problems" and "I can write an application". Maybe the reason these tutorial series are so popular, despite being very clear that nothing they are introducing to the reader is new to them, is because they provide a welcoming helping hand. They take you from being able to write a bunch of classes or functions to an actual *game* with a randomly generated dungeon with *monsters* and *combat* and *items* and all that cool stuff.

So, this is me attempting to do the same but for Haskell. A helping hand. Which is a bit of a challenge, given the reputation about how purity and functional programming isn't suited to gamedev, but it's far more doable than you think! A lot of the flak Haskell gets is that it's complicated (not unless you make it so), you need to know all about monads and algebraic effects and functional lenses (you don't), or that it's not suited to most sorts of application (again, not true).

This tutorial **won't be an introduction to Haskell for completely new people**, but it does aim to be followable for someone who has a decent understanding of the basics - perhaps most of a university functional programming course, or having read Learn You A Haskell or Real World Haskell. If you are coming from another functional language (F#, Scala, OCaml) then *most* of the concepts should be familiar to you and it is mostly syntax that isn't.

If the following list doesn't make you run away screaming, then fingers are crossed that this should be understandable:
- The Haskell language
  - Datatypes, records, ADTs, polymorphism, typeclasses, constraints, functions;
  - Polymorphism (type parameters, polymorphic functions);
  - Data structures - lists/vectors/arrays, maps, sets;
  - `map`/`fmap`/`mapM`, `filter`, `fold`;
- Monads
  - do-notation
  - State, Reader, Writer, IO;
  - Simple monad stacks, in whatever form (`StateT s IO a` or `MonadState s m => m a` or `State s :> es => Eff es a`)
- Modules

And it'd be *very useful* to know about:
- `cabal` - adding build-depends, modules, building and running projects
- Lenses/Optics
  - Just acknowledging their existence as nicer ways to get, set, and modify nested record fields (*especially* in combination with state monads or stateful effects);
  - `^./view`, `.~/set`, `%~/over`
  - the state monad equivalent optics - `use`, `~=`, and `%=`
  - Using overloaded labels to avoid the pain of duplicate record field names or having `typenameFoo` everywhere;
- Language extensions (e.g. `OverloadedLabels`, `OverloadedStrings`, `NoImplicitPrelude`, `TypeApplications`, etc)
- Basic roguelike concepts like generating tile-based dungeons and an obsession of rendering things in ASCII characters, but that's why you're here - right?

## Structure

- Part 0 (this one) Introduction, brief `optics` explanation, setting up the project
- Part 1: Drawing a @ and moving it around
- Part 2: Generating a couple of maps
- Part 3: Field of view
- Part 4: Monsters, AI
- Part 5: Combat
- Part 6: UI
- Part 7: Items and inventory
- ???

# Example screenshots

I'll update this as I go.

![](screenshot1.png)


# A brief introduction to optics (with `optics`)

Okay, I *love* using lenses in Haskell. I honestly cannot imagine doing something like this - a complex, stateful thing where you are relying heavily on composition - without them. Whilst I've tried to strip as much unnecessary quality-of-life complexity out of this tutorial (e.g. `effectful` rather than `mtl` and a sort of data-only subtyping + ID lookup framework that I really should write a blog post about) I feel the minor complexity boost from using `optics` is easily worth its weight in gold. It has a reputation (even more than Haskell) of being complex, but 90% of it comes down to the basic 3 operations of getting (`s -> a`), setting (`a -> s -> s`), and modifying (`(a -> a) -> s -> s`).

---

Optics and lenses are a solution to Haskell's record problem. As most Haskellers know, trying to modify a record field can be annoying:

```haskell

data R2 = R2 { nestedField :: Int } deriving Generic
data Record = R { someField :: R2 } deriving Generic
modifyIt :: R -> (Int -> Int) -> R
modifyIt record f =
  let f1 = someField record
  in record { someField = f1 { nestedField = f (nestedField f1) } }
```

and so can a nested lookup with some logic:

```haskell
lookupNestedMap :: Record -> Key -> (Value -> b) -> Maybe b
lookupNestedMap record k f = fmap f . lookup k . field3WhichIsAMap . field2 . field1 $ record
```

Yuck. Fortunately, there exists a solution in the form of optics. An optic is a combination of "here is how to look up the target(s) associated with this optic, and here is how to modify the target(s) associated with this optic". I say "target(s)", because this can differ on the optic. 95% of the time, you'll just need `Lens` and `Prism`.

- A `Lens'` is a 1-1 relationship. It's basically a record field. You can get the field and set the field.
- A `Prism'` is a 0-1 relationship (with some additional reconstruction properties I won't get into). It's basically `Maybe` a record field. You can *maybe* get the field, you can set the field *if it exists*. Yes, set is `fmap`.

And when it comes to doing things with optics, there's three main things:

- you can `view` the target, `view theLens x` or `x ^. theLens`.
- you can `set` the target, `set theLens newValue x` or `x & theLens .~ newValue`.
- you can apply a function to (`over`) the target, `over theLens f x` or `x & theLens %~ f`.

The order of arguments means that you can sequence operations to perform multiple `set` and `over` on something by using reverse function composition `(&)`:

```haskell

x & lens1 .~ val1 & lens2 %~ f2 & lens3 .~ val3

-- is the same as

let x1 = x & lens1 .~ val1
    x2 = x1 & lens2 %~ f2
    in
      x2 & lens3 .~ val3
```

Optics can be composed. If you have a lens from `s` to `a`, and a lens from `a` to `z`, you can make a lens from `a` to `z`. For the `lens` library this is `.` (same as function composition) and for `optics` it's `%`.

The first example can be written with `optics` as

```haskell

modifyIt record f = record & (#someField % #nestedField %~ f)
```

and the second as

```haskell
-- ^? is roughly "view a maybe value"

lookupNestedMap record k f = f <$> (record ^? #field1 % #field2 % #field3WhichIsAMap % at k)
```

Much nicer!

Optics (the concept) are mostly implemented by two big libraries, `lens` and `optics` (the library) as well as some others (e.g. `microlens`). Both differ slightly in implementation, scope, and so on and a discussion is out of scope and also my wheelhouse here but I prefer to use `optics` because:

- it supports making lenses with `OverloadedLabels` from a `Generic` instance
- the type errors are nicer
- easier support for adding extra label optics

For a more thorough introduction, check out:

- [School of Haskell's introduction to lens](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)
- [the tutorial for `lens`](https://hackage.haskell.org/package/lens-tutorial-1.0.5/docs/Control-Lens-Tutorial.html) (but swapping `%` and `.` and `lens` has a `Functor` based implementation)
- [documentation for `optics`](https://hackage.haskell.org/package/optics-0.4.2.1/docs/Optics.html)

But hopefully the uses of them in this tutorial series are well-enough explained at the place of their use.

# Setting up the project

The rest of this part is setting up an empty cabal project and installing the relevant libraries, and verifying that indeed the hell of library paths has been solved.

## Installing `bearlibterminal`

First, we need to download [bearlibterminal](https://github.com/cfyzium/bearlibterminal), which is the *amazing* graphics library that `roguefunctor` is built on. There are prebuilt binaries available at the bottom of the readme on GitHub. If you are on a platform that doesn't have a prebuilt binary (for example, an M1 Macbook with Apple Silicon) then you can build it yourself:

```bash

git clone https://github.com/cfyzium/bearlibterminal.git
cd bearlibterminal/Build
cmake ../
make all
```

With the library file (`.so`,`.dll`, `.dylib`) you have two options:

- you can either install this yourself into `/usr/lib` or equivalent.
- you can copy the library file into some directory of your project and pass the library option to cabal with `--extra-lib-dirs=/path/to/the/library`. The downside of this method is that you do *also* need to set `LD_LIBRARY_PATH`(Linux) or `DYLD_LIBRARY_PATH` (Mac) or something else (Windows) to actually do `cabal run`, because it doesn't copy the library into the cabal build directory. If anyone knows cabal better than I, please let me know how to set this up! PRs also incredibly welcome.

## Making a new `cabal` project

Let's begin by making a new project directory and initialising a blank `cabal` project. We'll go for the unexciting name `hs-rogue`.

```bash

mkdir hs-rogue
cd hs-rogue
cabal init
```

This will start the interactive wizard for setting up a new cabal project. The options of importance:

- we want just an executable project.
- we want **cabal version 3.4 minimum**, because `default-language: GHC2021` saves a lot of time with writing out language extensions.
- we want to use `GHC2021` as the language for our executable, as this enables a bunch of language extensions for us.

This gives us `hs-rogue.cabal` pre-populated. Unfortunately, the libraries we need `bearlibterminal-hs` and `roguefunctor` are not on Hackage so cabal cannot automatically download them. We need a `cabal.project` file that specifies where the repositories for these libraries can be found as well as the packages in our project. We only have one package, but otherwise `cabal run` will complain that the project file lists no packages to run. Create `cabal.project` in the directory and add the following:

```cabal
source-repository-package
  type: git
  location: https://github.com/PPKFS/roguefunctor.git
  tag: b2de932cffea43767df772582aea52c62d1843b3
source-repository-package
  type: git
  location: https://github.com/PPKFS/bearlibterminal-hs.git
  tag: c6684515801a500655e30c14d787dbb46e2e0529
packages:
  hs-rogue.cabal
```

Whilst we're at it, it's safe to assume we would like `haskell-language-server` to work with this project so we'll add `hie.yaml` as well:

```yaml
cradle:
  cabal:
    - path: "hs-rogue/app"
      component: "executable:hs-rogue"
```

So now we will have tooltips and code actions and hints. Nice.

We've got one final setup step, and that's adding some dependencies and extensions to the `cabal` file. These are the ones we'll need for the first part of the tutorial. I'll make sure in future parts to put all the modifications to the `.cabal` file at the start of the post. Under `build-depends`, add:

```cabal
build-depends:
  base
  , bearlibterminal
  , roguefunctor
  , containers
  , optics
```

- `bearlibterminal` and `roguefunctor` are the two libraries we are using for this tutorial. You *could* very easily do this tutorial without `roguefunctor` and just use `bearlibterminal`. However, it introduces some useful abstractions (like viewports, event handling, colours, field of view algorithm[s], and so on) so it means we can spend more time making a game and less time writing engine infrastructure.

- `containers` - as we're basically doing everything as a stateful `Map` of IDs to various *things*, this is kind of key to the whole thing.
- `optics` - making updating the stateful `Map`s a lot easier.

Now if you run `cabal build`, it should download and build the two libraries. You'll know your setup for installing `bearlibterminal` was correct if it successfully builds, as otherwise it will give errors about missing C libraries.

Finally we can add a few default extensions:
```cabal
    default-extensions:
      NoImplicitPrelude
      LambdaCase
      OverloadedLabels
      OverloadedStrings
      DerivingStrategies
    default-language: GHC2021 -- if it's not already added
```

- `NoImplicitPrelude` works better than cabal mixins for using a custom prelude. `Rogue.Prelude` is a thin wrapper around [relude](https://hackage.haskell.org/package/relude) which is my prelude of choice - `Text` over `String` by default, no partial functions, polymorphic `show`, and many other nice-to-haves.
- `LambdaCase` is just a no-brainer because `\case` is great.
- `OverloadedLabels` is needed for making `optics` super nice to work with.
- `OverloadedStrings` because we want to use `Text` over `String` for efficiency.
- `DerivingStrategies` is just good practice.

I typically use `TemplateHaskell` and `RecordWildCards` on a per-module basis.

There's a lot more extensions enabled by `GHC2021` which I won't go into, but the most useful ones are the various `Derive*` extensions and `TypeApplications`.

## Chat, does it work?

To verify that everything has been set up correctly, you can open a window by copying this into `Main.hs` - a full explanation will be in Part 1, along with drawing some things to the screen:

```haskell

import Rogue.Prelude
import Rogue.Window
import Rogue.Config
import Rogue.Geometry.V2
import Rogue.Colour
import Rogue.Events

screenSize :: V2
screenSize = V2 100 50

main :: IO ()
main =
  withWindow
  defaultWindowOptions { size = Just screenSize }
  pass
  (const runLoop)
  pass

runLoop :: MonadIO m => m ()
runLoop = do
  terminalRefresh
  -- event handling
  shouldContinue <- handleEvents Blocking $ \case
    WindowEvent Resize -> return True
    WindowEvent WindowClose -> return False
    Keypress TkEsc -> return False
    _ -> return False
  when (and shouldContinue) runLoop
```

and running your program with `cabal run hs-rogue`. If everything goes to plan, you should now have a black window open! If you cannot *build* the project because of missing libraries, make sure you are supplying `--extra-lib-dirs`. If you can build but not *run* the project because of missing libraries, make sure you've copied `libbearterminal.so/dylib/dll` to a location on your PATH or `export LD_LIBRARY_PATH/DYLIB_LIBRARY_PATH`.

# Wrapping up

Well, that's it for part 0. Not exactly much coding, but we can now hit the ground running for Part 1!

The code - well, a blank project structure - is available at the accompaying github repo here.

Hopefully this was understandable - any feedback is greatly appreciated. You can find me on the various functional programming/roguelike Discords as ppkfs and on Twitter as @ppk_fs_.