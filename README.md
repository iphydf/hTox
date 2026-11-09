# hTox

hTox is a Tox client for Android, written in Haskell with
[Hatter](https://github.com/jappeace/hatter) on hs-tox, the Tox client
without its UI.

- `src/`: the app. `Model` is the screens' state on top of hs-tox's client,
  `Store` what is kept between runs in SQLite (Tox gets only our secret key
  from it, and the schema moves on by migrations), `View` the widgets of each
  screen, `App` the wiring between them and Hatter. `Theme` has the light and
  dark colours, `Emoji` and `Links` read messages for display.
- `app/Main.hs`: the entry point Hatter's Android build runs.
- `browser/`: hTox in a browser, on Hatter's browser host, reaching Tox's
  relays through a WebSocket gateway.
- `desktop/`: hTox on the desktop, on Hatter's Qt Quick platform, with the
  Android icons.
- `platform/`: the Android manifest, theme, icons, activity, and the
  foreground service that keeps Tox running while no screen is open.
- `terminal/`: hTox in a terminal, on Hatter's terminal platform, with the
  icons as characters.
- `nix/`: the Hatter pin, the Android builds, and the Haskell packages nixpkgs
  lacks. Hatter builds from the workspace's `third_party/hatter`, which has our
  changes to it, and hs-toxcore, hs-toxav, hs-toxsync, hs-tox and the msgpack
  packages from their checkouts here.

Build the APK, for arm64 phones:

```sh
nix-build nix/apk.nix
adb install result/htox.apk
```

Every build is signed with `nix/debug.keystore`, so it installs over the last.

Run the tests on the desktop:

```sh
nix-shell --run 'cabal test'
```

The first Android build compiles a cross GHC, which takes a while.

Build hTox for the desktop, and run it:

```sh
nix-build desktop
result/bin/htox
```

Or from its shell, building Hatter's Qt platform from its checkout too:

```sh
cd desktop
nix-shell --run 'cabal run htox'
```

It keeps its chats in `~/.local/share/htox`, as the terminal build does.
Outside NixOS, Nix's Qt finds no GL driver, so hTox is drawn by the CPU,
without shadows; through nixGL it uses the GPU.

Run hTox in a terminal:

```sh
cd terminal
nix-shell --run 'cabal run htox'
```

It keeps its chats in `~/.local/share/htox`, encrypted with the key in
`database.key` there, and logs to `log.txt` there. Tab and the arrow keys move
between what can be tapped or typed in, Enter taps, Alt-Enter or a right click
holds, Esc goes back and Ctrl-Q quits. Calls, the camera and your location
aren't there.
