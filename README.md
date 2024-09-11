### The Goal

When it's ready, this repo will contain a very simple helix plugin.  The idea is that if you want to play around with writing your own helix plugin, you can clone it and have a (hopefully) sane dev environment while you change the simple plugin into one that you're interested in.

In the future, this will seem like overkill, but since the plugin system is not yet merged in to helix/master, I think there's an opportunity to remove some friction which might otherwise prevent a would-be plugin author from providing feedback on the in-development plugin system.

It uses a nix flake for this.  If that's not your jam, appologies.

### Goodies

- `steel` launches the steel repl
- running `hx` or `hxs` in the project dir configures the steel language server and a scheme formatter (you have to run `nix build` first).
- `hxs` launches [mattwparas:helix/steel-event-system](https://github.com/mattwparas/helix/tree/steel-event-system) ([overrides the theme to `hex_toxic`](init.scm)) to visually distinguish it from `hx`)

#### How To

Build the goodies and inject their paths into `.helix/languages.toml`:
```
git clone git@github.com:MatrixManAtYrService/helix-plugin-env.git
cd helix-plugin-env
nix build
```

This populates `./result` which is symlinked from `./helix/`.

```
❯  hx --health scheme
Configured language servers:
  ✓ /nix/store/nlcb3x91044749ilbfymk7bz26jipd3r-steel-interpreter-0.6.0/bin/steel-language-server: /nix/store/nlcb3x91044749ilbfymk7bz26jipd3r-steel-interpreter-0.6.0/bin/steel-language-server
Configured debug adapter: None
Configured formatter: /nix/store/lqw1cz2kg5s0kgglbhikgc8z4ldwn0bp-code-formatter/bin/scheme-format
Binary for formatter: /nix/store/lqw1cz2kg5s0kgglbhikgc8z4ldwn0bp-code-formatter/bin/scheme-format
Highlight queries: ✓
Textobject queries: ✘
Indent queries: ✓
```

To run `hxs` or `steel` enter a dev-shell
```
❯ nix build     # stages helix.scm and init.scm
❯ nix develop   # prepares 'steel' and 'hxs'
  $ steel

         _____ __            __
        / ___// /____  ___  / /          Version 0.6.0
        \__ \/ __/ _ \/ _ \/ /           https://github.com/mattwparas/steel
       ___/ / /_/  __/  __/ /            :? for help
      /____/\__/\___/\___/_/

    λ > (display "hello world")
    hello world
    λ >
    CTRL-D
  $ hxs         # helix.scm and init.scm are referenced
  $ exit        # exit the devshell
❯ hxs           # command not found
```

(instead of `nix develop` you can also use [direnv](https://github.com/nix-community/nix-direnv) to activate this automatically when you enter/leave the plugin project dir)

### Emacs?

As nice as it would be to use helix to write helix plugins, making helix a first class lisp editor requires... plugins.
It's a bit of a catch-22.

For now I'm bundling emacs, configured with lispyville, which I'm using as a steel editor:

TODO: make a wrapper instead of using this:
`emacs -nw -q -l config.emacs example.scm`

### Currently Broken

#### cargo xtask steel

Most of this README was written on a system where I had run `cargo xtask steel` at some point.  

Then I tried on a different machine and `hxs` failed with an error:
```
(require "helix/editor.scm")
     │  ^^^^^^^ Attempting to load module from: "/Users/matt/.steel/cogs/helix/editor.scm"
```
TODO: make the flake handle that too (something to do with STEEL_HOME I imagine).
That way the dev env can be self-contained.

# Contributing

I'm open to different ideas about what an ergonomic helix plugin development workflow might look like.
Feel free to create issues with whatever thoughts you have and we can chat about it.
