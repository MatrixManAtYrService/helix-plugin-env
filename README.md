This is a work in progresss.
When it's done it'll be a dev environment for nix users that want to write helix plugins.

### What Works

Build the steel language server and inject its path into `.helix/languages.toml`:
```
git clone git@github.com:MatrixManAtYrService/helix-plugin-env.git
cd helix-plugin-env
nix build .#helixConfig
```

This puts the build outputs in `./result` which is symlinked from `./helix/`.
Running `hx` in this dir will have it use the steel language server.

The default devshell in flake.nix uses `hx` from the plugin fork, use it like so:
```
❯ nix develop
$ hx --health scheme
Configured language servers:
    ✓ /nix/store/7k9sk1dws893js3svd2rjvx2cmk6j2zk-steel-interpreter-0.6.0/bin/steel-language-server: /nix/store/7k9sk1dws893js3svd2rjvx2cmk6j2zk-steel-interpreter-0.6.0/bin/steel-language-server
  Configured debug adapter: None
  Configured formatter: None
  Highlight queries: ✓
  Textobject queries: ✘
  Indent queries: ✓
```
(instead of `nix develop` you can also use `direnv` to activate this automatically when you enter your plugin's project dir)

# Doesn't work

`.helix/` is not consulted for `helix.scm`

```
$ hx
  error[E08]: Io
     ┌─ :1:2
     │
   1 │ (require "/home/matt/.config/helix/helix.scm")
     │  ^^^^^^^ Attempting to load module from: "/home/matt/.config/helix/helix.scm" No such file or directory (os error
   2)
```

I've found that I can create `helix.scm` in the main config location, but that sort of ruins my plan to make this env something that I can easily enter/exit just by `cd`ing in and out of my project dir.

# Contributing

I'm open to different ideas about what an ergonomic helix plugin development workflow might look like.
Feel free to create issues with whatever thoughts you have and we can chat about it.
