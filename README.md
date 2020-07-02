To use this, all you need is the package manager [Nix](https://nixos.org/download.html):
```
curl -L https://nixos.org/nix/install | sh
```
then run with `./Main.hs <path/to/pokemon-uranium> <path/to/godot-project>` 

Optionally compile with `nix build -f shell.nix` and run the executable `result/bin/ProjectUraniumAutoBuilderHs` instead. This way it'll execute faster (if for some reason you plan to run it more than once).
