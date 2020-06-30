
with (import <nixpkgs> {});
let
  drv = haskellPackages.mkDerivation {
    pname = "ProjectUraniumAutoBuilderHs";
    version = "0.0.0.0";
    executableHaskellDepends = [
      gmp
      zlib
      ncurses

    ] ++ (with haskellPackages; [
      cabal-install
      turtle
      text
    ]);
    src = ./.;
    isExecutable = true;
    shellHook = ''
      export LD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib
      export PATH=$PATH:$HOME/.local/bin
    '';
    license = stdenv.lib.licenses.agpl3;
  };
in if pkgs.lib.inNixShell then drv.env else drv
