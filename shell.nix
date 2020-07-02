
with import <nixpkgs> {};
let
  drv = haskellPackages.mkDerivation {
    pname = "ProjectUraniumAutoBuilderHs";
    version = "0.0.0.0";
    executableHaskellDepends = [
      cabal-install
      ffmpeg
      gmp
      zlib
    ] ++ (with haskellPackages; [
      turtle
      text
    ]);
    src = ./.;
    isExecutable = true;
    postConfigure = ''
      substituteInPlace ProjectUraniumAutoBuilder.hs --replace '(which "ffmpeg")' \
        '(let bin = "${ffmpeg}/bin/ffmpeg" in do { exists <- testfile bin; if exists then return (Just bin) else return Nothing })'
    '';
    shellHook = ''
      export LD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib
    '';
    license = stdenv.lib.licenses.agpl3;
  };
in if pkgs.lib.inNixShell then drv.env else drv
