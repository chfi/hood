{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "showdoom";
  inherit ghc;
  buildInputs = with pkgs; [
    haskellPackages.ghcid
    mesa_glu
    xorg.libX11
    xorg.libXinerama
    xorg.libXrandr
    xorg.libXi
    xorg.libXxf86vm
    xorg.libXcursor
    xorg.libXext
    xorg.xinput
    ncurses
    glfw
    zlib
    freeglut

    # udev
    # alsaLib
    # mesa_glu
    # ]) ++ (with pkgs.xorg;
    # libX11
    # libXcursor
    # libXrandr
                           ];
  LANG = "en_US.UTF-8";
}

# {ghc}:
# with (import <nixpkgs> {});

# haskell.lib.buildStackProject {
#   inherit ghc;
#   name = "streamsEnv";
#   buildInputs = [];
# }
