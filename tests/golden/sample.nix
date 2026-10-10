# A golden fixture of Nix: a development shell.
{
  pkgs ? import <nixpkgs> {},
  withTests ? true,
}: let
  inherit (pkgs) lib;
  python = pkgs.python3.withPackages (ps: [ps.requests ps.pytest]);
  greeting = "hello from ${python.name}";
in
  pkgs.mkShell {
    name = "sample";
    packages = [python pkgs.ripgrep] ++ lib.optional withTests pkgs.git;
    env.LIMIT = toString 16;
    shellHook = ''
      echo ${greeting}
      export PATH=$PWD/bin:$PATH
    '';
  }
