{
  description = "The programs that the public CI tests this configuration with";

  # The revision of nixpkgs-unstable that the NixOS machine runs, moved by
  # hand when that one moves, so that Linux tests the same Emacs build.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/31ecbaca1bdefa45dffc409f0250bd8039ea0010";

  outputs = {nixpkgs, ...}: let
    # What the tests need besides Emacs, on both systems: the requirements
    # of the runner, the programs that scenarios wait for or skip without
    # (zig is left out, so that gterm's expected skip holds), and what
    # jinx reads to build its module.
    tools = pkgs:
      with pkgs; [
        git
        ripgrep
        fd
        fzf
        python3
        zsh
        pkg-config
        basedpyright
        bash-language-server
        black
        btop
        clojure
        clojure-lsp
        delta
        isort
        jdk
        jdt-language-server
        ltex-ls-plus
        marksman
        opentofu
        python3Packages.grammalecte
        ruff
        shellcheck
        tofu-ls
      ];
    shell = pkgs: extra:
      pkgs.mkShellNoCC {
        packages = tools pkgs ++ extra;
        buildInputs = [pkgs.enchant];
        TREESIT_GRAMMARS = "${pkgs.emacsPackages.treesit-grammars.with-all-grammars}/lib";
        # The programs of the tools above that a test session must find
        # here, and not elsewhere on PATH.
        CI_PROGRAMS = builtins.concatStringsSep " " [
          "git"
          "rg"
          "fd"
          "fzf"
          "python3"
          "zsh"
          "basedpyright-langserver"
          "bash-language-server"
          "black"
          "btop"
          "clojure"
          "clojure-lsp"
          "delta"
          "isort"
          "java"
          "jdtls"
          "ltex-ls-plus"
          "marksman"
          "tofu"
          "grammalecte-cli.py"
          "ruff"
          "shellcheck"
          "tofu-ls"
        ];
      };
    linux = nixpkgs.legacyPackages.x86_64-linux;
    darwin = nixpkgs.legacyPackages.aarch64-darwin;
  in {
    devShells.x86_64-linux = {
      linux = shell linux [linux.emacs-pgtk linux.xvfb linux.gcc];
      # neomacs takes the name emacs on PATH; GNU emacsclient stays.
      linux-neomacs = shell linux [
        (linux.runCommand "emacsclient" {} ''
          mkdir -p $out/bin
          ln -s ${linux.emacs-pgtk}/bin/emacsclient $out/bin/emacsclient
        '')
        linux.gcc
      ];
    };
    # The Emacs of the Mac's Brewfile and Apple's compiler come from the
    # runner, not from here.
    devShells.aarch64-darwin.darwin = shell darwin [];
  };
}
