{
  description = "The programs, setup and trial of this Emacs configuration";

  # The revision of nixpkgs-unstable that the NixOS machine runs, moved by
  # hand when that one moves, so that the tests run its Emacs build.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/31ecbaca1bdefa45dffc409f0250bd8039ea0010";

  outputs = {
    self,
    nixpkgs,
  }: let
    # The two machines this configuration runs on.
    systems = ["x86_64-linux" "aarch64-darwin"];
    forEachSystem = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    programs = import ./nix/programs.nix;
  in {
    # The program lists, for a flake that installs them with its own nixpkgs.
    lib = {inherit programs;};

    packages = forEachSystem (pkgs: let
      inherit (pkgs) lib;
      listed = programs pkgs;
      emacs =
        if pkgs.stdenv.hostPlatform.isLinux
        then pkgs.emacs-pgtk
        else pkgs.emacs;
      grammars = pkgs.emacsPackages.treesit-grammars.with-all-grammars;
      setupEnvironment = {
        TREESIT_GRAMMARS = grammars;
        JINX_ENCHANT = pkgs.enchant;
        PKG_CONFIG_PATH = "${pkgs.enchant.dev}/lib/pkgconfig";
      };
      # A compiler for jinx's module: on macOS, Apple's.
      compiler = lib.optionals pkgs.stdenv.hostPlatform.isLinux [pkgs.gcc];
      testPrograms = pkgs.buildEnv {
        name = "emacs-configuration-test-programs";
        paths = listed.tests;
      };
      # Prepare a clone: nix run .#setup
      setup = pkgs.writeShellApplication {
        name = "setup";
        runtimeInputs = with pkgs; [git gnumake pkg-config clojure jdk] ++ compiler;
        runtimeEnv = setupEnvironment;
        text = builtins.readFile ./nix/setup.sh;
      };
    in {
      inherit emacs grammars setup;

      # Every program the flake installs, and the fonts.
      programs = pkgs.buildEnv {
        name = "emacs-configuration-programs";
        paths = listed.tests ++ listed.others ++ listed.fonts;
      };

      # Try the configuration: nix run github:piotr-yuxuan/.emacs.d
      default = pkgs.writeShellApplication {
        name = "try";
        runtimeInputs = [pkgs.coreutils pkgs.findutils testPrograms] ++ compiler;
        runtimeEnv =
          setupEnvironment
          // {
            EMACS = lib.getExe emacs;
            EMACS_TRY_SOURCE = self;
            EMACS_TRY_PROGRAMS = "${testPrograms}/bin";
            EMACS_TRY_SETUP = lib.getExe setup;
          }
          // lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
            # The configuration's fonts; macOS reads only installed ones.
            FONTCONFIG_FILE = pkgs.makeFontsConf {
              fontDirectories = listed.fonts ++ [pkgs.dejavu_fonts];
            };
          };
        text = builtins.readFile ./nix/try.sh;
      };
    });

    # The CI's shells, one per entry of its matrix, without what the
    # runner brings.  Each sets what the setup reads.
    devShells = forEachSystem (pkgs: let
      inherit (pkgs) lib;
      listed = programs pkgs;
      own = self.packages.${pkgs.stdenv.hostPlatform.system};
      shell = extra:
        pkgs.mkShellNoCC {
          packages = listed.tests ++ extra;
          TREESIT_GRAMMARS = own.grammars;
          JINX_ENCHANT = pkgs.enchant;
          PKG_CONFIG_PATH = "${pkgs.enchant.dev}/lib/pkgconfig";
          # The test programs that a session must find here, and not
          # elsewhere on PATH.  Xvfb is the runner's, and its
          # mainProgram names no program of its package.
          CI_PROGRAMS = lib.concatStringsSep " " (lib.concatMap
            (program: lib.optional (program.meta ? mainProgram) program.meta.mainProgram)
            (lib.remove pkgs.xvfb listed.tests));
        };
    in
      if pkgs.stdenv.hostPlatform.isLinux
      then rec {
        linux = shell [own.emacs pkgs.gcc];
        # neomacs takes the name emacs on PATH; GNU emacsclient stays.
        linux-neomacs = shell [
          (pkgs.runCommand "emacsclient" {} ''
            mkdir -p $out/bin
            ln -s ${own.emacs}/bin/emacsclient $out/bin/emacsclient
          '')
          pkgs.gcc
        ];
        default = linux;
      }
      else if pkgs.stdenv.hostPlatform.isDarwin
      then rec {
        # The Emacs of the Mac's Brewfile and Apple's compiler come from
        # the runner, not from here.
        darwin = shell [];
        default = darwin;
      }
      else throw "devShells: unsupported platform ${pkgs.stdenv.hostPlatform.system}");
  };
}
