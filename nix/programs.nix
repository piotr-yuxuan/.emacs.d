# The programs and fonts this configuration uses outside Emacs, each
# with what uses it.  flake.nix installs them; without Nix, install the
# same programs by other means: the names are nixpkgs attributes.  A
# missing program disables only its feature, except those the tests
# need.  docs/reference/dependencies.org says what Emacs itself needs.
pkgs: let
  inherit (pkgs) lib;
  inherit (pkgs.stdenv.hostPlatform) isLinux;
in {
  # What the tests use: the CI installs these, and the trial carries them.
  tests = with pkgs;
    [
      git # straight.el, magit, diff-hl, project.el, the tests
      ripgrep # project search (C-c s r), magit-todos
      fd # C-c f in Dirvish
      fzf # the shell keys of the terminals
      delta # magit's diffs (magit-delta)
      python3 # Treemacs, the task dashboard, the MCP bridge, the test runner
      zsh # the shell of the terminals
      btop # a full-screen program the terminal tests draw
      gnumake # jinx builds its module at first start
      pkg-config # jinx finds enchant's headers with it
      enchant # jinx, the spell checker
      basedpyright # Python's language server
      ruff # Python lint and fixes
      isort # Python formatting, then black
      black
      clojure # Clojure projects and C-c M-j
      clojure-lsp # Clojure's language server
      jdk # Clojure, jdtls
      jdt-language-server # Java's language server
      bash-language-server # shell scripts, with shellcheck
      shellcheck
      opentofu # OpenTofu formatting and tofu validate
      tofu-ls # OpenTofu's language server
      marksman # Markdown's language server
      ltex-ls-plus # grammar of prose buffers (LanguageTool)
      python3Packages.grammalecte # French grammar (flycheck-grammalecte)
    ]
    ++ lib.optionals isLinux [
      xvfb # the tests' private displays
    ];

  # What the configuration uses besides: install those of your languages.
  others = with pkgs; [
    difftastic # structural diffs
    gnupg # ~/.authinfo.gpg, the forge token
    (hunspell.withDicts (dicts: with dicts; [en_GB-ise fr-any pl_PL es_ES])) # jinx's dictionaries
    (aspellWithDicts (dicts: [dicts.la])) # jinx's Latin
    typescript-language-server # TypeScript, JavaScript
    vscode-langservers-extracted # HTML, CSS, JSON
    tailwindcss-language-server # Tailwind classes
    htmx-lsp # HTMX attributes
    rust-analyzer # Rust
    nixd # Nix
    yaml-language-server # YAML
    dockerfile-language-server # Dockerfile
    docker-compose-language-service # Docker Compose
    lua-language-server # Lua
    texlab # LaTeX
    cljfmt # Clojure formatting
    prettier # JavaScript, TypeScript, CSS, HTML, JSON, YAML formatting
    shfmt # shell formatting
    alejandra # Nix formatting
    rustfmt # Rust formatting
    uv # Python environments, which PET finds
    poetry
    pipenv
    leiningen # Leiningen projects
    babashka # babashka scripts
    cargo # Rust projects (cargo-mode)
    rustc
    tflint # Terraform lint
    mermaid-cli # Mermaid diagrams in Org (ob-mermaid)
    unzip # reading EPUB books (nov)
    socat # clients that talk to the MCP socket directly
  ];

  # Not installed by the flake: alternatives, heavy, or optional.
  #   zig, to build gterm's module (left out so that the tests skip gterm)
  #   texliveFull, for AUCTeX and the LaTeX book: several gigabytes
  #   podman or docker, for C-c D (docker.el)
  #   mpv, vlc, ffmpeg (ffplay) or mplayer, for ready-player
  #   starship, the shell prompt

  # The fonts: the default face, its icons, and CJK text.
  fonts = with pkgs; [
    nerd-fonts.iosevka
    nerd-fonts.symbols-only
    noto-fonts-cjk-sans
  ];
}
