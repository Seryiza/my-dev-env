{ pkgs, nixpkgs-unstable, ... }@inputs:
let
  system = pkgs.stdenv.hostPlatform.system;
  unstable-pkgs = nixpkgs-unstable.legacyPackages.${system};
  llm-agents-pkgs = inputs.llm-agents.packages.${system};
in {
  home.packages = [
    pkgs.httpie
    pkgs.gnumake
    pkgs.sops

    (pkgs.iosevka-bin.override { variant = "SGr-Iosevka"; })
    (pkgs.iosevka-bin.override { variant = "SGr-IosevkaSlab"; })
    pkgs.go-font

    pkgs.yaru-theme
    pkgs.libcanberra-gtk3
    llm-agents-pkgs.claude-code
    llm-agents-pkgs.claude-code-acp
    llm-agents-pkgs.codex

    (pkgs.google-cloud-sdk.withExtraComponents
      [ pkgs.google-cloud-sdk.components.gke-gcloud-auth-plugin ])

    pkgs.google-chrome
    pkgs.kubectl
    pkgs.dropbox
    pkgs.bun
    pkgs.telegram-desktop
    pkgs.enpass
    pkgs.postgresql_17
    pkgs.onlyoffice-desktopeditors
    pkgs.gh
    pkgs.evince
    pkgs.inkscape
    pkgs.comfortaa
    pkgs.less
    pkgs.dbeaver-bin
    pkgs.gparted
    pkgs.p7zip
    pkgs.unrar
    pkgs.hyprpicker
    pkgs.gimp
    pkgs.wireguard-tools
    pkgs.python3
    pkgs.python312Packages.pip
    pkgs.wf-recorder
    pkgs.ffmpeg-full
    pkgs.video-trimmer
    pkgs.libvterm
    pkgs.cmake
    pkgs.libtool
    pkgs.polylith
    pkgs.cifs-utils
    pkgs.celluloid
    pkgs.terraform
    pkgs.uxn
    pkgs.leiningen
    pkgs.act
    pkgs.appimage-run
    pkgs.lsb-release
    pkgs.dmidecode
    pkgs.jq
    pkgs.qbittorrent
    pkgs.rclone
    pkgs.wl-clipboard
    pkgs.grim
    pkgs.slurp
    pkgs.silver-searcher
    unstable-pkgs.clj-kondo
    unstable-pkgs.cljfmt
    pkgs.ntfs3g
    pkgs.libnotify
    pkgs.iw
    pkgs.brightnessctl
    pkgs.drawing
    pkgs.xfce.thunar
    pkgs.clojure-lsp
    pkgs.lua-language-server
    pkgs.nil
    pkgs.ripgrep
    pkgs.jdk
    pkgs.zprint
    pkgs.geckodriver
    pkgs.amberol
    pkgs.tuigreet
    pkgs.gcc
    pkgs.nodejs_22
    pkgs.unzip
    pkgs.clojure
    pkgs.babashka
    pkgs.emacs-lsp-booster
    pkgs.zed-editor-fhs
    pkgs.htop
    pkgs.evtest
    pkgs.chntpw
    pkgs.nixfmt-classic
    inputs.zen-browser.packages.${system}.default
    inputs.rep.packages.${system}.default
    pkgs.wev
  ];
}
