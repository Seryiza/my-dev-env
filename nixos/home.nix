{ config, pkgs, ... }: {
  imports = [
    ./home/sway.nix
    ./home/mako.nix
    ./home/packages.nix
    ./home/alacritty.nix
    ./home/waybar.nix
    ./home/email.nix
    ./home/xremap.nix
  ];

  xsession.enable = true;

  fonts.fontconfig.enable = true;

  home.sessionVariables = {
    WLR_DRM_NO_ATOMIC = "1";
    GRIM_DEFAULT_DIR = "$HOME/Pictures/Screenshots";
    EDITOR = "emacsclient -c -a emacs";
    VISUAL = "emacsclient -c -a emacs";
    SUDO_EDITOR = "emacsclient -c -a emacs";
    BROWSER = "firefox";
  };

  home.sessionVariables.LOCALES_ARCHIVE =
    "${pkgs.glibcLocales}/lib/locale/locale-archive";

  home.sessionPath = [ "$HOME/.local/bin" "$HOME/go/bin" ];

  gtk = {
    enable = true;
    iconTheme = {
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
    };
  };
  xdg.enable = true;
  xdg.desktopEntries.emacsclient = {
    name = "Emacs Client";
    genericName = "Text Editor";
    comment = "Edit text with Emacs (client)";
    type = "Application";
    exec = "emacsclient -c -a emacs %F";
    terminal = false;
    categories = [ "Development" "TextEditor" ];
    mimeType = [
      "text/plain"
      "text/markdown"
      "text/x-shellscript"
      "text/x-yaml"
      "text/x-toml"
      "text/x-python"
      "text/x-c"
      "text/x-c++"
      "text/x-go"
      "text/x-rust"
      "text/x-java"
      "text/x-kotlin"
      "text/x-clojure"
      "text/x-lua"
      "text/x-php"
      "text/x-ruby"
      "text/x-perl"
      "text/x-sql"
      "text/x-makefile"
      "application/json"
      "application/xml"
    ];
  };

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "text/html" = [ "firefox.desktop" ];
      "application/xhtml+xml" = [ "firefox.desktop" ];
      "x-scheme-handler/http" = [ "firefox.desktop" ];
      "x-scheme-handler/https" = [ "firefox.desktop" ];
      "x-scheme-handler/ftp" = [ "firefox.desktop" ];
      "text/plain" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/markdown" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/x-shellscript" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/x-yaml" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/x-toml" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/x-python" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/x-c" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/x-c++" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/x-go" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/x-rust" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/x-java" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/x-kotlin" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/x-clojure" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/x-lua" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/x-php" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/x-ruby" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/x-perl" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/x-sql" = [ "emacsclient.desktop" "emacs.desktop" ];
      "text/x-makefile" = [ "emacsclient.desktop" "emacs.desktop" ];
      "application/json" = [ "emacsclient.desktop" "emacs.desktop" ];
      "application/xml" = [ "emacsclient.desktop" "emacs.desktop" ];
    };
  };

  home.file = {
    ".emacs.d".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/code/my-dev-env/dotfiles/emacs";
  };

  home.pointerCursor = {
    package = pkgs.rose-pine-cursor;
    name = "BreezeX-RosePineDawn-Linux";
    size = 35;
    gtk.enable = true;
    sway.enable = true;
    x11 = { enable = true; };
  };

  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);
    settings = { PASSWORD_STORE_DIR = "$HOME/.password-store/"; };
  };

  programs.neovim = {
    enable = true;
    plugins = [ pkgs.vimPlugins.packer-nvim ];
  };

  programs.firefox = {
    enable = true;
    policies = {
      AppAutoUpdate = false;
      DisableAppUpdate = true;
      DisablePocket = true;
      DisableTelemetry = true;
      NoDefaultBookmarks = true;
      DefaultDownloadDirectory = "\${home}/Downloads";
    };

    profiles = {
      default = {
        extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
          ublock-origin
          violentmonkey
        ];
        search = { engines = { }; };
      };
    };
  };

  programs.mpv = { enable = true; };

  programs.tofi = {
    enable = true;
    settings = {
      background-color = "#000000CC";
      border-width = 0;
      font = "monospace";
      height = "50%";
      num-results = 10;
      font-size = 16;
      outline-width = 0;
      result-spacing = 20;
      width = "50%";
    };
  };

  programs.bash = {
    enable = true;
    bashrcExtra = builtins.readFile ../dotfiles/bashrc;
  };

  programs.gpg = { enable = true; };

  services.gpg-agent = {
    enable = true;
    enableBashIntegration = true;
    pinentry.package = pkgs.pinentry-qt;
    defaultCacheTtl = 86400;
    maxCacheTtl = 86400;
  };

  programs.git = {
    enable = true;
    ignores = [ ".direnv" ".envrc" "/.agent-shell" ];
    settings = {
      user = {
        email = "git@seryiza.xyz";
        name = "Sergey Zaborovsky";
        signingkey = "D551D39D7C487647";
      };

      commit = { gpgsign = true; };
      tag = { gpgSign = true; };
      core = { };
      interactive = { };
      delta = {
        features = "decorations light";
        true-color = "always";
      };
      init = { defaultBranch = "master"; };
      push = { autoSetupRemote = "true"; };
    };
  };

  services.playerctld.enable = true;

  services.syncthing.enable = true;

  services.activitywatch = {
    enable = true;
    watchers = {
      aw-watcher-window-wayland = { package = pkgs.aw-watcher-window-wayland; };
    };
  };

  home.stateVersion = "23.11";
}
