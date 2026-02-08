{ config, pkgs, ... }:
{
  imports = [
    ./home/sway.nix
    ./home/mako.nix
    ./home/packages.nix
    ./home/alacritty.nix
    ./home/waybar.nix
    ./home/email.nix
    ./home/ics-sync.nix
    ./home/xremap.nix
  ];

  xsession.enable = true;

  fonts.fontconfig.enable = true;

  home.sessionVariables = {
    WLR_DRM_NO_ATOMIC = "1";
    GRIM_DEFAULT_DIR = "$HOME/Pictures/Screenshots";
  };

  home.sessionVariables.LOCALES_ARCHIVE =
    "${pkgs.glibcLocales}/lib/locale/locale-archive";

  home.sessionPath = [ "$HOME/.local/bin" ];

  gtk.enable = true;
  xdg.enable = true;

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
