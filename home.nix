{ config, pkgs, lib, ... }: {
  xsession.enable = true;

  home.packages = [

    pkgs.httpie
    pkgs.vlc
    pkgs.epiphany
    pkgs.iosevka
    pkgs.gnumake
    pkgs.sops
    pkgs.clapper
    pkgs.zotero
    pkgs.flowtime
    pkgs.ngrok

    # gnome extensions
    pkgs.gnomeExtensions.blur-my-shell
    pkgs.gnomeExtensions.paperwm
    pkgs.gnomeExtensions.just-perfection
    pkgs.gnomeExtensions.xremap
    pkgs.gnomeExtensions.advanced-alttab-window-switcher
    pkgs.gnomeExtensions.activate-window-by-title
    #pkgs.gnomeExtensions.switcher
    #pkgs.gnomeExtensions.rounded-window-corners
    #pkgs.gnomeExtensions.colorblind-filters
    #pkgs.gnomeExtensions.colortint
    #pkgs.gnomeExtensions.dash-to-dock
    #pkgs.gnomeExtensions.transparent-top-bar-adjustable-transparency
    pkgs.gnomeExtensions.quick-lang-switch

    (pkgs.google-cloud-sdk.withExtraComponents
      [ pkgs.google-cloud-sdk.components.gke-gcloud-auth-plugin ])
  ];

  home.sessionVariables.LOCALES_ARCHIVE =
    "${pkgs.glibcLocales}/lib/locale/locale-archive";

  dconf.settings = {
    "org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = [
        "blur-my-shell@aunetx"
        "paperwm@paperwm.github.com"
        "launch-new-instance@gnome-shell-extensions.gcampax.github.com"
        "light-style@gnome-shell-extensions.gcampax.github.com"
        pkgs.gnomeExtensions.quick-lang-switch.extensionUuid
        pkgs.gnomeExtensions.just-perfection.extensionUuid
        pkgs.gnomeExtensions.xremap.extensionUuid
        pkgs.gnomeExtensions.activate-window-by-title.extensionUuid
      ];
    };
  };

  dconf.settings = {
    "org/gnome/desktop/background" = {
      "picture-uri" = "/home/seryiza/Pictures/Bierstadt_Sierra_-_1920-1080.jpg";
      "picture-options" = "zoom";
    };
    "org/gnome/desktop/screensaver" = {
      "picture-uri" = "/home/seryiza/Pictures/Bierstadt_Sierra_-_1920-1080.jpg";
      "picture-options" = "zoom";
    };
  };

  home.file."Pictures/Bierstadt_Sierra_-_1920-1080.jpg".source =
    ./wallpapers/Bierstadt_Sierra_-_1920-1080.jpg;

  gtk.enable = true;

  xdg.enable = true;
  xdg.configFile = {
    "nvim/init.lua".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/code/my-dev-env/dotfiles/nvim/init.lua";
    "nvim/lua".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/code/my-dev-env/dotfiles/nvim/lua";
  };

  #home.file.".emacs.d" = {
  #  source = ./emacs;
  #  recursive = true;
  #};

  home.file = {
    ".emacs.d".source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/code/my-dev-env/dotfiles/emacs";
  };

  home.pointerCursor = {
    package = pkgs.capitaine-cursors-themed;
    name = "Capitaine Cursors - White";
    size = 40;
    gtk.enable = true;
    x11 = {
      enable = true;
      defaultCursor = "Capitaine Cursors - White";
    };
  };

  programs.neovim = {
    enable = true;
    plugins = [ pkgs.vimPlugins.packer-nvim ];
  };

  #programs.emacs = {
  #  enable = true;
  #  package = pkgs.emacs-pgtk;
  #};

  programs.ssh = { enable = true; };

  programs.wezterm = {
    enable = true;
    extraConfig = ''
                return {
                font = wezterm.font("Iosevka"),
      	        font_size = 16.0,
      	        color_scheme = "Catppuccin Latte",
      	        window_decorations = "NONE",
      	        enable_tab_bar = false
                }
    '';
  };

  programs.tmux = {
    enable = true;
    extraConfig = builtins.readFile ./tmux.conf;
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
        search = {
          # TODO: add @ya
          engines = { };
        };
      };
    };
  };

  programs.bash = {
    enable = true;
    bashrcExtra = builtins.readFile ./bashrc;
  };

  programs.git.enable = true;
  programs.git.userEmail = "git@seryiza.xyz";
  programs.git.userName = "Sergey Zaborovsky";
  programs.git.extraConfig = {
    core = {
      #pager = "delta";
    };
    interactive = {
      #diffFilter = "delta --color-only --features decorations light";
    };
    delta = {
      features = "decorations light";
      true-color = "always";
    };
    "delta \"interactive\"" = { keep-plus-minus-markers = "false"; };
    "delta \"decorations\"" = {
      commit-decoration-style = "blue ol";
      commit-style = "raw";
      file-style = "omit";
      hunk-header-decoration-style = "blue box";
      hunk-header-file-style = "red";
      hunk-header-line-number-style = "#067a00";
      hunk-header-style = "file line-number syntax";
    };
    init = { defaultBranch = "master"; };
    push = { autoSetupRemote = "true"; };
  };

  programs.alacritty = {
    enable = true;
    settings = {
      env = { TERM = "xterm-256color"; };

      window = {
        decorations = "none";
        dimensions = {
          columns = 60;
          lines = 60;
        };
      };

      font = {
        size = 16;
        normal.family = "Iosevka";
      };

      colors = {
        primary = {
          background = "#EFF1F5";
          foreground = "#4C4F69";
          dim_foreground = "#4C4F69";
          bright_foreground = "#4C4F69";
        };

        cursor = {
          text = "#EFF1F5";
          cursor = "#DC8A78";
        };

        vi_mode_cursor = {
          text = "#EFF1F5";
          cursor = "#7287FD";
        };

        search = {
          matches = {
            foreground = "#EFF1F5";
            background = "#6C6F85";
          };

          focused_match = {
            foreground = "#EFF1F5";
            background = "#40A02B";
          };
        };

        hints = {
          start = {
            foreground = "#EFF1F5";
            background = "#DF8E1D";
          };

          end = {
            foreground = "#EFF1F5";
            background = "#6C6F85";
          };
        };

        selection = {
          text = "#EFF1F5";
          background = "#DC8A78";
        };

        normal = {
          black = "#5C5F77";
          red = "#D20F39";
          green = "#40A02B";
          yellow = "#DF8E1D";
          blue = "#1E66F5";
          magenta = "#EA76CB";
          cyan = "#179299";
          white = "#ACB0BE";
        };

        bright = {
          black = "#6C6F85";
          red = "#D20F39";
          green = "#40A02B";
          yellow = "#DF8E1D";
          blue = "#1E66F5";
          magenta = "#EA76CB";
          cyan = "#179299";
          white = "#BCC0CC";
        };

        dim = {
          black = "#5C5F77";
          red = "#D20F39";
          green = "#40A02B";
          yellow = "#DF8E1D";
          blue = "#1E66F5";
          magenta = "#EA76CB";
          cyan = "#179299";
          white = "#ACB0BE";
        };

        indexed_colors = [
          {
            index = 16;
            color = "#FE640B";
          }
          {
            index = 17;
            color = "#DC8A78";
          }
        ];
      };
    };
  };

  services.syncthing.enable = true;

  home.stateVersion = "23.11";
}
