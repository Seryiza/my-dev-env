{ config, pkgs, ... }:

{
    xsession.enable = true;

    home.packages = [
      pkgs.httpie
      pkgs.vlc
      pkgs.epiphany
      pkgs.iosevka
      pkgs.gnumake

      # gnome extensions
      pkgs.gnomeExtensions.blur-my-shell
      pkgs.gnomeExtensions.paperwm
      pkgs.gnomeExtensions.switcher
      pkgs.gnomeExtensions.rounded-window-corners
      pkgs.gnomeExtensions.colorblind-filters
      pkgs.gnomeExtensions.colortint
      pkgs.gnomeExtensions.dash-to-dock

      (pkgs.google-cloud-sdk.withExtraComponents [pkgs.google-cloud-sdk.components.gke-gcloud-auth-plugin])
    ];

    dconf.settings = {
      "org/gnome/shell" = {
        disable-user-extensions = false;
	enabled-extensions = [
          "blur-my-shell@aunetx"
	  "paperwm@paperwm.github.com"
	  "launch-new-instance@gnome-shell-extensions.gcampax.github.com"
	];
      };
    };

    gtk.enable = true;

    xdg.enable = true;
    xdg.configFile."nvim/lua".source = config.lib.file.mkOutOfStoreSymlink neovim/lua;
    xdg.portal = {
      enable = true;
      configPackages = [
        pkgs.gnome.gnome-session
      ];
      extraPortals = [
        pkgs.xdg-desktop-portal-gtk
        pkgs.xdg-desktop-portal-gnome
      ];
    };

    home.file.".emacs.d".source = config.lib.file.mkOutOfStoreSymlink ./emacs;

    home.pointerCursor = {
      package = pkgs.capitaine-cursors-themed;
      name = "Capitaine Cursors - White";
      size = 40;
      gtk.enable = true;
      #x11.enable = true;
    };

    programs.neovim = {
      enable = true;
      extraLuaConfig = builtins.readFile neovim/init.lua;
      plugins = [
        pkgs.vimPlugins.packer-nvim
      ];
    };

    programs.emacs = {
      enable = true;
      package = pkgs.emacs29-pgtk;
    };

    programs.ssh = {
      enable = true;
    };

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
	  extensions = with pkgs.nur.repos.rycee.firefox-addons; [
	    ublock-origin
	    violentmonkey
          ];
	  search = {
	    # TODO: add @ya
            engines = {};
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
      "delta \"interactive\"" = {
        keep-plus-minus-markers = "false";
      };
      "delta \"decorations\"" = {
        commit-decoration-style = "blue ol";
	commit-style = "raw";
	file-style = "omit";
	hunk-header-decoration-style = "blue box";
	hunk-header-file-style = "red";
	hunk-header-line-number-style = "#067a00";
	hunk-header-style = "file line-number syntax";
      };
      init = {
        defaultBranch = "master";
      };
    };

    programs.alacritty = {
      enable = true;
      settings = {
        env = {
          TERM = "xterm-256color";
	};

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
	    { index = 16; color = "#FE640B"; }
	    { index = 17; color = "#DC8A78"; }
	  ];
	};
      };
    };

    services.syncthing.enable = true;

    home.stateVersion = "23.11";
  }
