{ config, pkgs, nixpkgs-unstable, lib, ... }@inputs:
let
  unstable-pkgs = nixpkgs-unstable.legacyPackages."x86_64-linux";
  volumeSound =
    "${pkgs.yaru-theme}/share/sounds/Yaru/stereo/audio-volume-change.oga";
  backlightSound = "${pkgs.yaru-theme}/share/sounds/Yaru/stereo/complete.oga";
in {
  xsession.enable = true;

  services.swayidle = {
    enable = true;

    events = [{
      event = "before-sleep";
      command = "${pkgs.swaylock}/bin/swaylock -fF -c 000000";
    }];

    timeouts = [
      {
        timeout = 600;
        command = "${pkgs.swaylock}/bin/swaylock -fF -c 000000";
      }
      {
        timeout = 630;
        command = ''swaymsg "output * power off"'';
        resumeCommand = ''swaymsg "output * power on"'';
      }
    ];
  };

  services.mako = {
    enable = true;
    settings = {
      icons = false;

      "app-name=ya-vol" = {
        layer = "overlay";
        history = 0;
        anchor = "top-center";
        group-by = "app-name";
        format = "<b>%s</b>%b";
        "on-notify" =
          "exec ${pkgs.libcanberra-gtk3}/bin/canberra-gtk-play --cache-control=volatile --file=${volumeSound}";
      };

      "app-name=ya-backlight" = {
        layer = "overlay";
        history = 0;
        anchor = "top-center";
        group-by = "app-name";
        format = "<b>%s</b>%b";
      };

      "app-name=volume group-index=0" = { invisible = 0; };
    };
  };

  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    systemd.enable = true;
    package =
      nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system}.sway;

    extraSessionCommands = ''
      export SDL_VIDEODRIVER=wayland
      # needs qt5.qtwayland in systemPackages
      export QT_QPA_PLATFORM=wayland
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
      # Fix for some Java AWT applications (e.g. Android Studio),
      # use this if they aren't displayed properly:
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';

    extraConfig = ''
      titlebar_border_thickness 2
      titlebar_padding 6 3
    '';

    config = {
      startup = [{ command = "brightnessctl set 80%"; }];

      modifier = "Mod4";

      bindkeysToCode = true;

      fonts = {
        names = [ "Iosevka" ];
        size = 8.0;
      };

      focus.followMouse = false;

      # seat = { "*" = { xcursor_theme = ''"Capitaine Cursors - White" 40''; }; };

      keybindings =
        let modifier = config.wayland.windowManager.sway.config.modifier;
        in lib.mkOptionDefault {
          "${modifier}+Alt+h" = "workspace prev";
          "${modifier}+Alt+l" = "workspace next";
          "${modifier}+Return" = "exec alacritty";
          "${modifier}+Escape" = "exec swaylock -c 000000";
          "${modifier}+u" = "kill";
          "${modifier}+n" = "exec tofi-drun | xargs swaymsg exec --";
          # "${modifier}+n" = "exec emacsclient -n -e '(sway-app-launcher)'";
          "${modifier}+Alt+n" = "exec tofi-run | xargs swaymsg exec --";
          "${modifier}+m" = "exec emacsclient -c";
          "${modifier}+Shift+b" = "exec run-work-browser";
          "${modifier}+Alt+space" = "sticky toggle";
          "${modifier}+e" = "exec swaymsg exec -- QT_QPA_PLATFORM=xcb Enpass";
          "Print" = ''exec grim -g "$(slurp)" - | wl-copy'';
          "Ctrl+Print" = ''exec grim -g "$(slurp)"'';
          "Shift+Print" = "exec grim - | wl-copy | drawing -c";
          "XF86MonBrightnessUp" =
            "exec increase-backlight && display-backlight";
          "XF86MonBrightnessDown" =
            "exec descrease-backlight && display-backlight";
          "XF86AudioPlay" = "exec playerctl play-pause";
          "XF86AudioRaiseVolume" =
            "exec increase-current-volume && display-current-volume";
          "XF86AudioLowerVolume" =
            "exec decrease-current-volume && display-current-volume";
          "XF86AudioMicMute" =
            "exec toggle-microphone-mute && display-current-microphone";
          "${modifier}+Alt+m" =
            "exec toggle-microphone-mute && display-current-microphone";
          "XF86AudioMute" = "exec toggle-audio-mute && display-current-volume";
          "${modifier}+Shift+m" =
            "exec toggle-audio-mute && display-current-volume";
        };

      window = {
        border = 2;
        titlebar = false;

        commands = [
          {
            command = "border none";
            criteria = { app_id = "dev.benz.walker"; };
          }

          {
            command = "floating enable";
            criteria = { app_id = "dev.benz.walker"; };
          }

          {
            command = "sticky enable";
            criteria = { title = "Picture-in-Picture"; };
          }

          {
            command = "floating enable";
            criteria = { title = "Picture-in-Picture"; };
          }

          {
            command = "resize set width 300 height 150";
            criteria = { title = "Picture-in-Picture"; };
          }

          {
            command = "move position 0 0";
            criteria = { title = "Picture-in-Picture"; };
          }

          # Emacs Launcher
          {
            command = "floating enable";
            criteria = {
              app_id = "emacs";
              title = "^sway-app-launcher$";
            };
          }

          {
            command = "resize set width 900 px height 420 px";
            criteria = {
              app_id = "emacs";
              title = "^sway-app-launcher$";
            };
          }

          {
            command = "move position center";
            criteria = {
              app_id = "emacs";
              title = "^sway-app-launcher$";
            };
          }

          {
            command = "border pixel 2";
            criteria = {
              app_id = "emacs";
              title = "^sway-app-launcher$";
            };
          }
        ];
      };

      colors = {
        focused = {
          background = "#323232";
          border = "#323232";
          childBorder = "#323232";
          indicator = "#323232";
          text = "#FFFFFF";
        };
        unfocused = {
          background = "#FFFFFF";
          border = "#929292";
          childBorder = "#929292";
          indicator = "#929292";
          text = "#444444";
        };
        focusedInactive = {
          background = "#929292";
          border = "#b4b4b4";
          childBorder = "#929292";
          indicator = "#929292";
          text = "#FFFFFF";
        };
      };

      gaps = {
        left = 0;
        right = 0;
        top = 0;
        bottom = 0;
        inner = 5;
        smartGaps = false;
      };

      input = {
        "type:touchpad" = {
          dwt = "enabled";
          tap = "enabled";
          tap_button_map = "lrm";
          natural_scroll = "enabled";
          middle_emulation = "enabled";
        };
        "type:keyboard" = {
          xkb_layout = "us,ru";
          xkb_options = "ctrl:nocaps,grp:ctrl_space_toggle";
        };
      };

      # bars = [{
      #   command = "waybar";
      #   #extraConfig = "status_padding 0";
      #   # statusCommand =
      #   #   "${config.home.homeDirectory}/code/my-dev-env/8repl/run_status.sh";
      # }];

      bars = [ ];

      output = {
        eDP-1 = {
          mode = "2560x1600@240Hz";
          scale = "2";
        };

        "*" = {
          bg = "#C9C5C0 solid_color";
          subpixel = "none";
        };
      };
    };
  };

  # services.walker = {
  #   enable = true;
  #   #runAsService = false;
  #   package = unstable-pkgs.walker;

  #   settings = {
  #     as_window = true;
  #     builtins = {
  #       custom_commands = {
  #         weight = 5;
  #         commands = [
  #           {
  #             cmd = "zen -P default";
  #             name = "Zen Personal";
  #           }
  #           {
  #             cmd = "zen -P Work";
  #             name = "Zen Work";
  #           }
  #         ];
  #       };
  #     };
  #   };
  # };

  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);
    settings = { PASSWORD_STORE_DIR = "$HOME/.password-store/"; };
  };

  programs.waybar = {
    enable = true;
    systemd.enable = true;
    systemd.target = "sway-session.target";

    settings = [{
      spacing = 0;
      modules-left =
        [ "sway/workspaces" "sway/mode" "sway/scratchpad" "sway/window" ];
      modules-center = [ ];
      modules-right = [
        "privacy"
        "wireplumber"
        "network"
        "sway/language"
        "battery"
        "clock"
        "tray"
      ];

      "sway/workspaces" = { disable-scroll = true; };
      "privacy" = { icon-size = 14; };
      "battery" = {
        format = "{capacity}% battery";
        format-full = "";
      };
      "network" = {
        format-wifi = "{signalStrength}% wlan";
        format-linked = "{ifname} (No IP)";
        format-disconnected = "Disconnected";
        format-alt = "{ifname}: {ipaddr}/{cidr}";
      };
      "wireplumber" = {
        format = "{volume}%{format_source} audio";
        format-muted = "MUTED {format_source}";
        format-source = " MIC";
        format-source-muted = "";
      };
    }];

    style = ''
      * {
          font-family: FontAwesome, Iosevka, Roboto, Helvetica, Arial, sans-serif;
          font-size: 14px;
      }
      window#waybar {
          background-color: #0E1415;
          color: #CECECE;
      }
      button {
          /* Use box-shadow instead of border so the text isn't offset */
          box-shadow: inset 0 -3px transparent;
          /* Avoid rounded borders under each button name */
          border: none;
          border-radius: 0;
      }

      /* https://github.com/Alexays/Waybar/wiki/FAQ#the-workspace-buttons-have-a-strange-hover-effect */
      button:hover {
          background: inherit;
          box-shadow: inset 0 -3px #CECECE;
      }
      #workspaces button {
          padding: 0 5px;
          background-color: transparent;
          color: #CECECE;
      }
      #workspaces button:hover {
          background: rgba(0, 0, 0, 0.2);
      }

      #workspaces button.focused, #workspaces button.active {
          background-color: #64727D;
          box-shadow: inset 0 -3px #CECECE;
      }

      #workspaces button.urgent {
          background-color: #eb4d4b;
      }
      #mode {
          background-color: #64727D;
          box-shadow: inset 0 -3px #ffffff;
      }

      #clock,
      #battery,
      #cpu,
      #memory,
      #disk,
      #temperature,
      #backlight,
      #network,
      #pulseaudio,
      #wireplumber,
      #custom-media,
      #tray,
      #mode,
      #idle_inhibitor,
      #scratchpad,
      #power-profiles-daemon,
      #mpd {
          padding: 0 10px;
          color: #CECECE;
      }

      #window,
      #workspaces {
          margin: 0 10px;
      }

      /* If workspaces is the leftmost module, omit left margin */
      .modules-left > widget:first-child > #workspaces {
          margin-left: 0;
      }

      /* If workspaces is the rightmost module, omit right margin */
      .modules-right > widget:last-child > #workspaces {
          margin-right: 0;
      }
      #clock {
      }
      #network {
      }

      #network.disconnected {
          background-color: #f53c3c;
      }
      #wireplumber {
      }
      #wireplumber.muted {
      }
      #wireplumber:not(.source-muted) {
          color: #CC8BC9;
      }
      #tray {
          background-color: #2980b9;
      }

      #tray > .passive {
          -gtk-icon-effect: dim;
      }

      #tray > .needs-attention {
          -gtk-icon-effect: highlight;
          background-color: #eb4d4b;
      }
      #language {
          color: #CECECE;
          padding: 0 5px;
          min-width: 16px;
      }
      #scratchpad {
          background: rgba(0, 0, 0, 0.2);
      }

      #scratchpad.empty {
          background-color: transparent;
      }
      #privacy {
          padding: 0;
      }

      #privacy-item {
          padding: 0 5px;
          color: white;
      }

      #privacy-item.screenshare {
          background-color: #cf5700;
      }

      #privacy-item.audio-in {
          background-color: #1ca000;
      }

      #privacy-item.audio-out {
          background-color: #0069d4;
      }
    '';
  };

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
    unstable-pkgs.walker
    pkgs.yaru-theme
    pkgs.libcanberra-gtk3 # provides canberra-gtk-play

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

  home.sessionVariables = {
    WLR_DRM_NO_ATOMIC = "1";
    GRIM_DEFAULT_DIR = "$HOME/Pictures/Screenshots";
  };
  home.sessionPath = [ "$HOME/.local/bin" ];

  services.playerctld.enable = true;

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
    #package = pkgs.capitaine-cursors-themed;
    #name = ''"Capitaine Cursors - White"'';
    package = pkgs.rose-pine-cursor;
    name = "BreezeX-RosePineDawn-Linux";
    size = 35;
    gtk.enable = true;
    sway.enable = true;
    x11 = { enable = true; };
  };

  programs.neovim = {
    enable = true;
    plugins = [ pkgs.vimPlugins.packer-nvim ];
  };

  #programs.emacs = {
  #  enable = true;
  #  package = pkgs.emacs-pgtk;
  #};

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
      # padding-left = "35%";
      # padding-top = "10%";
      result-spacing = 20;
      width = "50%";
    };
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

  programs.mpv = { enable = true; };

  programs.bash = {
    enable = true;
    bashrcExtra = builtins.readFile ./bashrc;
  };

  programs.git = {
    enable = true;
    ignores = [
      # direnv
      ".direnv"
      ".envrc"
    ];
    settings = {
      user = {
        email = "git@seryiza.xyz";
        name = "Sergey Zaborovsky";
        signingkey = "D551D39D7C487647";
      };

      commit = { gpgsign = true; };
      tag = { gpgSign = true; };
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
  programs.offlineimap.enable = true;
  programs.mu.enable = true;

  accounts.email = {
    maildirBasePath = "${config.home.homeDirectory}/Maildir";
    accounts = {
      fastmail = {
        address = "seryiza@fastmail.com";
        userName = "seryiza@fastmail.com";
        primary = true;
        flavor = "fastmail.com";
        realName = "Sergey Zaborovsky";
        passwordCommand = "${pkgs.pass}/bin/pass mail/seryiza@fastmail.com";
        mu = { enable = true; };
        offlineimap = {
          enable = true;
          extraConfig.account = { autorefresh = 0; };
          # postSyncHookCommand = "${pkgs.mu}/bin/mu index";
        };
      };
    };
  };

  # Run OfflineIMAP every 5 minutes as a user service.
  systemd.user.services.offlineimap-sync = {
    Service = {
      ExecStart = "${pkgs.offlineimap}/bin/offlineimap";
      Environment =
        "PATH=${lib.makeBinPath [ pkgs.coreutils pkgs.mu pkgs.offlineimap ]}";
    };
    Unit.Description = "OfflineIMAP sync";
    Install.WantedBy = [ "default.target" ];
  };

  systemd.user.timers.offlineimap-sync = {
    Unit.Description = "Run OfflineIMAP periodically";
    Timer = {
      OnBootSec = "1m";
      OnUnitActiveSec = "5m";
    };
    Install.WantedBy = [ "timers.target" ];
  };

  services.activitywatch = {
    enable = true;
    watchers = {
      aw-watcher-window-wayland = { package = pkgs.aw-watcher-window-wayland; };
    };
  };

  home.stateVersion = "23.11";
}
