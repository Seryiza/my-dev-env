{ config, ... }: {
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    systemd.targets = [ "sway-session.target" ];

    settings = [
      {
        name = "top";
        position = "top";
        spacing = 0;
        modules-left =
          [ "sway/workspaces" "sway/mode" "sway/scratchpad" "sway/window" ];
        modules-center = [ ];
        modules-right = [
          "privacy"
          "wireplumber"
          "network"
          "custom/wireguard"
          "sway/language"
          "battery"
          "clock"
          "tray"
        ];

        "custom/wireguard" = {
          format = "{text}";
          exec =
            "${config.home.homeDirectory}/.local/bin/waybar-wireguard.sh short";
          interval = 15;
          return-type = "json";
        };

        "sway/workspaces" = { disable-scroll = true; };
        "privacy" = {
          icon-size = 12;
          icon-spacing = 0;
        };
        "battery" = {
          format = "{capacity}% battery";
          format-full = "";
        };
        "clock" = {
          interval = 60;
          format = "{:%d %b %H:%M}";
          tooltip = true;
          tooltip-format = "{:%A, %d %B %Y}\n<tt><small>{calendar}</small></tt>";

          calendar = {
            mode = "month";
            weeks-pos = "right";
            on-scroll = 1;
            format = {
              months = "<span color='#ffead3'><b>{}</b></span>";
              days = "<span color='#ecc6d9'><b>{}</b></span>";
              weeks = "<span color='#99ffdd'><b>W{}</b></span>";
              weekdays = "<span color='#ffcc66'><b>{}</b></span>";
              today = "<span color='#ff6699'><b><u>{}</u></b></span>";
            };
          };

          actions = {
            on-click = "shift_reset";
            on-click-right = "mode";
            on-scroll-up = "shift_up";
            on-scroll-down = "shift_down";
          };
        };
        "network" = {
          # Waybar does not expose arbitrary numeric format conditions for
          # signalStrength. A five-entry format-icons table maps to 20-point
          # buckets, so only the 0..19% bucket renders text; empty buckets hide
          # the module.
          format = "";
          format-wifi = "{icon}";
          format-icons = [ "<20% wlan" "" "" "" "" ];
          format-ethernet = "";
          format-linked = "{ifname} (No IP)";
          format-disconnected = "Disconnected";
          format-disabled = "Wi-Fi disabled";
          tooltip-format-wifi = ''
            {essid} ({signalStrength}%)
            {ifname}: {ipaddr}/{cidr}'';
        };
        "wireplumber" = {
          format = "{volume}% {node_name}{format_source}";
          format-muted = "MUTED {node_name}{format_source}";
          format-source = " +MIC";
          format-source-muted = "";
          tooltip-format = "{node_name}: {volume}%{format_source}";
        };
      }

      {
        name = "bottom";
        position = "bottom";
        spacing = 0;
        modules-left = [ "custom/org_timeblock" "custom/org_clock" ];
        modules-center = [ ];
        modules-right = [ ];

        "custom/org_timeblock" = {
          exec = "${config.home.homeDirectory}/.local/bin/waybar-org-timeblock";
          interval = 15;
          format = "{text}";
          max-length = 60;
          escape = true;
        };

        "custom/org_clock" = {
          exec = "${config.home.homeDirectory}/.local/bin/waybar-org-current-clock";
          interval = 15;
          format = "{text}";
          max-length = 60;
          escape = true;
        };
      }
    ];

    style = builtins.readFile ./waybar.css;
  };
}
