{ config, ... }: {
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    systemd.targets = [ "sway-session.target" ];

    settings = [{
      spacing = 0;
      modules-left =
        [ "sway/workspaces" "sway/mode" "sway/scratchpad" "sway/window" ];
      modules-center = [ ];
      modules-right = [
        "custom/org_timeblock"
        "privacy"
        "wireplumber"
        "network"
        "custom/wireguard"
        "sway/language"
        "battery"
        "clock"
        "tray"
      ];

      "custom/org_timeblock" = {
        exec = "${config.home.homeDirectory}/.local/bin/waybar-org-timeblock";
        interval = 15;
        format = "{text}";
        max-length = 60;
        escape = true;
      };

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
    }];

    style = builtins.readFile ./waybar.css;
  };
}
