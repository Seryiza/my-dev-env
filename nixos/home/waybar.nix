{ config, ... }:
{
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
        "custom/org_timeblock"
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

    style = builtins.readFile ./waybar.css;
  };
}
