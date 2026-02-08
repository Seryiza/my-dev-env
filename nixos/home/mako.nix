{ pkgs, ... }:
let
  volumeSound =
    "${pkgs.yaru-theme}/share/sounds/Yaru/stereo/audio-volume-change.oga";
in {
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
}
