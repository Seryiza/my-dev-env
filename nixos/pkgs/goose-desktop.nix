{
  lib,
  stdenv,
  fetchurl,
  rpmextract,
  autoPatchelfHook,
  makeWrapper,
  bash,
  coreutils,
  curl,
  findutils,
  gnugrep,
  gnused,
  gzip,
  which,
  alsa-lib,
  at-spi2-atk,
  at-spi2-core,
  atk,
  cairo,
  cups,
  dbus,
  expat,
  fontconfig,
  freetype,
  gdk-pixbuf,
  glib,
  gtk3,
  libGL,
  libdrm,
  libgbm,
  nspr,
  nss,
  pango,
  pipewire,
  systemd,
  wayland,
  libx11,
  libxcomposite,
  libxdamage,
  libxext,
  libxfixes,
  libxkbcommon,
  libxrandr,
  libxcb,
}:

stdenv.mkDerivation (finalAttrs: let
  runtimeLibs = [
    (lib.getLib stdenv.cc.cc)
    alsa-lib
    at-spi2-atk
    at-spi2-core
    atk
    cairo
    cups
    dbus
    expat
    fontconfig
    freetype
    gdk-pixbuf
    glib
    gtk3
    libGL
    libdrm
    libgbm
    nspr
    nss
    pango
    pipewire
    (lib.getLib systemd)
    wayland
    libx11
    libxcomposite
    libxdamage
    libxext
    libxfixes
    libxkbcommon
    libxrandr
    libxcb
  ];
in {
  pname = "goose-desktop";
  version = "1.31.0";

  src = fetchurl {
    url =
      "https://github.com/aaif-goose/goose/releases/download/v${finalAttrs.version}/Goose-${finalAttrs.version}-1.x86_64.rpm";
    hash = "sha256-OXGvEgYS1WKfQCDkGaNZNRLNMeOkrnodYalLorhqfi0=";
  };

  nativeBuildInputs = [
    autoPatchelfHook
    makeWrapper
    rpmextract
  ];

  buildInputs = runtimeLibs;
  runtimeDependencies = [ (lib.getLib systemd) ];

  dontConfigure = true;
  dontBuild = true;
  dontStrip = true;

  unpackPhase = ''
    runHook preUnpack
    rpmextract "$src"
    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/opt $out/share/applications

    cp -r usr/lib/Goose $out/opt/Goose

    install -Dm644 usr/share/applications/Goose.desktop \
      $out/share/applications/Goose.desktop
    install -Dm644 usr/share/pixmaps/Goose.png \
      $out/share/pixmaps/Goose.png
    install -Dm644 usr/share/pixmaps/Goose.png \
      $out/share/icons/hicolor/512x512/apps/Goose.png

    chmod +x \
      $out/opt/Goose/Goose \
      $out/opt/Goose/chrome-sandbox \
      $out/opt/Goose/chrome_crashpad_handler \
      $out/opt/Goose/resources/bin/goosed \
      $out/opt/Goose/resources/bin/node

    patchShebangs $out/opt/Goose/resources/bin

    substituteInPlace $out/share/applications/Goose.desktop \
      --replace-fail 'Exec=/usr/lib/Goose/Goose %U' 'Exec=Goose %U' \
      --replace-fail 'Icon=/usr/share/pixmaps/Goose.png' 'Icon=Goose'

    makeWrapper $out/opt/Goose/Goose $out/bin/Goose \
      --prefix LD_LIBRARY_PATH : "$out/opt/Goose:${lib.makeLibraryPath runtimeLibs}" \
      --prefix PATH : ${lib.makeBinPath [
        bash
        coreutils
        curl
        findutils
        gnugrep
        gnused
        gzip
        which
      ]} \
      --add-flags "\''${NIXOS_OZONE_WL:+\''${WAYLAND_DISPLAY:+--ozone-platform-hint=auto --enable-features=WaylandWindowDecorations --enable-wayland-ime=true}}"

    ln -s $out/bin/Goose $out/bin/goose-desktop

    runHook postInstall
  '';

  preFixup = ''
    addAutoPatchelfSearchPath $out/opt/Goose
  '';

  meta = {
    description = "Native desktop app for Goose AI agent";
    homepage = "https://goose-docs.ai/";
    changelog = "https://github.com/aaif-goose/goose/releases/tag/v${finalAttrs.version}";
    license = lib.licenses.asl20;
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
    mainProgram = "Goose";
    platforms = [ "x86_64-linux" ];
  };
})
