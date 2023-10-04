{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "seryiza";
  home.homeDirectory = "/home/seryiza";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')

    pkgs.tldr
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/seryiza/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  xsession.enable = true;

  programs.tmux = {
    enable = true;
    escapeTime = 10;
    keyMode = "vi";
    prefix = "C-n";
    terminal = "tmux";
    baseIndex = 1;
    clock24 = true;

    extraConfig = ''
    # Colors
    color_grey='#BBBBBB'
    color_black='#000000'

    # Common options
    #set -g base-index 1
    #set -g mouse off
    #set -g mode-keys vi
    #set -sg escape-time 10        # make it 0?
    set -g status-interval 10     # redraw status line every 10 seconds
    set -g status-position top

    # Fix colors
    #set -g default-terminal "screen-256color"
    set -g default-terminal "tmux"
    set -as terminal-features ",xterm-256color*:RGB"
    set -ga terminal-overrides ",xterm-256color*:Tc"

    # Change prefix
    #set-option -g prefix C-n
    #unbind C-b
    #bind C-n send-prefix

    # Bindings
    bind r source-file ~/.tmux.conf
    bind | split-window -h
    bind - split-window -v
    bind m set -g mouse on
    bind M set -g mouse off
    unbind '"'
    unbind %

    bind -n M-l next-window
    bind -n M-h previous-window

    bind -n M-д next-window
    bind -n M-р previous-window

    # Status options
    set -g status-justify centre
    set -g status-left '''
    set -g status-right '''
    set -g status-style 'none'
    set -g window-status-style fg=$color_grey
    set -g window-status-format " #I: #W "
    set -g window-status-current-format " #I: #W "
    set -g window-status-current-style fg=$color_black
    '';
  };

}
