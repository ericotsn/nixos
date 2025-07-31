{ pkgs, ... }:

{
  specialisation.xmonad.configuration = {
    imports = [
      ../polkit-gnome.nix
    ];

    services.xserver.enable = true;

    services.xserver.xkb = {
      layout = "us";
      options = "ctrl:nocaps"; # Replace Caps Lock with Control
    };

    services.xserver.dpi = 192; # Set display scaling to 200%
    services.xserver.upscaleDefaultCursor = true; # Upscale the default X cursor

    services.xserver.displayManager = {
      lightdm.enable = true;

      sessionCommands = ''
        ${pkgs.xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr
        ${pkgs.xorg.xsetroot}/bin/xsetroot -solid black
        ${pkgs.xorg.xset}/bin/xset r rate 200 40
      '';
    };

    services.displayManager.defaultSession = "none+xmonad";

    # XMonad, a tiling window manager for X11.
    services.xserver.windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

    environment.systemPackages = [
      pkgs.dmenu
    ];

    security.polkit.enable = true;

    services.gnome.gnome-keyring.enable = true;

    programs.polkit-gnome.enable = true;
  };
}
