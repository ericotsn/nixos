{ config, pkgs, ... }:

{
  imports = [
    ./vm-aarch64-hardware.nix
    ../modules/specialisations/xmonad.nix
  ];

  nix = {
    settings = {
      trusted-users = [
        "root"
        "@wheel"
      ];
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      substituters = [
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  virtualisation.vmware.guest.enable = true;

  virtualisation.docker.enable = true;

  # Mount VMWare Shared Folders.
  fileSystems."/host" = {
    fsType = "fuse./run/current-system/sw/bin/vmhgfs-fuse";
    device = ".host:/";
    options = [
      "umask=22"
      "uid=1000"
      "gid=100"
      "allow_other"
      "auto_unmount"
      "defaults"
    ];
  };

  networking.hostName = "vm-aarch64";

  networking.interfaces.ens160.useDHCP = true;

  time.timeZone = "Europe/Stockholm";

  i18n.defaultLocale = "en_US.UTF-8";

  # Don't require password for sudo.
  security.sudo.wheelNeedsPassword = false;

  # Only allow declarative creation of users and groups. This will reset any
  # user's password unless set with users.users.<name>.hashedPassword.
  users.mutableUsers = false;

  fonts.packages = [
    pkgs.adwaita-fonts
    pkgs.jetbrains-mono
  ];

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    cachix
    gnumake
    xclip
  ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.settings.PermitRootLogin = "no";

  # Disable the firewall to allow connections between the host and VM without
  # any fuss. This is only safe when using NAT networking, and should be
  # enabled otherwise.
  networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?
}
