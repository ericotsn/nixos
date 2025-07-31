# NixOS

This repository contains my personal NixOS and macOS system configuration, packaged as a Nix flake to be installed in a VMWare Fusion VM on macOS. In addition, you'll also find my dotfiles (mainly my literate [Emacs configuration](dotfiles/dot-emacs.d/init.org)) and some documentation for common workflows when developing using a VM.

Feel free to take inspiration and copy from this repository, but I recommend that you familiarize yourself with Nix and NixOS before doing so.

This setup is heavily inspired by Mitchell Hashimoto. You can find his configuration files on [GitHub](https://github.com/mitchellh/nixos-config).

## Motivation

The main reason for running this setup, i.e. a Linux VM on macOS, is that I get to enjoy the best of both worlds: Apple's great hardware support and native applications, and a powerful and flexible Linux environment with a tiling window manager, etc.

In addition, it lets me create isolated environments for work or personal projects—something which is especially useful when [streaming](https://twitch.tv/erlook).

It's possible to replace NixOS with any Linux distribution, but if you're interested in this type of dev workflow I'd highly recommend looking into NixOS.

## VM bootstrapping

If you haven't done so, install VMware Fusion.

> [!NOTE]
> Yes, we're all upset that VMware is now part of Broadcom, but let's just be happy that VMware Fusion (Pro) is now free for both personal and commercial use[^1].

[^1]: https://blogs.vmware.com/cloud-foundation/2024/11/11/vmware-fusion-and-workstation-are-now-free-for-all-users/].

**#1:** Start by creating a new VM with VMware Fusion. I use the following settings:

- OS: Other Linux 6.x kernel 64-bit Arm
- Sharing: Enable Shared Folders ✅ (I share my home directory with the VM)
- Processor & Memory: 5 cores (50%), 16384MB (50%)
- Display: Accelerate 3D graphics ✅, Shared graphics memory (100%), Use full resolution for Retina display ✅
- Network: Share with my Mac (Bridged networking should be avoided as the guest is configured to disable the firewall by default)
- Hard Disk (NVMe): 256,00GB
- Sound Card: *Remove Sound Card* ❌
- Camera: *Remove Camera* ❌

> [!IMPORTANT]
> It's **very important** to set up Shared Folders, otherwise you'll likely experience issues starting your VM (after installation) when it fails to mount the file system. If you don't care about Shared Folders, make sure to remove any related `fileSystems` from your NixOS configuration.

**#2:** Start the new VM and boot into the installer. We're going to use SSH to complete the rest of the installation, so we need to set a password for the root user using `sudo passwd`. Choose any password you'd like.

**#3:** If you haven't done so already, clone this repository. Get the IP of your VM using `ifconfig` and initiate the installation by running `NIXADDR=<IPADDR> make vm/bootstrap` (you'll be asked for the password you set in the previous step). Check the Makefile for additional options.

**#4:** Wait for the installation to complete and the VM to restart (~20 minutes).

**#5:** Complete the setup by running `NIXADDR=<IPADDR> make vm/init`. This will clone this repository to `~/nixos` in the VM and install the dotfiles. Make sure to use the same options as in step #3, unless you edited the Makefile directly.

**#6:** Restart the VM, select your specialization in the boot menu and sign in to your fresh dev environment! 🎉

### Fonts

I'm using a paid font, [PragmataPro](https://fsd.it/shop/fonts/pragmatapro/), which I can't redistribute, so unless you intend to use the same font you should update the relevant configuration and switch to your favorite font instead (use something like `grep` to find the locations). If you like the look of PragmataPro I recommend you to check out [Iosevka](https://typeof.net/Iosevka/), which is very similar but free.

### Post-installation

If you're planning on making changes to your configuration and sync these with your VCS host you'll want to change the remote for the `~/nixos` repository from HTTPS to SSH.

Now is also a good time to create a snapshot and clone the VM if you're planning on using multiple VMs.

After making any change to your configuration (excluding dotfiles, see below) you can apply it by running `make switch`.

> [!NOTE]
> I've purposefully used `stow` to manage dotfiles instead of tools such as [Home Manager](https://github.com/nix-community/home-manager) to avoid having to rebuild your NixOS configuration for every insignificant change. This is a personal preference of mine.

## macOS setup

I also include configuration files for macOS applications in my dotfiles, so if you plan on using AeroSpace or Alacritty you should also run `make dotfiles` in the folder you cloned to the host when creating the VM.

## Remote access

Unless disabled, there should be an SSH server running on the guest OS that can be accessed using the VMs IP address. If you'd rather use a hostname instead of an IP you could add the following line to `/etc/hosts` on the host (macOS):

```
<IPADDR> local.nixos
```

After updating, flush the DNS cache with:

```
sudo dscacheutil -flushcache; sudo killall -HUP mDNSResponder
```

You can then access the VM using `local.nixos` instead of its IP address.

### Proxying `localhost`

When running applications within the VM that interface with external services you can run into issues when using NAT networking. One I run into frequently is when an external service my team uses needs a callback URL. I could add my VM's IP to the list of callback URLs, but this is an inflexible solution. Instead we can use a tool like `socat` to proxy a specific port on `localhost` to my VM:

```
socat TCP-LISTEN:8080,reuseaddr,fork TCP:<IPADDR>:8080
```

### NodeJS debugging

I do a fair bit of webdev where I have a service running inside the VM that I need to access from the host using the Chrome DevTools debugging features.

For this to work it's important that any service is listening on `0.0.0.0` instead of the loopback address. Make sure to start the application using `NODE_OPTIONS="--inspect=0.0.0.0:9229"`.

In `chrome://inspect` enable the "Discover network targets" option and add the following entries to the list:

- `<IPADDR>:9229`
- `<IPADDR>:9230`

Alternatively, if you added an entry into `/etc/hosts` you could also use that instead, e.g. `local.nixos:9229`.

