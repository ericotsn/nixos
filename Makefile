NIXADDR ?= unset
NIXUSER ?= ericotsn
NIXNAME ?= vm-aarch64
NIXSPEC ?= xmonad

SSH_OPTIONS = -o PubkeyAuthentication=no -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no

switch:
	sudo nixos-rebuild switch --flake ".#$(NIXNAME)" --specialisation $(NIXSPEC)

.PHONY: dotfiles
dotfiles:
	cd dotfiles && sh install

vm/bootstrap:
	ssh $(SSH_OPTIONS) root@$(NIXADDR) "\
		parted /dev/nvme0n1 -- mklabel gpt; \
		parted /dev/nvme0n1 -- mkpart ESP fat32 2MiB 1026MiB; \
		parted /dev/nvme0n1 -- mkpart primary ext4 1026MiB 100%; \
		parted /dev/nvme0n1 -- set 1 esp on; \
		sleep 1; \
		mkfs.fat -F 32 -n EFI /dev/nvme0n1p1; \
		mkfs.ext4 -L ROOT /dev/nvme0n1p2; \
		sleep 1; \
		mount /dev/disk/by-label/ROOT /mnt; \
		mkdir -p /mnt/boot; \
		mount /dev/disk/by-label/EFI /mnt/boot -o fmask=0077,dmask=0077; \
		sleep 1; \
		nixos-install --flake github:ericotsn/nixos#$(NIXNAME) --no-root-passwd && reboot; \
	"

vm/init:
	ssh $(SSH_OPTIONS) $(NIXUSER)@$(NIXADDR) "\
		git clone https://github.com/ericotsn/nixos; \
		sleep 1; \
		cd nixos/dotfiles && sh install; \
	"
