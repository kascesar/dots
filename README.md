# dots

## Software necesario

### exa
```bash
sudo apt install exa
```
### kitty -emulador de termina que uso-
```bash
curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin && \
ln -sf ~/.local/kitty.app/bin/kitty ~/.local/kitty.app/bin/kitten ~/.local/bin/ && \
cp ~/.local/kitty.app/share/applications/kitty.desktop ~/.local/share/applications/ && \
sed -i "s|Icon=kitty|Icon=$(readlink -f ~)/.local/kitty.app/share/icons/hicolor/256x256/apps/kitty.png|g" ~/.local/share/applications/kitty*.desktop && \
sed -i "s|Exec=kitty|Exec=$(readlink -f ~)/.local/kitty.app/bin/kitty|g" ~/.local/share/applications/kitty*.desktop && \
sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator ~/.local/bin/kitty 40
```

Ahora podemos elegi que _emulador de terminal_ por defecto del sistema queremos con:
```bash
sudo update-alternatives --config x-terminal-emulator
```
*NOTA*: Recuerda poner la configuracion de kitty en `~/.config/kitty/kitty.conf`

### oh-my-posh

Instalar con:
```bash
curl -s https://ohmyposh.dev/install.sh | bash -s
```
Ademas, neceitamos instalar las fuentes necesarias
luego de instalar _oh my posh_
```bash
oh-my-posh font install meslo
```

---

## Qmk

* Compilar *firmawar QMK*
```shell
qmk compile -kb crkbd -km cesar
```

* Flashear KeyBoard
```shell
qmk flash -kb crkbd -km cesar
```
## Emacs

### Instalar las fuentes
  ```shell
  M-x all-the-icons-install-fonts
  ```
