# dots

## Software necesario

### exa
```bash
sudo apt install exa
```

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
