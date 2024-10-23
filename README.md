# dots
Varias configuraciones que uso en mi día a día 


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

* Instalar las fuentes
  ```shell
  M-x all-the-icons-install-fonts
  ```
* Instalar Emacs EAF
  ```shell
  git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git ~/.emacs.d/site-lisp/emacs-application-framework/ \
  cd ~/.emacs.d/site-lisp/emacs-application-framework/ \
  chmod +x ./install-eaf.py \
  ./install-eaf.py
  ```
