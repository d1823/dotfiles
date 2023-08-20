#!/usr/bin/env bash

type=$(gsettings get org.gnome.desktop.interface color-scheme | grep -i prefer-dark > /dev/null && echo dark || echo light)
sed -i -e "s/colors: .*/colors: *${type}_theme/" ~/.config/alacritty/alacritty.yml
