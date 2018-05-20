if exists('g:GtkGuiLoaded')
    call rpcnotify(1, 'Gui', 'Font', 'Hack 10')
else
    Guifont! Hack:h10
endif
