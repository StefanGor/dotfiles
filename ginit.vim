if exists('g:GtkGuiLoaded')
    call rpcnotify(1, 'Gui', 'Option', 'Tabline', 0) " use vim tabs instead of gtk tabs
	call rpcnotify(1, 'Gui', 'Font', 'Hack 10')
else
    Guifont! Hack:h10 " set font here instead of init.vim https://github.com/equalsraf/neovim-qt/issues/444
endif
