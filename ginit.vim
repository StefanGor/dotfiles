if exists('g:GtkGuiLoaded')
    call rpcnotify(1, 'Gui', 'Option', 'Tabline', 0) " use vim tabs instead of gtk tabs
	call rpcnotify(1, 'Gui', 'Font', 'Hack 10')
else
    "GuiFont! Hack:h10 " set font here instead of init.vim https://github.com/equalsraf/neovim-qt/issues/444
    "GuiTabline 0
    "GuiPopupmenu 0
endif

if exists('g:GuiLoaded')
    GuiPopupmenu 0
    GuiTabline 0
    GuiLinespace 3
    GuiFont! Hack:h10
    call GuiWindowMaximized(1)
endif
