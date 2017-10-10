if has("win32")
	source $VIMRUNTIME/vimrc_example.vim
	source $VIMRUNTIME/mswin.vim
	behave mswin

	set diffexpr=MyDiff()
	function! MyDiff()
	  let opt = '-a --binary '
	  if &diffopt =~ 'icase' | let opt = opt . '-i ' | endif
	  if &diffopt =~ 'iwhite' | let opt = opt . '-b ' | endif
	  let arg1 = v:fname_in
	  if arg1 =~ ' ' | let arg1 = '"' . arg1 . '"' | endif
	  let arg2 = v:fname_new
	  if arg2 =~ ' ' | let arg2 = '"' . arg2 . '"' | endif
	  let arg3 = v:fname_out
	  if arg3 =~ ' ' | let arg3 = '"' . arg3 . '"' | endif
	  if $VIMRUNTIME =~ ' '
		if &sh =~ '\<cmd'
		  if empty(&shellxquote)
			let l:shxq_sav = ''
			set shellxquote&
		  endif
		  let cmd = '"' . $VIMRUNTIME . '\diff"'
		else
		  let cmd = substitute($VIMRUNTIME, ' ', '" ', '') . '\diff"'
		endif
	  else
		let cmd = $VIMRUNTIME . '\diff'
	  endif
	  silent execute '!' . cmd . ' ' . opt . arg1 . ' ' . arg2 . ' > ' . arg3
	  if exists('l:shxq_sav')
		let &shellxquote=l:shxq_sav
	  endif
	endfunction
set backup " https://stackoverflow.com/questions/2197749/gvim-on-windows-way-to-disable-the-tmp-file-creation
set dir=%TMP%
set backupdir=%TMP%
set directory=%TMP%
endif
set noundofile

syntax on
filetype plugin indent on

set guifont=Hack:h10

set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite,*.meta,*.unity,*.controller,*.anim

colorscheme desert

" show existing tab with 4 spaces width
set tabstop=4
" when indenting with '>', use 4 spaces width
set shiftwidth=4

nnoremap j gj
nnoremap 0 g0
nnoremap k gk
nnoremap $ g$

" Nerdtree
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
map <C-n> :NERDTreeToggle<CR>
let NERDTreeMapActivateNode='l'

set belloff=all " http://vim.wikia.com/wiki/Disable_beeping

set number relativenumber "relative numbers
set scrolloff=10

" simplify common tab operations
map <C-Up> :tabnew<CR>
map <C-Down> :q<CR>
map <C-Left> gT
map <C-Right> gt

"easier movement between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" alt+j/k
nmap <M-j> mz:m+<cr>`z
nmap <M-k> mz:m-2<cr>`z
vmap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vmap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

" Pressing \ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>

:let g:session_autosave = 'no' " vim session warning

if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag --ignore *.meta -l --nocolor -g "" %s'

  " ag is fast enough that CtrlP doesn't need to cache
  "let g:ctrlp_use_caching = 0

endif

let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll|meta|anim|unity|controller)$'
  \ }

noremap <Space> <nop>
map <SPACE> <leader>

nnoremap <leader><Space> :w<cr>
noremap <leader>ev :execute 'e ' . resolve(expand($MYVIMRC))<CR>
nnoremap <leader>so :w<cr> <bar> :so $MYVIMRC<cr>
inoremap fd <Esc>

let g:sneak#label = 1
map <leader>f <Plug>Sneak_s
map <leader>F <Plug>Sneak_S

call plug#begin()
"Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'justinmk/vim-sneak'
Plug 'xolox/vim-misc'
Plug 'xolox/vim-session'

Plug 'vim-airline/vim-airline'
Plug 'airblade/vim-gitgutter'

"Plug 'easymotion/vim-easymotion'
Plug 'yegappan/mru'

Plug 'bronson/vim-trailing-whitespace'
Plug 'sheerun/vim-polyglot'
Plug 'jreybert/vimagit'
call plug#end()
