"BFOLD
if has("win32")
	set backup " https://stackoverflow.com/questions/2197749/gvim-on-windows-way-to-disable-the-tmp-file-creation
	set dir=%TMP%
	set backupdir=%TMP%
	set directory=%TMP%

	" Start on right half of first monitor
	set lines=70 columns=120
	winpos 960 10

	set rtp+=C:/Program\ Files/SumatraPDF
	let g:vimtex_view_general_viewer = 'SumatraPDF'
	let g:vimtex_view_general_options
		\ = '-reuse-instance -forward-search @tex @line @pdf'
		\ . ' -inverse-search "gvim --servername ' . v:servername
		\ . ' --remote-send \"^<C-\^>^<C-n^>'
		\ . ':drop \%f^<CR^>:\%l^<CR^>:normal\! zzzv^<CR^>'
		\ . ':execute ''drop '' . fnameescape(''\%f'')^<CR^>'
		\ . ':\%l^<CR^>:normal\! zzzv^<CR^>'
		\ . ':call remote_foreground('''.v:servername.''')^<CR^>^<CR^>\""'
else
	set clipboard=unnamedplus
endif
"EFOLD

set noundofile
syntax on
filetype plugin indent on
set foldmethod=marker
set foldmarker=BFOLD,EFOLD
set guifont=Hack:h10
set encoding=utf-8
set mouse=a
au BufRead,BufNewFile *.txt,*.tex set wrap linebreak nolist textwidth=0 wrapmargin=0
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite,*.meta,*.unity,*.controller,*.anim
set breakindent
set autoindent

colorscheme monokai

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

 "simplify common tab operations
map <C-Up> :tabnew<CR>
map <C-Down> :q<CR>
map <C-Left> gT
map <C-Right> gt

"buffer navigation
noremap <C-Tab> :bn<CR>
noremap <C-S-Tab> :bp<CR>
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'

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

nnoremap <silent> <Esc><Esc> <Esc>:nohlsearch<CR><Esc>
nnoremap <F5> :buffers<CR>:buffer<Space>

 "can type ':e %%\' to get the current file's path
cabbr <expr> %% expand('%:p:h')

nnoremap <F6> :VimtexTocToggle<CR>

nmap <C-m> <leader>c<Space>
vmap <C-m> <leader>c<Space>

call plug#begin()
"Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'justinmk/vim-sneak'
Plug 'xolox/vim-misc'
Plug 'xolox/vim-session'
Plug 'lervag/vimtex'
Plug 'vim-airline/vim-airline'
Plug 'airblade/vim-gitgutter'
Plug 'yegappan/mru'
Plug 'bronson/vim-trailing-whitespace'
Plug 'sirver/UltiSnips'
Plug 'honza/vim-snippets'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'Raimondi/delimitMate'
Plug 'ntpeters/vim-better-whitespace'
Plug 'ervandew/supertab'
call plug#end()
