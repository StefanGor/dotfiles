"BFOLD
if has("win32")
	set backup " https://stackoverflow.com/questions/2197749/gvim-on-windows-way-to-disable-the-tmp-file-creation
	set dir=%TMP%
	set backupdir=%TMP%
	set directory=%TMP%

	" Start on right half of first monitor
	set lines=70 columns=120
	"winpos 1000 0

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
	" linux-specific stuff
endif
"EFOLD

" check :h vim_diff.txt
" allow pasting on both windows and linux
set clipboard^=unnamed,unnamedplus
set mouse=a
set noundofile
syntax on
filetype plugin indent on

set foldmethod=marker
set foldmarker=BFOLD,EFOLD

set guifont=Hack:h10
set encoding=utf-8
au BufRead,BufNewFile *.txt,*.tex set wrap linebreak nolist textwidth=0 wrapmargin=0
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite,*.meta,*.unity,*.controller,*.anim
set breakindent
set autoindent
set backspace=indent,eol,start
set guioptions=
set lazyredraw
"colorscheme monokai
colorscheme desert
set nrformats-=octal

set incsearch
set hlsearch
nnoremap <silent> <Esc><Esc> <Esc>:nohlsearch<CR><Esc>

" show existing tab with 4 spaces width
set tabstop=4
" when indenting with '>', use 4 spaces width
set shiftwidth=4

set laststatus=2
set showtabline=1

" make Vim add new vertical splits to the right and new horizontal splits below
set splitright
set splitbelow

" Make movement work on wrapped lines
nnoremap j gj
nnoremap 0 g0
nnoremap k gk
nnoremap $ g$

inoremap <C-BS> <C-W>

" Nerdtree/MRU
autocmd StdinReadPre * let s:std_in=1
" Open MRU by default if a file is not being opened
"autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | MRU | endif
"autocmd VimEnter * MRU
"map <C-n> :NERDTreeToggle<CR>
let NERDTreeMapActivateNode='l'

set belloff=all " http://vim.wikia.com/wiki/Disable_beeping
set number relativenumber "relative numbers
set scrolloff=10

" Add some tab creation shortcuts
noremap <C-Up> :tabnew<CR>
noremap <C-Down> :q<CR>
noremap <C-Left> gT
noremap <C-Right> gt

noremap <C-Tab> :bn<CR>
noremap <C-S-Tab> :bp<CR>

" Easier movement between windows
noremap <C-j> <C-W>j
noremap <C-k> <C-W>k
noremap <C-h> <C-W>h
noremap <C-l> <C-W>l

" alt+j/k to swap lines up/down
nnoremap <M-j> mz:m+<cr>`z
nnoremap <M-k> mz:m-2<cr>`z
vnoremap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vnoremap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

" Toggle spellchecking
noremap <leader>ss :setlocal spell!<cr>

" Use CTRL-S for saving, also in Insert mode
noremap <C-S> :update<CR>
vnoremap <C-S> <C-C>:update<CR>
inoremap <C-S> <C-O>:update<CR>

let g:session_autosave = 'no' " vim session warning
let g:session_autoload = 'no'

if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor
  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag --ignore *.meta -l --nocolor -g "" %s'
  " ag is fast enough that CtrlP doesn't need to cache
  "let g:ctrlp_use_caching = 0
endif

"let g:ctrlp_custom_ignore = {
  "\ 'dir':  '\v[\/]\.(git|hg|svn)$',
  "\ 'file': '\v\.(exe|so|dll|meta|anim|unity|controller)$'
  "\ }
autocmd FileType cs setlocal omnifunc=OmniSharp#Complete
noremap <Space> <nop>
map <SPACE> <leader>

nnoremap <leader><Space> :w<cr>
noremap <leader>ev :execute 'e ' . resolve(expand($MYVIMRC))<CR>
nnoremap <leader>so :w<cr> <bar> :so $MYVIMRC<cr>
inoremap fd <Esc>

let g:sneak#label = 1
map <leader>f <Plug>Sneak_s
map <leader>F <Plug>Sneak_S

nnoremap <F5> :buffers<CR>:buffer<Space>

" Can type ':e %%\' to get the current file's path
cabbr <expr> %% expand('%:p:h')

"nnoremap <F6> :VimtexTocToggle<CR>

"nnoremap <leader>t :FZF<CR>

nmap <C-m> <leader>c<Space>
vmap <C-m> <leader>c<Space>

let g:vimtex_quickfix_open_on_warning = 0
call plug#begin('~/.vim/plugged') " :echo expand('~')
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'justinmk/vim-sneak'
"Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
"Plug 'junegunn/fzf.vim'
"Plug 'jremmen/vim-ripgrep'
Plug 'xolox/vim-misc'
Plug 'xolox/vim-session'
"Plug 'lervag/vimtex'
Plug 'itchyny/lightline.vim'
Plug 'airblade/vim-gitgutter'
Plug 'yegappan/mru'
Plug 'bronson/vim-trailing-whitespace'
"Plug 'sirver/UltiSnips'
"Plug 'honza/vim-snippets'
Plug 'nathanaelkane/vim-indent-guides'
"Plug 'Raimondi/delimitMate'
Plug 'ntpeters/vim-better-whitespace'
"Plug 'ervandew/supertab'
"Plug 'OmniSharp/omnisharp-vim'
Plug 'tpope/vim-dispatch' "dont need to run :OmnisharpStartServer with this
Plug 'Shougo/denite.nvim'
Plug 'vim-syntastic/syntastic'
"if has('nvim')
  "Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
"else
  "Plug 'Shougo/deoplete.nvim'
  "Plug 'roxma/nvim-yarp'
  "Plug 'roxma/vim-hug-neovim-rpc'
"endif
"Plug 'Valloric/YouCompleteMe'
call plug#end()

