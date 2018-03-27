set rtp+=~/.vim

" temp/test section
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * MRU
"let g:loaded_youcompleteme = 1 "disables YCM
"let g:ycm_always_populate_location_list = 1

" Editor Settings
colorscheme onedark
set clipboard^=unnamed,unnamedplus
set mouse=a
set guifont=Hack:h10
set lazyredraw
set undofile

command! Fs :GuiFont! Hack:h10
command! Fl :GuiFont! Hack:h12
command! -nargs=1 Font :GuiFont! Hack:h<args>

" tabs 4 spaces width and indent by 4 spaces with <
set tabstop=4
set shiftwidth=4
set smartindent
set expandtab
set showtabline=1
set number relativenumber "relative numbers

" make Vim add new vertical splits to the right and new horizontal splits below
set splitright
set splitbelow

au BufRead,BufNewFile *.txt,*.tex set wrap linebreak nolist textwidth=0 wrapmargin=0
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite,*.meta,*.unity,*.controller,*.anim

" Normal Key Bindings/Mappings
inoremap <C-BS> <C-W>

" Make movement work on wrapped lines
nnoremap j gj
nnoremap 0 g0
nnoremap k gk
nnoremap $ g$

" Buffers
nnoremap <F5> :buffers<CR>:buffer<Space>
noremap <C-Tab> :bn<CR>
noremap <C-S-Tab> :bp<CR>

" Tabs
noremap <C-Up> :tabnew<CR>
noremap <C-Down> :q<CR>
noremap <C-Left> gT
noremap <C-Right> gt

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

" Use CTRL-S for saving, also in Insert mode
noremap <C-S> :update<CR>
vnoremap <C-S> <C-C>:update<CR>
inoremap <C-S> <C-O>:update<CR>

map <SPACE> <leader>
nnoremap <leader><Space> :w<cr>
noremap <leader>ev :execute 'e ' . resolve(expand($MYVIMRC))<CR>
nnoremap <leader>so :w<cr> <bar> :so $MYVIMRC<cr>
inoremap fd <Esc>

" esc esc to remove highlighting from searched word
nnoremap <silent> <Esc><Esc> <Esc>:nohlsearch<CR><Esc>

" Can type ':e %%\' to get the current file's path
cabbr <expr> %% expand('%:p:h')

" Plugin variables

" Use The Silver Searcher https://github.com/ggreer/the_silver_searcher
if executable('ag')
  let g:ctrlp_user_command = 'ag -l --nocolor -g "" %s'
endif

" Plugins
call plug#begin('~/.vim/plugged') " :echo expand('~')

Plug 'scrooloose/nerdtree'

Plug 'scrooloose/nerdcommenter'
nmap <C-c> <leader>c<Space>
vmap <C-c> <leader>c<Space>

Plug 'justinmk/vim-sneak'
let g:sneak#label = 1
map <leader>f <Plug>Sneak_s
map <leader>F <Plug>Sneak_S

Plug 'itchyny/lightline.vim'
Plug 'airblade/vim-gitgutter'
Plug 'yegappan/mru'
Plug 'bronson/vim-trailing-whitespace'

Plug 'Valloric/YouCompleteMe'
let g:ycm_autoclose_preview_window_after_completion=1

"Plug 'OmniSharp/omnisharp-vim'
"let g:OmniSharp_server_type = 'v1'
"let g:OmniSharp_server_type = 'roslyn'
"let g:OmniSharp_server_path = expand('$HOME/Documents/GitHub/roslyn/omnisharp.exe')

Plug 'jiangmiao/auto-pairs'

Plug 'Shougo/denite.nvim'

"Plug 'airblade/vim-rooter'
"let g:rooter_manual_only = 1

Plug 'majutsushi/tagbar'
nmap <F8> :TagbarToggle<CR>

Plug 'ludovicchabant/vim-gutentags'
let g:gutentags_cache_dir = expand('$HOME/Documents/Tags')

Plug 'junegunn/vim-peekaboo'
"Plug 'demelev/TagHighlight'

"Plug 'tpope/vim-dispatch'

Plug 'kshenoy/vim-signature'
"Plug 'mhinz/vim-signify' " like gitgutter but does other VCS

Plug 'Yggdroot/LeaderF'
let g:Lf_ShortcutF = '<C-P>'
let g:Lf_WorkingDirectoryMode = 'A'

call plug#end()

"call denite#custom#var('file_rec', 'command',['pt', '--follow', '--nocolor', '--nogroup', '-g:', ''])
"map <C-P> :DeniteProjectDir -buffer-name=git  file_rec<CR>
