set rtp+=~/.vim

" temp/test section

" Editor Settings
colorscheme onedark
set clipboard^=unnamed,unnamedplus
set mouse=a
set guifont=Hack:h10
set lazyredraw
" tabs 4 spaces width and indent by 4 spaces with <
set tabstop=4
set shiftwidth=4

set showtabline=1
set number relativenumber "relative numbers

" make Vim add new vertical splits to the right and new horizontal splits below
set splitright
set splitbelow

au BufRead,BufNewFile *.txt,*.tex set wrap linebreak nolist textwidth=0 wrapmargin=0
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite,*.meta,*.unity,*.controller,*.anim

" Normal Key Bindings/Mappings

" Make movement work on wrapped lines
nnoremap j gj
nnoremap 0 g0
nnoremap k gk
nnoremap $ g$

nnoremap <F5> :buffers<CR>:buffer<Space>
noremap <C-Tab> :bn<CR>
noremap <C-S-Tab> :bp<CR>

" Add some tab creation shortcuts
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

" Plugin Key Bindings

" commenting - nerdcommenter
nmap <C-m> <leader>c<Space>
vmap <C-m> <leader>c<Space>

let g:sneak#label = 1
map <leader>f <Plug>Sneak_s
map <leader>F <Plug>Sneak_S

" Plugin variables

let g:indent_guides_enable_on_vim_startup = 1

" Use The Silver Searcher https://github.com/ggreer/the_silver_searcher
if executable('ag')
  let g:ctrlp_user_command = 'ag -l --nocolor -g "" %s'
endif

call plug#begin('~/.vim/plugged') " :echo expand('~')
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'
Plug 'justinmk/vim-sneak'
Plug 'itchyny/lightline.vim'
Plug 'airblade/vim-gitgutter'
Plug 'yegappan/mru'
Plug 'bronson/vim-trailing-whitespace'
Plug 'nathanaelkane/vim-indent-guides'
"Plug 'OmniSharp/omnisharp-vim'
"Plug 'Valloric/YouCompleteMe'
Plug 'ctrlpvim/ctrlp.vim'
call plug#end()
