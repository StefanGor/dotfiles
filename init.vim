" TEST SECTION
let g:OmniSharp_timeout = 5
"let g:OmniSharp_server_stdio = 1
"let g:loaded_youcompleteme = 1 "disables YCM
"let g:ycm_always_populate_location_list = 1
"let g:ycm_enable_diagnostic_signs = 0 "disabling until I can fix errors
"let g:python_host_prog = 'C:\Python37\python.exe' "python not detected without this, see issue #5360
set nofixendofline "fix vim changing end of line issue?
set completeopt=longest,menuone "insert the longest common completion

" EDITOR SETTINGS {{{
"set rtp+=~/.vim

let g:session_autosave = 'no'
"colorscheme onedark
set guifont=Hack:h10
set clipboard^=unnamed,unnamedplus
set mouse=a
set lazyredraw
set undofile
set shortmess=aAIsT

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

set scrolloff=10

" use case-insensitive search unless capital letters are used
set ignorecase
set smartcase

" Make \w toggle through the three wrapping modes.
function! ToggleWrap()
 if (&wrap == 1)
   if (&linebreak == 0)
     set linebreak
   else
     set nowrap
   endif
 else
   set wrap
   set nolinebreak
 endif
endfunction

map <leader>w :call ToggleWrap()<CR>

set autoread " refresh external changes when going back to the editor
au FocusGained * :checktime
" }}}

" NORMAL KEY BINDINGS/MAPPINGS {{{
inoremap <C-BS> <C-W>
nmap S :%s//g<LEFT><LEFT>

" Make movement work on wrapped lines
"nnoremap j gj
"nnoremap 0 g0
"nnoremap k gk
"nnoremap $ g$

" Buffers
nnoremap <F2> :buffers<CR>:buffer<Space>
noremap <C-Tab> :bn<CR>
noremap <C-S-Tab> :bp<CR>

" Tabs
noremap <C-Up> :tabnew<CR>
noremap <C-Down> :q<CR>
noremap <C-Left> gT
noremap <C-Right> gt

" Easier movement between windows
"noremap <C-j> <C-W>j
"noremap <C-k> <C-W>k
"noremap <C-h> <C-W>h
"noremap <C-l> <C-W>l

" alt+j/k to swap lines up/down
nnoremap <M-j> mz:m+<cr>`z
nnoremap <M-k> mz:m-2<cr>`z
vnoremap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vnoremap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

" Tab movement using alt + function key
"nnoremap <A-1> 1gt
"nnoremap <A-2> 2gt
"nnoremap <A-3> 3gt
"nnoremap <A-4> 4gt
"nnoremap <A-5> 5gt
"nnoremap <A-6> 6gt
"nnoremap <A-7> 7gt
"nnoremap <A-8> 8gt
"nnoremap <A-9> 9gt
"nnoremap <A-0> 10gt

for i in range(1, 9)
    execute 'nnoremap <A-'.i.'> '.i.'gt<cr>'
endfor

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

"delete without yanking
nnoremap <leader>d "_d 
" }}}

" CUSTOM COMMANDS {{{

"command! Fs :GuiFont! Hack:h8
"command! Fm :GuiFont! Hack:h10
"command! Fl :GuiFont! Hack:h12
"command! -nargs=1 Font :GuiFont! Hack:h<args>

" font functionality from https://github.com/christopher-l/dotfiles/blob/9c67eb42aeac3ade7848fd9aeb152f3e037d4e50/config/nvim/ginit.vim
let s:default_fontsize = 10
let s:fontsize = s:default_fontsize
let s:font = "Hack"

function! SetFont(fontsize) abort
  if exists('g:GtkGuiLoaded')
    call rpcnotify(1, 'Gui', 'Font', s:font . ' ' . a:fontsize)
  else
    exec "GuiFont! " . s:font . ":h" . s:fontsize
  endif
endfunction

function! SetFont() abort
  if exists('g:GtkGuiLoaded')
    call rpcnotify(1, 'Gui', 'Font', s:font . ' ' . s:fontsize)
  else
    exec "GuiFont! " . s:font . ":h" . s:fontsize
  endif
endfunction

function! AdjustFontSize(delta)
  let s:fontsize += a:delta
  call SetFont()
endfunction

function! ResetFontSize()
  let s:fontsize = s:default_fontsize
  call SetFont()
endfunction

nnoremap <C-=> :call AdjustFontSize(1)<CR>
nnoremap <C-+> :call AdjustFontSize(1)<CR>
nnoremap <C--> :call AdjustFontSize(-1)<CR>
nnoremap <C-0> :call ResetFontSize()<CR>

function! MakeSolution() abort
  let makeprg = 'msbuild /nologo /v:q /property:GenerateFullPaths=true /clp:ErrorsOnly '
  let sln = fnamemodify(OmniSharp#FindSolutionOrDir(), ':.')
  echomsg makeprg . sln
  call asyncdo#run(1, makeprg . sln)
endfunction

" }}}

" AUTOCMDS {{{

" let terminal resize scale the internal windows
autocmd VimResized * :wincmd =
autocmd StdinReadPre * let s:std_in=1
"autocmd VimEnter * MRU

"augroup omnisharp_commands
    "autocmd!

    "" When Syntastic is available but not ALE, automatic syntax check on events
    "" (TextChanged requires Vim 7.4)
    "autocmd BufEnter,TextChanged,InsertLeave *.cs SyntasticCheck

    "" Show type information automatically when the cursor stops moving. get some 500 errors when doing this
    ""autocmd CursorHold *.cs call OmniSharp#TypeLookupWithoutDocumentation()

    "" The following commands are contextual, based on the cursor position.
    "autocmd FileType cs nnoremap <buffer> gd :OmniSharpGotoDefinition<CR>
    "autocmd FileType cs nnoremap <buffer> <Leader>mi :OmniSharpFindImplementations<CR>
    "autocmd FileType cs nnoremap <buffer> <Leader>ms :OmniSharpFindSymbol<CR>
    "autocmd FileType cs nnoremap <buffer> <Leader>mu :OmniSharpFindUsages<CR>

    "" Finds members in the current buffer
    "autocmd FileType cs nnoremap <buffer> <Leader>mm :OmniSharpFindMembers<CR>

    "autocmd FileType cs nnoremap <buffer> <Leader>mf :OmniSharpFixUsings<CR>
    "autocmd FileType cs nnoremap <buffer> <Leader>mt :OmniSharpTypeLookup<CR>
    "autocmd FileType cs nnoremap <buffer> <Leader>md :OmniSharpDocumentation<CR>
    "autocmd FileType cs nnoremap <buffer> <C-\> :OmniSharpSignatureHelp<CR>
    "autocmd FileType cs inoremap <buffer> <C-\> <C-o>:OmniSharpSignatureHelp<CR>

	"autocmd FileType cs nnoremap <buffer> <Leader>mr :OmniSharpRestartServer<CR>

    "" Navigate up and down by method/property/field
    "autocmd FileType cs nnoremap <buffer> <C-k> :OmniSharpNavigateUp<CR>
    "autocmd FileType cs nnoremap <buffer> <C-j> :OmniSharpNavigateDown<CR>
	
	"autocmd FileType cs nnoremap <buffer> <Leader>mk :call MakeSolution()<CR>
"augroup END

nnoremap <Leader>ma :OmniSharpGetCodeActions<CR>
nnoremap <Leader>mf :OmniSharpCodeFormat<CR>
nnoremap <Leader>mt :OmniSharpHighlightTypes<CR>
"nnoremap <F2> :OmniSharpRename<CR>
" }}}

" PLUGINS {{{

call plug#begin('~/.vim/plugged') " :echo expand('~')
"https://github.com/equalsraf/neovim-qt/wiki#guifontguiforegroundetc-dont-exist
Plug 'equalsraf/neovim-gui-shim' "For GuiFont functionality without having to change environment variables

Plug 'scrooloose/nerdtree'
map <F6> :NERDTreeToggle<CR>

Plug 'scrooloose/nerdcommenter'
nmap <C-c> <leader>c<Space>
vmap <C-c> <leader>c<Space>

Plug 'justinmk/vim-sneak'
let g:sneak#label = 1
map <leader>f <Plug>Sneak_s
map <leader>F <Plug>Sneak_S

Plug 'itchyny/lightline.vim'
Plug 'airblade/vim-gitgutter'
nmap ]h <Plug>(GitGutterNextHunk)
nmap [h <Plug>(GitGutterPrevHunk)

Plug 'yegappan/mru'
"Plug 'bronson/vim-trailing-whitespace'
Plug 'Yggdroot/indentLine'

"Plug 'w0rp/ale'
let g:ale_linters = { 'cs': ['OmniSharp'] }

Plug 'vim-syntastic/syntastic'
let g:syntastic_cs_checkers = ['code_checker']

Plug 'OmniSharp/omnisharp-vim'
Plug 'nickspoons/vim-sharpenup'
let g:sharpenup_map_prefix = ','

Plug 'hauleth/asyncdo.vim' " for building with omnisharp

Plug 'jiangmiao/auto-pairs'

"Plug 'Shougo/denite.nvim'

Plug 'airblade/vim-rooter'
"let g:rooter_manual_only = 1

Plug 'majutsushi/tagbar'
nmap <F8> :TagbarToggle<CR>

"Plug 'ludovicchabant/vim-gutentags'
let g:gutentags_cache_dir = expand('$HOME/Documents/Tags')
let g:gutentags_ctags_exclude = ['*/node_modules/*']

Plug 'junegunn/vim-peekaboo'
Plug 'demelev/TagHighlight'

"Plug 'tpope/vim-dispatch'

Plug 'kshenoy/vim-signature'

"Plug 'Yggdroot/LeaderF', { 'do': '.\install.bat' }
"let g:Lf_ShortcutF = '<C-P>'
"let g:Lf_WorkingDirectoryMode = 'A'
"nmap <leader>t :LeaderfTag<CR>
" Press C-I or tab to open up the help menu

Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
"nnoremap <leader>g :GitFiles?<CR>
"nnoremap <leader>p :Files<CR>
"nnoremap <leader>t :Tags<CR>
"nnoremap <leader><Space> :Commands<CR>
nnoremap <leader>b :Buffers<CR>

Plug 'liuchengxu/vim-clap', { 'do': ':Clap install-binary!' }
nnoremap <leader>l :Clap 
nnoremap <leader>g :Clap git_diff_files<CR>
"nnoremap <leader>c :Clap commits<CR>
nnoremap <leader>p :Clap files<CR>
nnoremap <leader>j :Clap jumps<CR>
nnoremap <leader><Space> :Clap command<CR>
"nnoremap <leader>b :Clap buffers<CR> " get annoying errors
nnoremap <leader>c :Clap colors<CR>

Plug 'google/vim-searchindex'

Plug 'xolox/vim-misc'
Plug 'xolox/vim-session'
let g:session_autoload = "yes"

Plug 'mattn/emmet-vim'

Plug 'nickspoons/vim-cs'
Plug 'OrangeT/vim-csharp'

Plug 'joshdick/onedark.vim'

Plug 'tpope/vim-unimpaired'

Plug 'farmergreg/vim-lastplace'

Plug 'ajh17/VimCompletesMe'

"Plug 'kizza/ask-vscode.nvim' "doesnt work
"nnoremap <silent> <Leader>av :AskVisualStudioCode<CR>

Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'

"Plug 'neoclide/coc.nvim', {'branch': 'release'}
let g:coc_global_extensions=[ 'coc-omnisharp' ]

"inoremap <silent><expr> <TAB>
      "\ pumvisible() ? coc#_select_confirm() :
      "\ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
      "\ <SID>check_back_space() ? "\<TAB>" :
      "\ coc#refresh()

function! s:check_back_space() abort "why did i add this?
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gh <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

"Plug 'zxqfl/tabnine-vim'
Plug 'tpope/vim-rsi'

Plug 'sheerun/vim-polyglot'
call plug#end()
colo onedark
" }}}

"HANDY STUFF I ALWAYS FORGET
":copen opens the quickfix window
"modeline:
" vim: foldmethod=marker