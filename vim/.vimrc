" ________         	__________    
" |------|| ||   // ||________\   
" ||     || ||  //  ||        \\  
" ||     || || //   ||         \| 
" ||_____|| ||//    ||         || 
" ||-----|| ||\\    ||         || 
" ||        || \\   ||         || 
" ||        ||  \\  ||         /| 
" ||        ||   \\ ||________//  
" ||        ||    \\||________/   
"
"
call plug#begin()

Plug 'itchyny/lightline.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'sonph/onehalf', { 'rtp': 'vim' }
Plug 'pkradiator/netrw-file-icons'
Plug 'ryanoasis/vim-devicons'
Plug 'tmhedberg/SimpylFold'
Plug 'vim-python/python-syntax'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'vim-scripts/indentpython.vim'
Plug 'nvie/vim-flake8'
Plug 'junegunn/seoul256.vim'
Plug 'sainnhe/everforest'
if has('nvim') || has('patch-8.0.902')
  Plug 'mhinz/vim-signify'
else
  Plug 'mhinz/vim-signify', { 'tag': 'legacy' }
endif
Plug 'ericbn/vim-solarized'
Plug 'tpope/vim-fugitive'
Plug 'wolandark/vim-live-server'
Plug 'vim-scripts/vim-auto-save'
Plug 'digitaltoad/vim-pug' "For .pug files (jade)
Plug 'yuezk/vim-js'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'othree/html5.vim'
Plug 'luochen1990/rainbow'
"Scheme
Plug 'HiPhish/guile.vim'
" The detection works best if it can find the (use-modules) function
" call around the beginning of the file. That won't work for new files.
" As I'm only writing GNU Guile scheme code at this point, I'm alright
" with setting the filetype for all the *.scm files.
autocmd BufRead,BufNewFile *.scm set ft=scheme.guile

" A plugin that sends the current paragraph, or selected area to a
" configured target.
"
" The first time in a session when you'll want to send something to the
" target (via <Ctrl-C> <Ctrl-C>) it will ask you to point out which
" buffer is the vim terminal
Plug 'jpalardy/vim-slime'

" Configure slime to target the built in vim terminal. Only works with
" Vim8+ (any up to date distribution at this point, probably).
let g:slime_target = "vimterminal"

"For Markdown
" If you have nodejs
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && npx --yes yarn install' }
"refresh only after save
let g:mkdp_refresh_slow = 0
"Tabular should come before vim-markdown
Plug 'godlygeek/tabular' 
Plug 'preservim/vim-markdown'
autocmd FileType markdown setlocal foldlevel=99


call plug#end()

filetype plugin indent on    " required
set encoding=utf-8
"Tab should look like 4space and always use spaces
set shiftwidth=4 softtabstop=4 tabstop=4 expandtab
"define leader key
let mapleader = ","

"Start rainbow parantheses
let g:rainbow_active = 0 "set to 0 if you want to enable it later via :RainbowToggle or else 1 always on

"Map escape to get to terminal normal mode 
tnoremap <Esc> <C-\><C-n>

" Just run :GuileTerminal in your vim session to start it and use
command GuileTerminal rightbelow vertical terminal guile

" Load etags file for the builtin GNU Guile modules. Makes it easy to
" use <Ctrl-]> to jump to definitions and <Ctrl-T> to jump back.
"
" To generate the TAGS file I ran make etags within the source directory.
" Not sure if they are generated out of the box for installation using
" distribution packages.
autocmd BufRead,BufNewFile *.scm set tags+=$HOME/.local/binaries/guile-3.0.5/module/TAGS


""  coc complete config {{{ BEGIN

" Having longer updatetime (default is 4000 ms = 4s) leads to noticeable
" delays and poor user experience
set updatetime=300

inoremap <silent><expr> <TAB> coc#pum#visible() ? coc#pum#next(1) : "\<TAB>"
inoremap <silent><expr> <S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<S-TAB>"

" Use K to show documentation in preview window
nnoremap <silent> K :call ShowDocumentation()<CR>

function! ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
endfunction

" GoTo code navigation
nmap <silent> <leader>d <Plug>(coc-definition)
nmap <silent> <leader>t <Plug>(coc-type-definition)
nmap <silent> <leader>i <Plug>(coc-implementation)
nmap <silent> <leader>r <Plug>(coc-references)

" Remap <C-f> and <C-b> to scroll float windows/popups
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

" Use <C-l> for trigger snippet expand.
imap <C-l> <Plug>(coc-snippets-expand)

" Use <C-j> for select text for visual placeholder of snippet.
vmap <C-j> <Plug>(coc-snippets-select)

" Use <C-j> for jump to next placeholder, it's default of coc.nvim
let g:coc_snippet_next = '<c-j>'

" Use <C-k> for jump to previous placeholder, it's default of coc.nvim
let g:coc_snippet_prev = '<c-k>'

" Use <C-j> for both expand and jump (make expand higher priority.)
imap <C-j> <Plug>(coc-snippets-expand-jump)

" Use <leader>x for convert visual selected code to snippet
xmap <leader>x  <Plug>(coc-convert-snippet)

" }}} END coc-config



"Testing for live-server
function! StartmyLiveServer()
    let cmd = "live-server --open=" . bufname() . "&"
    call system(cmd)
    echo "Live server started in the background."
endfunction
command! StartmyLiveServer call StartmyLiveServer()





" for vim-signify
set updatetime=100
""statusline
set laststatus=2
set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P
"set statusline=
"set statusline+=%7*\[%n]                                  "buffernr
"set statusline+=%1*\ %<%F\                                "File+path
"set statusline+=%2*\ %y\                                  "FileType
"set statusline+=%3*\ %{''.(&fenc!=''?&fenc:&enc).''}      "Encoding
"set statusline+=%3*\ %{(&bomb?\",BOM\":\"\")}\            "Encoding2
"set statusline+=%4*\ %{&ff}\                              "FileFormat (dos/unix..)
"set statusline+=%5*\ %{FugitiveStatusline()}
"set statusline+=%8*\ %=\ row:%l/%L\                       "Rownumber/total (%)
"set statusline+=%9*\ col:%03v\                            "Colnr
"set statusline+=%0*\ \ %m%r%w\ %P\ \                      "Modified? Readonly? Top/bot.
""colors for women
"hi User1 ctermfg=black  ctermbg=7
"hi User2 ctermfg=white  ctermbg=darkred
"hi User3 ctermfg=white  ctermbg=black
"hi User4 ctermfg=white  ctermbg=166
"hi User7 ctermfg=white  ctermbg=52 gui=bold
"hi User8 ctermfg=white  ctermbg=grey
"hi User9 ctermfg=white  ctermbg=magenta
"hi User0 ctermfg=yellow  ctermbg=darkgrey


" TODO: python-venv-support
"python with virtualenv support
"py << EOF
"import os
"import sys
"if 'VIRTUAL_ENV' in os.environ:
"	project_base_dir = os.environ['VIRTUAL_ENV']
"	activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
"	execfile(activate_this, dict(__file__=activate_this))
"EOF

"make python code look pretty
let python_highlight_all=1
"for python syntax
let g:python_highlight_all=1

"-------color themes------------
"colo seoul256
"let g:seoul256_background = 234
"solarized
"syntax enable
"set background=dark
"colorscheme solarized

set autoindent
set smartindent
syntax on
set nu rnu
"set ruler
set backup
set swapfile
set backupdir=~/vimtmp//,.
set directory=~/vimtmp//,.
set undodir=~/vimtmp//,.
set undofile

"split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

"Toggle netrw Lexplore
nnoremap <leader>e :Lexplore<CR>
"Use tree listing style for netrw
let g:netrw_liststyle= 3
"width
let g:netrw_winsize=17
" Free <C-l> in Netrw
nmap <leader><leader><leader><leader><leader><leader>x <Plug>NetrwRefresh


"Enable folding
autocmd FileType python setlocal foldmethod=indent foldlevel=99
autocmd FileType python set shiftwidth=2 tabstop=2 expandtab 

"Folding for js,c, c++, 
autocmd FileType javascript,c,cpp,javascriptreact setlocal foldmethod=syntax foldlevel=99
"above requires za to fold and unfold
nnoremap <space> za
"see docstring for folded code
let g:SimpylFold_docstring_preview=1



"flagging unnecessary white space
"au BufRead,BufNewFile *.py,*.pyw,*.c,*.h match BadWhitespace /\s\+$/


"Removes background from signColumn (Gutter)
highlight SignColumn ctermbg=0

colorscheme torte_two
"Match pairs
set mps+=<:>
au FileType c,cpp,java,javascript setlocal mps+==:;


" Use Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
" Avoid side effects when it was already reset.
if &compatible
  set nocompatible
endif

" When the +eval feature is missing, the set command above will be skipped.
" Use a trick to reset compatible only when the +eval feature is missing.
silent! while 0
  set nocompatible
silent! endwhile

" Allow backspacing over everything in insert mode.
set backspace=indent,eol,start

set history=200		" keep 200 lines of command line history
"set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set wildmenu		" display completion matches in a status line

set ttimeout		" time out for key codes
set ttimeoutlen=100	" wait up to 100ms after Esc for special key

" Show @@@ in the last line if it is truncated.
set display=truncate

" Show a few lines of context around the cursor.  Note that this makes the
" text scroll if you mouse-click near the start or end of the window.
set scrolloff=5

" Do incremental searching when it's possible to timeout.
if has('reltime')
  set incsearch
endif

" Do not recognize octal numbers for Ctrl-A and Ctrl-X, most users find it
" confusing.
set nrformats-=octal

" For Win32 GUI: remove 't' flag from 'guioptions': no tearoff menu entries.
if has('win32')
  set guioptions-=t
endif

" Don't use Q for Ex mode, use it for formatting.  Except for Select mode.
" Revert with ":unmap Q".
map Q gq
sunmap Q

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
" Revert with ":iunmap <C-U>".
inoremap <C-U> <C-G>u<C-U>

" In many terminal emulators the mouse works just fine.  By enabling it you
" can position the cursor, Visually select and scroll with the mouse.
" Only xterm can grab the mouse events when using the shift key, for other
" terminals use ":", select text and press Esc.
if has('mouse')
  if &term =~ 'xterm'
    set mouse=a
  else
    set mouse=nvi
  endif
endif

" Only do this part when Vim was compiled with the +eval feature.
if 1

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  " Revert with ":filetype off".
  filetype plugin indent on

  " Put these in an autocmd group, so that you can revert them with:
  " ":augroup vimStartup | exe 'au!' | augroup END"
  augroup vimStartup
    au!

    " When editing a file, always jump to the last known cursor position.
    " Don't do it when the position is invalid, when inside an event handler
    " (happens when dropping a file on gvim) and for a commit message (it's
    " likely a different one than last time).
    autocmd BufReadPost *
      \ if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit'
      \ |   exe "normal! g`\""
      \ | endif

  augroup END
endif

" Switch syntax highlighting on when the terminal has colors or when using the
" GUI (which always has colors).
if &t_Co > 2 || has("gui_running")
  " Revert with ":syntax off".
  syntax on

  " I like highlighting strings inside C comments.
  " Revert with ":unlet c_comment_strings".
  let c_comment_strings=1
endif

" If terminal has true colors support turn it on
if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
" Revert with: ":delcommand DiffOrig".
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif

if has('langmap') && exists('+langremap')
  " Prevent that the langmap option applies to characters that result from a
  " mapping.  If set (default), this may break plugins (but it's backward
  " compatible).
  set nolangremap
endif

"Enable Man Page viewing inside vim using :Man
runtime ftplugin/man.vim

"" Ligtline config
" Show fullpath
let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'readonly', 'absolutepath', 'modified' ] ],
      \ }
      \ }
