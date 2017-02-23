"let's not be in vi mode
set nocompatible

"""Vundle"""
"load vundle
" filetype off
" set rtp+=~/.vim/bundle/Vundle.vim
" call vundle#rc()
"let Vundle manage Vundle
" Bundle 'gmarik/Vundle.vim'
"My bundles here:
" Bundle 'kien/ctrlp.vim'
" Bundle 'scrooloose/syntastic'
" Bundle 'scrooloose/nerdtree'
" Bundle 'vim-scripts/HJKL'
" Bundle 'Lokaltog/vim-powerline'
" Bundle 'plasticboy/vim-markdown'
"Bundle 'tomtom/quickfixsigns_vim'
" Bundle 'tpope/vim-surround'
" Bundle 'mattn/emmet-vim'
" Bundle 'tomtom/tcomment_vim'
" Bundle 'Townk/vim-autoclose'

"now we can turn filetype back on
" filetype plugin indent on

"""ctlp"""
" let g:ctrlp_map = '<c-0>'
" let g:ctrlp_cmd = 'ctrlp'
"""Copypasta"""
" toggle annoying clipboard behavior
set clipboard=unnamed
set pastetoggle=<F5>
" correct shift tab behavior
" this seems a problem on the debian servers via ssh from my osx box.
" exe 'set t_kB=' . nr2char(27) . '[Z'

"""Highlighting
"we like syntax highlighting
" syntax on

"""File behavior"""
" Set to auto read when a file is changed from the outside
set autoread

"""Mouse"""
set term=xterm
" set mouse=i

"""Search
" Ignore case when searching
set ignorecase
" When searching try to be smart about cases
set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

"""Colors"""
" colorscheme desert
set background=dark

"""Tabs"""
set expandtab
set smarttab
set shiftwidth=4
" remap tab and shift+tab for indenting
nnoremap <Tab> >>_
nnoremap <S-Tab> <<_
inoremap <S-Tab> <C-D>
vnoremap <Tab> >gv
vnoremap <S-Tab> <gv

"""Splits"""
set splitbelow
set splitright
"map <C-\> :sp<CR>
map <C-\> :vsp<CR>

"""Screen info"""
set ruler       " show line and column number
set showcmd     " show (partial) command in status line

"Enable omnicomplete
set ofu=syntaxcomplete#Complete
imap <s-space> <c-x><c-o>

"""Buffers switching"""
nmap <silent> <C-n> :bn<CR>
nmap <silent> <C-p> :bp<CR>
set hidden
set confirm
"""Motion"""
nmap <pageup> 20k
nmap <pagedown> 20j

"why not use semicolon
map ; :
noremap ;; ;

"get here quick
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

"""Highlight extra whitespace"""
":highlight ExtraWhitespace ctermbg=red guibg=red
":match ExtraWhitespace /\s\+$/
set listchars=tab:>-,trail:·
set list

"Syntastic
"let g:syntastic_python_checkers=['flake8']
"let g:syntastic_javascript_checkers=['jslint']

"""Powerline"""
set laststatus=2   " Always show the statusline
set encoding=utf-8 " Necessary to show Unicode glyphs
set t_Co=256 " Explicitly tell Vim that the terminal supports 256 colors
" let g:Powerline_symbols = 'fancy'

"""Line numbers"""
"set number

"filetype things
" au BufNewFile,BufRead *.pt set filetype=xml
" au BufReadPost *.rkt,*.rktl set filetype=scheme

" syntastic options
" let g:syntastic_check_on_open=1
" let g:syntastic_error_symbol='✗'
" let g:syntastic_enable_highlighting=0
