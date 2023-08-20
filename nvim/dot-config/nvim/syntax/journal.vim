if exists("b:current_syntax")
    finish
endif

highlight JournalAll   ctermfg=white guifg=#9ea7b2
highlight JournalDateTime cterm=bold gui=bold guifg=#fe6b6c
highlight JournalTodoSign cterm=bold gui=bold ctermfg=white guifg=white
highlight JournalTodo   ctermfg=white guifg=white
highlight JournalDoneSign cterm=bold gui=bold guifg=#9ea7b2
highlight JournalEventSign cterm=bold gui=bold ctermfg=cyan guifg=#b780fa
highlight JournalEvent  ctermfg=cyan guifg=#b780fa
highlight JournalNoteSign cterm=bold gui=bold ctermfg=yellow guifg=#df9b2f
highlight JournalNote   ctermfg=yellow guifg=#df9b2f
highlight JournalMovedSign cterm=bold gui=bold ctermfg=magenta guifg=#ee6faf
highlight JournalMoved  ctermfg=magenta guifg=#ee6faf

syntax match JournalAll /.*/
syntax match JournalDateTime /^@\d\{2}\/\d\{2}\/\d\{4}/
syntax match JournalDoneSign /^×/ contained
syntax match JournalDone /^×.*/ contains=JournalDoneSign
syntax match JournalTodoSign /^·]/ contained
syntax match JournalTodo /^·.*/ contains=JournalTodoSign
syntax match JournalEventSign /^o/ contained
syntax match JournalEvent /^o.*/ contains=JournalEventSign
syntax match JournalNoteSign /^-/ contained
syntax match JournalNote /^-.*/ contains=JournalNoteSign
syntax match JournalMovedSign /^>/ contained
syntax match JournalMoved /^>.*/ contains=JournalMovedSign

let b:current_syntax="journal"
