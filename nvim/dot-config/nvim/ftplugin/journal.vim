let s:state_signs = [ ['todo', '·'], ['done', '×'], ['event', 'o'], ['note', '-'], ['moved', '>'] ]

function RotateLeft()
    let save = winsaveview()
    let line = getline('.')

    let s = 0
    while s < len(s:state_signs)
        let sign = s:state_signs[s]
        if line =~ '^' . sign[1]
            break
        endif
        let s += 1
    endwhile

    let ps = s - 1
    if ps < 0
        let ps = len(s:state_signs) - 1
    endif

    call setline('.', substitute(line, '^' . s:state_signs[s][1], s:state_signs[ps][1], ""))
    call winrestview(save)
endfunction

function RotateRight()
    let save = winsaveview()
    let line = getline('.')

    let s = 0
    while s < len(s:state_signs)
        let sign = s:state_signs[s]
        if line =~ '^' . sign[1]
            break
        endif
        let s += 1
    endwhile

    let ps = s + 1
    if ps > len(s:state_signs) - 1
        let ps = 0
    endif

    call setline('.', substitute(line, '^' . s:state_signs[s][1], s:state_signs[ps][1], ""))
    call winrestview(save)
endfunction

" NOTE: The expression used in here prevents the abbreviation from expanding
" anywhere else than the beginning of the line.
let i = 0
while i < len(s:state_signs)
    exe $"iabbrev <expr><buffer> {s:state_signs[i][0]} (getline('.') =~ '^{s:state_signs[i][0]}') ? '{s:state_signs[i][1]}' : '{s:state_signs[i][0]}'"
    let i += 1
endwhile

" NOTE: Inserts the current date at the top of the file.
nnoremap <silent><buffer> <C-i> <ESC>ggO<ESC>O<ESC>:pu!=strftime('@%d/%m/%Y')<CR>jo
nnoremap <silent><buffer> <S-h> :call RotateLeft()<CR>
nnoremap <silent><buffer> <S-l> :call RotateRight()<CR>
vnoremap <silent><buffer> <S-h> :call RotateLeft()<CR>gv
vnoremap <silent><buffer> <S-l> :call RotateRight()<CR>gv

" NOTE: All item types are ordered using natural sort when formatting. This
" allows to store them in the following order: event, note, moved, todo, done.
" Honestly, the order isn't as important as the fact that it's consistent.
setl formatprg=sort\ -V
