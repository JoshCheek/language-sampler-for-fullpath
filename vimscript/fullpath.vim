function! <SID>ConvertCurrentLineToFullpath()
  let dirname  = getcwd()
  let basename = getline(".")
  let fullpath = dirname."/".basename
  call setline(".", fullpath)
  " :setline(".", expand('%:p'))
  " :.!ruby -ne 'p $_'

  " call setline(".", getline(".") . getline("."))
  " call setline(".", getline(".") . getline("."))
endfunction

" :%call setline(".", getline(".") . getline("."))
:%call <SID>ConvertCurrentLineToFullpath()
" call ConvertCurrentLineToFullpath()

" while (line(".") <= line("$"))
"   :norm yyp
"   +1
" endwhile
:%print
