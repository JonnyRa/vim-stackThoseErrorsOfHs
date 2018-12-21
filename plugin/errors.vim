command! OpenErrors cfile .errors

function! ReadErrors()
  "call the bash script (needs to be on path)
  silent !read-errors
  redraw!
  OpenErrors
  clast
endfunction

command! ReadErrors call ReadErrors()

if !exists('g:stackThoseErrorsCreateMappings')
  let g:stackThoseErrorsCreateMappings = 0
endif

"setup default mappings only if instructed to
if g:stackThoseErrorsCreateMappings
  "mnenomic "read errors"
  nnoremap <leader>re :ReadErrors<cr>
endif
