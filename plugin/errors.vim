command! StackOpenErrors cfile .errors

function! StackReadErrors()
  "call the bash script (needs to be on path)
  silent !read-errors
  redraw!
  silent StackOpenErrors
  clast
endfunction

command! StackReadErrors call StackReadErrors()

if !exists('g:stackThoseErrorsCreateMappings')
  let g:stackThoseErrorsCreateMappings = 0
endif

"setup default mappings only if instructed to
if g:stackThoseErrorsCreateMappings
  "mnenomic "read errors"
  nnoremap <leader>re :StackReadErrors<cr>
endif
