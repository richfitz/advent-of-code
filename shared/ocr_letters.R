local({
  ## Letters, via this paste
  ## https://nopaste.ml/#XQAAAQDNDwAAAAAAAAAyGksy5FB9TGMxsNq5JQAuJRjP6PqEkC20GpAhdPJoEZR0E5LAb+k9M87jq3PvxCPxwgfWu8Syv4EqinvCYmWqceTdAc5tvRl3Z8Eh0J/LODHFk5DcX/CWUITUngxpRC3HMk5TGCEUGpYdQBnl2fRnYBQm5OZI0bvs75R8+W1mOfSDwsETt/ZApF+Gp+g+LIJq57uYAq1v4pXu+WvWoKkxzgnpjz+VawPdplIKTgtnzIb+zVjk2ytS8zBL5DIbqwX0t+X1ZtCfrbpVg5W4qx/nI2eKYqWp8KUtg9y0hNHCaplBMYgjGNUxVEkryT8yKuU0SZey2oHGdd34Y9naYnFOCHHh9AF1gitRHOo3KNWq+AVCUMHb3KxnQXVbrSqqgQq51g0kMReKjxnOleFD3+raYZC07KfDwDsZQupPYFp5w6XhnA7IxDcxqeA9qQ7tJqCzJHCb8ntwrxiaJrCRG3aUUk3i7puWboBX2NzifbWTiWgO3lTF5KsxI5MhFLbdk/H2cNYOwGYaSw3a7jXfQVDpNVhLS8qhGdgu5sBAsim9BxCyqcGxgR1KdNH/A0T/J5SePMtbjfgt9N+Wf0/97ePSqbEOs3OpM1MFS0kMcB2/ZLlQ5OyePPQvFgwGK2QwBie42QSYjZN4xqEodKkrInyk0YxAHIPZlf4K3h+Fdn5EnGO53nuv5tCIrOyL/N98JCbRR6hrHUjRU7ag6Jlor3FPWT84WY/VQG6E2bplhjFvwG6q/E5QBN1JX1VrgKnbMlMQOBdjNkCG0xylInnpB7b5GLX2J5AgHxmczzCDmiRw2oGB5PSjv9Cn4ZLA4OpL+xDNbptd5DkJTdV8Gj2D0c/09tDgcnUI7s4Pp8s3/n/pWlg0x02V+NblyzjUOAUigrb5AQAq2zmNxOI0QzEIgRk8EsAQADItDy3/+o3Rffk8Jegibz1jAmAsNvztujtpSSspBeCKFz+jHAhfzkOg32rjapjWaBCxvyPaAwOqnD7UTDa/bes+bcG0yN7d2I5hRntt6VQMGNPnkI4nnq0Dvjj8dUl5Vc+3//mrzuA=local({
  parse <- function(...) {
    x <- c(...)
    idx <- which(matrix(unlist(strsplit(x[-1], NULL)) == "X", 6, byrow = TRUE))
    message(sprintf("%s = %s", x[[1]], as.integer(sum(2^(idx - 1)))))
  }
  parse('A',
      ' XX  ',
      'X  X ',
      'X  X ',
      'XXXX ',
      'X  X ',
      'X  X ')
  parse('B',
      'XXX  ',
      'X  X ',
      'XXX  ',
      'X  X ',
      'X  X ',
      'XXX  ')
  parse('C',
      ' XX  ',
      'X  X ',
      'X    ',
      'X    ',
      'X  X ',
      ' XX  ')
  parse('E',
      'XXXX ',
      'X    ',
      'XXX  ',
      'X    ',
      'X    ',
      'XXXX ')
  parse('F',
      'XXXX ',
      'X    ',
      'XXX  ',
      'X    ',
      'X    ',
      'X    ')
  parse('G',
      ' XX  ',
      'X  X ',
      'X    ',
      'X XX ',
      'X  X ',
      ' XXX ')
  parse('H',
      'X  X ',
      'X  X ',
      'XXXX ',
      'X  X ',
      'X  X ',
      'X  X ')
  parse('I',
      'XXX  ',
      ' X   ',
      ' X   ',
      ' X   ',
      ' X   ',
      'XXX  ')
  parse('J',
      '  XX ',
      '   X ',
      '   X ',
      '   X ',
      'X  X ',
      ' XX  ')
  parse('K',
      'X  X ',
      'X X  ',
      'XX   ',
      'X X  ',
      'X X  ',
      'X  X ')
  parse('L',
      'X    ',
      'X    ',
      'X    ',
      'X    ',
      'X    ',
      'XXXX ')
  parse('O',
      ' XX  ',
      'X  X ',
      'X  X ',
      'X  X ',
      'X  X ',
      ' XX  ')
  parse('P',
      'XXX  ',
      'X  X ',
      'X  X ',
      'XXX  ',
      'X    ',
      'X    ')
  parse('R',
      'XXX  ',
      'X  X ',
      'X  X ',
      'XXX  ',
      'X X  ',
      'X  X ')
  parse('S',
      ' XXX ',
      'X    ',
      'X    ',
      ' XX  ',
      '   X ',
      'XXX  ')
  parse('U',
      'X  X ',
      'X  X ',
      'X  X ',
      'X  X ',
      'X  X ',
      ' XX  ')
  parse('Y',
      'X   X',
      'X   X',
      ' X X ',
      '  X  ',
      '  X  ',
      '  X  ')
  parse('Z',
      'XXXX ',
      '   X ',
      '  X  ',
      ' X   ',
      'X    ',
      'XXXX ')
})
