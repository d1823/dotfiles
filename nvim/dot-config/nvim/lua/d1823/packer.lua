vim.cmd.packadd('packer.nvim')

return require('packer').startup(function(use)
    use 'projekt0n/github-nvim-theme'
    use 'd1823/vim-bullet-journal'
    use 'd1823/vim-notepad'
    use 'wbthomason/packer.nvim'
    use({'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'})
    use({'junegunn/fzf'})
    use({'junegunn/fzf.vim'})
end);
