vim.cmd.packadd('packer.nvim')

return require('packer').startup(function(use)
    --use 'd1823/vim-bullet-journal'
    use 'lifepillar/vim-solarized8'
    --use 'd1823/vim-notepad'
    use 'wbthomason/packer.nvim'
    use 'tpope/vim-eunuch'
    use 'junegunn/fzf'
    use 'junegunn/fzf.vim'
    use 'neovim/nvim-lspconfig'
    use({'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'})
end);
