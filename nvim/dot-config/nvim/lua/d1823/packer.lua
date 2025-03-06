vim.cmd.packadd('packer.nvim')

return require('packer').startup(function(use)
    use 'lifepillar/vim-solarized8'
    use 'wbthomason/packer.nvim'
    use 'tpope/vim-eunuch'
    use 'ibhagwan/fzf-lua'
    use 'neovim/nvim-lspconfig'
    use 'mistweaverco/kulala.nvim'
    use({'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'})
end);
