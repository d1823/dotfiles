vim.cmd.packadd('packer.nvim')

return require('packer').startup(function(use)
    use({'lifepillar/vim-solarized8', branch = 'neovim'})
    use 'wbthomason/packer.nvim'
    use 'tpope/vim-eunuch'
    use 'tpope/vim-sleuth'
    use 'ibhagwan/fzf-lua'
    use 'neovim/nvim-lspconfig'
    use 'mfussenegger/nvim-lint'
    use 'mistweaverco/kulala.nvim'
    use 'nvim-tree/nvim-tree.lua'
    use({'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'})
end);
