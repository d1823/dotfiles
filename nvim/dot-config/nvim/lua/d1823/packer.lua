vim.cmd.packadd('packer.nvim')

return require('packer').startup(function(use)
	use({ 'projekt0n/github-nvim-theme', tag = 'v0.0.7' })
    use 'd1823/vim-notepad'
    use 'wbthomason/packer.nvim'
    use({'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'})
end);
