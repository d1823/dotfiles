require("d1823.packer")
require("d1823.background_sync"):run()

-- I don't mind the default status bar aside from the lack of padding.
-- Let's use this default status bar approximation with some spaces on each side.
vim.o.statusline = " %<%f %h%m%r%=%-14.(%l,%c%V%) %P "

vim.keymap.set('n', '<ESC><ESC>', ':nohl<ENTER>')

vim.keymap.set('n', '<C-p>', ':Files<CR>')
vim.keymap.set('n', '<C-e>', ':Buffers<CR>')

vim.keymap.set('n', 'j', 'gj')
vim.keymap.set('n', 'k', 'gk')

vim.keymap.set('v', '<', '<gv')
vim.keymap.set('v', '>', '>gv')

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

vim.opt.smartindent = true

vim.opt.wrap = false

vim.opt.hlsearch = true
vim.opt.incsearch = true

vim.opt.scrolloff = 8

vim.opt.updatetime = 50

vim.opt.clipboard = "unnamedplus"

require'nvim-treesitter.configs'.setup {
  ensure_installed = { "go", "php", "javascript", "typescript", "json", "yaml", "lua", "vim", "vimdoc", "ini" },
  auto_install = false,
  highlight = {
    enable = true,

    disable = function(lang, buf)
        local max_filesize = 100 * 1024 -- 100 KB
        local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
        if ok and stats and stats.size > max_filesize then
            return true
        end
    end,

    additional_vim_regex_highlighting = false,
  },
}
