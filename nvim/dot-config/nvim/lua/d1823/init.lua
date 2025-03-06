require("d1823.packer")

-- TODO: Snippets
-- TODO: Autocompletion
-- TODO: [PHP] `namespace` autocompletion
-- TODO: File browser (maybe https://github.com/pablopunk/native-sidebar.vim/blob/master/plugin/native-sidebar.vim?)

-- I don't mind the default status bar aside from the lack of padding.
-- Let's use this default status bar approximation with some spaces on each side.
vim.o.statusline = " %<%f %h%m%r%=%-14.(%l,%c%V%) %P "

vim.keymap.set('n', 'U', ':redo<CR>')
vim.keymap.set('n', '<ESC><ESC>', ':nohl<ENTER>')

vim.keymap.set('n', '<C-p>', ':FzfLua files<CR>')
vim.keymap.set('n', '<C-e>', ':FzfLua buffers<CR>')
vim.keymap.set('n', '<C-f>', ':FzfLua lsp_document_symbols<CR>')
vim.keymap.set('n', '<C-g>', ':FzfLua lsp_live_workspace_symbols<CR>')
vim.keymap.set('n', '<C-s>', ':FzfLua live_grep_native<CR>')
vim.keymap.set('v', '<C-s>', ':FzfLua grep_visual<CR>')

vim.keymap.set('n', 'j', 'gj')
vim.keymap.set('n', 'k', 'gk')

vim.keymap.set('v', '<', '<gv')
vim.keymap.set('v', '>', '>gv')

vim.keymap.set({ 'i', 's' }, '<Tab>', function()
   if vim.snippet.active({ direction = 1 }) then
     return '<cmd>lua vim.snippet.jump(1)<cr>'
   else
     return '<Tab>'
   end
 end, { expr = true })

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

vim.opt.autoindent = true
vim.opt.smartindent = true

vim.opt.wrap = false

vim.opt.hlsearch = true
vim.opt.incsearch = true

vim.opt.scrolloff = 8

vim.opt.updatetime = 50

vim.opt.termguicolors = true

vim.o.completeopt = "menu,menuone,popup,fuzzy"

vim.opt.list = true
vim.opt.listchars = {
    tab = "▏ ",
    trail = "·",
    extends = "»",
    precedes = "«",
}

vim.opt.clipboard = "unnamedplus"

vim.opt.cursorline = true
vim.opt.number = true
vim.opt.signcolumn = "yes"

vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.g.netrw_keepdir = 0

require'nvim-treesitter.configs'.setup {
  ensure_installed = { "go", "php", "javascript", "typescript", "json", "yaml", "lua", "vim", "vimdoc", "ini" },
  auto_install = true,
  indent = {
      enable = true
  },
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

require'lspconfig'.intelephense.setup{
    on_attach = on_attach,
    init_options = {
        licenceKey = os.getenv("HOME") .. "/.config/intelephense/license.txt"
    }
}

require'lspconfig'.gopls.setup({
    on_attach = on_attach,
})

vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)

    if client.supports_method('textDocument/codeAction') then
        vim.keymap.set('n', 'fa', '<cmd>lua vim.lsp.buf.code_action()<CR>')
    end
    if client.supports_method('textDocument/rename') then
        vim.keymap.set('n', 'fr', '<cmd>lua vim.lsp.buf.rename()<CR>')
    end
    if client.supports_method('textDocument/implementation') then
        vim.keymap.set('n', 'gi', '<cmd>lua require("fzf-lua").lsp_implementations()<CR>')
    end
    if client.supports_method('textDocument/definition') then
        vim.keymap.set('n', 'gd', '<cmd>lua require("fzf-lua").lsp_definitions()<CR>')
    end
    if client.supports_method('textDocument/declaration') then
        vim.keymap.set('n', 'gD', '<cmd>lua require("fzf-lua").lsp_declarations()<CR>')
    end
    if client.supports_method('textDocument/signatureHelp') then
        vim.keymap.set('n', 'gs', '<cmd>lua vim.lsp.buf.signature_help()<CR>')
    end
    if client.supports_method('textDocument/references') then
        vim.keymap.set('n', 'gr', '<cmd>lua require("fzf-lua").lsp_references()<CR>')
    end
    if client.supports_method('textDocument/hover') then
        vim.keymap.set('n', 'gh', '<cmd>lua vim.lsp.buf.hover()<CR>')
    end

    vim.diagnostic.config({underline=true, virtual_text=true, update_in_insert=true})
    vim.lsp.completion.enable(true, args.data.client_id, 0)
  end,
})

local my = {
    snippet = {}
}

function my.snippet.add(trigger, body, opts)
    vim.keymap.set("ia", trigger, function()
        -- If abbrev is expanded with keys like "(", ")", "<cr>", "<space>",
        -- don't expand the snippet. Only accept "<c-]>" as trigger key.
        local c = vim.fn.nr2char(vim.fn.getchar(0))
        if c ~= "" then
            vim.api.nvim_feedkeys(trigger .. c, "i", true)
            return
        end
        vim.snippet.expand(body)
    end, opts)
end

my.snippet.add(
    "privf", 
    table.concat({
        "private function ${1}(${2}): ${3}",
        "{",
        "    ${4}",
        "}"
    }, "\n")
)

my.snippet.add(
    "privsf", 
    table.concat({
        "private static function ${1}(${2}): ${3}",
        "{",
        "    ${4}",
        "}"
    }, "\n")
)

my.snippet.add(
    "prof", 
    table.concat({
        "protected function ${1}(${2}): ${3}",
        "{",
        "    ${4}",
        "}"
    }, "\n")
)

my.snippet.add(
    "prosf", 
    table.concat({
        "protected static function ${1}(${2}): ${3}",
        "{",
        "    ${4}",
        "}"
    }, "\n")
)

my.snippet.add(
    "pubf", 
    table.concat({
        "public function ${1}(${2}): ${3}",
        "{",
        "    ${4}",
        "}"
    }, "\n")
)

my.snippet.add(
    "pubsf", 
    table.concat({
        "public static function ${1}(${2}): ${3}",
        "{",
        "    ${4}",
        "}"
    }, "\n")
)

my.snippet.add(
    "con", 
    table.concat({
        "public function __construct(${1})",
        "{",
        "    ${2}",
        "}"
    }, "\n")
)

my.snippet.add(
    "pcon", 
    "parent::__construct(${1});"
)

require('kulala').setup({
    additional_curl_options = { "--insecure" },
})

require('fzf-lua').setup({
    'telescope',
    winopts = {
        backdrop = 100, -- Disable dark background
    },
    fzf_colors = true
})

require("d1823.background_sync").run()
