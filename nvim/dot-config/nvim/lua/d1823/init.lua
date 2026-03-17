require("d1823.background_sync")

vim.g.mapleader = " "

require("d1823.run_configs")

local float_term = require("d1823.floating_terminal")
vim.keymap.set('n', '<leader>cc', function() float_term.toggle('claude') end, { desc = 'Toggle Claude Code' })

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- TODO: Snippets
-- TODO: Autocompletion
-- TODO: [PHP] `namespace` autocompletion
-- TODO: File browser (maybe https://github.com/pablopunk/native-sidebar.vim/blob/master/plugin/native-sidebar.vim?)
-- TODO: Debugging.

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

local nvimTreeFocusOrToggle = function ()
    local nvimTree = require("nvim-tree.api")
    local currentBuf = vim.api.nvim_get_current_buf()
    local currentBufFt = vim.api.nvim_get_option_value("filetype", { buf = currentBuf })
    if currentBufFt == "NvimTree" then
        nvimTree.tree.toggle({
            find_file = true,
        })
    else
        nvimTree.tree.focus({
            find_file = true,
        })
    end
end

vim.keymap.set('n', '<C-b>', nvimTreeFocusOrToggle)

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

vim.opt.smartindent = true

vim.opt.wrap = false

vim.opt.scrolloff = 8

vim.opt.timeoutlen = 200
vim.opt.updatetime = 50
vim.cmd [[silent colorscheme solarized8]]

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

vim.o.exrc = true

vim.filetype.add({
  pattern = {
    ['.*.j2'] = 'jinja',
  },
})

vim.treesitter.language.register('jinja', 'j2')

require('nvim-treesitter').install({ "go", "php", "javascript", "typescript", "json", "yaml", "lua", "vim", "vimdoc", "ini", "twig", "jinja", "kulala_http" })

vim.api.nvim_create_autocmd('FileType', {
  callback = function(ev)
    local max_filesize = 100 * 1024 -- 100 KB
    local ok, stats = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(ev.buf))
    if ok and stats and stats.size > max_filesize then
      return
    end
    local lang = vim.treesitter.language.get_lang(vim.bo[ev.buf].filetype)
    if lang and pcall(vim.treesitter.language.add, lang) and pcall(vim.treesitter.start, ev.buf) then
      vim.bo[ev.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
    end
  end,
})

vim.lsp.config('intelephense', {
    init_options = {
        licenceKey = os.getenv("HOME") .. "/.config/intelephense/license.txt"
    }
})

vim.lsp.enable('intelephense')
vim.lsp.enable('gopls')
vim.lsp.enable('ts_ls')

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
    if client.supports_method('textDocument/formatting') then
        vim.keymap.set('n', 'ff', '<cmd>lua vim.lsp.buf.format()<CR>')
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


vim.filetype.add({
  extension = {
    ['http'] = 'http',
  },
})

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

require('fzf-lua').register_ui_select()

require("nvim-tree").setup({
    view = {
        width = 60
    },
})

local keywords = {
  TODO  = '#b58900',
  FIXME = '#dc322f',
  NOTE  = '#d33682',
}

local function set_highlights()
  for kw, color in pairs(keywords) do
    vim.api.nvim_set_hl(0, 'Comment' .. kw .. 'KeyType', { fg = color, reverse = true, bold = true })
    vim.api.nvim_set_hl(0, 'Comment' .. kw .. 'KeyContext', { fg = color, reverse = true, underline = true })
    vim.api.nvim_set_hl(0, 'Comment' .. kw .. 'Rest', { fg = color })
  end
end

set_highlights()

vim.api.nvim_create_autocmd('ColorScheme', {
  callback = set_highlights,
})

local ns = vim.api.nvim_create_namespace('comment_keywords')

local function is_comment(buf, row, col)
  local ok, node = pcall(vim.treesitter.get_node, { bufnr = buf, pos = { row, col } })
  if ok and node then
    while node do
      if node:type():match('comment') then
        return true
      end
      node = node:parent()
    end
  end
  return false
end

function highlight_buffer(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_clear_namespace(buf, ns, 0, -1)

  local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
  local i = 1

  while i <= #lines do
    local line = lines[i]

    for kw, _ in pairs(keywords) do
      local prefix, matched_kw = line:match('^(%s*//%s*)(' .. kw .. ')')

      if prefix and matched_kw and is_comment(buf, i - 1, #prefix) then
        local full_prefix = line:match('^(%s*//%s*' .. kw .. '%b()%s*)')
          or line:match('^(%s*//%s*' .. kw .. ':?%s*)')
        local cont_col = full_prefix and #full_prefix or (#prefix + #kw + 1)

        -- comment chars
        vim.api.nvim_buf_set_extmark(buf, ns, i - 1, 0, {
          end_col = #prefix,
          hl_group = 'Comment' .. kw .. 'Rest',
          priority = 200,
        })

        -- check for parens after keyword
        local after_kw = line:sub(#prefix + #kw + 1)
        local paren_open, paren_content, paren_close = after_kw:match('^(%()(.-)(%))')

        if paren_content then
          -- keyword + opening paren: reversed
          vim.api.nvim_buf_set_extmark(buf, ns, i - 1, #prefix, {
            end_col = #prefix + #kw + 1,
            hl_group = 'Comment' .. kw .. 'KeyType',
            priority = 200,
          })
          -- content inside parens: underlined
          vim.api.nvim_buf_set_extmark(buf, ns, i - 1, #prefix + #kw + 1, {
            end_col = #prefix + #kw + 1 + #paren_content,
            hl_group = 'Comment' .. kw .. 'KeyContext',
            priority = 200,
          })
          -- closing paren: reversed
          vim.api.nvim_buf_set_extmark(buf, ns, i - 1, #prefix + #kw + 1 + #paren_content, {
            end_col = #prefix + #kw + 1 + #paren_content + 1,
            hl_group = 'Comment' .. kw .. 'KeyType',
            priority = 200,
          })
          -- rest of line after parens
          vim.api.nvim_buf_set_extmark(buf, ns, i - 1, #prefix + #kw + 1 + #paren_content + 1, {
            end_col = #line,
            hl_group = 'Comment' .. kw .. 'Rest',
            priority = 200,
          })
        else
          -- keyword only, no parens
          vim.api.nvim_buf_set_extmark(buf, ns, i - 1, #prefix, {
            end_col = #prefix + #kw,
            hl_group = 'Comment' .. kw .. 'KeyType',
            priority = 200,
          })
          -- rest of line
          vim.api.nvim_buf_set_extmark(buf, ns, i - 1, #prefix + #kw, {
            end_col = #line,
            hl_group = 'Comment' .. kw .. 'Rest',
            priority = 200,
          })
        end

        -- continuation lines
        local j = i + 1
        while j <= #lines do
          local next_line = lines[j]
          local next_prefix = next_line:match('^(%s*//%s+)')
          if next_prefix and #next_prefix >= cont_col
            and is_comment(buf, j - 1, #next_prefix - 1)
            and not next_line:match('//%s*TODO')
            and not next_line:match('//%s*FIXME')
            and not next_line:match('//%s*NOTE') then
            vim.api.nvim_buf_set_extmark(buf, ns, j - 1, 0, {
              end_col = #next_line,
              hl_group = 'Comment' .. kw .. 'Rest',
              priority = 200,
            })
            j = j + 1
          else
            break
          end
        end

        i = j
        goto continue
      end
    end

    i = i + 1
    ::continue::
  end
end

vim.api.nvim_create_autocmd({ 'BufEnter', 'BufWritePost', 'TextChanged', 'TextChangedI' }, {
  callback = function(ev)
    vim.schedule(function()
      if vim.api.nvim_buf_is_valid(ev.buf) then
        highlight_buffer(ev.buf)
      end
    end)
  end,
})

-- also re-highlight when treesitter finishes parsing
vim.api.nvim_create_autocmd('FileType', {
  callback = function(ev)
    vim.defer_fn(function()
      if vim.api.nvim_buf_is_valid(ev.buf) then
        highlight_buffer(ev.buf)
      end
    end, 100)
  end,
})
