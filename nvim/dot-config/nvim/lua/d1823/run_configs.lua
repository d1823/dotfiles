local fzf = require('fzf-lua')

print("run_configs loaded")

local function run_floating(cmd)
  local width = math.floor(vim.o.columns * .79)
  local height = math.floor(vim.o.lines * .83)
  local col = (vim.o.columns - width) / 2
  local row = (vim.o.lines - height) * .2
  local winopts = {
    relative = 'editor',
    width = width,
    height = height,
    col = col,
    row = row,
    style = 'minimal',
    border = 'rounded',
  }

  local buf = vim.api.nvim_create_buf(false, true)
  local win = vim.api.nvim_open_win(buf, true, winopts)
  vim.api.nvim_set_option_value('winhl', 'Normal:FzfLuaNormal,FloatBorder:FzfLuaBorder,FloatTitle:FzfLuaTitle', { win = win })
  vim.cmd('terminal ' .. cmd)

  local term_buf = vim.api.nvim_get_current_buf()
  local kill = true

  vim.keymap.set({'t', 'n', 'i'}, '<esc>', function()
    kill = false
    vim.cmd('close')
  end, { buffer = term_buf, silent = true })

  vim.api.nvim_create_autocmd('WinClosed', {
    pattern = tostring(win),
    once = true,
    callback = function()
      if kill and vim.api.nvim_buf_is_valid(term_buf) then
        vim.api.nvim_buf_delete(term_buf, { force = true })
      end
    end,
  })
end

local function run_config_picker()
  local configs = vim.g.run_configs

  if not configs or #configs == 0 then
    vim.notify('No run configs for this project', vim.log.levels.WARN)
    return
  end

  fzf.fzf_exec(
    vim.tbl_map(function(c) return c.name end, configs),
    {
      prompt = "Run config> ",
      actions = {
        ['default'] = function(selected)
          local choice = vim.tbl_filter(function(c) return c.name == selected[1] end, configs)[1]
          if choice then run_floating(choice.cmd) end
        end,
        ['ctrl-s'] = function(selected)
          local choice = vim.tbl_filter(function(c) return c.name == selected[1] end, configs)[1]
          if choice then run_config('terminal ' .. choice.cmd) end
        end,
        ['ctrl-v'] = function(selected)
          local choice = vim.tbl_filter(function(c) return c.name == selected[1] end, configs)[1]
          if choice then vim.cmd('vsplit | terminal ' .. choice.cmd) end
        end,
        ['ctrl-t'] = function(selected)
          local choice = vim.tbl_filter(function(c) return c.name == selected[1] end, configs)[1]
          if choice then vim.cmd('tabnew | terminal ' .. choice.cmd) end
        end,
        ['ctrl-x'] = function(selected)
          local choice = vim.tbl_filter(function(c) return c.name == selected[1] end, configs)[1]
          if choice then vim.cmd('split | terminal ' .. choice.cmd) end
        end,
      },
    }
  )
end

vim.keymap.set('n', '<leader>r', run_config_picker, { desc = 'Run configurations' })
