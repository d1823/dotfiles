local fzf = require('fzf-lua')
local float_term = require('d1823.floating_terminal')

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
          if choice then float_term.run(choice.cmd) end
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
