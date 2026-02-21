local M = {}

local function open_float(buf)
  local width = math.floor(vim.o.columns * .79)
  local height = math.floor(vim.o.lines * .83)
  local col = (vim.o.columns - width) / 2
  local row = (vim.o.lines - height) * .2
  local win = vim.api.nvim_open_win(buf, true, {
    relative = 'editor',
    width = width,
    height = height,
    col = col,
    row = row,
    style = 'minimal',
    border = 'rounded',
  })
  vim.api.nvim_set_option_value('winhl', 'Normal:FzfLuaNormal,FloatBorder:FzfLuaBorder,FloatTitle:FzfLuaTitle', { win = win })
  return win
end

function M.run(cmd)
  local buf = vim.api.nvim_create_buf(false, true)
  local win = open_float(buf)
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

local toggles = {}

function M.toggle(cmd)
  local state = toggles[cmd]

  if state and vim.api.nvim_buf_is_valid(state.buf) then
    -- Buffer is alive. Is it currently visible in a float?
    if state.win and vim.api.nvim_win_is_valid(state.win) then
      vim.api.nvim_win_close(state.win, false)
      state.win = nil
    else
      state.win = open_float(state.buf)
      vim.cmd('startinsert')
    end
    return
  end

  -- First launch
  local buf = vim.api.nvim_create_buf(false, true)
  local win = open_float(buf)
  vim.cmd('terminal ' .. cmd)
  local term_buf = vim.api.nvim_get_current_buf()

  toggles[cmd] = { buf = term_buf, win = win }

  vim.keymap.set({'t', 'n', 'i'}, '<esc>', function()
    if toggles[cmd] and toggles[cmd].win and vim.api.nvim_win_is_valid(toggles[cmd].win) then
      vim.api.nvim_win_close(toggles[cmd].win, false)
      toggles[cmd].win = nil
    end
  end, { buffer = term_buf, silent = true })
end

return M
