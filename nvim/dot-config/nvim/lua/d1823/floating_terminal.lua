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

local running = {}

local function launch(cmd)
  local buf = vim.api.nvim_create_buf(false, true)
  local win = open_float(buf)
  vim.cmd('terminal ' .. cmd)
  vim.cmd('startinsert')

  local term_buf = vim.api.nvim_get_current_buf()
  local kill = true

  running[cmd] = term_buf

  vim.keymap.set({'t', 'n', 'i'}, '<C-w>q', function()
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

  vim.api.nvim_create_autocmd('BufDelete', {
    buffer = term_buf,
    once = true,
    callback = function()
      if running[cmd] == term_buf then
        running[cmd] = nil
      end
    end,
  })
end

function M.run(cmd)
  local existing = running[cmd]
  if existing and vim.api.nvim_buf_is_valid(existing) then
    vim.ui.select({ 'New instance', 'Replace existing', 'Cancel' }, { prompt = 'Already running: ' .. cmd }, function(choice)
      if choice == 'New instance' then
        launch(cmd)
      elseif choice == 'Replace existing' then
        vim.api.nvim_buf_delete(existing, { force = true })
        running[cmd] = nil
        launch(cmd)
      end
    end)
    return
  end
  launch(cmd)
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
  vim.cmd('startinsert')
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
