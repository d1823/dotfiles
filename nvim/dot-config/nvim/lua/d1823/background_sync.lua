local M = { timer_timeout = 0, timer_repeat = 1000, timer = nil }

function M.run()
    if M.timer ~= nil then
        M.timer:close()
    end

    M.timer = vim.loop.new_timer()
    M.timer:start(M.timer_timeout, M.timer_repeat, vim.schedule_wrap(function()
        -- Use the GNOME specific org.freedesktop.appearance color-scheme setting to
        -- figure out the vim background through the system-wide color-scheme
        -- preference.
        local bg = vim.fn.system({
            "bash",
            "-c",
            "gsettings get org.gnome.desktop.interface color-scheme | grep -i prefer-dark > /dev/null && echo dark || echo light"
        })
        vim.o.background = bg:gsub("%s+", "")
	vim.cmd [[silent colorscheme solarized8]]
    end))
end

return M
