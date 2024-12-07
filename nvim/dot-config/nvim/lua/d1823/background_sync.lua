local M = { timer_timeout = 0, timer_repeat = 1000, timer = nil }

function M.run()
    if M.timer ~= nil then
        M.timer:close()
    end

    M.timer = vim.loop.new_timer()
    M.timer:start(M.timer_timeout, M.timer_repeat, vim.schedule_wrap(function()
        -- Use the org.freedesktop.appearance.color-scheme setting to figure out
        -- the vim background.
        --
        -- Spec: https://github.com/flatpak/xdg-desktop-portal/blob/main/data/org.freedesktop.impl.portal.Settings.xml#L34
        local bg = vim.fn.system({
            "bash",
            "-c",
            table.concat({
                "dbus-send --session --dest=org.freedesktop.portal.Desktop --print-reply /org/freedesktop/portal/desktop org.freedesktop.portal.Settings.Read string:'org.freedesktop.appearance' string:'color-scheme'",
                "tail -n 1",
                "awk '{print ($4 == 0) ? \"light\" : \"dark\"}'"
            }, "|")
        })
        vim.o.background = bg:gsub("%s+", "")
	vim.cmd [[silent colorscheme solarized8]]
    end))
end

return M
