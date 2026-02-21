local M = {
    timer_timeout = 0, -- milliseconds before the first check
    timer_repeat = 1000, -- milliseconds for repeat interval (e.g., 1000 = 1 second)
    timer = nil,

    -- You can configure the theme names here.
    -- For 'solarized8', it uses the same name, and vim.o.background dictates the variant.
    -- If you used themes with distinct names for dark/light (e.g., 'catppuccin-mocha' and 'catppuccin-latte'),
    -- you would set them accordingly.
    active_colorscheme = "solarized8",
}

-- Function to get the system theme ("dark" or "light")
function M.get_system_theme()
    local current_mode = "light" -- Default to light

    if vim.fn.has("macunix") then
        -- macOS: Check AppleInterfaceStyle
        -- 'defaults read -g AppleInterfaceStyle' outputs "Dark" in dark mode.
        -- In Light Mode, the key might not exist or the command might error.
        -- '2>/dev/null' suppresses stderr to handle cases where the key doesn't exist.
        local command_output = vim.fn.trim(vim.fn.system("defaults read -g AppleInterfaceStyle 2>/dev/null"))
        if command_output == "Dark" then
            current_mode = "dark"
        else
            current_mode = "light" -- Default if not 'Dark' or command failed/key absent
        end
    elseif vim.fn.has("unix") then
        -- Linux/Other Unix: Use dbus-send for XDG Desktop Portals color-scheme
        -- Spec: https://github.com/flatpak/xdg-desktop-portal/blob/main/data/org.freedesktop.impl.portal.Settings.xml#L34
        -- Portal values: 0 for 'no-preference', 1 for 'prefer-dark', 2 for 'prefer-light'.
        local dbus_command = "dbus-send --session --dest=org.freedesktop.portal.Desktop --print-reply /org/freedesktop/portal/desktop org.freedesktop.portal.Settings.Read string:'org.freedesktop.appearance' string:'color-scheme' 2>/dev/null | tail -n1 | awk '{print $NF}'"
        local command_output = vim.fn.trim(vim.fn.system({ "bash", "-c", dbus_command }))

        if command_output ~= "" then
            local portal_value = tonumber(command_output)
            if portal_value == 1 then -- 1 means prefer-dark
                current_mode = "dark"
            elseif portal_value == 2 then -- 2 means prefer-light
                current_mode = "light"
            else -- 0 (no-preference) or any other case (e.g., error, non-numeric output)
                current_mode = "light" -- Default to light for "no preference" or if detection is unclear
            end
        else
            -- dbus command failed or produced no relevant output
            -- This might happen if the desktop environment doesn't support the portal.
            -- print("System theme via D-Bus: Failed to get value, defaulting to light.")
            current_mode = "light"
        end
    else
        -- Fallback for other operating systems (e.g., Windows) or if detection fails.
        -- Users on Windows would need to implement a check (e.g., registry query).
        -- print("System theme: OS not supported for automatic detection or detection failed. Defaulting to light mode.")
        current_mode = "light"
    end

    return current_mode
end

-- Main function to set up and run the theme updater
function M.run()
    -- Stop and clear any existing timer to prevent duplicates if run() is called again
    if M.timer ~= nil then
        M.timer:close()
        M.timer = nil
    end

    M.timer = vim.uv.new_timer()
    if not M.timer then
        vim.notify("Error: Failed to create Neovim timer for theme switching.", vim.log.levels.ERROR)
        return
    end

    M.timer:start(M.timer_timeout, M.timer_repeat, vim.schedule_wrap(function()
        local detected_system_mode = M.get_system_theme()

        -- Only update if Neovim's background setting differs from the detected system mode
        if vim.o.background ~= detected_system_mode then
            vim.o.background = detected_system_mode
            vim.notify("System theme is now '" .. detected_system_mode .. "'. Updating Neovim.", vim.log.levels.INFO)

            -- Re-apply the colorscheme. For themes like 'solarized8',
            -- this allows them to pick up the change in 'vim.o.background'.
            -- The 'silent' keyword prevents error messages if the theme is already set
            -- or if the theme command itself produces output.
            vim.cmd("silent colorscheme " .. M.active_colorscheme)

            -- Optional: Trigger a User event that other plugins or parts of your config can listen to
            -- vim.api.nvim_exec_autocmds("User", { pattern = "SystemThemeChanged", modeline = false })
        end
    end))
end

-- Optional: Function to stop the timer
function M.stop()
    if M.timer ~= nil then
        M.timer:stop() -- Stop the timer from firing again
        M.timer:close() -- Release resources associated with the timer handle
        M.timer = nil
        vim.notify("Neovim theme synchronization timer stopped.", vim.log.levels.INFO)
    end
end

-- Automatically start the theme synchronization when this module is loaded.
-- If you prefer to start it manually (e.g., via a command or keymap),
-- you can comment out the `M.run()` line below.
M.run()

-- Example of how to set up an autocommand to stop the timer when Neovim exits.
-- This is good practice for cleaning up resources.
-- vim.api.nvim_create_autocmd("VimLeavePre", {
--   pattern = "*",
--   callback = function()
--     M.stop()
--   end,
--   desc = "Stop system theme synchronizer on Vim exit",
-- })

return M
