local keymap = function (key, cmd, mode, opts)
  mode = mode or 'n'
  opts = opts or {noremap = true, silent = true}
  vim.api.nvim_set_keymap(mode, key, cmd, opts)
end

-- Files tree
keymap('<C-n>', ':NvimTreeToggle<CR>')

-- Tabs
keymap('<A-.>', ':tabnext<CR>')
keymap('<A-,>', ':tabprevious<CR>')
keymap('<A-;>', ':tabclose<CR>')

-- Git diff view
keymap('<leader>do', ':DiffviewOpen<CR>')
keymap('<leader>dc', ':DiffviewClose<CR>')

-- Find files using Telescope command-line sugar
keymap('<leader>fg', "<CMD>lua require('telescope.builtin').live_grep({additional_args = function() return { '--fixed-strings' } end})<CR>")
keymap('<leader>ff', "<cmd>lua require('telescope.builtin').find_files()<CR>")
