local map = vim.api.nvim_set_keymap
local default_opts = {noremap = true, silent = true}

map('n', '<C-n>', ':NvimTreeToggle<CR>', default_opts)
map('n', '<A-,>', ':BufferPrevious<CR>', default_opts)
map('n', '<A-.>', ':BufferNext<CR>', default_opts)
map('n', '<A-;>', ':BufferClose<CR>', default_opts)

map('n', '<leader>do', ':DiffviewOpen<CR>', default_opts)
map('n', '<leader>dc', ':DiffviewClose<CR>', default_opts)

-- Find files using Telescope command-line sugar.
map('n', '<leader>fg', "<CMD>lua require('telescope.builtin').live_grep({additional_args = function() return { '--fixed-strings' } end})<CR>", default_opts)
