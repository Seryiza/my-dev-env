local keymap = function (key, cmd, mode, opts)
  mode = mode or 'n'
  opts = opts or {noremap = true, silent = true}
  vim.api.nvim_set_keymap(mode, key, cmd, opts)
end

-- clear search results by ESC
keymap('<ESC>', ':noh<CR>')

-- window management
keymap('<C-h>', '<C-w>h')
keymap('<C-j>', '<C-w>j')
keymap('<C-k>', '<C-w>k')
keymap('<C-l>', '<C-w>l')

-- Files tree
keymap('<leader>ft', ':NvimTreeToggle<CR>')

-- Tabs
keymap('<A-.>', ':tabnext<CR>')
keymap('<A-,>', ':tabprevious<CR>')
keymap('<A-;>', ':tabclose<CR>')

-- Git diff view
keymap('<leader>do', ':DiffviewOpen<CR>')
keymap('<leader>dc', ':DiffviewClose<CR>')

-- Find files using Telescope command-line sugar
keymap('<leader>fg', "<CMD>lua require('telescope.builtin').live_grep({additional_args = function() return { '--fixed-strings' } end, layout_strategy = 'vertical', layout_config = {preview_height = 15, height = 40, preview_cutoff = 1}})<CR>")
keymap('<leader>ff', "<cmd>lua require('telescope.builtin').find_files()<CR>")
keymap('<leader>bb', "<cmd>lua require('telescope.builtin').buffers(require('telescope.themes').get_ivy({previewer = false}))<CR>")
