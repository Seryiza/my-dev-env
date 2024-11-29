local keymap = function (key, cmd, mode, opts)
  mode = mode or 'n'
  opts = opts or {noremap = true, silent = true}
  vim.keymap.set(mode, key, cmd, opts)
end

-- clear search results by ESC
keymap('<ESC>', ':noh<CR>')

-- window management
keymap('<C-h>', '<C-w>h')
keymap('<C-j>', '<C-w>j')
keymap('<C-k>', '<C-w>k')
keymap('<C-l>', '<C-w>l')

-- indent
-- keymap('<Tab>', '>>', {'n', 'v'})
-- keymap('<S-Tab>', '<<', {'n', 'v'})

-- Files tree
keymap('<leader>ft', ':NvimTreeFindFileToggle<CR>')

-- Tabs
keymap('<A-.>', ':tabnext<CR>')
keymap('<A-,>', ':tabprevious<CR>')
keymap('<A-;>', ':tabclose<CR>')

-- Git diff view
keymap('<leader>do', ':DiffviewOpen<CR>')
keymap('<leader>dc', ':DiffviewClose<CR>')

-- VGit
keymap('<leader>gb', "<cmd>lua require('vgit').buffer_blame_preview()<CR>")

-- Find files using Telescope command-line sugar
keymap('<leader>fg', "<CMD>lua require('telescope.builtin').live_grep({additional_args = function() return { '--fixed-strings' } end, layout_strategy = 'vertical', layout_config = {preview_height = 10, height = 40, preview_cutoff = 1}})<CR>")
keymap('<leader>ff', "<cmd>lua require('telescope.builtin').find_files()<CR>")
keymap('<leader>bb', "<cmd>lua require('telescope.builtin').buffers(require('telescope.themes').get_ivy({previewer = false, sort_mru = true, layout_config = {height = 20}}))<CR>")
keymap('<leader>fl', "<cmd>lua require('telescope.builtin').resume()<CR>")

-- Harpoon
keymap('<leader>hh', "<cmd>lua require('harpoon.ui').toggle_quick_menu()<CR>")
keymap('<leader>hm', "<cmd>lua require('harpoon.mark').add_file()<CR>")

-- LSP
keymap('gD', vim.lsp.buf.declaration)
keymap('gd', vim.lsp.buf.definition)
keymap('K', vim.lsp.buf.hover)
keymap('gi', vim.lsp.buf.implementation)
keymap('<C-k>', vim.lsp.buf.signature_help)
keymap('<space>wa', vim.lsp.buf.add_workspace_folder)
keymap('<space>wr', vim.lsp.buf.remove_workspace_folder)
keymap('<space>wl', function()
  print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
end)
keymap('<space>D', vim.lsp.buf.type_definition)
keymap('<space>rn', vim.lsp.buf.rename)
keymap('<space>ca', vim.lsp.buf.code_action)
keymap('gr', vim.lsp.buf.references)
keymap('<leader>f', function() vim.lsp.buf.format { async = true } end, {'n', 'v', 'x'})

-- Markdown
-- keymap('<C-n>', require('markdown-togglecheck').toggle)

