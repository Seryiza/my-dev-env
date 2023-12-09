local opt = vim.opt
local g = vim.g

opt.termguicolors = true
opt.cursorline = true
opt.number = true
--opt.relativenumber = true
opt.ignorecase = true
opt.wrap = false
opt.background = 'light'
opt.guifont = 'Iosevka:h22'
opt.updatetime = 250
-- opt.incsearch = false
opt.splitbelow = true
opt.splitright = true
opt.expandtab = true
opt.shiftwidth = 2
opt.tabstop = 2
opt.compatible = false
opt.completeopt = 'menu,menuone,noselect'
opt.timeoutlen = 2000
-- opt.showcmd = false
--opt.smartindent = true

-- TODO: fix it
--g.editorconfig = false

-- Remap space as leader key
vim.api.nvim_set_keymap('', '<Space>', '<Nop>', { noremap = true, silent = true })
g.mapleader = ' '
g.maplocalleader = ' '

-- ensure that changes to buffers are saved when you navigate away from that buffer, e.g. by following a link to another file. See :h awa
vim.api.nvim_create_autocmd("FileType", {pattern = "markdown", command = "set awa"})
