vim.opt.termguicolors = true
vim.opt.cursorline = true
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.ignorecase = true
vim.opt.wrap = false
-- vim.cmd(':colorscheme alabaster-nvim')
vim.o.background = 'light'
vim.opt.guifont = 'Iosevka:h22'

vim.opt.expandtab = true
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4

--Remap space as leader key
vim.api.nvim_set_keymap('', '<Space>', '<Nop>', { noremap = true, silent = true })
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

vim.g.XkbSwitchEnabled = 1
vim.g.XkbSwitchLib = '/usr/local/lib/libg3kbswitch.so'
vim.g.XkbSwitchNLayout = 'us'
vim.g.XkbSwitchIMappings = {'ru'}
vim.g.XkbSwitchAssistNKeymap = 1
vim.g.XkbSwitchAssistSKeymap = 1

vim.g['conjure#log#hud#enabled'] = true
vim.g['conjure#eval#inline_results'] = true
vim.g['conjure#log#wrap`'] = true

-- vim.cmd(':set langmap=ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz')

require('plugins')
require('keymappings')
