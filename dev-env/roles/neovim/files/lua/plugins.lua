require('packer').startup(function()
	use 'wbthomason/packer.nvim'
    use 'nvim-lualine/lualine.nvim'
    use 'kyazdani42/nvim-tree.lua'
    use 'nvim-treesitter/nvim-treesitter'
    use 'neovim/nvim-lspconfig'
    use 'williamboman/nvim-lsp-installer'
    use 'hrsh7th/nvim-cmp'
    use 'hrsh7th/cmp-nvim-lsp'
    use 'L3MON4D3/LuaSnip'
    use 'saadparwaiz1/cmp_luasnip'
    use 'nvim-treesitter/playground'
    use 'romgrk/barbar.nvim'
    use 'Olical/conjure'
    use 'tpope/vim-dispatch'
    use 'clojure-vim/vim-jack-in'
    use 'steelsojka/pears.nvim'
    use 'lyokha/vim-xkbswitch'
    use 'tjdevries/colorbuddy.vim'
    use 'Th3Whit3Wolf/onebuddy'
    use 'Pocco81/AutoSave.nvim'
    use {
        'nvim-telescope/telescope.nvim',
        requires = {'nvim-lua/plenary.nvim'},
    }
    use 'wlangstroth/vim-racket'
    use 'mattn/emmet-vim'
    use {
        'sindrets/diffview.nvim',
        requires = {'nvim-lua/plenary.nvim'},
    }
end)

require('plugin-configs.autosave')
require('plugin-configs.bufferline')
require('plugin-configs.cmp')
require('plugin-configs.conjure')
require('plugin-configs.diffview')
require('plugin-configs.lsp')
require('plugin-configs.lualine')
require('plugin-configs.nvim-tree')
require('plugin-configs.pears')
require('plugin-configs.onebuddy')
require('plugin-configs.treesitter')
require('plugin-configs.xkb-switch')
