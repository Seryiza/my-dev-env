local packer = require('packer')

local plugin = function (package)
  packer.use(package)

  local package_full_name = package[1]
  local package_short_name = string.lower(string.match(package_full_name, "/(.+)$"))
  local package_short_safe_name = string.gsub(package_short_name, "%.", "-")

  -- Safe require: ignore errors if the module isn't exist
  local config_module = "plugins/" .. package_short_safe_name
  pcall(require, config_module)
end

packer.startup(function()
	plugin {'wbthomason/packer.nvim'}
  plugin {'nvim-lualine/lualine.nvim'}
  plugin {'kyazdani42/nvim-tree.lua'}
  plugin {'nvim-treesitter/nvim-treesitter',
          run = ':TSUpdate'}
  plugin {'nvim-treesitter/nvim-treesitter-textobjects'}
  plugin {'neovim/nvim-lspconfig'}
  plugin {'williamboman/nvim-lsp-installer'}
  plugin {'hrsh7th/nvim-cmp'}
  plugin {'hrsh7th/cmp-nvim-lsp'}
  plugin {'L3MON4D3/LuaSnip'}
  plugin {'saadparwaiz1/cmp_luasnip'}
  plugin {'nvim-treesitter/playground'}
  plugin {'crispgm/nvim-tabline'}
  plugin {'Olical/conjure'}
  plugin {'tpope/vim-dispatch'}
  plugin {'clojure-vim/vim-jack-in'}
  plugin {'steelsojka/pears.nvim'}
  plugin {'lyokha/vim-xkbswitch'}
  plugin {'Th3Whit3Wolf/onebuddy',
          requires = {'tjdevries/colorbuddy.vim'}}
  plugin {'Pocco81/AutoSave.nvim'}
  plugin {'nvim-telescope/telescope.nvim',
          requires = {'nvim-lua/plenary.nvim'}}
  plugin {'wlangstroth/vim-racket'}
  plugin {'mattn/emmet-vim'}
  plugin {'sindrets/diffview.nvim',
          requires = {'nvim-lua/plenary.nvim'}}
end)
