local packer = require('packer')

-- update packer when something changes here
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])

packer.startup(function(use)
	use {'wbthomason/packer.nvim'}
  use {'nvim-lualine/lualine.nvim',
       config = function ()
         require('lualine').setup({
           options = {
             component_separators = '',
             section_separators = '',
           },
         })
       end}
  use {'kyazdani42/nvim-tree.lua',
       config = function ()
         require('nvim-tree').setup({
           view = {
             side = "right",
           },
           renderer = {
             icons = {
               show = {
                 git = false,
                 folder = false,
                 file = false,
                 folder_arrow = false,
               }
             }
           }
         })
       end}
  use {'nvim-treesitter/nvim-treesitter',
       run = ':TSUpdate',
       config = function ()
         require "nvim-treesitter.configs".setup {
           ensure_installed = "all",
           highlight = {
             enable = true,
           },
         }
       end}
  use {'nvim-treesitter/nvim-treesitter-textobjects'}
  use {'williamboman/mason.nvim',
       config = function ()
         require('mason').setup()
       end}
  use {'williamboman/mason-lspconfig.nvim',
       config = function ()
         require('mason-lspconfig').setup()
       end}
  use {'neovim/nvim-lspconfig',
       config = function ()
         local config = {
           init_options = {
             ['source-aliases'] = {":dev", ":test", ":backend-deps"},
           },
           on_attach = function (client, bufnr)
             local bufopts = { noremap=true, silent=true, buffer=bufnr }
             vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
             vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
             vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
             vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
             vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
             vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
             vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
             vim.keymap.set('n', '<space>wl', function()
               print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
             end, bufopts)
             vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
             vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
             vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
             vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
             vim.keymap.set({'n', 'v', 'x'}, '<leader>f', function() vim.lsp.buf.format { async = true } end, bufopts)
           end
         }

         require('lspconfig').sumneko_lua.setup(config)
         require('lspconfig').clojure_lsp.setup(config)
         require('lspconfig').tsserver.setup(config)
         require('lspconfig').jsonls.setup(config)
       end}
  use {'hrsh7th/cmp-buffer'}
  use {'hrsh7th/cmp-path'}
  use {'hrsh7th/nvim-cmp',
       config = function ()
         local cmp = require("cmp")
         cmp.setup({
           sources = {
             {name = 'nvim_lsp'},
             --{name = 'conjure'},
             --{name = 'path'},
             {name = 'buffer'},
             {name = 'luasnip'},
           },
           mapping = {
             ["<CR>"] = cmp.mapping.confirm({select = true}),
             ["<Tab>"] = cmp.mapping.select_next_item(),
           },
           snippet = {
             expand = function (args)
               require('luasnip').lsp_expand(args.body)
             end
           },
         })
       end}
  use {'hrsh7th/cmp-nvim-lsp',
       config = function ()
         local capabilities = require('cmp_nvim_lsp').default_capabilities()
         --require('lspconfig').lua_language_server.setup({capabilities = capabilities})
       end}
  use {'L3MON4D3/LuaSnip'}
  use {'saadparwaiz1/cmp_luasnip'}
  use {'Olical/conjure',
       config = function ()
         vim.g['conjure#log#hud#enabled'] = true
         vim.g['conjure#eval#inline_results'] = true
         vim.g['conjure#log#wrap'] = true
         vim.g['conjure#log#hud#anchor'] = 'SE'
         vim.g['conjure#client#clojure#nrepl#refresh#after'] = 'health-repl/restart'
       end}
  use {'PaterJason/cmp-conjure'}
  use {'tpope/vim-dispatch'}
  use {'clojure-vim/vim-jack-in'}
  use {'steelsojka/pears.nvim',
       config = function ()
         require 'pears'.setup()
       end}
  use {'lyokha/vim-xkbswitch',
       config = function ()
         vim.g.XkbSwitchEnabled = 1
         vim.g.XkbSwitchLib = '/usr/local/lib/libg3kbswitch.so'
         vim.g.XkbSwitchNLayout = 'us'
         vim.g.XkbSwitchIMappings = {'ru'}
         vim.g.XkbSwitchAssistNKeymap = 1
       end}
  use {'Pocco81/auto-save.nvim',
       config = function ()
         require('auto-save').setup({
           enabled = true,
           execution_message = {
             message = function () return "" end,
           },
           debounce_delay = 5000,
         })
       end}
  use {'nvim-telescope/telescope.nvim',
       requires = {'nvim-lua/plenary.nvim'},
       config = function ()
         require('telescope').setup({
           defaults = {
             mappings = {
               i = {
                 ["<C-j>"] = "move_selection_next",
                 ["<C-k>"] = "move_selection_previous",
               },
               n = {
                 ["q"] = "close",
               },
             },
           },
         })
        require('telescope').load_extension('fzf')
      end
  }
  use {'nvim-telescope/telescope-fzf-native.nvim',
       run = 'make'}
  use {'wlangstroth/vim-racket'}
  use {'mattn/emmet-vim'}
  use {'sindrets/diffview.nvim',
       requires = {'nvim-lua/plenary.nvim'},
       config = function ()
         vim.opt.fillchars:append('diff:╱')
         require('diffview').setup {
           file_panel = {
             win_config = {
               position = 'bottom',
               height = 3,
               listing_style = 'list',
             }
           }
         }
       end}
  use {'p00f/alabaster.nvim',
       config = function ()
         --vim.cmd('colorscheme alabaster')
       end}
  use {'mcchrish/zenbones.nvim',
       requires = "rktjmp/lush.nvim",
       config = function ()
         vim.cmd('colorscheme zenbones')
       end}
end)
