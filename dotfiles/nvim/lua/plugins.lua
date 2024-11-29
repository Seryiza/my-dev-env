local packer = require('packer')
local autocmd = vim.api.nvim_create_autocmd

-- close lsp references buffer after leaving
autocmd({ "BufLeave" }, { pattern = { "*" }, command = "if &buftype == 'quickfix'|q|endif" })

-- update packer when something changes here
--vim.cmd([[
--  augroup packer_user_config
--    autocmd!
--    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
--  augroup end
--]])

packer.startup(function(use)
	use {'wbthomason/packer.nvim'}
  use {'nvim-lualine/lualine.nvim',
       config = function ()
         require('lualine').setup({
           options = {
             component_separators = '',
             section_separators = '',
           },
           sections = {
             lualine_a = {'mode'},
             lualine_b = {},
             lualine_c = {'filename'},
             lualine_x = {},
             lualine_y = {},
             lualine_z = {'location'}
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
               } }
           }
         })
       end}
  use {'nvim-treesitter/nvim-treesitter',
       config = function ()
         require "nvim-treesitter.configs".setup {
           ensure_installed = { "lua", "clojure", "javascript", "vim", "vimdoc", "query", "bash", "gitignore", "java", "nix", "yaml", "json" },
           -- check inc selection
           highlight = {
             enable = true,
             additional_vim_regex_highlighting = false
           },
           --indent = {
           --  enable = true
           --},
         }
       end}
  use {'nvim-treesitter/nvim-treesitter-textobjects',
       after = "nvim-treesitter",
       requires = "nvim-treesitter/nvim-treesitter"}
--[[  use {'williamboman/mason.nvim',
       config = function ()
         require('mason').setup()
       end}
  use {'williamboman/mason-lspconfig.nvim',
       after = "mason.nvim",
       config = function ()
        require('mason-lspconfig').setup()
       end}]]--
  use {'neovim/nvim-lspconfig',
       --after = "mason-lspconfig.nvim",
       config = function ()
         local config = {
           -- Can I drop it? (or move it to project-specific settings)
           --init_options = {},

           --[[vim.diagnostic.config({
             signs = false,
             virtual_text = {
               prefix = '--',
             }
           }),]]--
         }

         require('lspconfig').lua_ls.setup(config)
         require('lspconfig').clojure_lsp.setup(config)
         --require('lspconfig').tsserver.setup(config)
         --require('lspconfig').jsonls.setup(config)
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
             --{name = 'buffer'},
             {name = 'luasnip'},
           },
           mapping = {
             ["<CR>"] = cmp.mapping({
               i = function(fallback)
                 if cmp.visible() and cmp.get_active_entry() then
                   cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false })
                 else
                   fallback()
                 end
               end,
               s = cmp.mapping.confirm({ select = true }),
               c = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
             }),
             ["<Tab>"] = cmp.mapping.select_next_item(),
             ["<S-Tab>"] = cmp.mapping.select_prev_item(),
             ["<q>"] = cmp.mapping.abort(),
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
  --[[use {'ms-jpq/coq_nvim',
       config = function ()
         vim.g.coq_settings = {
           auto_start = true,
           clients = {
             lsp = {
               always_on_top = {},
             },
           },
           keymap = {
             manual_complete = '<c-k>',
           },
           display = {
             icons = {
               mode = 'none',
             },
           },
           limits = {
             completion_auto_timeout = 0.30,
           },
         }
       end}
       ]]--
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
  use {'windwp/nvim-autopairs',
       config = function ()
         require('nvim-autopairs').setup({
           check_ts = true,
           enable_check_bracket_line = false
         })
       end}
  --[[use {'lyokha/vim-xkbswitch',
       config = function ()
         vim.g.XkbSwitchEnabled = 1
         vim.g.XkbSwitchLib = '/usr/local/lib/libg3kbswitch.so'
         vim.g.XkbSwitchNLayout = 'us'
         vim.g.XkbSwitchIMappings = {'ru'}
         vim.g.XkbSwitchAssistNKeymap = 1
       end}
  --]]
  use {'okuuva/auto-save.nvim',
       config = function ()
         require('auto-save').setup({
           enabled = true,
           execution_message = {
             enabled = false
           },
           debounce_delay = 5000,
         })
       end}
  use {'nvim-telescope/telescope.nvim',
       requires = {'nvim-lua/plenary.nvim', 'nvim-telescope/telescope-fzf-native.nvim'},
       config = function ()
         local actions = require('telescope.actions')

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
           pickers = {
             buffers = {
               mappings = {
                 i = {
                   ["<c-d>"] = actions.delete_buffer + actions.move_to_top,
                 },
               },
             },
           },
         })
        require('telescope').load_extension('fzf')
      end
  }
  use {'ThePrimeagen/harpoon',
       requires = {'nvim-lua/plenary.nvim'}}
  use { 'nvim-telescope/telescope-fzf-native.nvim',
        run = 'make' }
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
 -- use {'p00f/alabaster.nvim',
 --      config = function ()
 --        vim.cmd('colorscheme alabaster')
 --      end}
  use {'catppuccin/nvim',
       config = function ()
         require('catppuccin').setup({
           transparent_background = false,
           no_italic = false,
           no_bold = true,
           --no_underline = false,
           integrations = {
             cmp = true,
             nvimtree = true,
             treesitter = true,
             native_lsp = {
               enabled = true
             },
             telescope = {
               enabled = true
             }
           }
         })

         vim.cmd.colorscheme("catppuccin-latte")
       end}
  use {'mcchrish/zenbones.nvim',
       requires = "rktjmp/lush.nvim",
       config = function ()
         --vim.cmd('colorscheme zenbones')
       end}
  use {'tanvirtin/vgit.nvim',
       requires = {'nvim-lua/plenary.nvim'},
       config = function ()
         require('vgit').setup({
           settings = {
             live_blame = {
               enabled = false,
             },
             live_gutter = {
               enabled = false,
             },
             authorship_code_lens = {
               enabled = false,
             },
             scene = {
               keymaps = {
                 quit = 'q',
               }
             },
           },
         })
       end}
  use {'ggandor/leap.nvim',
       requires = {'tpope/vim-repeat'},
       config = function ()
         require('leap').add_default_mappings()

         vim.api.nvim_create_autocmd(
         "User",
         { callback = function()
             vim.cmd.hi("Cursor", "blend=100")
             vim.opt.guicursor:append { "a:Cursor/lCursor" }
           end,
           pattern = "LeapEnter"
         })
         vim.api.nvim_create_autocmd(
         "User",
         { callback = function()
             vim.cmd.hi("Cursor", "blend=0")
             vim.opt.guicursor:remove { "a:Cursor/lCursor" }
           end,
           pattern = "LeapLeave"
         })
       end}
  -- use {'gpanders/editorconfig.nvim'}
--use { '/home/seryiza/code/mkdnflow.nvim',
--  config = function()
--    require('mkdnflow').setup({
--      modules = {
--        conceal = false,
--        bib = false,
--        cursor = false,
--        links = false,
--        paths = false
--      },
--      to_do = {
--        symbols = {' ', 'x'},
--        update_parents = false
--      },
--      mappings = {
--        MkdnNewListItem = {'i', '<CR>'},
--        MkdnToggleToDo = {{'n', 'v'}, '<C-j>'}
--      }
--    })
--  end
--}
  use { 'mickael-menu/zk-nvim',
    config = function()
      require('zk').setup({
        picker = 'telescope'
      })
    end }

  --[[use { 'nfrid/markdown-togglecheck',
    requires = { 'nfrid/treesitter-utils' },
    -- TODO: fix keymapping for markdown files
    --ft = { 'markdown' },
    config = function()
      require('markdown-togglecheck').setup()
    end}]]--

  use { 'chrishrb/gx.nvim',
    config = function()
      require('gx').setup()
    end}

  use { 'bfontaine/zprint.vim',
    config = function ()
      vim.g["zprint#options_map"] = '{:search-config? true}'
    end}
  --]]
end)
