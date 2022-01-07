require('packer').startup(function()
	use 'wbthomason/packer.nvim'
    use {
        'nvim-lualine/lualine.nvim',
        config = function()
            require('lualine').setup({
                options = {
                    component_separators = '',
                    section_separators = '',
                },
            })
        end
    }
    use {
        'kyazdani42/nvim-tree.lua',
        config = function()
            vim.g.nvim_tree_show_icons = {
                git = 0,
                folders = 0,
                files = 0,
                folder_arrows = 0,
            }
            require('nvim-tree').setup()
        end
    }
    use 'nvim-treesitter/nvim-treesitter'
    use 'neovim/nvim-lspconfig'
    use 'williamboman/nvim-lsp-installer'
    use 'hrsh7th/nvim-cmp'
    use 'hrsh7th/cmp-nvim-lsp'
    use 'L3MON4D3/LuaSnip'
    use 'saadparwaiz1/cmp_luasnip'
    --use 'rktjmp/lush.nvim'
    --use '~/code/alabaster-nvim'
    use 'nvim-treesitter/playground'
    use 'romgrk/barbar.nvim'
    use 'Olical/conjure'
    use 'tpope/vim-dispatch'
    use 'clojure-vim/vim-jack-in'
    use 'radenling/vim-dispatch-neovim'
    --use 'kovisoft/paredit'
    use {
        'windwp/nvim-autopairs',
        config = function()
            require('nvim-autopairs').setup{
                enable_check_bracket_line = false,
            }
        end
    }
    use 'lyokha/vim-xkbswitch'
    use 'tjdevries/colorbuddy.vim'
    use {
        'Th3Whit3Wolf/onebuddy',
        config = function ()
            vim.cmd(':colorscheme onebuddy')
        end
    }
    use {
        'Pocco81/AutoSave.nvim',
        config = function ()
            require('autosave').setup({
                enabled = true,
                execution_message = '',
                debounce_delay = 250,
            })
        end
    }
    use {
        'nvim-telescope/telescope.nvim',
        requires = { {'nvim-lua/plenary.nvim'} },
        config = function ()
        end
    }
end)

-- tabline
vim.g.bufferline = {
    icons = false,
}

-- lsp
local lsp_installer = require("nvim-lsp-installer")
lsp_installer.on_server_ready(function(server)
    local opts = {}
    if server.name == "sumneko_lua" then
        -- only apply these settings for the "sumneko_lua" server
        opts.settings = {
            Lua = {
                diagnostics = {
                    -- Get the language server to recognize the 'vim', 'use' global
                    globals = {'vim', 'use'},
                },
                workspace = {
                    -- Make the server aware of Neovim runtime files
                    library = vim.api.nvim_get_runtime_file("", true),
                },
                -- Do not send telemetry data containing a randomized but unique identifier
                telemetry = {
                    enable = false,
                },
            },
        }
    end
    server:setup(opts)
end)

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)
vim.o.completeopt = 'menuone,noselect'

vim.lsp.handlers['textDocument/publishDiagnostics'] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
  update_in_insert = true,
  signs = false,
})

local cmp = require 'cmp'
cmp.setup {
    snippet = {
      expand = function(args)
        require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
      end,
    },
    sources = {
        { name = 'nvim_lsp' },
        { name = 'luasnip' },
        { name = 'path' },
        { name = 'buffer', options = {
            get_bufnrs = function()
                return vim.api.nvim_list_bufs()
            end
        }},
    },
    mapping = {
        ["<Tab>"] = cmp.mapping(function(fallback)
            -- This little snippet will confirm with tab, and if no entry is selected, will confirm the first item
            if cmp.visible() then
                local entry = cmp.get_selected_entry()
	            if not entry then
	                cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
	            end
	            cmp.confirm()
            else
                fallback()
            end
        end, {"i","s","c",}),
    },
}

-- treesitter
require "nvim-treesitter.configs".setup {
    highlight = {
        enable = true,
        custom_captures = {
            ['class_declaration.name'] = "Define",
        }
    },
  playground = {
    enable = true,
    disable = {},
    updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
    persist_queries = false, -- Whether the query persists across vim sessions
    keybindings = {
      toggle_query_editor = 'o',
      toggle_hl_groups = 'i',
      toggle_injected_languages = 't',
      toggle_anonymous_nodes = 'a',
      toggle_language_display = 'I',
      focus_language = 'f',
      unfocus_language = 'F',
      update = 'R',
      goto_node = '<cr>',
      show_help = '?',
    },
  }
}
