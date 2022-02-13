local opt = vim.opt
local capabilities = vim.lsp.protocol.make_client_capabilities()
local cmp = require('cmp')
local cmp_neovim_lsp = require('cmp_nvim_lsp')

opt.completeopt = 'menuone,noselect'
capabilities = cmp_neovim_lsp.update_capabilities(capabilities)

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
