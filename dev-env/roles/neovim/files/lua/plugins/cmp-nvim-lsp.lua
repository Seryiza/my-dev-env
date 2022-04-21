local cmp_neovim_lsp = require('cmp_nvim_lsp')
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = cmp_neovim_lsp.update_capabilities(capabilities)
