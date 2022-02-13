local lsp_installer = require("nvim-lsp-installer")

-- LSP configs
vim.lsp.handlers['textDocument/publishDiagnostics'] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
    update_in_insert = true,
    signs = false,
})


-- LSP Installer
lsp_installer.on_server_ready(function(server)
    local opts = {
        on_attach = function (client)
            if client.resolved_capabilities.document_highlight then
                vim.cmd [[
                    hi LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow
                    hi LspReferenceText cterm=bold ctermbg=red guibg=LightYellow
                    hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow
                    augroup lsp_document_highlight
                        autocmd! * <buffer>
                        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
                        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
                    augroup END
                ]]
            end
        end
    }
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


