local opt = vim.opt

opt.fillchars:append('diff:╱')

require('diffview').setup {
    file_panel = {
        position = 'bottom',
        height = 3,
        listing_style = 'list',
    }
}
