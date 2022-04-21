```
   _______   ________       ______   ________  ________       _______  ________  ________ 
  ╱       ╲╲╱    ╱   ╲    _╱      ╲╲╱        ╲╱    ╱   ╲    ╱╱       ╲╱    ╱   ╲╱    ╱   ╲
 ╱        ╱╱         ╱   ╱        ╱╱         ╱         ╱   ╱╱        ╱         ╱         ╱
╱         ╱╲__      ╱   ╱         ╱        _╱╲        ╱   ╱        _╱         ╱╲        ╱ 
╲__╱__╱__╱   ╲_____╱    ╲________╱╲________╱  ╲______╱    ╲________╱╲__╱_____╱  ╲______╱  
```

# My Dev Env
My developer environment based on Ansible, Ubuntu.

## Setup
```bash
# if you don't have ansible, the script will install it
./sync-dev-env.sh
```

## Dotfiles
[neovim](https://github.com/Seryiza/my-dev-env/tree/master/dev-env/roles/neovim/files)
[tmux](https://github.com/Seryiza/my-dev-env/tree/master/dev-env/roles/tmux/files)

## Developer environment development
(interesting phrase 🤔)

```bash
vagrant up
vagrant ssh
cd /vagrant
# test changes...
```
