# My Dev Env
My developer environment based on Ansible, Ubuntu.

## Setup
```bash
# install make if you don't have it
sudo apt update && sudo apt install -y make

# install ansible if you don't have it
make install-ansible

# setup dev env with ansible
make setup
```

## Developer environment development
(interesting phrase 🤔)

```bash
vagrant up
vagrant ssh
cd /vagrant
# test changes...
```
