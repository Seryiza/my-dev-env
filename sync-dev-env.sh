#!/usr/bin/env bash

# Install Ansible if you don't have it
if !command -v ansible &> /dev/null
then
    sudo apt update
	sudo apt install software-properties-common
	sudo add-apt-repository --yes --update ppa:ansible/ansible
	sudo apt install -y ansible
fi

# Run playbook
ansible-playbook -i hosts setup-dev-env.yml
