#!/usr/bin/env bash

cd `dirname "$0"`

# Install Ansible if you don't have it
if !command -v ansible &> /dev/null
then
    sudo apt update
    sudo apt install software-properties-common
    sudo add-apt-repository --yes --update ppa:ansible/ansible
    sudo apt install -y ansible
fi

if [ -f ./passwords.yml ] && [ -f ./vault-password ]
then
    # Run playbook with passwords from vault
    ansible-playbook -i hosts -e @passwords.yml --vault-password-file vault-password dev-env/playbook.yml
else
    # Run playbook with password from prompt
    ansible-playbook -i hosts --ask-become-pass dev-env/playbook.yml
fi

