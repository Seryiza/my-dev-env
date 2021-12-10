Vagrant.configure('2') do |config|
    config.vm.box = 'ubuntu/impish64'
    config.vm.hostname = 'testdevenv'
    config.vm.network :private_network, ip: '192.168.33.2'

    config.vm.provider 'virtualbox' do |v|
        v.memory = 4096
        v.cpus = 2
    end
end
