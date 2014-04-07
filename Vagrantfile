# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  # Every Vagrant virtual environment requires a box to build off of.
  config.vm.box = "hashicorp/precise32"

  # config.vm.box_url = "http://domain.com/path/to/above.box"

  # config.vm.network :forwarded_port, guest: 80, host: 8080

  config.vm.provision :shell, :path => "bootstrap.sh"

  config.vm.network :private_network, ip: "192.168.33.10"

  # Create a public network, which generally matched to bridged network.
  # Bridged networks make the machine appear as another physical device on
  # your network.
  # config.vm.network :public_network

  # If true, then any SSH connections made will enable agent forwarding.
  # Default value: false
  # config.ssh.forward_agent = true

  # Share an additional folder to the guest VM. 
  # config.vm.synced_folder "../data", "/vagrant_data"

  config.vm.provider :virtualbox do |vb|
    vb.name = "erlide"
  #   # Don't boot with headless mode
  #   vb.gui = true

    vb.customize ["modifyvm", :id, "--memory", "2048", "--cpus", "3", "--ioapic", "on"]
  end
end
