# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|

bootstrap_script = <<SCRIPT
  if [ ! -f /usr/bin/ansible ]; then
    yum install -y http://dl.fedoraproject.org/pub/epel/6/x86_64/epel-release-6-8.noarch.rpm
    yum install -y libselinux-python
    yum -y install ansible
  fi
  ANSIBLE_PLAYBOOK=ansible/playbook.yml
  ANSIBLE_HOSTS=ansible/inventory
  TEMP_HOSTS="/tmp/ansible_hosts"

  if [ ! -f /vagrant/$ANSIBLE_PLAYBOOK ]; then
    echo "Cannot find Ansible playbook."
    exit 1
  fi

  if [ ! -f /vagrant/$ANSIBLE_HOSTS ]; then
    echo "Cannot find Ansible hosts."
    exit 2
  fi

  cp /vagrant/${ANSIBLE_HOSTS} ${TEMP_HOSTS} && chmod -x ${TEMP_HOSTS}
  echo "Running Ansible provisioner defined in Vagrantfile."
  ansible-playbook /vagrant/${ANSIBLE_PLAYBOOK} --inventory-file=${TEMP_HOSTS} --connection=local
  rm ${TEMP_HOSTS}
SCRIPT

config.vm.box = "chef/centos-6.5"
config.vm.provider "virtualbox" do |v, override|
    v.customize ["modifyvm", :id, "--memory", 2*1024]
    override.vm.network :private_network, ip: "10.10.10.10"
end

config.vm.provision "shell", inline: bootstrap_script

end
