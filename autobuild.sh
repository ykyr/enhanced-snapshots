#! /bin/sh
cd './WebContent/';
sudo npm install -g bower;
bower install;
cd '..';
sudo mvn clean install;
