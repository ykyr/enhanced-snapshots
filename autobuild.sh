################### file for snapdirector app automation deploynment ###################
#! /bin/sh
cd './WebContent/';

################### bower silent install ###############################################
sudo npm install -g bower --silent;

################### installing dependencies ############################################
bower install --config.interactive=false;

cd '..';

################### building snapdirector app ##########################################
sudo mvn clean install;

################### deploying snapdirector app to the tomcatserver #####################
sudo rm -rf /opt/tomcat-latest/webapps/*
sudo cp target/snapdirector*.war /opt/tomcat-latest/webapps/ROOT.war
