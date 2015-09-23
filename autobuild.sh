################### file for EnhancedSnapshots app automation deploynment ###################
#! /bin/sh
cd './WebContent/';

################### bower silent install ###############################################
sudo npm install -g bower --silent;

################### installing dependencies ############################################
bower install --config.interactive=false;

cd '..';

################### building EnhancedSnapshots app ##########################################
sudo mvn clean install;

################### deploying EnhancedSnapshots app to the tomcatserver #####################
sudo service tomcat8 stop
sudo rm -rf /opt/tomcat-latest/webapps/*
sudo cp target/enhancedsnapshots-*.war /opt/tomcat-latest/webapps/ROOT.war
sudo service tomcat8 start
