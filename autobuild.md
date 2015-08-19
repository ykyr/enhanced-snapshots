Use autobuild.sh script ONLY to build artefacts for local vagrant test environment.
Usage of this script to provide production builds is not recommended.
Run this script with non-root permitions. 
==========
Instruction for manual build.

0) For any build (manual or script-automated) you have to install following software and properly configure its env vars:
	- Node.js
	- OpenJDK 7
	- Apache Maven
	- Git
1) Ensure that you have the proper version of source code to build. 
	Optionally you can issue the "git pull" in %{project_dir}
2) Change current directory to %{project_dir}/WebContent
3) Provide following commands to update JS libs:
	- sudo npm install -g bower;
	- bower install;
4) Change current directory to %{project_dir}
5) Provide following command to start Apache Maven's build process
	- sudo mvn clean install;
	Please note that first start of build process or every start after you make any changes to Maven configuration may take up to 60 minutes to download all Maven dependencies for build.
	