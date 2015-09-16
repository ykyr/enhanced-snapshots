Use autobuild.sh script ONLY to build artefacts for local vagrant test environment.
Usage of this script to provide production builds is not recommended.
Run this script with non-root permissions.
==========
Instruction for manual build.

1) Ensure that you have the proper version of source code to build.
	Optionally you can issue the "git pull" in %{project_dir}
2) Change current directory to %{project_dir}
3) Make autobuild.sh script executable with:
	sudo chmod +x autobuild.sh
4) Run autobuild.sh script:
	./autobuild.sh
