package com.sungardas.snapdirector.service;

import java.io.File;
import java.io.IOException;

import com.amazonaws.services.ec2.model.Volume;

public interface StorageService {

    void deleteFile(String fileName);

	void copyFile(String sourceFileName, String destFileName) throws IOException;

	void copyFile(File sourceFile, File destFile) throws IOException;

	long getSize(String filename);

	long getBackupCreationTime(String filename);

	String detectFsDevName(Volume volume);

	int binaryCopy(String source, String destination) throws IOException, InterruptedException;
}
