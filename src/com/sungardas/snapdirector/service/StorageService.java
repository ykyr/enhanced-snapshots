package com.sungardas.snapdirector.service;

import com.amazonaws.services.ec2.model.Volume;

import java.io.IOException;

public interface StorageService {

    void deleteFile(String fileName);

    long getSize(String filename);

    long getBackupCreationTime(String filename);

    String detectFsDevName(Volume volume);

    void javaBinaryCopy(String source, String destination) throws IOException, InterruptedException;

}
