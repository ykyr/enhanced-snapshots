package com.sungardas.snapdirector.service.impl;

import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.snapdirector.exception.SDFSException;
import com.sungardas.snapdirector.service.ConfigurationService;
import com.sungardas.snapdirector.service.StorageService;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;

@Service
public class SDFSServiceImpl implements StorageService {

	public static final Log LOG = LogFactory.getLog(SDFSServiceImpl.class);

	private String sdfs;
	private String mountPoint;

	@Autowired
	private ConfigurationService configurationService;

	@PostConstruct
	public void init() {
		WorkerConfiguration configuration = configurationService.getConfiguration();
		this.sdfs = configuration.getSdfsVolumeName();
		this.mountPoint = configuration.getSdfsMountPoint();
	}

	@Override
	public void deleteFile(String fileName) {
		Path path = Paths.get(mountPoint, fileName);
		File file = path.toFile();
		if (file.exists()) {
			file.delete();
		} else {
			LOG.error("File not found " + file.getAbsolutePath());
			throw new SDFSException("File not found " + file.getAbsolutePath());
		}
	}

	@Override
	public void copyFile(File sourceFile, File destFile) throws IOException {
		if (!destFile.exists()) {
			destFile.createNewFile();
		}

		FileChannel source = null;
		FileChannel destination = null;
		try {
			source = new FileInputStream(sourceFile).getChannel();
			destination = new FileOutputStream(destFile).getChannel();
			destination.transferFrom(source, 0, source.size());
		} finally {
			if (source != null) {
				source.close();
			}
			if (destination != null) {
				destination.close();
			}
		}
	}
	
	@Override
	public void copyFile(String sourceFileName, String destFileName) throws IOException {
		copyFile(new File(sourceFileName), new File(destFileName));
	}
	
	@Override
	public long getSize( String filename) {
		Path file = Paths.get(filename) ; 
		long size=-1;
		try {
			BasicFileAttributes attrs = Files.readAttributes(file, BasicFileAttributes.class);
			size= attrs.size();
		} catch (IOException e) { e.printStackTrace();}
		return size;
	}
	
	@Override
	public long getBackupCreationTime( String filename) {
		Path file = Paths.get(filename) ; 
		long timestamp=-1;
		try {
			BasicFileAttributes attrs = Files.readAttributes(file, BasicFileAttributes.class);
			timestamp= attrs.creationTime().toMillis();
		} catch (IOException e) { e.printStackTrace();}
		return timestamp;
	}

}
