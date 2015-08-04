package com.sungardas.snapdirector.tasks;

import static java.lang.String.format;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.amazonaws.auth.AWSCredentialsProvider;


public class AWSBackupVolumeTask implements Task {
	//TODO: remove temporary counter, use dbdata instead
	public static int taskId = 0;
	
	public static final Log LOG = LogFactory.getLog(AWSBackupVolumeTask.class);
	private AWSCredentialsProvider awsCredentialsProvider;
	private String volumeId;
	private String routineInstanceId;
	private String propertyFile;


	public AWSBackupVolumeTask(AWSCredentialsProvider awsCredentialsProvider, String volumeId, String routineInstanceId,String propertyFile) {
		this.awsCredentialsProvider = awsCredentialsProvider;
		this.volumeId = volumeId;
		this.routineInstanceId = routineInstanceId;
		this.propertyFile = propertyFile;
	}


	public void execute() {
		LOG.info(format("AWSBackupVolumeTask[%d]: Starting backup process for volume %s",taskId, volumeId));
		
		
//		Properties properties= new Properties();
//		
//		try {
//		InputStream is = Thread.currentThread().getContextClassLoader().getResourceAsStream(propertyFile);
//			properties.load(is);
//		} catch (IOException e1) {
//			e1.printStackTrace();
//			return;
//		}
//		
//		properties.remove("ACCESS_KEY");
//		properties.remove("SECRET_KEY");
//		properties.remove("INSTANCE_ID");
//		properties.remove("VOLUME_TO_BACKUP");
//		properties.put("ACCESS_KEY", awsCredentialsProvider.getCredentials().getAWSAccessKeyId());
//		properties.put("SECRET_KEY", awsCredentialsProvider.getCredentials().getAWSSecretKey());
//		properties.put("INSTANCE_ID", routineInstanceId);
//		System.out.println(properties.get("INSTANCE_ID"));
//		properties.put("VOLUME_TO_BACKUP", volumeId);
//		try {
//		VolumeBackup.backupFlow(properties);
//		}catch(IOException e) {
//			e.printStackTrace();
//		}

		LOG.info(format("AWSBackupVolumeTask[%d]: Backup process for volume %s finished successfully ",taskId, volumeId));
	}

}
