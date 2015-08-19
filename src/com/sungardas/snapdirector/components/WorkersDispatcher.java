package com.sungardas.snapdirector.components;

import static java.lang.String.format;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.springframework.beans.factory.ObjectFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.model.DeleteMessageRequest;
import com.amazonaws.services.sqs.model.Message;
import com.amazonaws.services.sqs.model.ReceiveMessageRequest;
import com.amazonaws.services.sqs.model.ReceiveMessageResult;
import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.snapdirector.aws.dynamodb.repository.WorkerConfigurationRepository;
import com.sungardas.snapdirector.tasks.BackupTask;
import com.sungardas.snapdirector.tasks.DeleteTask;
import com.sungardas.snapdirector.tasks.RestoreTask;
import com.sungardas.snapdirector.tasks.Task;

@Component
public class WorkersDispatcher {
	@Autowired
	private WorkerConfigurationRepository confRepository;
	@Value("${amazon.aws.region}")
	private String region;
	@Value("${sungardas.worker.configuration}")
	private String configurationId;
	@Autowired
	private AmazonSQS sqs;
	@Autowired
    private ObjectFactory<BackupTask> backupTaskObjectFactory;

	@Autowired
    private ObjectFactory<DeleteTask> deleteTaskObjectFactory;
	
	@Autowired
    private ObjectFactory<RestoreTask> restoreTaskObjectFactory;
	

	private WorkerConfiguration configuration;
	private ExecutorService  executor;

	@PostConstruct
	private void init() {
		configuration = confRepository.findOne(configurationId);
		executor = Executors.newSingleThreadExecutor();
		executor.execute(new TaskWorker());
	}

	@PreDestroy
	public void destroy() {
		executor.shutdownNow();
	}

	private class TaskWorker implements Runnable {
		private  final Logger LOGtw = LogManager.getLogger(TaskWorker.class);

		@Override
		public void run() {
			String queueURL = configuration.getTaskQueueURL();

			LOGtw.info(format("Starting listening to tasks queue: %s", queueURL));

			while (true) {

				//LOGtw.info("\n\nLook for sended tasks..");
				ReceiveMessageRequest receiveMessageRequest = new ReceiveMessageRequest(queueURL);
				ReceiveMessageResult result = sqs.receiveMessage(receiveMessageRequest);
				List<Message> messages = result.getMessages();
				if (messages.size() > 0) {
					String body = messages.get(0).getBody();
					LOGtw.info(format("Got message : %s", messages.get(0).getMessageId()));
					String messageRecieptHandle = messages.get(0).getReceiptHandle();
					sqs.deleteMessage(new DeleteMessageRequest(queueURL, messageRecieptHandle));

					Task task = null;
					TaskEntry entry = new TaskEntry(new JSONObject(body));
					switch (entry.getType()) {
					case "backup":
						LOGtw.info("Task was identified as backup");
						task = backupTaskObjectFactory.getObject();
						task.setTaskEntry(entry);
						break;
					case "restore":
						LOGtw.info("Task was identified as restore");
						task= restoreTaskObjectFactory.getObject();
						task.setTaskEntry(entry);
						break;
					case "deleteBackupfile": {
						LOGtw.info("Task was identified as delete backup");
                        task = deleteTaskObjectFactory.getObject();
                        task.setTaskEntry(entry);
						break;
					}
					default:
						LOGtw.info("Task type not implemented");
					}

					if (task != null) {
						task.execute();
					}

				}
				sleep();
			}
		}

		private void sleep() {
			try {
				TimeUnit.SECONDS.sleep(20);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}


}
