package com.sungardas.enhancedsnapshots.components;

import com.amazonaws.AmazonClientException;
import com.amazonaws.services.sqs.AmazonSQS;
import com.amazonaws.services.sqs.model.DeleteMessageRequest;
import com.amazonaws.services.sqs.model.Message;
import com.amazonaws.services.sqs.model.ReceiveMessageRequest;
import com.amazonaws.services.sqs.model.ReceiveMessageResult;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.TaskEntry;
import com.sungardas.enhancedsnapshots.aws.dynamodb.model.WorkerConfiguration;
import com.sungardas.enhancedsnapshots.aws.dynamodb.repository.TaskRepository;
import com.sungardas.enhancedsnapshots.service.ConfigurationService;
import com.sungardas.enhancedsnapshots.service.SDFSStateService;
import com.sungardas.enhancedsnapshots.service.TaskService;
import com.sungardas.enhancedsnapshots.tasks.BackupTask;
import com.sungardas.enhancedsnapshots.tasks.DeleteTask;
import com.sungardas.enhancedsnapshots.tasks.RestoreTask;
import com.sungardas.enhancedsnapshots.tasks.Task;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.springframework.beans.factory.ObjectFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.context.annotation.DependsOn;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static com.sungardas.enhancedsnapshots.aws.dynamodb.model.TaskEntry.TaskEntryStatus.ERROR;
import static com.sungardas.enhancedsnapshots.aws.dynamodb.model.TaskEntry.TaskEntryStatus.RUNNING;
import static java.lang.String.format;

@Service
@DependsOn("CreateAppConfiguration")
public class WorkersDispatcher {
    @Autowired
    private ConfigurationService configurationService;

    @Autowired
    private AmazonSQS sqs;

    @Autowired
    private ObjectFactory<BackupTask> backupTaskObjectFactory;

    @Autowired
    private ObjectFactory<DeleteTask> deleteTaskObjectFactory;

    @Autowired
    private ObjectFactory<RestoreTask> restoreTaskObjectFactory;

    @Autowired
    private TaskService taskService;

    @Autowired
    private SDFSStateService sdfsStateService;

    @Autowired
    private TaskRepository taskRepository;

    @Value("${enhancedsnapshots.worker.maxNumberOfMessages}")
    private int maxNumberOfMessages;

    @Value("${enhancedsnapshots.polling.rate}")
    private int pollingRate;

    private WorkerConfiguration configuration;

    private ExecutorService executor;

    @PostConstruct
    private void init() {
        configuration = configurationService.getWorkerConfiguration();
        executor = Executors.newSingleThreadExecutor();
        executor.execute(new TaskWorker());
    }

    @PreDestroy
    public void destroy() {
        executor.shutdownNow();
    }

    private class TaskWorker implements Runnable {
        private final Logger LOGtw = LogManager.getLogger(TaskWorker.class);

        @Override
        public void run() {
            String queueURL = configuration.getTaskQueueURL();

            LOGtw.info(format("Starting listening to tasks queue: %s", queueURL));
            TaskEntry entry = null;
            while (true) {
                try {
                    ReceiveMessageRequest receiveMessageRequest = new ReceiveMessageRequest(queueURL);
                    receiveMessageRequest.setMaxNumberOfMessages(maxNumberOfMessages);
                    ReceiveMessageResult result = sqs.receiveMessage(receiveMessageRequest);
                    List<Message> messages = result.getMessages();
                    for (int i = 0; i < messages.size(); i++) {
                        Message message = messages.get(i);
                        String body = message.getBody();
                        LOGtw.info(format("Got message : %s", message.getMessageId()));
                        String messageRecieptHandle = message.getReceiptHandle();
                        sqs.deleteMessage(new DeleteMessageRequest(queueURL, messageRecieptHandle));
                        Task task = null;
                        entry = new TaskEntry(new JSONObject(body));
                        if (!taskService.isCanceled(entry.getId())) {
                            switch (TaskEntry.TaskEntryType.getType(entry.getType())) {
                                case BACKUP:
                                    LOGtw.info("Task was identified as backup");
                                    task = backupTaskObjectFactory.getObject();
                                    task.setTaskEntry(entry);
                                    break;
                                case DELETE: {
                                    LOGtw.info("Task was identified as delete backup");
                                    task = deleteTaskObjectFactory.getObject();
                                    task.setTaskEntry(entry);
                                    break;
                                }
                                case RESTORE:
                                    LOGtw.info("Task was identified as restore");
                                    task = restoreTaskObjectFactory.getObject();
                                    task.setTaskEntry(entry);
                                    break;
                                case SYSTEM_BACKUP: {
                                    LOGtw.info("Task was identified as system backup");
                                    entry.setStatus(RUNNING.getStatus());
                                    taskRepository.save(entry);
                                    sdfsStateService.backupState();
                                    taskRepository.delete(entry);
                                    break;
                                }
                                case UNKNOWN:
                                    LOGtw.warn("Executor for type {} is not implemented. Task {} is going to be removed.", entry.getType(), entry.getId());
                                    taskService.removeTask(entry.getId());
                            }
                        } else {
                            LOGtw.debug("Task canceled: {}", entry);
                        }
                        if (task != null) {
                            task.execute();
                        }
                    }
                    sleep();
                } catch (AmazonClientException e) {
                    // Skip amazon exceptions
                } catch (Exception e) {
                    LOGtw.error(e);
                    if(entry != null) {
                        entry.setStatus(ERROR.getStatus());
                        taskRepository.save(entry);
                    }
                    if (executor.isShutdown() || executor.isTerminated()) break;
                }
            }
        }

        private void sleep() {
            try {
                TimeUnit.MILLISECONDS.sleep(pollingRate);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }


}
