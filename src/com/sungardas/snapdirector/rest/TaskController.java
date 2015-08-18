package com.sungardas.snapdirector.rest;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.List;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.WebApplicationException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.sungardas.snapdirector.aws.EnvironmentBasedCredentialsProvider;
import com.sungardas.snapdirector.aws.dynamodb.DynamoUtils;
import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.service.ConfigurationService;

@RestController
@RequestMapping("/task")
public class TaskController {
	@Value("${sungardas.worker.configuration}")
	private String configurationId;

    private static final Logger LOG = LogManager.getLogger(TaskController.class);

    @Autowired
    private ServletContext context;

    @Autowired
    private HttpServletRequest servletRequest;

    @Autowired
    private ConfigurationService configuration;

    @RequestMapping(method = RequestMethod.GET)
    public String getTasks() throws ParseException {
        try {

            List<TaskEntry> taskModels = DynamoUtils.getTasks(getMapper());
            SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
            JSONArray tasks = new JSONArray();
            for (TaskEntry nextTask : taskModels) {
                JSONObject jsonTask = new JSONObject();
                jsonTask.put("id", nextTask.getId());
                jsonTask.put("priority", nextTask.getPriority());
                jsonTask.put("schedulerManual", Boolean.valueOf(nextTask.getSchedulerManual()));
                jsonTask.put("schedulerName", nextTask.getSchedulerName());
                jsonTask.put("schedulerTime", Long.valueOf(format.parse(nextTask.getSchedulerTime()).getTime()));
                jsonTask.put("status", nextTask.getStatus());
                jsonTask.put("type", nextTask.getType());
                jsonTask.put("volume", nextTask.getVolume());
                jsonTask.put("instanceId", nextTask.getInstanceId());
                tasks.put(jsonTask);
            }
            return tasks.toString();
        } catch (Exception e) {
            throw new WebApplicationException(e);
        }
    }

    @RequestMapping(method = RequestMethod.POST)
    public String addTask(@RequestBody String taskJsonString) {
    	String configurationId = configuration.getConfiguration().getConfigurationId();
        try {
            JSONObject jsonTask = new JSONObject(taskJsonString);
            jsonTask.put("worker", configurationId);
            jsonTask.put("instanceId", configurationId);
            TaskEntry task = new TaskEntry(jsonTask);
            DynamoUtils.putTask(task, getMapper());
            return null;

        } catch (Exception e) {
            throw new WebApplicationException(e);
        }
    }

    private DynamoDBMapper getMapper() {
        AmazonDynamoDBClient client = new AmazonDynamoDBClient(new EnvironmentBasedCredentialsProvider());
        String region = configuration.getConfiguration().getEc2Region();
        client.setRegion(Region.getRegion(Regions.fromName(region)));
        return new DynamoDBMapper(client);
    }
}
