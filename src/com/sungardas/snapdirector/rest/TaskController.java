package com.sungardas.snapdirector.rest;

import com.amazonaws.regions.Region;
import com.amazonaws.regions.Regions;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.sungardas.snapdirector.aws.EnvironmentBasedCredentialsProvider;
import com.sungardas.snapdirector.aws.dynamodb.DynamoUtils;
import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.WebApplicationException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.List;

@RestController
@RequestMapping("/task")
public class TaskController {

    private static final Logger LOG = LogManager.getLogger(TaskController.class);

    @Autowired
    private ServletContext context;

    @Autowired
    private HttpServletRequest servletRequest;


    @RequestMapping(method = RequestMethod.GET)
    public String getTasks() throws ParseException {
        try {

            List<TaskEntry> taskModels = DynamoUtils.getTasks(getMapper(servletRequest));
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
                tasks.put(jsonTask);
            }
            return tasks.toString();
        } catch (Exception e) {
            throw new WebApplicationException(e);
        }
    }

    @RequestMapping(method = RequestMethod.POST)
    public String addTask(String taskJsonString) {
        try {
            JSONObject jsonTask =new JSONObject(taskJsonString);
            jsonTask.put("worker", context.getInitParameter("aws:routine-inst-id"));
            TaskEntry task = new TaskEntry(jsonTask);
            DynamoUtils.putTask(task, getMapper(servletRequest));
            return null;

        } catch (Exception e) {
            throw new WebApplicationException(e);
        }
    }

    private DynamoDBMapper getMapper(ServletRequest request) {
        AmazonDynamoDBClient client = new AmazonDynamoDBClient(new EnvironmentBasedCredentialsProvider());
        String region = request.getServletContext().getInitParameter("aws:dynamodb-region");
        client.setRegion(Region.getRegion(Regions.fromName(region)));
        return new DynamoDBMapper(client);
    }
}
