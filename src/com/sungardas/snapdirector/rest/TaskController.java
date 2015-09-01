package com.sungardas.snapdirector.rest;

import com.sungardas.snapdirector.dto.TaskDto;
import com.sungardas.snapdirector.exception.SnapdirectorException;
import com.sungardas.snapdirector.service.TaskService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.text.ParseException;

import static org.springframework.http.HttpStatus.OK;


@RestController
@RequestMapping("/task")
public class TaskController {

    @Autowired
    private TaskService taskService;

    @RequestMapping(method = RequestMethod.GET)
    public ResponseEntity getTasks() throws ParseException {
        try {
            return new ResponseEntity(taskService.getAllTasks(), OK);
        } catch (SnapdirectorException e) {
            return new ResponseEntity(e.getMessage(), HttpStatus.NO_CONTENT);
        }
    }

    @RequestMapping(method = RequestMethod.GET, value = "/{valueId}")
    public ResponseEntity getRegularTasks(@PathVariable String valueId) throws ParseException {
        try {
            return new ResponseEntity(taskService.getAllRegularTasks(valueId), OK);
        } catch (SnapdirectorException e) {
            return new ResponseEntity(e.getMessage(), HttpStatus.NO_CONTENT);
        }
    }

    @RequestMapping(method = RequestMethod.POST)
    public ResponseEntity addTask(@RequestBody TaskDto taskInfo) {
        try {
            taskService.createTask(taskInfo);
            return new ResponseEntity("", OK);
        } catch (SnapdirectorException e) {
            return new ResponseEntity(e.getMessage(), HttpStatus.NO_CONTENT);
        }
    }

    @RequestMapping(method = RequestMethod.PUT)
    public ResponseEntity updateTask(@RequestBody TaskDto taskInfo) {
        try {
            taskService.updateTask(taskInfo);
            return new ResponseEntity("", OK);
        } catch (SnapdirectorException e) {
            return new ResponseEntity(e.getMessage(), HttpStatus.NO_CONTENT);
        }
    }

    @RequestMapping(value = "/{taskId}", method = RequestMethod.DELETE)
    @ResponseStatus(OK)
    public void removeTask(@PathVariable String taskId) {
        taskService.removeTask(taskId);
    }
}
