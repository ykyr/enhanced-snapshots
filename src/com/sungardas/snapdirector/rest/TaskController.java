package com.sungardas.snapdirector.rest;

import java.text.ParseException;

import com.sungardas.snapdirector.dto.TaskDto;
import com.sungardas.snapdirector.exception.SnapdirectorException;
import com.sungardas.snapdirector.service.TaskService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;


@RestController
@RequestMapping("/task")
public class TaskController {

	@Autowired
	private TaskService taskService;

	@RequestMapping(method = RequestMethod.GET)
	public ResponseEntity getTasks() throws ParseException {
		try {
			return new ResponseEntity(taskService.getAllTasks(), HttpStatus.OK);
		} catch (SnapdirectorException e) {
			return new ResponseEntity(e.getMessage(), HttpStatus.NO_CONTENT);
		}
	}

	@RequestMapping(method = RequestMethod.POST)
	public ResponseEntity addTask(@RequestBody TaskDto taskInfo) {
		try {
			taskService.createTask(taskInfo);
			return new ResponseEntity("", HttpStatus.OK);
		} catch (SnapdirectorException e) {
			return new ResponseEntity(e.getMessage(), HttpStatus.NO_CONTENT);
		}
	}
}
