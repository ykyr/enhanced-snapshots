package com.sungardas.snapdirector.service.actions;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.sungardas.snapdirector.service.ScheduleService;
import com.sungardas.snapdirector.service.TaskService;

@Service
public class ScheduleAction {

	@Autowired
	private ScheduleService scheduleService;
	
	@Autowired
	private TaskService taskService;
	
	
	
}
