package com.sungardas.snapdirector.quartz;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.springframework.beans.factory.annotation.Autowired;

import com.sungardas.snapdirector.service.actions.ScheduleAction;

public class SchedulerJob implements Job {

	private static final Logger LOG = LogManager.getLogger(SchedulerJob.class);
	
	@Autowired
	private ScheduleAction scheduleAction;
	
	@Override
	public void execute(JobExecutionContext context)
			throws JobExecutionException {
		

	}

}
