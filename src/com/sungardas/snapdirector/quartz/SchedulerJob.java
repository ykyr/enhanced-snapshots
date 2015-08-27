package com.sungardas.snapdirector.quartz;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;

public class SchedulerJob implements Job {

	private static final Logger LOG = LogManager.getLogger(SchedulerJob.class);
	
	@Override
	public void execute(JobExecutionContext context)
			throws JobExecutionException {
		System.out.println("SchedulerJob Fired!!!");

	}

}
