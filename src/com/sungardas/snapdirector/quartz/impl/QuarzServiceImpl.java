package com.sungardas.snapdirector.quartz.impl;

import javax.annotation.PostConstruct;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.quartz.JobBuilder;
import org.quartz.JobDetail;
import org.quartz.Scheduler;
import org.quartz.SimpleScheduleBuilder;
import org.quartz.Trigger;
import org.quartz.TriggerBuilder;
import org.quartz.impl.StdSchedulerFactory;
import org.springframework.stereotype.Service;


import com.sungardas.snapdirector.quartz.SchedulerJob;


public class QuarzServiceImpl {

	private static final Logger LOG = LogManager
			.getLogger(QuarzServiceImpl.class);

	
	private void init() {

		/*System.out.println("Quarz service started");
		
		StdSchedulerFactory factory = new StdSchedulerFactory();
		try {
			Scheduler scheduler = factory.getScheduler();
			JobDetail jobDetail = JobBuilder.newJob(SchedulerJob.class)
					.build();
			// Trigger trigger =
			// TriggerBuilder.newTrigger().withIdentity("simple").withSchedule(
			// CronScheduleBuilder.cronSchedule("0 0/1 * 1/1 * ? *")).startNow().build();

			Trigger trigger = TriggerBuilder
					.newTrigger()
					.withIdentity("simple")
					.withSchedule(
							SimpleScheduleBuilder.repeatSecondlyForever(5))
					.startNow().build();

			scheduler.scheduleJob(jobDetail, trigger);
			scheduler.start();
		} catch (Exception e) {
			LOG.error("There was an error scheduling the job.");
		}*/
	}

}
