package com.sungardas.snapdirector.quartz;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.quartz.CronScheduleBuilder;
import org.quartz.JobBuilder;
import org.quartz.JobDetail;
import org.quartz.Trigger;
import org.quartz.TriggerBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.quartz.SchedulerFactoryBean;

@Configuration
public class QuartzConfiguration {

	private static final Logger LOG = LogManager
			.getLogger(QuartzConfiguration.class);

	@Autowired
	private ApplicationContext applicationContext;

	@Value("${scheduler.cron.start}")
	private String cronExpressionForStart;

	/*
	 * @Value("${scheduler.timezone}") private String timeZone;
	 */

	@Bean
	public JobDetail schedulerJobDetails() {
		JobDetail jobDetail = JobBuilder.newJob(SchedulerJob.class)
				.withIdentity("Scheduler Job", "Scheduler Job group")
				.storeDurably(true).build();

		return jobDetail;
	}

	@Bean
	SchedulerFactoryBean schedulerFactoryBean() {
		SchedulerFactoryBean schedulerFactoryBean = new SchedulerFactoryBean();
		AutowiringSpringBeanJobFactory springBeanJobFactory = new AutowiringSpringBeanJobFactory();
		springBeanJobFactory.setApplicationContext(applicationContext);
		schedulerFactoryBean.setJobFactory(springBeanJobFactory);
		schedulerFactoryBean.setJobDetails(schedulerJobDetails());
		schedulerFactoryBean.setTriggers(schedulerTrigger());
		return schedulerFactoryBean;
	}

	@Bean
	public Trigger schedulerTrigger() {
		Trigger trigger = TriggerBuilder
				.newTrigger()
				.withIdentity("Scheduler Job", "Scheduler Job group")
				.withSchedule(
						CronScheduleBuilder
								.cronSchedule(cronExpressionForStart))
				.forJob(schedulerJobDetails().getKey()).build();
		return trigger;
	}
}
