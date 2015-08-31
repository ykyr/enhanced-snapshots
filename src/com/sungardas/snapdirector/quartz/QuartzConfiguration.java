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
	public JobDetail someJobDetails() {
		JobDetail jobDetail = JobBuilder.newJob(SchedulerJob.class)
				.withIdentity("Some job name", "Some job group")
				.storeDurably(true).build();

		return jobDetail;
	}

	@Bean
	SchedulerFactoryBean schedulerFactoryBean() {
		SchedulerFactoryBean schedulerFactoryBean = new SchedulerFactoryBean();
		AutowiringSpringBeanJobFactory springBeanJobFactory = new AutowiringSpringBeanJobFactory();
		springBeanJobFactory.setApplicationContext(applicationContext);
		schedulerFactoryBean.setJobDetails(someJobDetails());
		schedulerFactoryBean.setTriggers(someTrigger());
		return schedulerFactoryBean;
	}

	@Bean
	public Trigger someTrigger() {
		Trigger trigger = TriggerBuilder
				.newTrigger()
				.withIdentity("Some job name", "Some job group")
				.withSchedule(
						CronScheduleBuilder
								.cronSchedule(cronExpressionForStart))
				.forJob(someJobDetails().getKey()).build();
		return trigger;
	}
}
