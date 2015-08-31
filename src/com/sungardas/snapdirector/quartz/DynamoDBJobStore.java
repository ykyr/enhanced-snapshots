package com.sungardas.snapdirector.quartz;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.quartz.Calendar;
import org.quartz.JobDetail;
import org.quartz.JobKey;
import org.quartz.JobPersistenceException;
import org.quartz.ObjectAlreadyExistsException;
import org.quartz.SchedulerConfigException;
import org.quartz.SchedulerException;
import org.quartz.Trigger;
import org.quartz.Trigger.CompletedExecutionInstruction;
import org.quartz.Trigger.TriggerState;
import org.quartz.TriggerKey;
import org.quartz.impl.matchers.GroupMatcher;
import org.quartz.spi.ClassLoadHelper;
import org.quartz.spi.JobStore;
import org.quartz.spi.OperableTrigger;
import org.quartz.spi.SchedulerSignaler;
import org.quartz.spi.TriggerFiredResult;

public class DynamoDBJobStore implements JobStore {

	@Override
	public void initialize(ClassLoadHelper loadHelper,
			SchedulerSignaler signaler) throws SchedulerConfigException {
		// TODO Auto-generated method stub

	}

	@Override
	public void schedulerStarted() throws SchedulerException {
		// TODO Auto-generated method stub

	}

	@Override
	public void schedulerPaused() {
		// TODO Auto-generated method stub

	}

	@Override
	public void schedulerResumed() {
		// TODO Auto-generated method stub

	}

	@Override
	public void shutdown() {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean supportsPersistence() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public long getEstimatedTimeToReleaseAndAcquireTrigger() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public boolean isClustered() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void storeJobAndTrigger(JobDetail newJob, OperableTrigger newTrigger)
			throws ObjectAlreadyExistsException, JobPersistenceException {
		// TODO Auto-generated method stub

	}

	@Override
	public void storeJob(JobDetail newJob, boolean replaceExisting)
			throws ObjectAlreadyExistsException, JobPersistenceException {
		// TODO Auto-generated method stub

	}

	@Override
	public void storeJobsAndTriggers(
			Map<JobDetail, Set<? extends Trigger>> triggersAndJobs,
			boolean replace) throws ObjectAlreadyExistsException,
			JobPersistenceException {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean removeJob(JobKey jobKey) throws JobPersistenceException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean removeJobs(List<JobKey> jobKeys)
			throws JobPersistenceException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public JobDetail retrieveJob(JobKey jobKey) throws JobPersistenceException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void storeTrigger(OperableTrigger newTrigger, boolean replaceExisting)
			throws ObjectAlreadyExistsException, JobPersistenceException {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean removeTrigger(TriggerKey triggerKey)
			throws JobPersistenceException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean removeTriggers(List<TriggerKey> triggerKeys)
			throws JobPersistenceException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean replaceTrigger(TriggerKey triggerKey,
			OperableTrigger newTrigger) throws JobPersistenceException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public OperableTrigger retrieveTrigger(TriggerKey triggerKey)
			throws JobPersistenceException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean checkExists(JobKey jobKey) throws JobPersistenceException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean checkExists(TriggerKey triggerKey)
			throws JobPersistenceException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void clearAllSchedulingData() throws JobPersistenceException {
		// TODO Auto-generated method stub

	}

	@Override
	public void storeCalendar(String name, Calendar calendar,
			boolean replaceExisting, boolean updateTriggers)
			throws ObjectAlreadyExistsException, JobPersistenceException {
		// TODO Auto-generated method stub

	}

	@Override
	public boolean removeCalendar(String calName)
			throws JobPersistenceException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public Calendar retrieveCalendar(String calName)
			throws JobPersistenceException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int getNumberOfJobs() throws JobPersistenceException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getNumberOfTriggers() throws JobPersistenceException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int getNumberOfCalendars() throws JobPersistenceException {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public Set<JobKey> getJobKeys(GroupMatcher<JobKey> matcher)
			throws JobPersistenceException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Set<TriggerKey> getTriggerKeys(GroupMatcher<TriggerKey> matcher)
			throws JobPersistenceException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<String> getJobGroupNames() throws JobPersistenceException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<String> getTriggerGroupNames() throws JobPersistenceException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<String> getCalendarNames() throws JobPersistenceException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<OperableTrigger> getTriggersForJob(JobKey jobKey)
			throws JobPersistenceException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public TriggerState getTriggerState(TriggerKey triggerKey)
			throws JobPersistenceException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void pauseTrigger(TriggerKey triggerKey)
			throws JobPersistenceException {
		// TODO Auto-generated method stub

	}

	@Override
	public Collection<String> pauseTriggers(GroupMatcher<TriggerKey> matcher)
			throws JobPersistenceException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void pauseJob(JobKey jobKey) throws JobPersistenceException {
		// TODO Auto-generated method stub

	}

	@Override
	public Collection<String> pauseJobs(GroupMatcher<JobKey> groupMatcher)
			throws JobPersistenceException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void resumeTrigger(TriggerKey triggerKey)
			throws JobPersistenceException {
		// TODO Auto-generated method stub

	}

	@Override
	public Collection<String> resumeTriggers(GroupMatcher<TriggerKey> matcher)
			throws JobPersistenceException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Set<String> getPausedTriggerGroups() throws JobPersistenceException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void resumeJob(JobKey jobKey) throws JobPersistenceException {
		// TODO Auto-generated method stub

	}

	@Override
	public Collection<String> resumeJobs(GroupMatcher<JobKey> matcher)
			throws JobPersistenceException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void pauseAll() throws JobPersistenceException {
		// TODO Auto-generated method stub

	}

	@Override
	public void resumeAll() throws JobPersistenceException {
		// TODO Auto-generated method stub

	}

	@Override
	public List<OperableTrigger> acquireNextTriggers(long noLaterThan,
			int maxCount, long timeWindow) throws JobPersistenceException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void releaseAcquiredTrigger(OperableTrigger trigger) {
		// TODO Auto-generated method stub

	}

	@Override
	public List<TriggerFiredResult> triggersFired(List<OperableTrigger> triggers)
			throws JobPersistenceException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void triggeredJobComplete(OperableTrigger trigger,
			JobDetail jobDetail, CompletedExecutionInstruction triggerInstCode) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setInstanceId(String schedInstId) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setInstanceName(String schedName) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setThreadPoolSize(int poolSize) {
		// TODO Auto-generated method stub

	}

}
