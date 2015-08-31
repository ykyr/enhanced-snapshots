package com.sungardas.snapdirector.service.actions;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry.TaskEntryStatus;
import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry.TaskEntryType;
import com.sungardas.snapdirector.dto.ScheduleDto;
import com.sungardas.snapdirector.dto.TaskDto;
import com.sungardas.snapdirector.service.ScheduleService;
import com.sungardas.snapdirector.service.TaskService;

@Service
public class ScheduleAction {

	private static final Logger LOG = LogManager.getLogger(ScheduleAction.class);
	private static SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
	
	@Autowired
	private ScheduleService scheduleService;

	@Autowired
	private TaskService taskService;

	public void createScheduledTasks() throws ParseException {

		populateTasks(getScheduledTasks());

	}

	private List<ScheduleDto> getScheduledTasks() {
		
		System.out.println("Attempting to find schedules to be fired now");
		
		List<ScheduleDto> schedulesToFire = scheduleService
				.findSchedulersToFire();

		LOG.info("Found %d schedules to fire", schedulesToFire.size());
		
		return schedulesToFire;
	}

	private void populateTasks(List<ScheduleDto> schedulesToFire) throws ParseException {
		
		for (ScheduleDto schedule : schedulesToFire) {
			TaskDto taskDto = new TaskDto();
			
			//TODO: remove all stubbed strings 
			taskDto.setInstanceId("STUB instance");
			taskDto.setPriority("0"); //STUB
			
			taskDto.setSchedulerName(schedule.getId());
			taskDto.setVolume(schedule.getVolumeId());
			taskDto.setType(TaskEntryType.BACKUP.getType());
			taskDto.setStatus(TaskEntryStatus.WAITING.getStatus());
			taskDto.setSchedulerTime(Long.valueOf(format.format(new Date(schedule.getNextFire()))).toString());
			taskDto.setSchedulerManual("false");
			
			taskService.createTask(taskDto);
			
			scheduleService.setNewFireTime(schedule);
			
			
		}
	}

}
