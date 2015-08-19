package com.sungardas.snapdirector.dto.converter;

import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.dto.TaskDto;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.BeanUtils;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;


public class TaskDtoConverter {

	private static final Logger LOG = LogManager.getLogger(TaskDtoConverter.class);
	private static SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");

	public static TaskDto convert(TaskEntry task) {
		TaskDto taskDto = new TaskDto();
		BeanUtils.copyProperties(task, taskDto);
		try {
			taskDto.setSchedulerTime(Long.valueOf(format.parse(task.getSchedulerTime()).getTime()).toString());
		} catch (Exception e) {
			LOG.error("Failed to parse scheduler time. This field will be empty in task dto object");
		}
		return taskDto;
	}

	public static TaskEntry convert(TaskDto taskDto) {
		TaskEntry task = new TaskEntry();
		BeanUtils.copyProperties(taskDto, task);
		return task;
	}

	public static List<TaskDto> convert(Iterable<TaskEntry> taskEntries) {
		List<TaskDto> dtos = new ArrayList<>();
		for (TaskEntry task : taskEntries) {
			dtos.add(convert(task));
		}
		return dtos;
	}
}
