package com.sungardas.snapdirector.dto.converter;

import com.sungardas.snapdirector.aws.dynamodb.model.TaskEntry;
import com.sungardas.snapdirector.dto.TaskDto;

public class TaskDtoConverter {
	
	private TaskDtoConverter(){}
	
	public static TaskDto convert(TaskEntry taskEntry){
		TaskDto taskDto = new TaskDto();
		
		//taskDto.setId(id);
		taskDto.setInstanceid(taskEntry.getInstanceId());
		//taskDto.setOptions(taskEntry.ge);
		
		return taskDto;
	}
	
	public static TaskEntry convert(TaskDto taskDto){
		return null;
	}
}
