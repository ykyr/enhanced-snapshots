package com.sungardas.snapdirector.dto.converter;

import org.springframework.beans.BeanUtils;

import com.sungardas.snapdirector.aws.dynamodb.model.Schedule;
import com.sungardas.snapdirector.dto.ScheduleDto;

public class ScheduleDtoConverter {

	public static ScheduleDto convert(Schedule schedule){
		ScheduleDto scheduleDto = new ScheduleDto();
		
		BeanUtils.copyProperties(schedule, scheduleDto);
		
		return scheduleDto;
	}
	
	public static Schedule convert(ScheduleDto scheduleDto){
		Schedule schedule = new Schedule();
		
		BeanUtils.copyProperties(scheduleDto, schedule);
		
		return schedule;
	}
	
}
