package com.sungardas.snapdirector.dto.converter;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;

import com.sungardas.snapdirector.aws.dynamodb.model.Schedule;
import com.sungardas.snapdirector.dto.ScheduleDto;

public class ScheduleDtoConverter {

	public static ScheduleDto convert(Schedule schedule){
		ScheduleDto scheduleDto = new ScheduleDto();
		
		BeanUtils.copyProperties(schedule, scheduleDto);
		
		return scheduleDto;
	}
	
	public static List<ScheduleDto> convert(List<Schedule> schedules){
		
		List<ScheduleDto> dtoList = new ArrayList<ScheduleDto>();
		
		for (Schedule schedule : schedules) {
			dtoList.add(ScheduleDtoConverter.convert(schedule));
		}
		
		return dtoList;
	}
	
	public static Schedule convert(ScheduleDto scheduleDto){
		Schedule schedule = new Schedule();
		
		BeanUtils.copyProperties(scheduleDto, schedule);
		
		return schedule;
	}
	
}
