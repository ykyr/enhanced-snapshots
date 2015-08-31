package com.sungardas.snapdirector.service;

import java.text.ParseException;
import java.util.List;

import com.sungardas.snapdirector.dto.ScheduleDto;


public interface ScheduleService {

	ScheduleDto findById(String id);
	
	List<ScheduleDto> findSchedulersToFire();
	
	void setNewFireTime(ScheduleDto newSchedule) throws ParseException;
	
}
