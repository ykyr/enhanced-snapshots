package com.sungardas.snapdirector.service;

import java.util.List;

import com.sungardas.snapdirector.aws.dynamodb.model.ScheduleEntry;

public interface ScheduleService {

	List<ScheduleEntry> getEnabledSchedules();
	
	
}
