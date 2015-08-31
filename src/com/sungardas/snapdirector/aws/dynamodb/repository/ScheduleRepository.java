package com.sungardas.snapdirector.aws.dynamodb.repository;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import com.sungardas.snapdirector.aws.dynamodb.model.Schedule;

public interface ScheduleRepository extends CrudRepository<Schedule, String> {

	List<Schedule> findByEnabled(Boolean enabled);
	
	List<Schedule> findByEnabledAndNextFireLessThanEqual(Boolean enabled, Long nextFire);
}
