package com.sungardas.snapdirector.aws.dynamodb.repository;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import com.sungardas.snapdirector.aws.dynamodb.model.ScheduleEntry;

public interface ScheduleRepository extends CrudRepository<ScheduleEntry, String> {

	List<ScheduleEntry> findByEnabled(Boolean enabled);
	
}
