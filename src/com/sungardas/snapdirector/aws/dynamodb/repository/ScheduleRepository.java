package com.sungardas.snapdirector.aws.dynamodb.repository;

import java.util.List;

import org.socialsignin.spring.data.dynamodb.repository.EnableScan;
import org.springframework.data.repository.CrudRepository;

import com.sungardas.snapdirector.aws.dynamodb.model.Schedule;

@EnableScan
public interface ScheduleRepository extends CrudRepository<Schedule, String> {

	List<Schedule> findByEnabled(Boolean enabled);
	
	List<Schedule> findByNextFireLessThanEqual(Long nextFire);
}
